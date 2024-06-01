with TC.Picking;

with TC.GWin.Display,
     TC.GWin.Lang,
     TC.GWin.MDI_Main,
     TC.GWin.Morphing,
     TC.GWin.New_objects,
     TC.GWin.Object_Editing,
     TC.GWin.Phantoms;

with GWindows.Base;

with Ada.Strings.Fixed;

package body TC.GWin.Mousing is

  procedure Show_coordinates (w : TC_Picture_Panel) is
    function FullCoord return GString is
      function Coord (x, y : Integer; P : Point) return GString is
        pragma Warnings (Off, x);
        pragma Warnings (Off, y);
        sx, sy : String (1 .. 20);
        use Ada.Strings, Ada.Strings.Fixed;
      begin
        RIO.Put (sx, P.x, 2, 0);
        RIO.Put (sy, P.y, 2, 0);
        return
          S2G ('(' & Trim (sx, Left) & ',' & Trim (sy, Left) & ')')
          --  & " = [" & integer'image(x) & ',' & integer'image(y) & ']'
          --  ^ Verification with mouse coords.
        ;
      end Coord;
    begin
      if (w.X /= w.Xs or w.Y /= w.Ys) and w.capture /= bez_click2 then
        return Coord (w.Xs, w.Ys, w.PS)      & " > " &
               Coord (w.X, w.Y, w.PU)      & ": "  &
               Coord (w.Xs, w.Ys, w.PU - w.PS);
      else
        return Coord (w.X, w.Y, w.PU);
      end if;
    end FullCoord;
  begin
    w.main.Update_Status_Bar (MDI_Main.coords, FullCoord);
  end Show_coordinates;

  last_shown_mode : Capture_mode := none;

  use Lang;

  msg_for_mode : constant array (Capture_mode) of Message :=
    (none                        => ready,
     pick | unpick               => expl_pick,
     select_area | unselect_area => mouse_drag,
     click_1                     => empty,
     figure_2                    => mouse_drag,
     click_2                     => bez_pt2,
     bez_click0                  => empty,
     bez_click1                  => bez_pt2,
     bez_click2                  => bez_ptc,
     paste0 | paste1             => m_paste);

  procedure Show_mouse_mode (w : TC_Picture_Panel) is
  begin
    if last_shown_mode /= w.capture then
      last_shown_mode := w.capture;
      w.main.Update_Status_Bar (MDI_Main.comment, Msg (msg_for_mode (w.capture)));
    end if;
  end Show_mouse_mode;

  snappable : constant array (Capture_mode) of Boolean :=
    (none .. unselect_area => False, others => True);

  procedure Tranform_coordinates (w : in out TC_Picture_Panel) is
    YM : constant Integer :=  Height (w) - 1;
    snap : constant Boolean := snappable (w.capture) and w.Picture.opt.snapping;
    s, ivs : Real;

    procedure P2U (px, py : Integer; PU : out TC.Point) is
    begin
      Display.Pixels_to_Units (w.Picture, px, py, YM, PU);
      if snap then
        PU := s * (Real'Floor (PU.x * ivs), Real'Floor (PU.y * ivs));
      end if;
    end P2U;
  begin
    if snap then
      s := Real (w.Picture.opt.snap_asp);
      ivs := 1.0 / s;
    end if;
    if w.capture /= click_2 then
      P2U (w.Xs, w.Ys, w.PS);
      --  For click_2 (can be \line,\vector), w.PS has been set in math plane
    end if;
    P2U (w.X, w.Y,  w.PU);
    P2U (w.Xb, w.Yb, w.PE);
  end Tranform_coordinates;

  procedure Change_Cursor (w : in out TC_Picture_Panel; cur : Cursor_Type) is
  begin
    w.Cursor := cur;
    Set_Cursor (cur);
  end Change_Cursor;

  procedure Mouse_Down (w    : in out TC_Picture_Panel;
                        X, Y : in     Integer;
                        Btn  : in     GWindows.Windows.Mouse_Keys)
  is
    new_Capture : Capture_mode;
    use GWindows.Windows, Phantoms;
  begin
    Capture_Mouse (w);
    w.X := X;
    w.Y := Y;
    if w.capture = none or w.capture = paste0 then
      w.Xs := w.X;
      w.Ys := w.Y;
    end if;
    Tranform_coordinates (w);
    if w.capture = paste0 then
      case Btn is
        when Left_Button  => w.capture := paste1;
        when Right_Button => w.capture := none;  --  cancelled;
        when others => null;
      end case;
    else
      w.phantom_ls := w.current_ls;
      case w.current_cmd is
        when pick_obj =>
          case Btn is
            when Left_Button  =>
              w.capture := pick;
              Change_Cursor (w, cur_pick);
            when Right_Button =>
              w.capture := unpick;
              Change_Cursor (w, cur_unpick);
            when others => null;
          end case;
        when Drawing_cmd =>
          if Btn = Left_Button then
            new_Capture := figure_2;
            case Drawing_cmd (w.current_cmd) is
              when Box_cmd    =>
                w.phantomart := box;
              when line =>
                w.phantomart := line;
                case w.capture is
                  when none =>
                    null;  --  normal figure_2
                  when click_2 =>
                    new_Capture := w.capture; -- handled on button release
                  when others =>
                    null;  --  never seen
                end case;
              when circle        => w.phantomart := circ;
              when filled_circle => w.phantomart := disc;
              when oval          => w.phantomart := oval;
              when bez =>
                case w.capture is
                  when none       => new_Capture := bez_click0;
                  when bez_click1 |
                       bez_click2 => new_Capture := w.capture;
                  when others => null;
                end case;
              when text | put | par_cur_2d_cmd =>
                new_Capture := click_1;
                w.phantomart := txt;
            end case;
            if new_Capture in click_1 .. figure_2 then
              Invert_Phantom (w);  --  Show
            end if;
            w.capture := new_Capture;
          elsif Btn = Right_Button then  --  Cancel draw operation
            w.capture := none;
            GWindows.Base.Release_Mouse;
            w.Redraw;
          end if;
        when change_text =>
          if Btn = Left_Button then
            w.capture := pick;
          end if;
        when Deformation_cmd =>
          if Btn = Left_Button then
            w.capture := figure_2;
            case Deformation_cmd (w.current_cmd) is
              when translate =>
                w.phantomart := line;
                w.phantom_ls := normal_line_settings;
                w.phantom_ls.arrows := head;
              when rotate | mirror | homoth =>
                w.capture := click_1;
                w.phantomart := txt;
            end case;
            Invert_Phantom (w);  --  Show
          end if;
      end case;
    end if;
    Show_mouse_mode (w);
  end Mouse_Down;

  procedure Mouse_Move (w    : in out TC_Picture_Panel;
                        X, Y : in     Integer)
  is
    procedure Scroll_if_needed is
      X00, Y00 : Integer;
      use GWindows.Base;
    begin
      X00 := w.X0;
      Y00 := w.Y0;
      if X < w.X0 then
        On_Horizontal_Scroll (w.pic_parent.all, Previous_Unit, null);
      elsif X > w.Disp_W + w.X0 then
        On_Horizontal_Scroll (w.pic_parent.all, Next_Unit, null);
      end if;

      if  Y < w.Y0 then
        On_Vertical_Scroll (w.pic_parent.all, Previous_Unit, null);
      elsif Y > w.Disp_H + w.Y0 then
        On_Vertical_Scroll (w.pic_parent.all, Next_Unit, null);
      end if;
      if X00 /= w.X0 or Y00 /= w.Y0 then
        Redraw (w, Redraw_Now => True);
        Tranform_coordinates (w);
      end if;
    end Scroll_if_needed;

    dist : Integer;
    dist_max : constant := 3**2;
    use Phantoms;

  begin  --  Mouse_Move
    case w.capture is
      when select_area   => Invert_Rubber_Box (w, picked);  --  Hide
      when unselect_area => Invert_Rubber_Box (w, normal);  --  Hide
      when click_1 |
           figure_2 |
           click_2 |
           bez_click2 => Invert_Phantom (w);  --  Hide
      when others => null;
    end case;
    w.X := X;
    w.Y := Y;
    Tranform_coordinates (w);
    dist := (w.X - w.Xs)**2 + (w.Y - w.Ys)**2;
    case w.capture is
      when none | paste0 | paste1 =>
        w.Xs := w.X;
        w.Ys := w.Y;
      when pick =>
        case w.current_cmd is
          when change_text   =>
            w.Xs := w.X;
            w.Ys := w.Y;  -- only "pick", but we follow mouse
          when pick_obj =>
            if dist > dist_max then
              w.capture := select_area;
              Change_Cursor (w, cur_select);
              Invert_Rubber_Box (w, picked);  --  Show
            end if;
          when text | put | par_cur_2d_cmd =>
            null; -- Capture = click_1
          when Box_cmd | line |
               circle | filled_circle | oval   =>
            null; -- Capture = figure_2 for this
          when bez => null; -- Capture = bez_click*
          when Deformation_cmd => null;
        end case;
      when unpick =>
        case w.current_cmd is
          when pick_obj =>
            if dist > dist_max then
              w.capture := unselect_area;
              Change_Cursor (w, cur_unselect);
              Invert_Rubber_Box (w, normal);  --  Show
            end if;
          when others => null; -- Right button meaningless for not picking
        end case;
      when select_area =>
        Scroll_if_needed;
        Invert_Rubber_Box (w, picked);  --  show
      when unselect_area =>
        Scroll_if_needed;
        Invert_Rubber_Box (w, normal);  --  show
      when figure_2 | bez_click2 | click_2 =>
        Scroll_if_needed;
        Invert_Phantom (w);  --  show
      when click_1  =>
        Scroll_if_needed;
        w.Xs := w.X;
        w.Ys := w.Y;  --  only one click, but we follow mouse
        Invert_Phantom (w);  --  show
      when bez_click0 =>
        --  Bezier is a 3-click operation!
        w.Xs := w.X;
        w.Ys := w.Y;
        --  Just to calm down Show_Coordinates
      when bez_click1 =>
        null;
    end case;
    Show_coordinates (w);
  end Mouse_Move;

  procedure Mouse_Up (w    : in out TC_Picture_Panel;
                      X, Y : in     Integer)
  is
    modif : Boolean;
    Capture_mem : constant Capture_mode := w.capture;
    use New_objects, Object_Editing, Phantoms, Picking;
  begin
    w.X := X;
    w.Y := Y;
    Tranform_coordinates (w);
    w.capture := none;
    w.Picture.refresh := shadows_and_objects;  --  18-Jun-2003 not THERE!
    case Capture_mem is
      when none => null;
      when paste0 => null; -- shoudn't happen...
      when paste1 =>
        Load_Macro (w.pic_parent.all);
        Change_Cursor (w, cur_picking);
      when click_1 =>
        GWindows.Base.Release_Mouse;
        case w.current_cmd is
          when text          =>
            New_text (w.Picture, w.pic_parent.all, w.main.all, w.PU, txt, w.current_ls);
          when put           =>
            New_text (w.Picture, w.pic_parent.all, w.main.all, w.PU, putaux, w.current_ls);
          when par_cur_2d_cmd  =>
            New_paramcurve_2d (w.Picture, w.pic_parent.all, w.main.all, w.PU, w.current_ls);
          when Deformation_cmd =>
            Morphing.Deformation (w);
          when others =>
            null; -- Meaningless
        end case;
      when pick =>
        case w.current_cmd is
          when change_text   =>
            PicPic (w.Picture, pick_text, w.PU);
            if w.Picture.picked = 1 then
              w.Picture.refresh := every;
              Subtle_Redraw (w);
              GWindows.Base.Release_Mouse;
              Change_Cursor (w, cur_arrow);
              case w.Picture.memo.art is
                when txt | putaux | box =>
                  Change_Text (w.pic_parent.all, w.main.all, w.Picture.memo.all, modif);
                when oval =>
                  Change_Oval (w.pic_parent.all, w.main.all, w.Picture.memo.all, modif);
                when bezier =>
                  Change_Bezier
                    (w.pic_parent.all, w.main.all, w.Picture.ul_in_pt,
                     w.Picture.memo.all, modif);
                when paramcurve2d =>
                  Change_Param_2D
                    (w.pic_parent.all, w.main.all,
                     w.Picture.memo.all, modif);
                when others =>
                  modif := False;
              end case;
              w.Picture.saved := w.Picture.saved and not modif;
              Change_Cursor (w, cur_chg_text);
              if modif then
                w.Picture.refresh := full; -- 12-Jan-2004
                Redraw (w);
              end if;
            end if;
          when pick_obj =>
            PicPic (w.Picture, pick, w.PU);
            Change_Cursor (w, cur_picking);
          when others =>
            null;  --  Capture = click_1, figure_2 or bez* for these
        end case;
      when unpick =>
        case w.current_cmd is
          when pick_obj =>
            PicPic (w.Picture, unpick, w.PU);
            Change_Cursor (w, cur_picking);
          when others => null;  --  Right button meaningless for not picking
        end case;
      when select_area =>
        Invert_Rubber_Box (w, picked);  --  hide
        PicPic (w.Picture, pick_area, w.PS, w.PU);
        Change_Cursor (w, cur_picking);
      when unselect_area =>
        Invert_Rubber_Box (w, normal);  --  hide
        PicPic (w.Picture, unpick_area, w.PS, w.PU);
        Change_Cursor (w, cur_picking);
      when figure_2 =>
        --  Avoid an accidental click :
        --  if w.Xs /= w.X or w.Ys /= w.Y then
        if not Almost_Zero (Norm (w.PU - w.PS)) then
        --  ^ this test is also valid with snapping enabled 2-Nov-2005
          case w.current_cmd is
            when Box_cmd | oval =>
              GWindows.Base.Release_Mouse;
              New_boxoval (w.Picture, w.pic_parent.all, w.main.all,
                (Real'Min (w.PU.x, w.PS.x), Real'Min (w.PU.y, w.PS.y)),
                (Real'Max (w.PU.x, w.PS.x), Real'Max (w.PU.y, w.PS.y)),
                w.current_ls, w.current_cmd);
              w.Picture.refresh := only_last;
            when line =>
              New_linvec (w.Picture, w.PS, w.PU, w.current_ls);
              w.PS := w.PU;  --  \line has modified w.PU
              w.capture := click_2;
            when circle | filled_circle =>
              w.Picture.refresh := only_last;
              New_circdisc (w.Picture, w.PS, w.PU, w.current_cmd, w.current_ls);
            when Deformation_cmd =>
              GWindows.Base.Release_Mouse;
              Morphing.Deformation (w);
            when others =>
              null;
          end case;
        end if;
      when bez_click0 =>
        w.Xs := w.X;
        w.Ys := w.Y;
        w.capture := bez_click1;
      when bez_click1 =>
        w.Xb := w.X;
        w.Yb := w.Y;
        w.capture := bez_click2;
        w.phantomart := bezier;
        Tranform_coordinates (w);
        Invert_Phantom (w);  --  show
      when bez_click2 =>
        --  Figure already done on last Mouse_Down
        Invert_Phantom (w);  --  hide
        New_bezier (w.Picture, w.PS, w.PE, w.PU, w.current_ls);
        --  Chaining: new start on last end point
        w.Xs := w.Xb;
        w.Ys := w.Yb;
        w.capture := bez_click1;
      when click_2 =>
        Invert_Phantom (w);  --  hide
        New_linvec (w.Picture, w.PS, w.PU, w.current_ls);
        w.PS := w.PU;  --  \line has modified w.PU
        w.capture := click_2;
    end case;
    Update_Information (w.pic_parent.all);
    Show_mouse_mode (w);
    case Capture_mem is
      when click_2 | figure_2 =>
        Invert_Phantom (w);  --  hide
      when others => null;
    end case;
    case w.capture is
      when bez_click2 => null;
      when click_2 => null;
        w.Picture.refresh := only_last;
        Subtle_Redraw (w);
      when others =>  --  includes "none" (default)
        Subtle_Redraw (w);
        GWindows.Base.Release_Mouse;
    end case;
  end Mouse_Up;

end TC.GWin.Mousing;
