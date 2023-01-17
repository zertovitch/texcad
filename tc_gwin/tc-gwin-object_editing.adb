with TC.GWin.Lang;

with TeXCAD_Resource_GUI;

with GWindows.Buttons,
     GWindows.Constants,
     GWindows.Edit_Boxes,
     GWindows.Message_Boxes,
     GWindows.Static_Controls,
     GWindows.Windows;

with GWin_Util;

with Ada.Exceptions,
     Ada.Strings.Fixed;

package body TC.GWin.Object_Editing is

  use Ada.Strings, Ada.Strings.Fixed, TC.RIO;

  procedure Change_Text
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     t        : in out Obj_type;
     modified :    out Boolean)
  is
    pan                     : GWindows.Windows.Window_Type;
    text_eb, dash_length_eb : GWindows.Edit_Boxes.Edit_Box_Type;
    h_group                 : GWindows.Buttons.Group_Box_Type;
    h_radio                 : array (H_Justify) of GWindows.Buttons.Radio_Button_Type;
    v_group                 : GWindows.Buttons.Group_Box_Type;
    v_radio                 : array (V_Justify) of GWindows.Buttons.Radio_Button_Type;
    oki                     : GWindows.Buttons.Default_Button_Type;
    cancel                  : GWindows.Buttons.Button_Type;
    Result, w, x, y, wprop  : Integer;

    h_just: H_Justify;
    v_just: V_Justify;
    candidate: Obj_type(t.art);
    dum_str: String(1..20);

    h_adj_groups: constant:= 95;

    function Panel_title return String is
    begin
      case t.art is
        when putaux => return "\put(x,y){...}";
        when txt    => return "\makebox(0,0){...}";
        when box    =>
          case t.ls.pattern is
            when plain => return "\framebox(){...}";
            when dot   => return "%\dottedbox(){...}";
            when dash  => return "\dashbox(){...}";
          end case;
        when others => return Obj_art_type'Image(t.art); -- should not happen!
      end case;
    end Panel_title;

    procedure Get_Box_Data
      (Window : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings (off, Window);
      use GWindows.Buttons;
    begin
      candidate.inhalt := To_Unbounded_String (G2S (text_eb.Text));
      if t.art /= putaux then
        for h in H_Justify loop
          if h_radio(h).State = Checked then
            h_just := h;
          end if;
        end loop;
        for v in V_Justify loop
          if v_radio(v).State = Checked then
            v_just := v;
          end if;
        end loop;
        Images (h_just, v_just, candidate.adjust, candidate.adjust_len);
      end if;
      if t.ls.pattern = dash then
        candidate.ls.dash_length := TC.Real'Value (G2S (dash_length_eb.Text));
      end if;
    end Get_Box_Data;

    use GWindows.Constants, GWindows.Static_Controls, GWin_Util, Lang;

  begin
    Values( t.adjust(1..t.adjust_len), h_just, v_just );

    y:= 0;
    if t.art /= putaux then
      y:= h_adj_groups;
    end if;

    if t.ls.pattern /= plain then
      y:= y + 30;
    end if;

    pan.Create_As_Dialog (parent, S2G (Panel_title), Width => 630, Height => 110+y);

    pan.Center;
    pan.Small_Icon ("Options_Icon");
    pan.On_Destroy_Handler (Get_Box_Data'Unrestricted_Access);

    GWin_Util.Use_GUI_Font (pan);

    w := pan.Client_Area_Width - 20;

    text_eb.Create (pan, S2G (To_String (t.inhalt)), 10, 8, w, 24);

    if t.art /= putaux then

      wprop:= (w*2)/3;

      h_group.Create (pan, "", 10, 30, wprop - 5, h_adj_groups);

      for h in H_Justify loop
        x:= 20 + 120* H_Justify'Pos(h);
        Create_Label (pan,
          Msg(Message'Val(Message'Pos(left)+H_Justify'Pos(h))),
          x,  70, 120, 40);
        h_radio (h).Create (pan, "",  x,  45,  80, 15);
        h_radio (h).State (boolean_to_state(h=h_just));
      end loop;

      x:= 10 + 5 + wprop + 5;

      v_group.Create (pan, "", x, 30, (w-wprop) - 10, h_adj_groups);

      for v in V_Justify loop
        y:= 45 + 25* V_Justify'Pos(v);
        Create_Label (pan,
           Msg(Message'Val(Message'Pos(top)+V_Justify'Pos(v))),
           x + 50,  y, 130, 20);
        v_radio (v).Create (pan, "",  x+10,  y,  30, 15);
        v_radio (v).State (boolean_to_state(v = v_just));
      end loop;

    end if;
    y:= y + 40;
    case t.ls.pattern is
      when plain => null;
      when dash =>
        Create_Label (pan,Msg(dash_size), 10,  y, 130, 20);
        Put (dum_str,t.ls.dash_length,2,0);
        dash_length_eb.Create (pan, S2G (Trim(dum_str,Left)), 180, y,  40, 20);
      when dot => null;
    end case;

    oki.Create (pan, "O&K", 20,
            pan.Client_Area_Height - 40, 60, 25, ID => IDOK);
    cancel.Create (pan, Msg(mcancel), 100,
            pan.Client_Area_Height - 40, 60, 25, ID => IDCANCEL);

    candidate := t;

    text_eb.Focus;

    MDI_Main.Show_Dialog_with_Toolbars_off (pan, parent, main, Result);

    case Result is
      when IDOK     =>
        modified:=
          t.adjust(1..t.adjust_len) /=
            candidate.adjust(1..candidate.adjust_len) or else
          t.inhalt /= candidate.inhalt or else
          (t.ls.pattern = dash and then
           candidate.ls.dash_length /= t.ls.dash_length);
        t:= candidate;
      when others   =>
        modified:= False; -- Contains IDCANCEL
    end case;

  end Change_Text;

  procedure Change_Oval
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     t        : in out Obj_type;
     modified :    out Boolean)
  is
    pan       : GWindows.Windows.Window_Type;
    bordcoin  : array (Ovop) of GWindows.Buttons.Dialog_Button_Type;
    result, i : Integer;
    nov       : Ovop;
    ID_offset : constant := 10000;

  begin
    pan.Create_As_Dialog (parent, "\oval", Width => 460, Height => 225);
    pan.Center;
    pan.Small_Icon ("Options_Icon");
    GWin_Util.Use_GUI_Font (pan);

    for o in Ovop loop
      i := Ovop'Pos(o);
      bordcoin (o).Create
        (pan,
         GWin_Util.To_Lower( S2G (Ovop'Image(o)) ),
         100 + (i mod 3) * 100,
         30  + (i / 3) * 50,
         60, 25, ID => i+ID_offset );
    end loop;
    bordcoin (t.part).Focus;

    MDI_Main.Show_Dialog_with_Toolbars_off (pan, parent, main, result);

    if result = GWindows.Constants.IDCANCEL then
      modified := False;
    else
      i := result-ID_offset;
      -- 13-Jan-2004: sometimes command outside of range.
      if i in Ovop'Pos (Ovop'First) .. Ovop'Pos (Ovop'Last) then
        nov := Ovop'Val (i);
      else
        nov := entire;
      end if;
      modified := t.part /= nov;
      t.part := nov;
    end if;

  end Change_Oval;

  procedure Change_Bezier
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     ul_in_pt :        Real;
     t        : in out Obj_type;
     modified :    out Boolean)
  is
    pan                : GWindows.Windows.Window_Type;
    pts_box            : GWindows.Edit_Boxes.Edit_Box_Type;
    qbezier_radio      : array( Boolean ) of GWindows.Buttons.Radio_Button_Type;
    oki                : GWindows.Buttons.Default_Button_Type;
    cancel             : GWindows.Buttons.Button_Type;
    Result             : Integer;
    candidate, i0      : Natural;

    procedure Get_Data
      (Window : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings (off, Window);
      i : Integer;
      use GWindows.Buttons;
    begin
      if State (qbezier_radio (True)) = Checked then
        candidate := 0;
      else
        pts_box.Focus;
        i := Integer'Value (G2S (pts_box.Text));
        if i >= 1 then
          candidate := i;
        end if;
      end if;
    exception
      when others => null;  --  Wrong data
    end Get_Data;

    use GWindows.Constants, GWindows.Static_Controls, GWin_Util, Lang;

  begin
    pan.Create_As_Dialog(parent, "\qbezier, \bezier", Width => 320, Height => 140);
    pan.Center;
    pan.Small_Icon ("Options_Icon");
    pan.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    GWin_Util.Use_GUI_Font (pan);

    candidate:= t.num;
    if candidate = 0 then
      i0:= Good_num_of_bezier_points(t,ul_in_pt);
    else
      i0:= candidate;
    end if;

    qbezier_radio (True).Create (pan, "",             10,  20,  20, 20);
    Create_Label (pan, "Auto  (\qbezier)",            40,  20, 140, 20);
    qbezier_radio (False).Create (pan, "",           190,  20,  20, 20);
    pts_box.Create (pan, S2G (Integer'Image (i0)),   220,  20,  60, 20);

    qbezier_radio (True).State  (boolean_to_state (candidate = 0));
    qbezier_radio (False).State (boolean_to_state (candidate /= 0));

    oki.Create (pan, "O&K", 20,
            pan.Client_Area_Height - 40, 60, 25, ID => IDOK);
    cancel.Create (pan, Msg (mcancel), 100,
            pan.Client_Area_Height - 40, 60, 25, ID => IDCANCEL);

    pts_box.Focus;

    MDI_Main.Show_Dialog_with_Toolbars_off(pan, parent, main, Result);

    case Result is
      when IDOK     => modified:= candidate /= t.num;
                       t.num:= candidate;
      when others   => modified:= False; -- Contains IDCANCEL
    end case;

  end Change_Bezier;

  ---------------------------------------------
  --  2D parametric curve properties dialog  --
  ---------------------------------------------

  procedure Change_Param_2D
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     t        : in out Obj_type;
     modified :    out Boolean)
  is
    pan       : TeXCAD_Resource_GUI.Param_Curve_2D_Dialog_Type;
    Result    : Integer;
    candidate : Param_curve_2D_data:= t.data_2d;
    valid     : Boolean:= False;
    px, py    : TC_Formulas.Formula;
    err       : Unbounded_String;

    procedure Set_Data is
    begin
      pan.Segments_Box.Text (S2G (Trim(Integer'Image(candidate.segments), Left)));
      pan.Scale_Box.Text    (S2G (TeX_Number(candidate.scale)));
      pan.X_Form_Box.Text   (S2G (To_String(candidate.form_x)));
      pan.Y_Form_Box.Text   (S2G (To_String(candidate.form_y)));
      pan.T_Min_Box.Text    (S2G (TeX_Number(candidate.min_t)));
      pan.T_Max_Box.Text    (S2G (TeX_Number(candidate.max_t)));
    end Set_Data;

    use GWin_Util, Lang;

    procedure Get_Data
      (Window : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings (off, Window);
      use Ada.Exceptions;
    begin
      candidate:=
        ( segments => Natural'Value(G2S (pan.Segments_Box.Text)),
          scale    => TeX_Number(   G2S (pan.Scale_Box.Text)),
          form_x   => U(            G2S (pan.X_Form_Box.Text)),
          form_y   => U(            G2S (pan.Y_Form_Box.Text)),
          min_t    => TeX_Number(   G2S (pan.T_Min_Box.Text)),
          max_t    => TeX_Number(   G2S (pan.T_Max_Box.Text))
        );
      px.Parse(candidate.form_x);
      py.Parse(candidate.form_y);
      valid:= True;
    exception
      when E: others =>
        err := U(Exception_Name(E) &
               ASCII.CR & ASCII.LF &
               Exception_Message(E));  --  Wrong data
    end Get_Data;

  begin
    loop
      pan.Create_Full_Dialog (parent, Msg (param2d_title));
      pan.Center;
      pan.Small_Icon ("Options_Icon");
      pan.On_Destroy_Handler (Get_Data'Unrestricted_Access);
      Set_Data;
      pan.Segments_Label.Text(Msg(param2d_segments));
      pan.Scale_Label.Text(Msg(param2d_scale));
      pan.IDCANCEL.Text(Msg(mcancel));

      MDI_Main.Show_Dialog_with_Toolbars_off(pan, parent, main, Result);
      case Result is
        when GWindows.Constants.IDOK =>
          if valid then
            modified:= candidate /= t.data_2d;
            if modified then
              t.data_2d:= candidate;
              t.parsed_2d_x := px;
              t.parsed_2d_y := py;
            end if;
            exit;
          else
            GWindows.Message_Boxes.Message_Box
              (parent,
               "Error",
               "Error in data" & NL & "Details:" & NL & S2G (To_String(err)));
            modified := False;
          end if;
        when others =>  --  Contains the IDCANCEL case.
          modified := False;
          exit;
      end case;

    end loop;
    end Change_Param_2D;

end TC.GWin.Object_Editing;
