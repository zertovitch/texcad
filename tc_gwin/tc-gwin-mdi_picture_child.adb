with TC.Picking;                        use TC.Picking;
with TC.Input, TC.Output;

with TC.GWin.Lang;                      use TC.GWin.Lang;
with TC.GWin.Display;                   use TC.GWin.Display;
with TC.GWin.Mousing;                   use TC.GWin.Mousing;
with TC.GWin.Menus;
with TC.GWin.Previewing;
with TC.GWin.Tools;

with GWindows.Application;              use GWindows.Application;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Clipboard;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Cursors;                  use GWindows.Cursors;
with GWindows.Drawing.Capabilities;     use GWindows.Drawing.Capabilities;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Windows;                  use GWindows.Windows;

with GWin_Util;                         use GWin_Util;

with Ada.Directories;                   use Ada.Directories;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Text_IO;                       use Ada.Text_IO;

with Interfaces.C;

with GNAT.OS_Lib;

package body TC.GWin.MDI_Picture_Child is

  use TC.REF;

  -- Adjust the Draw control's position to those of the scroll controls.
  --
  procedure Adjust_Draw_Control_Position(Window : in out MDI_Picture_Child_Type)
  is
  begin
     Window.Draw_Control.X0:= Scroll_Position (Window, Horizontal);
     Window.Draw_Control.Y0:= Scroll_Position (Window, Vertical);
     Move (Window.Draw_Control,
           0 - Window.Draw_Control.X0,
           0 - Window.Draw_Control.Y0
     );
  end Adjust_Draw_Control_Position;

  procedure On_Size (Window : in out MDI_Picture_Child_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
    pragma Warnings (Off, Width);
    pragma Warnings (Off, Height);
  begin
    if user_maximize_restore then
      MDI_childen_maximized:= Zoom(Window);
    end if;

    -- Taken from GWindows.Scroll_Panels
    if
      Client_Area_Width (Window) < Client_Area_Width (Window.Draw_Control)
    then
       Horizontal_Scroll_Bar (Window);
       Scroll_Range
         (Window, Horizontal, 0,
          Client_Area_Width (Window.Draw_Control) -
          Client_Area_Width (Window) + 30);
       Scroll_Page_Size (Window, Horizontal, 30);
       Adjust_Draw_Control_Position(Window);
    else
       Left (Window.Draw_Control, 0);
       Scroll_Position (Window, Horizontal, 0);
       Window.Draw_Control.X0:= 0;
       Horizontal_Scroll_Bar (Window, False);
    end if;

    if
      Client_Area_Height (Window) < Client_Area_Height (Window.Draw_Control)
    then
       Vertical_Scroll_Bar (Window);
       Scroll_Range
         (Window, Vertical, 0,
          Client_Area_Height (Window.Draw_Control) -
          Client_Area_Height (Window) + 30);
       Scroll_Page_Size (Window, Vertical, 30);
       Adjust_Draw_Control_Position(Window);
    else
       Top (Window.Draw_Control, 0);
       Scroll_Position (Window, Vertical, 0);
       Window.Draw_Control.Y0:= 0;
       Vertical_Scroll_Bar (Window, False);
    end if;

    Window.Draw_Control.Disp_W:= Client_Area_Width (Window);
    Window.Draw_Control.Disp_H:= Client_Area_Height (Window);
  end On_Size;

  procedure On_Horizontal_Scroll
    (Window  : in out MDI_Picture_Child_Type;
     Request : in     GWindows.Base.Scroll_Request_Type;
     Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Request = Thumb_Drag then
      Window.Draw_Control.X0:= Scroll_Drag_Position (Window, Horizontal);
      Window.Draw_Control.Y0:= Scroll_Drag_Position (Window, Vertical);
      Move (Window.Draw_Control,
            0 - Window.Draw_Control.X0,
            0 - Window.Draw_Control.Y0);
    else
       Adjust_Draw_Control_Position(Window);
       On_Horizontal_Scroll
         (GWindows.Windows.Window_Type (Window),
          Request,
          Control);
    end if;
  end On_Horizontal_Scroll;

  ------------------------
  -- On_Vertical_Scroll --
  ------------------------

  procedure On_Vertical_Scroll
    (Window  : in out MDI_Picture_Child_Type;
     Request : in     GWindows.Base.Scroll_Request_Type;
     Control : in     GWindows.Base.Pointer_To_Base_Window_Class)

  is
  begin
    if Request = Thumb_Drag then
      Window.Draw_Control.X0:= Scroll_Drag_Position (Window, Horizontal);
      Window.Draw_Control.Y0:= Scroll_Drag_Position (Window, Vertical);
      Move (Window.Draw_Control,
            0 - Window.Draw_Control.X0,
            0 - Window.Draw_Control.Y0);
    else
      Adjust_Draw_Control_Position(Window);
      On_Vertical_Scroll (GWindows.Windows.Window_Type (Window),
                          Request,
                          Control);
    end if;
  end On_Vertical_Scroll;

  procedure On_Erase_Background
    (Window : in out MDI_Picture_Child_Type;
     Canvas : in out GWindows.Drawing.Canvas_Type;
     Area   : in     GWindows.Types.Rectangle_Type)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, Canvas);
    pragma Warnings (Off, Area);
  begin
    null; -- Do nothing! Avoids the flickering background/canvas.
  end On_Erase_Background;

  procedure Display_saved_bitmap(
    window : in out TC_Picture_Panel;
    area   :        GWindows.Types.Rectangle_Type)
  is
  begin
    BitBlt( window.Drawing_Area,
            area.Left, area.Top, area.Right-area.Left, area.Bottom-area.top,
            Window.Saved_Area,
            area.Left,area.Top
          );
    --### For monitoring (interesting!): ###
    -- Line(window.Drawing_Area,Area.Left,Area.Top,Area.Right,Area.Bottom);
  end Display_saved_bitmap;

  procedure Update_bitmap(Window : in out TC_Picture_Panel ) is
  begin
    if Window.picture.refresh /= no then
      Draw(Window.Saved_Area, Window.picture, null, Width(Window), Height(Window));
    end if;
  end Update_bitmap;

  procedure Subtle_redraw (Window : in out TC_Picture_Panel ) is
    mem: constant TC.Refresh_mode:= Window.picture.refresh;
  begin
    Update_bitmap(Window);
    if mem /= no then -- Bitmap has changed, then repaint
      Redraw(Window); -- the visible area(s) on screen
    end if;
  end Subtle_redraw;

  procedure On_Paint (Window : in out TC_Picture_Panel;
                      Canvas : in out GWindows.Drawing.Canvas_Type;
                      Area   : in     GWindows.Types.Rectangle_Type) is
    pragma Warnings (Off, Canvas); -- Canvas == Window.Drawing_Area
    pragma Warnings (Off, Area);
  begin
    Update_bitmap(Window);
    Display_saved_bitmap(Window,Area);
  end On_Paint;

  procedure Create_Menus (Window : in out MDI_Picture_Child_Type)  is
    Main: constant Menu_Type := Create_Menu;
  begin
    Window.File_menu:= TC.GWin.Menus.Create_File_Menu(is_child => True);
    Append_Menu (Main, Msg(ffile), Window.File_menu);

    Window.Draw_menu:= TC.GWin.Menus.Create_Draw_Menu;
    Append_Menu (Main, Msg(ddraw), Window.Draw_menu);

    Window.Line_menu:= TC.GWin.Menus.Create_Line_Menu;
    Append_Menu (Main, Msg(lline), Window.Line_menu);

    Window.Edit_menu:= TC.GWin.Menus.Create_Edit_Menu;
    Append_Menu (Main, Msg(eedit), Window.Edit_menu);

    Append_Menu (Main, Msg(ttools), TC.GWin.Menus.Create_Tools_Menu);

    Window.View_menu:= TC.GWin.Menus.Create_View_Menu;
    Append_Menu (Main, Msg(vview), Window.View_menu);

    Append_Menu (Main, Msg(oopt),TC.GWin.Menus.Create_Options_Menu(is_child => True));
    Append_Menu (Main, Msg(wwindow), TC.GWin.Menus.Create_Wndw_Menu);
    Append_Menu (Main, Msg(hhelp), TC.GWin.Menus.Create_Help_Menu);

    MDI_Menu (Window, Main, Window_Menu => 8);
  end Create_Menus;

  procedure Do_Change_Cursor(Window : in out Base_Window_Type'Class) is
  begin
    if Window in TC_Picture_Panel'Class then
      Set_Cursor(TC_Picture_Panel(Window).Cursor);
    end if;
  end Do_Change_Cursor;

  procedure Do_Left_Mouse_Down (Window : in out Base_Window_Type'Class;
                                X, Y   : in     Integer;
                                Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mouse_Down(TC_Picture_Panel(Window), X,Y, Left_Button);
    end if;
  end Do_Left_Mouse_Down;

  procedure Do_Right_Mouse_Down (Window : in out Base_Window_Type'Class;
                                 X, Y   : in     Integer;
                                 Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mouse_Down(TC_Picture_Panel(Window), X,Y, Right_Button);
    end if;
  end Do_right_Mouse_Down;

  procedure Do_Mouse_Up (Window : in out Base_Window_Type'Class;
                         X, Y   : in     Integer;
                         Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mouse_Up(TC_Picture_Panel(Window),X,Y);
    end if;
  end Do_Mouse_Up;

  procedure Do_Mouse_Move (Window : in out Base_Window_Type'Class;
                           X, Y   : in     Integer;
                           Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mouse_Move(TC_Picture_Panel(Window), X,Y);
    end if;
  end Do_Mouse_Move;

  procedure Do_Mouse_Wheel(Window  : in out Base_Window_Type'Class;
                           X       : in     Integer;
                           Y       : in     Integer;
                           Keys    : in     Mouse_Key_States;
                           Z_Delta : in Integer)
  is
    pragma Unreferenced (X, Y);
    WW: MDI_Picture_Child_Type renames MDI_Picture_Child_Type(Window);
    v_pos: constant Natural:= Scroll_Position (WW, Vertical);
    dy: constant Natural:= Scroll_Page_Size(WW, Vertical);
    z: Integer;
  begin
    if Z_Delta /= 0 then
      z:= Z_Delta / abs Z_Delta;
      if Keys(Control) then
        Zoom_picture(WW, z);
      else
        Scroll_Position(WW, Vertical, v_pos - dy * z);
        Adjust_Draw_Control_Position(WW);
      end if;
    end if;
  end Do_Mouse_Wheel;

  -- Added Key down handler for steering mouse cursor with arrow keys 14-Oct-2005

  procedure Do_Key_Down
              (Window      : in out GWindows.Base.Base_Window_Type'Class;
               Special_Key : in     Special_Key_Type;
               Value       : in     GCharacter)
  is
    WW: MDI_Picture_Child_Type renames MDI_Picture_Child_Type(Window);
    v_pos: constant Natural:= Scroll_Position (WW, Vertical);
    dy: constant Natural:= Scroll_Page_Size(WW, Vertical);
    pragma Warnings (Off, Value);
    --
    procedure Move_mouse_cursor(dx,dy: Integer) is
      p: constant GWindows.Types.Point_Type:= Get_Cursor_Position;
    begin
      Set_Cursor_Position (p.X+dx, p.Y+dy);
    end Move_mouse_cursor;
  begin
    case Special_Key is
      when Left_Key     => Move_mouse_cursor(-1,0);
      when Up_Key       => Move_mouse_cursor(0,-1);
      when Right_Key    => Move_mouse_cursor(+1,0);
      when Down_Key     => Move_mouse_cursor(0,+1);
      when Page_Up      =>
        Scroll_Position(WW, Vertical, v_pos - dy);
        Adjust_Draw_Control_Position(WW);
      when Page_Down    =>
        Scroll_Position(WW, Vertical, v_pos + dy);
        Adjust_Draw_Control_Position(WW);
      when others       => null;
    end case;
  end Do_Key_Down;

  -- This will update file menu of parent and all brothers
  procedure Update_Common_Menus(Window : MDI_Picture_Child_Type;
                                top_entry : GString:= "" ) is
  begin
    Update_Common_Menus( Window.parent.all, top_entry );
  end Update_Common_Menus;

  procedure Update_Permanent_Command(Window : in out MDI_Picture_Child_Type) is

    procedure Radio_Check_Custom(
      m                 : in Menu_Type;
      first, last, check: Custom_cmd)
    is
    begin
      Radio_Check(
        m, Command,
        ID_Custom(first), ID_Custom(last), ID_Custom(check)
      );
    end Radio_Check_Custom;

    procedure Update_cmd(m: in Menu_Type) is
    begin
      Radio_Check_Custom(
        m,
        Permanent_cmd'First, Permanent_cmd'Last, Window.Draw_Control.current_cmd
      );
    end Update_cmd;

    procedure Update_ls is
      c: Custom_cmd;
    begin
      Radio_Check_Custom(
        Window.Line_Menu,
        Line_thickness_cmd'First,
        Line_thickness_cmd'Last,
        Custom_cmd'Val(
          Custom_cmd'Pos(Line_thickness_cmd'First) +
          Line_thickness'Pos(Window.Draw_Control.current_ls.thickness)
        )
      );
      c:= Custom_cmd'Val(
            Custom_cmd'Pos(Line_pattern_cmd'First) +
            Line_pattern'Pos(Window.Draw_Control.current_ls.pattern)
          );
      if c = dot_param then
        c:= dash;
      end if;
      Radio_Check_Custom(
        Window.Line_Menu,
        Line_pattern_cmd'First, Line_pattern_cmd'Last, c
      );
      Radio_Check_Custom(Window.Line_Menu,
        Line_arrows_cmd'First,
        Line_arrows_cmd'Last,
        Custom_cmd'Val(
          Custom_cmd'Pos(Line_arrows_cmd'First) +
          Line_arrows'Pos(Window.Draw_Control.current_ls.arrows)
        )
      );
    end Update_ls;

  begin
    Update_cmd(Window.Draw_Menu);
    Update_cmd(Window.Edit_Menu);
    Update_ls;
    case Window.Draw_Control.current_cmd is
      when pick_obj    => Change_Cursor(Window.Draw_Control, cur_picking);
      when change_text => Change_Cursor(Window.Draw_Control, cur_chg_text);
      when others      => Change_Cursor(Window.Draw_Control, cur_arrow);
    end case;
    Update_Status_Bar( Window.parent.all, command,
      Filter_amp(Msg(msg_for_command(Window.Draw_Control.current_cmd)))
    );
  end Update_Permanent_Command;

  zoom_factor: constant Real:= 2.0 ** (1.0/8.0);

  procedure Zoom_picture(
    Window   : in out MDI_Picture_Child_Type;
    direction:        Integer )
  is
    opt: TC.Picture_options renames Window.Draw_Control.picture.opt;
    sf: GString(1..20);
  begin
    if direction /= 0 then
      opt.zoom_fac:= opt.zoom_fac * (zoom_factor ** direction);
      Window.Draw_Control.picture.refresh:= full;
      Subtle_redraw(window.Draw_Control);
    end if;
    RIO.Put(sf,opt.zoom_fac,2,0);
    Update_Status_Bar( window.parent.all, zoom, Trim(sf,left) );
  end Zoom_picture;

  procedure Show_Totals( Window: in out MDI_Picture_Child_Type ) is
    p: TC.Picture renames Window.Draw_Control.picture;
    function Stotal( pfx: GString; t, th: Integer ) return GString is
    begin
      if t = 0 then
        return "";
      elsif th = 0 then
        return pfx & Integer'Image(t);
      else
        return pfx & Integer'Image(t) & "  (h:" & Integer'Image(th) & ')';
      end if;
    end Stotal;
    Star: constant array(Boolean) of GCharacter:= (False => ' ',True => '*');
  begin
    Update_Status_Bar(Window.parent.all,stat_objects,
      Stotal("Objects:", p.total, p.total_hidden) &
      Stotal("; picked:", p.picked, p.picked_hidden)
    );
    Update_Status_Bar(Window.parent.all,modified,(1=>Star(not p.saved)));
  end Show_Totals;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out MDI_Picture_Child_Type) is
     use Interfaces.C;
     win_asp_x, win_asp_y: Interfaces.C.Unsigned;
   begin
     Small_Icon (Window, "Picture_Icon");

     Create_As_Control (Window.Draw_Control, Window,
                        Top    => 0,
                        Left   => 0,
                        Width  => Desktop_Width,
                        Height => Desktop_Height);

      Window.Draw_Control.Picture.refresh:= full;

      -- Filial feelings:
      Window.parent:= MDI_Main_Access(Controlling_Parent(Window));
      Window.Draw_Control.parent:=
        MDI_Picture_Child_Access(Controlling_Parent(Window.Draw_Control));
      Window.Draw_Control.main:= Window.parent;

      Use_GUI_Font(Window.Draw_Control);

      -- Dock (Window.Draw_Control, GWindows.Base.Fill);
      -- Auto_Resize(Window.Draw_Control);
      -- Dock_Children (Window.Scroll_Panel.Panel);
      Background_Mode (Window.Draw_Control.Drawing_Area, Transparent);
      Background_Mode (Window.Draw_Control.Saved_Area, Transparent);

      On_Change_Cursor_Handler (Window.Draw_Control, Do_Change_Cursor'Access);
      On_Left_Mouse_Button_Down_Handler(Window.Draw_Control, Do_Left_Mouse_Down'Access);
      On_Right_Mouse_Button_Down_Handler(Window.Draw_Control, Do_Right_Mouse_Down'Access);
      On_Left_Mouse_Button_Up_Handler(Window.Draw_Control, Do_Mouse_Up'Access);
      On_Right_Mouse_Button_Up_Handler(Window.Draw_Control, Do_Mouse_Up'Access);
      On_Mouse_Move_Handler (Window.Draw_Control, Do_Mouse_Move'Access);
      On_Mouse_Wheel_Handler (Window, Do_Mouse_Wheel'Access);
      --
      On_Character_Down_Handler (Window, Do_Key_Down'Access); -- 14-Oct-2005

      Get_Canvas (Window.Draw_Control, Window.Draw_Control.Drawing_Area);
      Create_Memory_Canvas (Window.Draw_Control.Saved_Area, Window.Draw_Control.Drawing_Area);

      Create_Compatible_Bitmap (Window.Draw_Control.Drawing_Area,
                                Window.Draw_Control.Saved_Bitmap,
                                Desktop_Width,
                                Desktop_Height);
      Select_Object (Window.Draw_Control.Saved_Area,
                     Window.Draw_Control.Saved_Bitmap);

      Refresh_size_dependent_parameters(
        Window.Draw_Control.Picture,
        objects => True
      );
      -- ^default pt, not yet from options.
      Subtle_redraw (Window.Draw_Control);

      --Dock_Children (Window);

      win_asp_x:= Get_Capability(Window.Draw_Control.Drawing_Area,ASPECTX);
      win_asp_y:= Get_Capability(Window.Draw_Control.Drawing_Area,ASPECTY);
      --  GWindows.Message_Boxes.Message_Box(Window,
      --    "Aspect",
      --            "ASPECTX" & Interfaces.C.Unsigned'image(win_asp_x)
      --     & NL & "ASPECTY" & Interfaces.C.Unsigned'image(win_asp_y)
      --  );

      if win_asp_x = 0 or win_asp_y = 0 then
        Window.Draw_Control.Picture.aspect:= 1.0;
      else
        Window.Draw_Control.Picture.aspect:= Real(win_asp_y) / Real(win_asp_x);
      end if;

      Create_Menus(Window);

      Window.Draw_Control.current_cmd:= Line;
      Window.Draw_Control.current_ls:= normal_line_settings;
      Window.Draw_Control.capture:= none;

      -- 2007: Maximize-demaximize (non-maximized case) to
      -- avoid invisible windows...
      declare
        memo_unmaximized_children: constant Boolean:= not MDI_childen_maximized;
      begin
        if memo_unmaximized_children then
          Freeze(Window.parent.all);
          Zoom(Window);
        end if;
        On_Size(Window,Width(Window),Height(window));
        if memo_unmaximized_children then
          Thaw(Window.parent.all); -- Before Zoom, otherwise uncomplete draw.
          Zoom(Window, False);
          Window.parent.Tool_Bar.Redraw;
        end if;
      end;

      Scroll_Position(Window, Vertical, Scroll_Maximum(Window, Vertical));
      Adjust_Draw_Control_Position(Window);
      Window.Use_Mouse_Wheel;
   end On_Create;

   procedure On_Picture_Options
     (window  : in out GWindows.Base.Base_Window_Type'Class;
      pic_opt : in out TC.Picture_options;
      main    : in out MDI_Main_Type;
      modified:    out Boolean;
      title   : String )
   is
     pan: Window_Type;
     subtype Tab_subject is Lang.Message range
       pic_opt_tab_drawing ..  pic_opt_tab_latex;

    package Tabbing is
      new GWin_Util.Property_Tabs_Package(Tab_subject,Lang.Msg,"O&K",Msg(mcancel));

     -- Snap group --
     snap_group: Group_Box_Type;
     snap: Check_Box_Type;
     snap_asp : Edit_Box_Type;
     -- Reduce group --
     redu_group: Group_Box_Type;
     reduce : Check_Box_Type;
     -- Slope group --
     slope_group: Group_Box_Type;
     any_slope, ltx_slope: Radio_Button_Type; -- Push_
     stdiff : Edit_Box_Type;
     quality, zoom, origx, origy: Edit_Box_Type;
     -- Basics group --
     basics_group: Group_Box_Type;
     ul, lw : Edit_Box_Type;
     -- Compatibility group --
     compat_group: Group_Box_Type;
     sty_box: array(Supposing_sty) of Check_Box_Type;
     compat_x: constant:= 5;
     compat_y: constant:= 10;
     -- Preview insertions --
     preview_insert_box: Multi_Line_Edit_Box_Type;

     -- Panel's variables
     Result, y  : Integer;
     wmax    : constant:= 420;
     candidate: TC.Picture_Options:= pic_opt;
     dum_str: String(1..20);

     use TC.RIO;

     procedure Get_Data
       (Window : in out GWindows.Base.Base_Window_Type'Class)
     is
     begin
       candidate.snapping:= State(snap)=checked;
       candidate.snap_asp:= Integer'Value(Text(snap_asp));
       candidate.zoom_fac:= TC.Real'Value(Text(zoom));
       candidate.quality := TC.Real'Value(Text(quality));
       candidate.reduce  := State(reduce)=checked;
       candidate.steigung:= State(any_slope)=checked;
       candidate.stdiff  := TC.Real'Value(Text(stdiff));
       for s in sty_box'Range loop
         candidate.sty(s):= State(sty_box(s)) = checked;
       end loop;
       --
       candidate.unitlength:= To_Unbounded_String(Text(ul));
       candidate.linewidth := To_Unbounded_String(Text(lw));
       candidate.P0.x:= TC.Real'Value(Text(origx));
       candidate.P0.y:= TC.Real'Value(Text(origy));
       candidate.pv_insert := To_Unbounded_String(Text(preview_insert_box));
     exception
       when others =>
         Message_Box(
           Window,
           "Invalid data", "Incomplete reading of your changes",
           OK_Box, Error_Icon);
     end Get_Data;

  begin
    Create_As_Dialog(pan, Window, title, Width => wmax + 50, Height => 330);
    -- Fix_Dialog(pan); -- 2007. No effect, alas...
    Center(pan);
    Small_Icon (Pan, "Options_Icon");
    On_Destroy_Handler (pan, Get_Data'Unrestricted_Access);

    GWin_Util.Use_GUI_Font(pan);

    Tabbing.Create(pan);

    Create(basics_group, Tabbing.tab(pic_opt_tab_drawing), Msg(dimensions),
        5,  10, 215, 65);
    Create_Label (Tabbing.tab(pic_opt_tab_drawing), Msg(unitlength),
       20,  30, 130, 20);
    Create (ul, Tabbing.tab(pic_opt_tab_drawing),
            To_String(candidate.unitlength),
      155,  30,  60, 20);
    Create_Label (Tabbing.tab(pic_opt_tab_drawing), Msg(linewidth),
       20,  50, 140, 20);
    Create (lw, Tabbing.tab(pic_opt_tab_drawing),
            To_String(candidate.linewidth),
      165,  50,  50, 20);

    Create(redu_group, Tabbing.tab(pic_opt_tab_drawing), Msg(linechain),
        5,  80, 215, 65);
    Create_Label (Tabbing.tab(pic_opt_tab_drawing), Msg(reduchain),
       20, 100, 180, 20);
    Create(reduce,Tabbing.tab(pic_opt_tab_drawing), "",
      200, 100,  15, 15);
    State(reduce,boolean_to_state(candidate.reduce));
    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(slopetol),
       20, 120, 155, 20);
    Put(dum_str,candidate.stdiff,4,0);
    Create (stdiff, Tabbing.tab(pic_opt_tab_drawing), Trim(dum_str,left),
      175, 120,  40, 20);

    Put(dum_str,candidate.zoom_fac,4,0);
    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(zoom_fac),
        5, 160, 165, 20);
    Create (zoom, Tabbing.tab(pic_opt_tab_drawing), Trim(dum_str,left),
      180, 160,  40, 20);

    Put(dum_str,candidate.quality,4,0);
    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(qualcirc),
        5, 180, 165, 20);
    Create (quality, Tabbing.tab(pic_opt_tab_drawing), Trim(dum_str,left),
      180, 180,  40, 20);

    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(origin),
        5, 200, 125, 20);
    Put(dum_str,candidate.P0.x,4,0);
    Create (origx, Tabbing.tab(pic_opt_tab_drawing), Trim(dum_str,left),
      130, 200,  40, 20);
    Put(dum_str,candidate.P0.y,4,0);
    Create (origy, Tabbing.tab(pic_opt_tab_drawing), Trim(dum_str,left),
      180, 200,  40, 20);

    Create(snap_group, Tabbing.tab(pic_opt_tab_drawing), Msg(Snapping),
      235,  10, 200, 65);
    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(activated),
      250,  30,  90, 20);
    Create(snap,Tabbing.tab(pic_opt_tab_drawing), "",
      350,  30,  15, 15);
    State(snap,boolean_to_state(candidate.snapping));
    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(stepping),
      250,  50,  90, 20);
    Create (snap_asp, Tabbing.tab(pic_opt_tab_drawing),
      Trim(Integer'Image(candidate.snap_asp),left),
      350,  50,  30, 20);

    Create(slope_group, Tabbing.tab(pic_opt_tab_drawing), Msg(slopes),
      235,  80, 200, 65);
    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(anyslope),
      250,  100, 100, 20);
    Create(any_slope,Tabbing.tab(pic_opt_tab_drawing), "",
      350,  100,  15, 15);
    State(any_slope,boolean_to_state(candidate.steigung));
    Create_Label (Tabbing.tab(pic_opt_tab_drawing),  Msg(txslopes),
      250,  120, 100, 20);
    Create(ltx_slope,Tabbing.tab(pic_opt_tab_drawing), "",
      350,  120,  15, 15);
    State(ltx_slope,boolean_to_state(not candidate.steigung));

    -- LaTeX tab --
    -- .sty assumptions group
    Create(compat_group, Tabbing.tab(pic_opt_tab_latex), Msg(compat),
      compat_x,  compat_y, 200, 25 + 20 * sty_box'Length);

    for s in sty_box'Range loop
      y:= compat_y + 20 + 20 * Supposing_sty'Pos(s);
      Create_Label (Tabbing.tab(pic_opt_tab_latex),  Sty_title(s), compat_x + 15,  y, 160, 20);
      Create(sty_box(s),Tabbing.tab(pic_opt_tab_latex), "", compat_x + 180,  y,  15, 15);
      State(sty_box(s), boolean_to_state(candidate.sty(s)));
    end loop;

    -- preview insertions
    y:= y + 20 + 25;
    Create_Label(
      Tabbing.tab(pic_opt_tab_latex),
      Msg(preview_insertions), compat_x,  y, wmax, 20
    );
    Create(
      preview_insert_box,
      Tabbing.tab(pic_opt_tab_latex),
      To_GString_From_Unbounded(candidate.pv_insert),
      compat_x, y + 25, wmax, 80
    );

    Show_Dialog_with_Toolbars_off(pan, window, main, result);

    case Result is
      when IDOK     =>
        modified:= pic_opt /= candidate;
        -- ^ try to do it so short in another language!
        pic_opt:= candidate;
      when others   =>
        modified:= False; -- Contains IDCANCEL
    end case;
    GWindows.Base.Redraw(window);
  end On_Picture_Options;

  procedure Preview( window : in out MDI_Picture_Child_Type ) is
    ttl: constant String:= Text(Window);
    ti: Integer;
    stop_char: constant array(Character) of Boolean:=
      ( '\'|'/'|'-' => True, others => False);
  begin
    if Window.Draw_control.Picture.opt.sty(emlines) then
      case Message_Box (
        Window, Msg(preview), Msg(noemlines1) & NL & Msg(noemlines2),
        Yes_No_Cancel_Box, Question_Icon)
      is
        when Yes =>
          Window.Draw_control.Picture.opt.sty(emlines):= False;
          Window.Draw_control.Picture.saved:= False;
          Show_Totals(Window); -- show the '*' for modified 2-Aug-2005
        when Cancel =>
          return;
        when others => -- includes No
          null;
      end case;
    end if;
    ti:= ttl'Last;
    for i in reverse ttl'Range loop
      exit when stop_char(ttl(i));
      ti:= i;
    end loop;
    TC.GWin.Previewing.Create_files(
      pic   => Window.Draw_control.Picture,
      title => ttl(ti..ttl'Last)
    );
    TC.GWin.Previewing.Start;
  exception
    when E: TC.GWin.Previewing.Preview_error =>
      Message_Box (
        Window,
        Msg(preview),
        Msg(prev_fail) & NL & NL & Exception_Message(E),
        OK_Box, Exclamation_Icon);
      TC.GWin.Previewing.Cleanup;
  end Preview;

  function Clip_filename return String is
  begin
    return Temp_dir & "TeXCADcp.txt";
  end Clip_filename;

  procedure Clipboard_to_Clip_file( Window : in Window_Type'Class ) is
    contents: constant String:= GWindows.Clipboard.Clipboard_Text(Window_Type(Window));
    f: File_Type;
  begin
    if contents = "" then
      null; -- do nothing, we still might have the clipboard file
    else
      Create(f, Out_File, Clip_filename);
      Put_Line(f, contents);
      Close(f);
    end if;
  end Clipboard_to_Clip_file;

  procedure Clip_file_to_Clipboard( Window : in Window_Type'Class ) is
    f: File_Type;
    contents: Unbounded_String:= Null_Unbounded_String;
  begin
    if not Exists(Clip_filename) then
      null; -- do nothing, we still might have something on the Windows clipboard
    else
      Open(f, In_File, Clip_filename);
      while not End_Of_File(f) loop
        contents:= contents & Get_Line(f) & NL;
      end loop;
      Close(f);
      GWindows.Clipboard.Clipboard_Text(Window_Type(Window), contents);
    end if;
  end Clip_file_to_Clipboard;

  procedure Copy_clip( Window : in out MDI_Picture_Child_Type ) is
  begin
    TC.Output.Save(
      pic            => Window.Draw_control.Picture,
      macro          => True,
      file_name      => Clip_filename,
      displayed_name => "Clipboard"
    );
    Clip_file_to_Clipboard(Window);
  end Copy_clip;

  procedure On_Open_Macro (
    Window  : in out MDI_Picture_Child_Type;
    Success :    out Boolean )
  is
    File_Name, File_Title : GString_Unbounded;
  begin
    Open_File (Window, Msg(Open),
      File_Name,
      ((To_Gstring_Unbounded (Msg(Tcd_Mac) & " (*." & Mac_Suffix & ")"),
          To_Gstring_Unbounded ("*." & Mac_Suffix )),
        (To_Gstring_Unbounded (Msg(All_Files) & " (*.*)"),
          To_Gstring_Unbounded ("*.*"))),
      '.' & Mac_Suffix,
      File_Title,
      Success);
      if success then
        Window.Macro_Name:= File_Name;
      end if;
  end On_Open_Macro;

  procedure Load_macro(Window: in out MDI_Picture_Child_Type) is
    pw: Picture renames Window.Draw_control.Picture;
    mo: Picture_options; -- save all picture options, overwritten by loading
  begin
    mo:= pw.opt;
    pw.opt.P0:= Window.Draw_control.PU; -- origin on mouse cursor
    begin
      TC.Input.Load( pw, True, To_GString_From_unbounded(Window.Macro_Name) );
    exception
      when E : TC.Input.Load_error =>
        Message_Box(
          Window,
          "Error when loading picture data",
          "The LaTeX picture on" & NL &
          "clipboard / macro / partial / temp / scrap / (whatever)" & NL &
          "is ill-formed." & NL &
          "Items were only partially loaded, or not at all." & NL &
          "--- Message ---" & NL &
          Exception_Message(E),
          Icon => Exclamation_Icon
        );
    end;
    pw.opt:= mo; -- restore options
    Refresh_size_dependent_parameters(pw,objects => True);
    -- ^ was missing ! Fixed 14-Jan-2004
    pw.refresh:= shadows_and_objects;
  end Load_Macro;

  -- 23-Feb-2004

  procedure Change_Pattern_Params(
    Window     : in out MDI_Picture_Child_Type;
    new_pattern:        Line_pattern)
  is
    pan                : Window_Type;
    sym_eb, len_eb     : Edit_Box_Type;
    oki                : Default_Button_Type;
    cancel             : Button_Type;
    Result,y           : Integer;

    candidate: Line_settings;

    procedure Get_Box_Data
      (parent : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings(off,parent);
    begin
      case candidate.pattern is
        when plain => null;
        when dot =>
          candidate.dot_symbol:= To_Unbounded_String(GWindows.Edit_Boxes.Text(sym_eb));
          candidate.dot_gap:= TC.Real'Value(Text(len_eb));
        when dash =>
          candidate.dash_length:= TC.Real'Value(Text(len_eb));
      end case;
    end Get_Box_Data;

  begin
    y:= 30;

    Create_As_Dialog(
      pan, window.parent.all,
      Filter_amp(Msg(vtogltb)),
      Width => 300, Height => 140+y
    );

    Center(pan);
    Small_Icon (Pan, "Options_Icon");
    On_Destroy_Handler (pan, Get_Box_Data'Unrestricted_Access);

    GWin_Util.Use_GUI_Font(pan);

    candidate:= Window.Draw_Control.current_ls;
    candidate.pattern:= new_pattern;

    case candidate.pattern is
      when plain => null;
      when dash =>
        Create_Label(pan,Msg(dash_size), 10,  y, 130, 20);
        Create (len_eb, pan, TeX_Number(candidate.dash_length,2), 180, y,  40, 20);
      when dot =>
        Create_Label(pan,Msg(dot_gap), 10,  y, 130, 20);
        Create(len_eb, pan, TeX_Number(candidate.dot_gap,2), 180, y,  40, 20);
        Create_Label(pan,Msg(dot_symbol), 10,  y+30, 130, 20);
        Create(sym_eb, pan, To_String(candidate.dot_symbol), 180, y+30, 80, 22);
    end case;

    Create (oki, pan, "O&K", 20,
            Client_Area_Height (pan) - 40, 60, 25, ID => IDOK);
    Create (cancel, pan, Msg(mcancel), 100,
            Client_Area_Height (pan) - 40, 60, 25, ID => IDCANCEL);

    case candidate.pattern is
      when plain => null;
      when dot   =>  Focus(sym_eb);
      when dash  =>  Focus(len_eb);
    end case;

    Show_Dialog_with_Toolbars_off(pan, window.parent.all, window.parent.all, result);

    case Result is
      when IDOK     => Window.Draw_Control.current_ls:= candidate;
      when others   => null; -- Contains IDCANCEL
    end case;

  end Change_Pattern_Params;

  --------------------
  -- On_Menu_Select --
  --------------------

  procedure On_Menu_Select
    (Window : in out MDI_Picture_Child_Type;
     Item   : in     Integer)
  is

    procedure Command( c: MDI_child_cmd ) is
      modified, success: Boolean;
    begin
      case c is
        when save       => On_Save (Window);
        when save_as    => On_Save_As (Window, macro=> False);
        when close      => Close (Window);
        when zoom_minus => Zoom_picture(Window,-2);
        when zoom_plus  => Zoom_picture(Window,+2);
        when preview    => Preview(Window);
        when clean_pic  => TC.GWin.Tools.Cleanup_dialog(Window);
        when pic_opt_dialog =>
          On_Picture_Options(
            Window,
            Window.Draw_Control.picture.opt,
            Window.parent.all,
            modified,
            Msg(opicopt) & " - '" & Shorten_filename(Text(Window)) & ''');
          Window.Draw_Control.picture.saved:=
            Window.Draw_Control.picture.saved and not modified;
          if modified then
            Refresh_size_dependent_parameters(
              Window.Draw_Control.Picture,
              objects => True
            );
            Zoom_picture(Window,0); -- Show new zoom factor
            Window.Draw_Control.picture.refresh:= full; -- 14-Oct-2003
            Show_Totals(Window); -- show the '*' for modified 2-Aug-2005
          end if;

        when Permanent_direct_cmd =>
          Window.Draw_Control.current_cmd:= c;
          Update_Permanent_Command(Window);

        when Line_setting_cmd =>
          case Line_setting_cmd(c) is
            when thin  => Window.Draw_Control.current_ls.thickness:= thin;
            when thick => Window.Draw_Control.current_ls.thickness:= thick;
            --
            when plain      => Window.Draw_Control.current_ls.pattern:= plain;
            when dot        => Window.Draw_Control.current_ls.pattern:= dot;
            when dot_param  => Change_Pattern_Params(Window, dot);
            when dash       => Window.Draw_Control.current_ls.pattern:= dash;
            when dash_param => Change_Pattern_Params(Window, dash);
            --
            when no_arrow => Window.Draw_Control.current_ls.arrows:= no_arrow;
            when head     => Window.Draw_Control.current_ls.arrows:= head;
            when both     => Window.Draw_Control.current_ls.arrows:= both;
            when middle   => Window.Draw_Control.current_ls.arrows:= middle;
          end case;
          Update_Permanent_Command(Window);

        when Select_cmd =>
          case Select_cmd(c) is
            when unselect   =>
              PicPic( Window.Draw_Control.picture, unpick_all );
              Window.Draw_Control.picture.refresh:= unpicked;
            when select_all =>
              PicPic( Window.Draw_Control.picture, pick_all );
              Window.Draw_Control.picture.refresh:= picked;
          end case;
          Window.Draw_Control.current_cmd:= pick_obj;
          Update_Permanent_Command(Window);
          Subtle_redraw(Window.Draw_Control);
          Show_Totals(Window);

        when Action_on_picked_cmd =>
          if Window.Draw_Control.picture.picked = 0 then
            Message_Box(Window, "",Msg(no_picked), OK_Box, Error_Icon);
          else
            case Action_on_picked_cmd(c) is
              when Deformation_cmd =>
                Window.Draw_Control.current_cmd:= c;
                Update_Permanent_Command(Window);
              when Removes_picked_cmd =>
                if c = cut_clip then    -- "Cut" is like "Copy" and "Delete"
                  Copy_clip( Window );
                end if;
                Del_picked( Window.Draw_Control.picture );
                Window.Draw_Control.picture.refresh:= full;
                Window.Draw_Control.current_cmd:= pick_obj;
                Update_Permanent_Command(Window);
              when copy_clip  => Copy_clip( Window );
              when save_macro => On_Save_As (Window, macro=> True);
            end case;
            Subtle_redraw(Window.Draw_Control);
            Show_Totals(Window);
          end if;

        when paste_clip | load_macro  =>
          if c = load_macro then
            On_Open_Macro(Window,success);
          else
            Clipboard_to_Clip_file(Window);
            declare
              n: constant String:= Clip_filename;
            begin
              success:= GWin_Util.Exist(n);
              if success then
                Window.Macro_Name:= To_GString_unbounded(n);
              end if;
            end;
          end if;
          if success then
            Window.Draw_Control.capture:= paste0;
            Show_mouse_mode(Window.Draw_Control);
            Change_Cursor(Window.Draw_Control, cur_set_origin);
            -- ^ The ball is in Mousing camp now...
          else
            Message_Box(Window, Msg(error), Msg(fnotfound), Icon => Exclamation_Icon);
          end if;
      end case;
      -- In any case, we don't want arrows with boxes. What a bad taste!
      if Window.Draw_Control.current_cmd in Box_cmd then
        Window.Draw_Control.current_ls.arrows:= no_arrow;
      end if;
    end Command;

  begin
    for c in MDI_child_cmd loop
      if  Item = ID_custom(c) or else
         (Item = ID_std(c) and then Item /= no_std_id)
      then
        Command(c);
        exit;
      end if;
    end loop;
  end On_Menu_Select;

  -------------
  -- On_Save --
  -------------

  procedure On_Save (Window : in out MDI_Picture_Child_Type)
  is
    File_Name : constant GWindows.GString := To_GString_From_Unbounded (Window.File_Name);
  begin
    if File_Name = "" then
      On_Save_As (Window, macro=> False);
    else
      Save (Window, File_Name, macro=> False);
    end if;
  end On_Save;

  ----------------
  -- On_Save_As --
  ----------------

  procedure On_Save_As (Window : in out MDI_Picture_Child_Type; macro: Boolean)
  is
     New_File_Name : GWindows.GString_Unbounded;
     File_Title    : GWindows.GString_Unbounded;
     Success       : Boolean;
     saveas_or_macro: constant array(Boolean) of Message:=
       (True=> save_macro, False=> save_as);
     file_kind: constant array(Boolean) of Message:=
       (True=> tcd_mac, False=> ltx_pic);
     function Suffix return String is
     begin
       if macro then
         return Mac_suffix;
       else
         return Pic_suffix;
       end if;
     end Suffix;
  begin
    if macro then
      New_File_Name := To_GString_Unbounded("");
    else
      New_File_Name := Window.File_Name;
    end if;
    Save_File (Window, Msg(saveas_or_macro(macro)) & "...",
               New_File_Name,
               ((To_GString_Unbounded (Msg(file_kind(macro)) &
                   " (*." & Suffix & ")"),
                 To_GString_Unbounded ("*." & Suffix )),
                (To_GString_Unbounded (Msg(All_Files) & " (*.*)"),
                 To_GString_Unbounded ("*.*"))),
               '.' & Suffix,
               File_Title,
               Success);
    if Success then
      if
        GWin_Util.Exist(To_String (To_GString_From_Unbounded (New_File_Name)))
      then
         begin
            if Message_Box (Window,
                            Msg(saveas_or_macro(macro)),
                            To_GString_From_Unbounded (New_File_Name) &
                            Msg(exists) & NL &
                            Msg(replace),
                            Yes_No_Box,
                            Exclamation_Icon) = No
            then
               return;
            end if;
         end;
      end if;

      if not macro then
        Window.File_Name := New_File_Name;
        Text (Window, To_GString_From_Unbounded (File_Title));
        Window.Short_Name:= File_Title;
        Update_Common_Menus(Window,To_GString_from_Unbounded(New_File_Name));
      end if;
      Save (Window, To_GString_From_Unbounded (New_File_Name), macro=> macro);
    end if;
  end On_Save_As;

  ----------
  -- Save --
  ----------

  procedure Save (Window    : in out MDI_Picture_Child_Type;
                  File_Name : in     GWindows.GString;
                  macro     :        Boolean )
  is
    written_name: GString_Unbounded:=
      To_GString_Unbounded(File_Name);
    temp_ext: constant GString:= ".$$$";
    backup_name: constant String:=
      To_String(File_Name) & '.' &
      To_String(gen_opt.bak_suff);

    with_backup: constant Boolean:= TC.gen_opt.bak_enabled and not macro;

    use GNAT.OS_Lib;
    ok : Boolean;
    save_error, backup_error: exception;

  begin
    if with_backup then
      written_name:= written_name & temp_ext;
    end if;
    TC.Output.Save(
      pic            => Window.Draw_control.Picture,
      macro          => macro,
      file_name      => To_String(written_name),
      displayed_name => To_String(file_name)
    );
    if with_backup then
      -- If there was an exception at writing,
      -- the original file is untouched.
      --
      -- 1/ delete old backup
      if Exist(backup_name) then
        Delete_File( backup_name, ok );
        if not ok then
          raise backup_error;
        end if;
      end if;
      -- 2/ file -> backup
      if Exist(To_String(file_name)) then
        Rename_File(
          To_String(file_name),
          backup_name,
          ok
        );
        if not ok then
          raise backup_error;
        end if;
      end if;
      -- 3/ new file -> file
      Rename_File(
        To_String(written_name),
        To_String(file_name),
        ok
      );
      if not ok then
        raise save_error;
      end if;
    end if;
    Window.extra_first:= False;
    -- ^ even if it is the extra new window and is saved, we won't close
    --   it now on next open
    if not macro then
      Update_Common_Menus(Window,File_Name);
    end if;
    Show_Totals(Window);
  exception
    when backup_error =>
      begin
        Message_Box (Window,
                     Msg(save),
                     Msg(cannotbackup) &
                       ASCII.LF & "-> " &
                       To_GString_From_String(backup_name),
                     OK_Box,
                     Exclamation_Icon);
      end;
    when others =>
      begin
        Message_Box (Window,
                     Msg(save),
                     Msg(cannotsave) &
                       ASCII.LF & "-> " &
                       file_name,
                     OK_Box,
                     Exclamation_Icon);
      end;
  end Save;

  procedure On_Close (Window    : in out MDI_Picture_Child_Type;
                      Can_Close :    out Boolean) is
  begin
    Can_close:= True;
    if Window.Draw_Control.picture.saved then
      Update_Common_Menus(Window,To_GString_from_Unbounded(Window.File_Name));
    else
      loop
        case Message_Box
               (Window,
                Msg(close_not_saved),
                Msg(do_you_want_to_save) & ' ' &
                Msg(the_changes_you_made_to) & " '" &
                To_GString_from_Unbounded(Window.Short_Name) & "' ?",
                Yes_No_Cancel_Box,
                Exclamation_Icon)
        is
          when Yes    => On_Save(Window);
                         exit when Window.Draw_Control.picture.saved;
          when No     => exit;
          when Cancel => Success_In_Enumerated_Close:= False;
                         Can_close:= False;
                         exit;
          when others => null;
        end case;
      end loop;
    end if;
  end On_Close;

  -- !! bad try !!

  --  procedure On_Focus (Window : in out MDI_Picture_Child_Type) is
  --  begin
  --    Parent(Window.parent.Drawing_toolbar,Window);
  --  end;

  --  procedure On_Lost_Focus (Window : in out MDI_Picture_Child_Type) is
  --  begin
  --    --GWindows.Base.  Base_Window_Type(
  --    Parent(Window.parent.Drawing_toolbar,Window.parent.all);
  --  end;

end TC.GWin.MDI_Picture_Child;
