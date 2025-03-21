with TC.Input,
     TC.Output,
     TC.Picking;

with TC.GWin.Lang,
     TC.GWin.Display,
     TC.GWin.Mousing,
     TC.GWin.Menus,
     TC.GWin.Options_Dialogs,
     TC.GWin.Previewing,
     TC.GWin.Tabs,
     TC.GWin.Tools;

with GWindows.Application,
     GWindows.Buttons,
     GWindows.Clipboard,
     GWindows.Common_Dialogs,
     GWindows.Constants,
     GWindows.Drawing.Capabilities,
     GWindows.Edit_Boxes,
     GWindows.Message_Boxes,
     GWindows.Static_Controls,
     GWindows.Windows;

with GWin_Util;

with Ada.Directories,
     Ada.Exceptions,
     Ada.Strings.Fixed,
     Ada.Strings.Wide_Unbounded,
     Ada.Text_IO;

with Interfaces.C;

with GNAT.OS_Lib;

package body TC.GWin.MDI_Picture_Child is

  use TC.REF;
  use TC.GWin.Lang;
  use GWindows, GWindows.Base, GWindows.Menus, GWindows.Message_Boxes, GWindows.Windows;
  use GWin_Util;

  --  Adjust the Draw control's position to those of the scroll controls.
  --
  procedure Adjust_Draw_Control_Position (Window : in out MDI_Picture_Child_Type)
  is
  begin
    Window.Draw_Control.X0 := Scroll_Position (Window, Horizontal);
    Window.Draw_Control.Y0 := Scroll_Position (Window, Vertical);
    Move
      (Window.Draw_Control,
       0 - Window.Draw_Control.X0,
       0 - Window.Draw_Control.Y0);
  end Adjust_Draw_Control_Position;

  overriding procedure On_Size (Window : in out MDI_Picture_Child_Type;
                                Width  : in     Integer;
                                Height : in     Integer) is
  begin
    if user_maximize_restore then
      MDI_childen_maximized := Window.Zoom;
    end if;

    --  Taken from GWindows.Scroll_Panels
    if
      Client_Area_Width (Window) < Client_Area_Width (Window.Draw_Control)
    then
       Horizontal_Scroll_Bar (Window);
       Scroll_Range
         (Window, Horizontal, 0,
          Client_Area_Width (Window.Draw_Control) -
          Client_Area_Width (Window) + 30);
       Scroll_Page_Size (Window, Horizontal, 30);
       Adjust_Draw_Control_Position (Window);
    else
       Left (Window.Draw_Control, 0);
       Scroll_Position (Window, Horizontal, 0);
       Window.Draw_Control.X0 := 0;
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
       Adjust_Draw_Control_Position (Window);
    else
       Top (Window.Draw_Control, 0);
       Scroll_Position (Window, Vertical, 0);
       Window.Draw_Control.Y0 := 0;
       Vertical_Scroll_Bar (Window, False);
    end if;

    Window.Draw_Control.Disp_W := Client_Area_Width (Window);
    Window.Draw_Control.Disp_H := Client_Area_Height (Window);
  end On_Size;

  overriding procedure On_Horizontal_Scroll
    (Window  : in out MDI_Picture_Child_Type;
     Request : in     GWindows.Base.Scroll_Request_Type;
     Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Request = Thumb_Drag then
      Window.Draw_Control.X0 := Scroll_Drag_Position (Window, Horizontal);
      Window.Draw_Control.Y0 := Scroll_Drag_Position (Window, Vertical);
      Move (Window.Draw_Control,
            0 - Window.Draw_Control.X0,
            0 - Window.Draw_Control.Y0);
    else
       Adjust_Draw_Control_Position (Window);
       On_Horizontal_Scroll
         (GWindows.Windows.Window_Type (Window),
          Request,
          Control);
    end if;
  end On_Horizontal_Scroll;

  ------------------------
  -- On_Vertical_Scroll --
  ------------------------

  overriding procedure On_Vertical_Scroll
    (Window  : in out MDI_Picture_Child_Type;
     Request : in     GWindows.Base.Scroll_Request_Type;
     Control : in     GWindows.Base.Pointer_To_Base_Window_Class)

  is
  begin
    if Request = Thumb_Drag then
      Window.Draw_Control.X0 := Scroll_Drag_Position (Window, Horizontal);
      Window.Draw_Control.Y0 := Scroll_Drag_Position (Window, Vertical);
      Move (Window.Draw_Control,
            0 - Window.Draw_Control.X0,
            0 - Window.Draw_Control.Y0);
    else
      Adjust_Draw_Control_Position (Window);
      On_Vertical_Scroll (GWindows.Windows.Window_Type (Window),
                          Request,
                          Control);
    end if;
  end On_Vertical_Scroll;

  overriding procedure On_Erase_Background
    (Window : in out MDI_Picture_Child_Type;
     Canvas : in out GWindows.Drawing.Canvas_Type;
     Area   : in     GWindows.Types.Rectangle_Type)
  is
    pragma Unmodified (Window);
    pragma Unmodified (Canvas);
  begin
    null;  --  Do nothing! Avoids the flickering background/canvas.
  end On_Erase_Background;

  procedure Display_saved_bitmap
    (window : in out TC_Picture_Panel;
     area   :        GWindows.Types.Rectangle_Type)
  is
  begin
    window.Drawing_Area.BitBlt
      (area.Left,
       area.Top,
       area.Right - area.Left,
       area.Bottom - area.Top,
       window.Saved_Area,
       area.Left,
       area.Top);
    --  ### For monitoring (interesting!): ###
    --  Line(window.Drawing_Area,Area.Left,Area.Top,Area.Right,Area.Bottom);
  end Display_saved_bitmap;

  procedure Update_bitmap (Window : in out TC_Picture_Panel) is
  begin
    if Window.Picture.refresh /= no then
      Display.Draw
        (Window.Saved_Area, Window.Picture, null, Width (Window), Height (Window));
    end if;
  end Update_bitmap;

  procedure Subtle_Redraw (Window : in out TC_Picture_Panel) is
    mem : constant TC.Refresh_mode := Window.Picture.refresh;
  begin
    Update_bitmap (Window);
    if mem /= no then   --  Bitmap has changed, then repaint
      Redraw (Window);  --  the visible area(s) on screen.
    end if;
  end Subtle_Redraw;

  overriding procedure On_Paint (Window : in out TC_Picture_Panel;
                                 Canvas : in out GWindows.Drawing.Canvas_Type;
                                 Area   : in     GWindows.Types.Rectangle_Type)
  is
    pragma Warnings (Off, Canvas);  --  Canvas == Window.Drawing_Area
  begin
    Update_bitmap (Window);
    Display_saved_bitmap (Window, Area);
  end On_Paint;

  procedure Create_Menus (Window : in out MDI_Picture_Child_Type) is
    Main : constant Menu_Type := Create_Menu;
  begin
    Window.File_Menu := TC.GWin.Menus.Create_File_Menu (is_child => True);
    Append_Menu (Main, Msg (ffile), Window.File_Menu);

    Window.Draw_Menu := TC.GWin.Menus.Create_Draw_Menu;
    Append_Menu (Main, Msg (ddraw), Window.Draw_Menu);

    Window.Line_Menu := TC.GWin.Menus.Create_Line_Menu;
    Append_Menu (Main, Msg (lline), Window.Line_Menu);

    Window.Edit_Menu := TC.GWin.Menus.Create_Edit_Menu;
    Append_Menu (Main, Msg (eedit), Window.Edit_Menu);

    Append_Menu (Main, Msg (ttools), TC.GWin.Menus.Create_Tools_Menu);

    Window.View_Menu := TC.GWin.Menus.Create_View_Menu;
    Append_Menu (Main, Msg (vview), Window.View_Menu);

    Append_Menu
      (Main, Msg (oopt),
       TC.GWin.Menus.Create_Options_Menu (is_child => True));
    Append_Menu (Main, Msg (wwindow), TC.GWin.Menus.Create_Wndw_Menu);
    Append_Menu (Main, Msg (hhelp), TC.GWin.Menus.Create_Help_Menu);

    MDI_Menu (Window, Main, Window_Menu => 8);
  end Create_Menus;

  procedure Do_Change_Cursor (Window : in out Base_Window_Type'Class) is
  begin
    if Window in TC_Picture_Panel'Class then
      GWindows.Cursors.Set_Cursor (TC_Picture_Panel (Window).Cursor);
    end if;
  end Do_Change_Cursor;

  procedure Do_Left_Mouse_Down (Window : in out Base_Window_Type'Class;
                                X, Y   : in     Integer;
                                Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mousing.Mouse_Down (TC_Picture_Panel (Window), X, Y, Left_Button);
    end if;
  end Do_Left_Mouse_Down;

  procedure Do_Right_Mouse_Down (Window : in out Base_Window_Type'Class;
                                 X, Y   : in     Integer;
                                 Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mousing.Mouse_Down (TC_Picture_Panel (Window), X, Y, Right_Button);
    end if;
  end Do_Right_Mouse_Down;

  procedure Do_Mouse_Up (Window : in out Base_Window_Type'Class;
                         X, Y   : in     Integer;
                         Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mousing.Mouse_Up (TC_Picture_Panel (Window), X, Y);
    end if;
  end Do_Mouse_Up;

  procedure Do_Mouse_Move (Window : in out Base_Window_Type'Class;
                           X, Y   : in     Integer;
                           Keys   : in     Mouse_Key_States)
  is
    pragma Warnings (Off, Keys);
  begin
    if Window in TC_Picture_Panel'Class then
      Mousing.Mouse_Move (TC_Picture_Panel (Window), X, Y);
    end if;
  end Do_Mouse_Move;

  procedure Do_Mouse_Wheel (Window  : in out Base_Window_Type'Class;
                            X       : in     Integer;
                            Y       : in     Integer;
                            Keys    : in     Mouse_Key_States;
                            Z_Delta : in Integer)
  is
    pragma Unreferenced (X, Y);
    --  NB: the handler is attached to the MDI_Picture_Child_Type.
    --  This need GWindows rev. 472+ to work correctly (without
    --  the mouse cursor having "blind spots" on the window client area)..
    --  When handler is attached to the TC_Picture_Panel, the mouse wheel
    --  movements have an effect even if the mouse pointer is off the child
    --  window (non-maximized mode) on the right / bottom sides.
    --  The reason is that the panel's width may be larger than
    --  the visible portion in the window.
    pic_child : MDI_Picture_Child_Type renames MDI_Picture_Child_Type (Window);
    v_pos : constant Natural := Scroll_Position (pic_child, Vertical);
    dy : constant Natural := Scroll_Page_Size (pic_child, Vertical);
    z : Integer;
  begin
    if Z_Delta /= 0 then
      z := Z_Delta / abs Z_Delta;
      if Keys (Control) then
        Zoom_Picture (pic_child, z);
      else
        Scroll_Position (pic_child, Vertical, v_pos - dy * z);
        Adjust_Draw_Control_Position (pic_child);
      end if;
    end if;
  end Do_Mouse_Wheel;

  --  Added Key down handler for steering mouse cursor with arrow keys 14-Oct-2005

  procedure Do_Key_Down
              (Window      : in out GWindows.Base.Base_Window_Type'Class;
               Special_Key : in     Special_Key_Type;
               Value       : in     GCharacter)
  is
    pragma Unreferenced (Value);
    --
    pic_child : MDI_Picture_Child_Type renames MDI_Picture_Child_Type (Window);
    v_pos : constant Natural := Scroll_Position (pic_child, Vertical);
    dy : constant Natural := Scroll_Page_Size (pic_child, Vertical);
    --
    procedure Move_mouse_cursor (mdx, mdy : Integer) is
      p : constant GWindows.Types.Point_Type := GWindows.Cursors.Get_Cursor_Position;
    begin
      GWindows.Cursors.Set_Cursor_Position (p.X + mdx, p.Y + mdy);
    end Move_mouse_cursor;
  begin
    case Special_Key is
      when Left_Key     => Move_mouse_cursor (-1,  0);
      when Up_Key       => Move_mouse_cursor  (0, -1);
      when Right_Key    => Move_mouse_cursor (+1,  0);
      when Down_Key     => Move_mouse_cursor  (0, +1);
      when Page_Up      =>
        Scroll_Position (pic_child, Vertical, v_pos - dy);
        Adjust_Draw_Control_Position (pic_child);
      when Page_Down    =>
        Scroll_Position (pic_child, Vertical, v_pos + dy);
        Adjust_Draw_Control_Position (pic_child);
      when others       => null;
    end case;
  end Do_Key_Down;

  --  This will update file menu of MDI_Root and all of this windows' brothers.
  procedure Update_Common_Menus (Window        : MDI_Picture_Child_Type;
                                 top_mru_entry : GString := "")
  is
  begin
    Window.mdi_root.Update_Common_Menus (top_mru_entry);
  end Update_Common_Menus;

  procedure Update_Permanent_Command (Window : in out MDI_Picture_Child_Type) is

    procedure Radio_Check_Custom
      (m                    : in Menu_Type;
       first, last, checked : in Custom_cmd)
    is
    begin
      Radio_Check
        (m, Command,
         ID_custom (first), ID_custom (last), ID_custom (checked));
    end Radio_Check_Custom;

    procedure Update_cmd (m : in Menu_Type) is
    begin
      Radio_Check_Custom
        (m,
         Permanent_cmd'First, Permanent_cmd'Last, Window.Draw_Control.current_cmd);
    end Update_cmd;

    procedure Update_ls is
      c : Custom_cmd;
    begin
      Radio_Check_Custom
        (Window.Line_Menu,
         Line_thickness_cmd'First,
         Line_thickness_cmd'Last,
         Custom_cmd'Val
           (Custom_cmd'Pos (Line_thickness_cmd'First) +
            Line_Thickness'Pos (Window.Draw_Control.current_ls.thickness)));
      c := Custom_cmd'Val
            (Custom_cmd'Pos (Line_pattern_cmd'First) +
             Line_Pattern'Pos (Window.Draw_Control.current_ls.pattern));
      if c = dot_param then
        c := dash;
      end if;
      Radio_Check_Custom
        (Window.Line_Menu,
         Line_pattern_cmd'First, Line_pattern_cmd'Last, c);
      Radio_Check_Custom
        (Window.Line_Menu,
         Line_arrows_cmd'First,
         Line_arrows_cmd'Last,
         Custom_cmd'Val
           (Custom_cmd'Pos (Line_arrows_cmd'First) +
            Line_Arrows'Pos (Window.Draw_Control.current_ls.arrows)));
    end Update_ls;

    use MDI_Main, Mousing;

  begin
    Update_cmd (Window.Draw_Menu);
    Update_cmd (Window.Edit_Menu);
    Update_ls;
    case Window.Draw_Control.current_cmd is
      when pick_obj    => Change_Cursor (Window.Draw_Control, cur_picking);
      when change_text => Change_Cursor (Window.Draw_Control, cur_chg_text);
      when others      => Change_Cursor (Window.Draw_Control, cur_arrow);
    end case;
    Window.mdi_root.Update_Status_Bar
      (command,
       Filter_amp (Msg (msg_for_command (Window.Draw_Control.current_cmd))));
  end Update_Permanent_Command;

  zoom_factor : constant Real := 2.0 ** (1.0 / 8.0);

  procedure Zoom_Picture
    (Window                : in out MDI_Picture_Child_Type;
     exponential_direction : in     Integer)
  is
    opt : TC.Picture_Options renames Window.Draw_Control.Picture.opt;
    sf : String (1 .. 20);
    use Ada.Strings, Ada.Strings.Fixed, MDI_Main;
  begin
    if exponential_direction /= 0 then
      opt.zoom_fac := opt.zoom_fac * (zoom_factor ** exponential_direction);
      Window.Draw_Control.Picture.refresh := full;
      Subtle_Redraw (Window.Draw_Control);
    end if;
    RIO.Put (sf, opt.zoom_fac, 2, 0);
    Window.mdi_root.Update_Status_Bar (zoom, S2G (Trim (sf, Left)));
  end Zoom_Picture;

  procedure Update_Information (Window : in out MDI_Picture_Child_Type) is
    p : TC.Picture renames Window.Draw_Control.Picture;
    is_modified : constant Boolean := not p.saved;
    Star : constant array (Boolean) of GCharacter := (False => ' ', True => '*');

    procedure Update_Tool_Bar is
      bar : Office_Applications.Classic_Main_Tool_Bar_Type
        renames Window.mdi_root.Tool_Bar;
    begin
      bar.Enabled (ID_custom (save), is_modified);
      --  !!  update on possible undo/redo
      bar.Enabled (ID_custom (tc_undo), False);
      bar.Enabled (ID_custom (tc_redo), False);
    end Update_Tool_Bar;

    procedure Update_Menus is
    begin
      State (Window.File_Menu, Command, ID_custom (save), bool_to_state (is_modified));
      State
        (Window.File_Menu,
         Command,
         ID_custom (open_folder),
         bool_to_state (Ada.Strings.Wide_Unbounded.Length (Window.ID.file_name) > 0));
      --  !!  update on possible undo/redo
      State (Window.Edit_Menu, Command, ID_custom (tc_undo), Disabled);
      State (Window.Edit_Menu, Command, ID_custom (tc_redo), Disabled);
    end Update_Menus;

    function Show_Total (pfx : GString; t, th : Integer) return GString is
    begin
      if t = 0 then
        return "";
      elsif th = 0 then
        return pfx & S2G (t'Image);
      else
        return pfx & S2G (t'Image & "  (h:" & th'Image & ')');
      end if;
    end Show_Total;

    use MDI_Main;

  begin
    --  Update window title.
    if is_modified then
      Window.Text ("* " & GU2G (Window.ID.short_name));
    else
      Window.Text (GU2G (Window.ID.short_name));
    end if;
    --  Update statistics
    Window.mdi_root.Update_Status_Bar
      (stat_objects,
       Show_Total ("Objects:", p.total, p.total_hidden) &
       Show_Total ("; picked:", p.picked, p.picked_hidden));
    --  Put a '*' if picture is modified.
    Window.mdi_root.Update_Status_Bar (modified, (1 => Star (is_modified)));
    --  Update state of "Save" button, "Save " menu entry and window title.
    Update_Tool_Bar;
    Update_Menus;
    --  Show picture's zoom factor.
    Window.Zoom_Picture (0);
  end Update_Information;

  ---------------
  -- On_Create --
  ---------------

  overriding procedure On_Create (Window : in out MDI_Picture_Child_Type) is
    win_asp_x, win_asp_y : Interfaces.C.unsigned;
    use type Interfaces.C.unsigned;
    use GWindows.Application, GWindows.Drawing, GWindows.Drawing.Capabilities;
    use MDI_Main;
  begin
    Small_Icon (Window, "Picture_Icon");

    Create_As_Control (Window.Draw_Control, Window,
                       Top    => 0,
                       Left   => 0,
                       Width  => Desktop_Width,
                       Height => Desktop_Height);

    Window.Draw_Control.Picture.refresh := full;

    --  Filial feelings:
    Window.mdi_root := MDI_Main_Access (Controlling_Parent (Window));
    Window.Draw_Control.pic_parent :=
      MDI_Picture_Child_Access (Controlling_Parent (Window.Draw_Control));
    Window.Draw_Control.main := Window.mdi_root;

    Use_GUI_Font (Window.Draw_Control);

      --  Dock (Window.Draw_Control, GWindows.Base.Fill);
      --  Auto_Resize(Window.Draw_Control);
      --  Dock_Children (Window.Scroll_Panel.Panel);
      Window.Draw_Control.Drawing_Area.Background_Mode (Transparent);
      Window.Draw_Control.Saved_Area.Background_Mode (Transparent);

      On_Change_Cursor_Handler (Window.Draw_Control, Do_Change_Cursor'Access);
      On_Left_Mouse_Button_Down_Handler (Window.Draw_Control, Do_Left_Mouse_Down'Access);
      On_Right_Mouse_Button_Down_Handler (Window.Draw_Control, Do_Right_Mouse_Down'Access);
      On_Left_Mouse_Button_Up_Handler (Window.Draw_Control, Do_Mouse_Up'Access);
      On_Right_Mouse_Button_Up_Handler (Window.Draw_Control, Do_Mouse_Up'Access);
      On_Mouse_Move_Handler (Window.Draw_Control, Do_Mouse_Move'Access);
      --
      On_Mouse_Wheel_Handler (Window, Do_Mouse_Wheel'Access);
      --
      On_Character_Down_Handler (Window, Do_Key_Down'Access);  --  14-Oct-2005

      Get_Canvas (Window.Draw_Control, Window.Draw_Control.Drawing_Area);
      Window.Draw_Control.Saved_Area.Create_Memory_Canvas (Window.Draw_Control.Drawing_Area);

      Window.Draw_Control.Drawing_Area.Create_Compatible_Bitmap
        (Window.Draw_Control.Saved_Bitmap,
         Desktop_Width,
         Desktop_Height);
      Window.Draw_Control.Saved_Area.Select_Object
        (Window.Draw_Control.Saved_Bitmap);

      Refresh_Size_Dependent_Parameters
        (Window.Draw_Control.Picture,
         objects => True);
      --  ^default pt, not yet from options.

      Subtle_Redraw (Window.Draw_Control);

      --  Dock_Children (Window);

      win_asp_x := Get_Capability (Window.Draw_Control.Drawing_Area, ASPECTX);
      win_asp_y := Get_Capability (Window.Draw_Control.Drawing_Area, ASPECTY);
      --  GWindows.Message_Boxes.Message_Box(Window,
      --    "Aspect",
      --            "ASPECTX" & Interfaces.C.Unsigned'image(win_asp_x)
      --     & NL & "ASPECTY" & Interfaces.C.Unsigned'image(win_asp_y)
      --  );

      if win_asp_x = 0 or win_asp_y = 0 then
        Window.Draw_Control.Picture.aspect := 1.0;
      else
        Window.Draw_Control.Picture.aspect := Real (win_asp_y) / Real (win_asp_x);
      end if;

      Create_Menus (Window);

      Window.Draw_Control.current_cmd := line;
      Window.Draw_Control.current_ls := normal_line_settings;
      Window.Draw_Control.capture := none;

      --  2007: Maximize-demaximize (non-maximized case) to
      --        avoid invisible windows...
      declare
        memo_unmaximized_children : constant Boolean := not MDI_childen_maximized;
      begin
        if memo_unmaximized_children then
          Window.mdi_root.Freeze;
          Window.Zoom;
        end if;
        Window.On_Size (Window.Width, Window.Height);
        if memo_unmaximized_children then
          Window.mdi_root.Thaw;  --  Before Zoom, otherwise uncomplete draw.
          Window.Zoom (False);
          Window.mdi_root.Tool_Bar.Redraw;
        end if;
      end;

      Window.Scroll_Position (Vertical, Window.Scroll_Maximum (Vertical));
      Adjust_Draw_Control_Position (Window);
      Update_Common_Menus (Window);
      Update_Information (Window);
  end On_Create;

  procedure Create_TeXCAD_MDI_Child
    (Window : in out MDI_Picture_Child_Type;
     Parent : in out MDI_Main.MDI_Main_Type;
     ID     : in     ID_Type)
  is
    procedure Append_Tab is
      title : constant GString := GU2G (Window.ID.short_name);
    begin
      Parent.tab_bar.Insert_Tab (Parent.tab_bar.Tab_Count, Simple_Name (title));
      Parent.tab_bar.Selected_Tab (Parent.tab_bar.Tab_Count - 1);
      Parent.tab_bar.info.Append ((Window.ID, Window'Unrestricted_Access));
    end Append_Tab;
  begin
    Window.ID := ID;
    Create_MDI_Child (Window, Parent, GU2G (ID.short_name), Is_Dynamic => True);
    Parent.MDI_Active_Window (Window);
    Append_Tab;
  end Create_TeXCAD_MDI_Child;

  procedure Preview (window : in out MDI_Picture_Child_Type) is
    ttl : constant GString := Text (window);
    ti : Integer;
  begin
    if window.Draw_Control.Picture.opt.sty (emlines) then
      case Message_Box
        (window, Msg (preview), Msg (noemlines1) & NL & Msg (noemlines2),
         Yes_No_Cancel_Box, Question_Icon)
      is
        when Yes =>
          window.Draw_Control.Picture.opt.sty (emlines) := False;
          window.Draw_Control.Picture.saved := False;
          Update_Information (window);  --  Show the '*' for modified. 2-Aug-2005.
        when Cancel =>
          return;
        when others =>  --  includes the "No" choice.
          null;
      end case;
    end if;
    ti := ttl'Last;
    for i in reverse ttl'Range loop
      case ttl (i) is
        when '\' | '/' | '-' => exit;  --  stop character
        when others => null;
      end case;
      ti := i;
    end loop;
    TC.GWin.Previewing.Create_files
      (pic   => window.Draw_Control.Picture,
       title => G2S (ttl (ti .. ttl'Last)));
    TC.GWin.Previewing.Start;
  exception
    when E : TC.GWin.Previewing.Preview_Error =>
      Message_Box (
        window,
        Msg (preview),
        Msg (prev_fail) & NL & NL & S2G (Ada.Exceptions.Exception_Message (E)),
        OK_Box, Exclamation_Icon);
      TC.GWin.Previewing.Cleanup;
  end Preview;

  function Clip_filename return String is
  begin
    return Temp_dir & "TeXCADcp.txt";
  end Clip_filename;

  procedure Clipboard_to_Clip_file (Window : in Window_Type'Class) is
    contents : constant GString :=
      GWindows.Clipboard.Clipboard_Text (Window_Type (Window));
    use Ada.Text_IO;
    f : File_Type;
  begin
    if contents = "" then
      null; -- do nothing, we still might have the clipboard file
    else
      Create (f, Out_File, Clip_filename);
      Put_Line (f, G2S (contents));
      Close (f);
    end if;
  end Clipboard_to_Clip_file;

  procedure Clip_file_to_Clipboard (Window : in Window_Type'Class) is
    use Ada.Text_IO;
    f : File_Type;
    contents : Unbounded_String := Null_Unbounded_String;
  begin
    if not Ada.Directories.Exists (Clip_filename) then
      null; -- do nothing, we still might have something on the Windows clipboard
    else
      Open (f, In_File, Clip_filename);
      while not End_Of_File (f) loop
        contents := contents & Get_Line (f) & ASCII.LF;
      end loop;
      Close (f);
      GWindows.Clipboard.Clipboard_Text (Window_Type (Window), S2G (To_String (contents)));
    end if;
  end Clip_file_to_Clipboard;

  procedure Copy_clip (Window : in out MDI_Picture_Child_Type) is
  begin
    TC.Output.Save
      (pic            => Window.Draw_Control.Picture,
       macro          => True,
       file_name      => Clip_filename,
       displayed_name => "Clipboard");
    Clip_file_to_Clipboard (Window);
  end Copy_clip;

  procedure On_Open_Macro
    (Window  : in out MDI_Picture_Child_Type;
     Success :    out Boolean)
  is
    File_Name, File_Title : GString_Unbounded;
  begin
    GWindows.Common_Dialogs.Open_File
      (Window, Msg (open),
       File_Name,
       ((G2GU (Msg (tcd_mac) & " (*." & S2G (Mac_Suffix) & ")"),
           G2GU ("*." & S2G (Mac_Suffix))),
         (G2GU (Msg (all_files) & " (*.*)"),
           G2GU ("*.*"))),
       '.' & S2G (Mac_Suffix),
       File_Title,
       Success);
    if Success then
      Window.Macro_Name := File_Name;
    end if;
  end On_Open_Macro;

  procedure Load_Macro (Window : in out MDI_Picture_Child_Type) is
    pw : Picture renames Window.Draw_Control.Picture;
    mo : Picture_Options;  --  save all picture options, overwritten by loading
  begin
    mo := pw.opt;
    pw.opt.P0 := Window.Draw_Control.PU;  --  origin on mouse cursor
    begin
      TC.Input.Load (pw, True, G2S (GU2G (Window.Macro_Name)));
    exception
      when E : TC.Input.Load_error =>
        Message_Box
          (Window,
           "Error when loading picture data",
           "The LaTeX picture on" & NL &
           "clipboard / macro / partial / temp / scrap / (whatever)" & NL &
           "is ill-formed." & NL &
           "Items were only partially loaded, or not at all." & NL &
           "--- Message ---" & NL &
           S2G (Ada.Exceptions.Exception_Message (E)),
           Icon => Exclamation_Icon);
    end;
    pw.opt := mo;  --  restore options
    Refresh_Size_Dependent_Parameters (pw, objects => True);
    --  ^ was missing ! Fixed 14-Jan-2004
    pw.refresh := shadows_and_objects;
  end Load_Macro;

  --  23-Feb-2004

  procedure Change_Pattern_Params
    (Window      : in out MDI_Picture_Child_Type;
     new_pattern :        Line_Pattern)
  is
    pan             : Window_Type;
    sym_eb, len_eb  : GWindows.Edit_Boxes.Edit_Box_Type;
    oki             : GWindows.Buttons.Default_Button_Type;
    cancel          : GWindows.Buttons.Button_Type;
    Result, y       : Integer;

    candidate : Line_Settings;

    use GWindows.Constants, GWindows.Static_Controls;

    procedure Get_Box_Data
      (parent : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Unreferenced (parent);
    begin
      case candidate.pattern is
        when plain => null;
        when dot =>
          candidate.dot_symbol := To_Unbounded_String (G2S (GWindows.Edit_Boxes.Text (sym_eb)));
          candidate.dot_gap := TC.Real'Value (G2S (len_eb.Text));
        when dash =>
          candidate.dash_length := TC.Real'Value (G2S (len_eb.Text));
      end case;
    end Get_Box_Data;

  begin
    y := 30;

    Create_As_Dialog
      (pan, Window.mdi_root.all,
       Filter_amp (Msg (vtogltb)),
       Width => 300, Height => 140 + y);

    pan.Center (Window);
    Small_Icon (pan, "Options_Icon");
    On_Destroy_Handler (pan, Get_Box_Data'Unrestricted_Access);

    GWin_Util.Use_GUI_Font (pan);

    candidate := Window.Draw_Control.current_ls;
    candidate.pattern := new_pattern;

    case candidate.pattern is
      when plain => null;
      when dash =>
        Create_Label (pan, Msg (dash_size), 10,  y, 130, 20);
        len_eb.Create (pan, S2G (TeX_Number (candidate.dash_length, 2)), 180, y,  40, 20);
      when dot =>
        Create_Label (pan, Msg (dot_gap), 10,  y, 130, 20);
        len_eb.Create (pan, S2G (TeX_Number (candidate.dot_gap, 2)), 180, y,  40, 20);
        Create_Label (pan, Msg (dot_symbol), 10,  y + 30, 130, 20);
        sym_eb.Create (pan, S2G (To_String (candidate.dot_symbol)), 180, y + 30, 80, 22);
    end case;

    oki.Create
      (pan, "O&K", 20, Client_Area_Height (pan) - 40, 60, 25, ID => IDOK);
    cancel.Create
      (pan, Msg (mcancel), 100, Client_Area_Height (pan) - 40, 60, 25, ID => IDCANCEL);

    case candidate.pattern is
      when plain => null;
      when dot   => sym_eb.Focus;
      when dash  => len_eb.Focus;
    end case;

    MDI_Main.Show_Dialog_with_Toolbars_off
      (pan, Window.mdi_root.all, Window.mdi_root.all, Result);

    case Result is
      when IDOK   => Window.Draw_Control.current_ls := candidate;
      when others => null;  --  Contains the IDCANCEL case.
    end case;

  end Change_Pattern_Params;

  --------------------
  -- On_Menu_Select --
  --------------------

  overriding procedure On_Menu_Select
    (Window : in out MDI_Picture_Child_Type;
     Item   : in     Integer)
  is

    procedure Command (c : MDI_child_cmd) is
      modified, success : Boolean;
      use TC.Picking, Ada.Strings.Wide_Unbounded;
    begin
      case c is
        when save        => On_Save (Window);
        when save_as     => On_Save_As (Window, macro => False);
        when close       => Close (Window);
        when open_folder =>
          if Window.ID.file_name /= "" then
            GWin_Util.Start (Ada.Directories.Containing_Directory (G2S (GU2G (Window.ID.file_name))));
          end if;
        when zoom_minus => Window.Zoom_Picture (-2);
        when zoom_plus  => Window.Zoom_Picture (+2);
        when preview    => Preview (Window);
        when clean_pic  => TC.GWin.Tools.Cleanup_Dialog (Window);
        when pic_opt_dialog =>
          TC.GWin.Options_Dialogs.On_Picture_Options
            (Window,
             Window.Draw_Control.Picture.opt,
             Window.mdi_root.all,
             modified,
             G2S (Msg (opicopt) & " - '" &
               Office_Applications.Shorten_File_Name
                 (Text (Window), 30) & '''));
          Window.Draw_Control.Picture.saved :=
            Window.Draw_Control.Picture.saved and not modified;
          if modified then
            Refresh_Size_Dependent_Parameters
              (Window.Draw_Control.Picture,
               objects => True);
            Window.Draw_Control.Picture.refresh := full;  --  14-Oct-2003
            Update_Information (Window);  --  Show the '*' for modified. 2-Aug-2005.
          end if;

        when Permanent_direct_cmd =>
          Window.Draw_Control.current_cmd := c;
          Update_Permanent_Command (Window);

        when Line_setting_cmd =>
          case Line_setting_cmd (c) is
            when thin  => Window.Draw_Control.current_ls.thickness := thin;
            when thick => Window.Draw_Control.current_ls.thickness := thick;
            --
            when plain      => Window.Draw_Control.current_ls.pattern := plain;
            when dot        => Window.Draw_Control.current_ls.pattern := dot;
            when dot_param  => Change_Pattern_Params (Window, dot);
            when dash       => Window.Draw_Control.current_ls.pattern := dash;
            when dash_param => Change_Pattern_Params (Window, dash);
            --
            when no_arrow => Window.Draw_Control.current_ls.arrows := no_arrow;
            when head     => Window.Draw_Control.current_ls.arrows := head;
            when both     => Window.Draw_Control.current_ls.arrows := both;
            when middle   => Window.Draw_Control.current_ls.arrows := middle;
          end case;
          Update_Permanent_Command (Window);

        when Select_cmd =>
          case Select_cmd (c) is
            when unselect   =>
              PicPic (Window.Draw_Control.Picture, unpick_all);
              Window.Draw_Control.Picture.refresh := unpicked;
            when select_all =>
              PicPic (Window.Draw_Control.Picture, pick_all);
              Window.Draw_Control.Picture.refresh := picked;
          end case;
          Window.Draw_Control.current_cmd := pick_obj;
          Update_Permanent_Command (Window);
          Subtle_Redraw (Window.Draw_Control);
          Update_Information (Window);

        when Action_on_picked_cmd =>
          if Window.Draw_Control.Picture.picked = 0 then
            Message_Box (Window, "", Msg (no_picked), OK_Box, Error_Icon);
          else
            case Action_on_picked_cmd (c) is
              when Deformation_cmd =>
                Window.Draw_Control.current_cmd := c;
                Update_Permanent_Command (Window);
              when Removes_picked_cmd =>
                if c = cut_clip then    -- "Cut" is like "Copy" and "Delete"
                  Copy_clip (Window);
                end if;
                Del_picked (Window.Draw_Control.Picture);
                Window.Draw_Control.Picture.refresh := full;
                Window.Draw_Control.current_cmd := pick_obj;
                Update_Permanent_Command (Window);
              when copy_clip  => Copy_clip (Window);
              when save_macro => On_Save_As (Window, macro => True);
            end case;
            Subtle_Redraw (Window.Draw_Control);
            Update_Information (Window);
          end if;

        when paste_clip | load_macro  =>
          if c = load_macro then
            On_Open_Macro (Window, success);
          else
            Clipboard_to_Clip_file (Window);
            declare
              n : constant String := Clip_filename;
            begin
              success := Ada.Directories.Exists (n);
              if success then
                Window.Macro_Name := G2GU (S2G (n));
              end if;
            end;
          end if;
          if success then
            Window.Draw_Control.capture := paste0;
            Mousing.Show_mouse_mode (Window.Draw_Control);
            Mousing.Change_Cursor (Window.Draw_Control, Mousing.cur_set_origin);
            --  ^ The ball is in Mousing camp now...
          else
            Message_Box (Window, Msg (error), Msg (fnotfound), Icon => Exclamation_Icon);
          end if;
        when tc_undo =>
          null;  --  !!
        when tc_redo =>
          null;  --  !!
      end case;
      --  In any case, we don't want arrows with boxes. What a bad taste!
      if Window.Draw_Control.current_cmd in Box_cmd then
        Window.Draw_Control.current_ls.arrows := no_arrow;
      end if;
    end Command;

  begin
    for c in MDI_child_cmd loop
      if  Item = ID_custom (c) or else
         (Item = ID_std (c) and then Item /= no_std_id)
      then
        Command (c);
        exit;
      end if;
    end loop;
  end On_Menu_Select;

  -------------
  -- On_Save --
  -------------

  procedure On_Save (Window : in out MDI_Picture_Child_Type)
  is
    File_Name : constant GWindows.GString := GU2G (Window.ID.file_name);
  begin
    if File_Name = "" then
      On_Save_As (Window, macro => False);
    else
      Save (Window, File_Name, macro => False);
    end if;
  end On_Save;

  ----------------
  -- On_Save_As --
  ----------------

  procedure On_Save_As (Window : in out MDI_Picture_Child_Type; macro : Boolean)
  is
    New_File_Name   : GWindows.GString_Unbounded;
    File_Title      : GWindows.GString_Unbounded;
    new_ID          : ID_Type;
    tab_bar         : Tabs.TeXCAD_Tab_Bar_Type renames Window.mdi_root.tab_bar;
    Success         : Boolean;
    saveas_or_macro : constant array (Boolean) of Message :=
      (True => save_macro, False => save_as);
    file_kind : constant array (Boolean) of Message :=
      (True => tcd_mac, False => ltx_pic);
    --
    function Suffix return String is
    begin
      if macro then
        return Mac_Suffix;
      else
        return Pic_Suffix;
      end if;
    end Suffix;
    --
  begin
    if macro then
      New_File_Name := G2GU ("");
    else
      New_File_Name := Window.ID.file_name;
    end if;
    GWindows.Common_Dialogs.Save_File
      (Window,
       Msg (saveas_or_macro (macro)) & "...",
       New_File_Name,
       ((G2GU (Msg (file_kind (macro)) &
         " (*." & S2G (Suffix) & ")"),
         G2GU ("*." & S2G (Suffix))),
        (G2GU (Msg (all_files) & " (*.*)"),
         G2GU ("*.*"))),
        '.' & S2G (Suffix),
        File_Title,
        Success);
    if Success then
      if Ada.Directories.Exists (G2S (GU2G (New_File_Name)))
      then
         begin
            if Message_Box (Window,
                            Msg (saveas_or_macro (macro)),
                            GU2G (New_File_Name) &
                            Msg (exists) & NL &
                            Msg (replace),
                            Yes_No_Box,
                            Exclamation_Icon) = No
            then
               return;
            end if;
         end;
      end if;

      Save (Window, GU2G (New_File_Name), macro => macro);
      new_ID := (file_name => New_File_Name, short_name => File_Title);

      if not macro then
        --  Change title in the tab bar.
        for index in 0 .. tab_bar.Tab_Count - 1 loop
          if tab_bar.info (index).ID = Window.ID then
            tab_bar.info (index).ID := new_ID;
            tab_bar.Text (index, Simple_Name (GU2G (New_File_Name)));
            exit;
          end if;
        end loop;
        Window.ID := new_ID;
        Update_Common_Menus (Window, GU2G (New_File_Name));
        Update_Information (Window);  --  Refresh window title, menus, ...
      end if;
    end if;
  end On_Save_As;

  ----------
  -- Save --
  ----------

  procedure Save (Window    : in out MDI_Picture_Child_Type;
                  File_Name : in     GWindows.GString;
                  macro     :        Boolean)
  is
    use type GString_Unbounded;
    written_name : GString_Unbounded :=
      G2GU (File_Name);
    temp_ext : constant GString := ".$$$";
    backup_name : constant String := G2S (File_Name) & '.' & To_String (gen_opt.bak_suff);

    with_backup : constant Boolean := TC.gen_opt.bak_enabled and not macro;

    use GNAT.OS_Lib;
    ok : Boolean;
    save_error, backup_error : exception;

  begin
    if with_backup then
      written_name := written_name & temp_ext;
    end if;
    TC.Output.Save
      (pic            => Window.Draw_Control.Picture,
       macro          => macro,
       file_name      => G2S (GU2G (written_name)),
       displayed_name => G2S (File_Name));
    if with_backup then
      --  If there was an exception at writing,
      --  the original file is untouched.
      --
      --  1/ delete old backup
      if Ada.Directories.Exists (backup_name) then
        Delete_File (backup_name, ok);
        if not ok then
          raise backup_error;
        end if;
      end if;
      --  2/ file -> backup
      if Ada.Directories.Exists (G2S (File_Name)) then
        Rename_File (G2S (File_Name), backup_name, ok);
        if not ok then
          raise backup_error;
        end if;
      end if;
      --  3/ new file -> file
      Rename_File
        (G2S (GU2G (written_name)),
         G2S (File_Name),
         ok);
      if not ok then
        raise save_error;
      end if;
    end if;
    Window.Extra_First_Doc := False;
    --  ^ even if it is the extra new window and is saved, we won't close
    --    it now on opening of another document.
    if not macro then
      Update_Common_Menus (Window, File_Name);
    end if;
    Update_Information (Window);
  exception
    when backup_error =>
      begin
        Message_Box (Window,
                     Msg (save),
                     Msg (cannotbackup) &
                       NL & "-> " &
                       S2G (backup_name),
                     OK_Box,
                     Exclamation_Icon);
      end;
    when others =>
      begin
        Message_Box (Window,
                     Msg (save),
                     Msg (cannotsave) &
                       NL & "-> " &
                       File_Name,
                     OK_Box,
                     Exclamation_Icon);
      end;
  end Save;

  overriding procedure On_Close (Window    : in out MDI_Picture_Child_Type;
                                 Can_Close :    out Boolean)
  is
    tab_bar : Tabs.TeXCAD_Tab_Bar_Type renames Window.mdi_root.tab_bar;
  begin
    Can_Close := True;
    if Window.Is_Document_Modified then
      loop
        case Message_Box
               (Window,
                Msg (close_not_saved),
                Msg (do_you_want_to_save) & ' ' &
                Msg (the_changes_you_made_to) & " '" &
                GU2G (Window.ID.short_name) & "' ?",
                Yes_No_Cancel_Box,
                Exclamation_Icon)
        is
          when Yes =>
            On_Save (Window);
            exit when not Window.Is_Document_Modified;
          when No =>
            exit;
          when Cancel =>
            success_in_enumerated_close := False;
            Can_Close := False;
            exit;
          when others =>
            null;
        end case;
      end loop;
    else
      Update_Common_Menus
        (Window, GU2G (Window.ID.file_name));
    end if;
    if Can_Close then
      tab_bar.Delete_Tab (tab_bar.Tab_Index (Window.ID));
    end if;
  end On_Close;

  overriding function Is_Document_Modified (Window : MDI_Picture_Child_Type) return Boolean is
  begin
    return not Window.Draw_Control.Picture.saved;
  end Is_Document_Modified;

  overriding procedure On_Focus (Window : in out MDI_Picture_Child_Type) is
    tab_bar : Tabs.TeXCAD_Tab_Bar_Type renames Window.mdi_root.tab_bar;
    tab_index : Integer;
  begin
    Window.Update_Information;
    tab_index := tab_bar.Tab_Index (Window.ID);
    if tab_index >= 0 then
      tab_bar.Selected_Tab (tab_index);
    end if;
  end On_Focus;

end TC.GWin.MDI_Picture_Child;
