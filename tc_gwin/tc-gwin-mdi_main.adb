with TC.Input;

with TC.GWin.Lang,
     TC.GWin.MDI_Picture_Child,
     TC.GWin.Options,
     TC.GWin.Options_Dialogs,
     TC.GWin.Menus,
     TC.GWin.Toolbars,
     TC.GWin.Previewing;

with TeXCAD_Resource_GUI;

with GWindows.Application,
     GWindows.Buttons,
     GWindows.Common_Dialogs,
     GWindows.Constants,
     GWindows.Message_Boxes,
     GWindows.Static_Controls.Web;

with GWin_Util;

with Ada.Command_Line,
     Ada.Exceptions;

package body TC.GWin.MDI_Main is

  use TC.GWin.MDI_Picture_Child;
  use GWindows.Message_Boxes;

  procedure Update_Toolbar_Menu (m : in GWindows.Menus.Menu_Type; tba : Floating_toolbar_array) is
    use Floating_Toolbars, GWindows.Menus;
  begin
    for c in Floating_toolbar_categ loop
      Check (m, Command, ID_custom (c), tba (c).status /= invisible);
    end loop;
  end Update_Toolbar_Menu;

  procedure Update_Common_Menus_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
    use Office_Applications;
  begin
    if Window.all in MDI_Picture_Child_Type'Class then
      declare
        cw : MDI_Picture_Child_Type renames MDI_Picture_Child_Type (Window.all);
      begin
        Update_MRU_Menu (cw.MDI_Root.MRU, cw.File_Menu);
        Update_Toolbar_Menu (cw.View_Menu, cw.MDI_Root.Floating_toolbars);
      end;
    end if;
  end Update_Common_Menus_Child;

  procedure Update_Common_Menus (Window        : in out MDI_Main_Type;
                                 top_mru_entry :        GString := "")
  is
    use Office_Applications;
  begin
    if top_mru_entry /= "" then
      Add_MRU (Window.MRU, top_mru_entry);
    end if;
    Update_MRU_Menu (Window.MRU, Window.File_menu);
    Update_Toolbar_Menu (Window.View_Menu, Window.Floating_toolbars);
    GWindows.Base.Enumerate_Children
      (MDI_Client_Window (Window).all,
       Update_Common_Menus_Child'Access);
  end Update_Common_Menus;

  procedure Create_Menus (Window : in out MDI_Main_Type) is
    use GWindows.Menus, Lang, Office_Applications;
    Main : constant Menu_Type := Create_Menu;
  begin
    Window.File_menu := TC.GWin.Menus.Create_File_Menu (is_child => False);
    Update_MRU_Menu (Window.MRU, Window.File_menu);
    Append_Menu (Main, Msg (ffile), Window.File_menu);

    Window.View_Menu := TC.GWin.Menus.Create_View_Menu;
    Append_Menu (Main, Msg (vview), Window.View_Menu);

    Append_Menu (Main, Msg (oopt), TC.GWin.Menus.Create_Options_Menu (is_child => False));
    Append_Menu (Main, Msg (wwindow), TC.GWin.Menus.Create_Wndw_Menu);
    Append_Menu (Main, Msg (hhelp), TC.GWin.Menus.Create_Help_Menu);

    MDI_Menu (Window, Main, Window_Menu => 4);
  end Create_Menus;

  procedure Focus_an_open_window
    (Window    : MDI_Main_Type;
     file_name : GString_Unbounded;
     is_open   : out Boolean)
  is
    procedure Identify (Any_Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
      use type GString_Unbounded;
    begin
      if Any_Window.all in MDI_Picture_Child_Type'Class then
        declare
          pw : MDI_Picture_Child_Type renames MDI_Picture_Child_Type (Any_Window.all);
        begin
          if pw.File_Name = file_name then
            is_open := True;
            pw.Focus;
          end if;
        end;
      end if;
    end Identify;

  begin
    is_open := False;
    GWindows.Base.Enumerate_Children
      (MDI_Client_Window (Window).all,
       Identify'Unrestricted_Access);
  end Focus_an_open_window;

  procedure Redraw_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Picture_Child_Type'Class then
      MDI_Picture_Child_Type (Window.all).Draw_Control.Picture.refresh := full;
      --  ^ 17-Oct-2003 : otherwise the buffer is just copied to screen
      Window.Redraw;
    end if;
  end Redraw_Child;

  procedure Redraw_all (Window : in out MDI_Main_Type) is
  begin
    Window.Redraw;
    Window.Tool_Bar.Redraw;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Window).all, Redraw_Child'Access);
  end Redraw_all;

  procedure Finish_subwindow_opening
    (m : in out MDI_Main_Type;
     c : in out MDI_Picture_Child_Type)
  is
  begin
    user_maximize_restore := True;
    if MDI_childen_maximized then
      c.Zoom;
      m.Redraw_all;
    end if;
    --  Show things in the main status bar - effective only after Thaw!
    c.Zoom_Picture (0);
    c.Update_Information;
    c.Update_Permanent_Command;
  end Finish_subwindow_opening;

  procedure Open_Child_Window_And_Load_Picture (
        Window     : in out MDI_Main_Type;
        File_Name,
        File_Title :        GWindows.GString_Unbounded)
  is
    Candidate : TC.Picture;
    is_open : Boolean;
    use Lang;
  begin
    Focus_an_open_window (Window, File_Name, is_open);
    if is_open then
      return;        -- nothing to do, picture already in a window
    end if;
    TC.Input.Load (Candidate, False, G2S (GU2G (File_Name)));
    declare
      New_Window : constant MDI_Picture_Child_Access :=
        new MDI_Picture_Child_Type;
    begin
      --  We do here like Excel or Word: close the unused blank window
      Window.Close_Initial_Document;
      user_maximize_restore := False;
      New_Window.Draw_Control.Picture := Candidate;
      Refresh_size_dependent_parameters
        (New_Window.Draw_Control.Picture,
         objects => True);
      New_Window.File_Name := File_Name;
      Create_MDI_Child (New_Window.all,
        Window,
        GU2G (File_Title),
        Is_Dynamic => True);
      New_Window.Short_Name := File_Title;
      MDI_Active_Window (Window, New_Window.all);
      Update_Common_Menus (Window, GU2G (New_Window.File_Name));
      Update_Information (New_Window.all);
      Finish_subwindow_opening (Window, New_Window.all);
    end;
  exception
    when E : TC.Input.Load_error =>
      Message_Box
        (Window,
         "Error when loading picture data",
         S2G (Ada.Exceptions.Exception_Message (E)),
         Icon => Exclamation_Icon);
    when Ada.Text_IO.Name_Error =>
      Message_Box
        (Window, Msg (error), Msg (fnotfound), Icon => Exclamation_Icon);
  end Open_Child_Window_And_Load_Picture;

  procedure Open_Child_Window_And_Load_Picture
    (Window     : in out MDI_Main_Type;
     File_Name  :        GWindows.GString_Unbounded)
  is
  begin
    Open_Child_Window_And_Load_Picture
      (Window,
       File_Name,
       File_Name);   --  File name used as title (could be nicer)
  end Open_Child_Window_And_Load_Picture;

  ---------------
  -- On_Create --
  ---------------

  overriding procedure On_Create (Window : in out MDI_Main_Type) is
    use Ada.Command_Line, GWindows.Application;
  begin
    GWindows.Base.Mouse_Wheel_Target := GWindows.Base.Mouse_Window;
    TC.GWin.Options.Load (Window.MRU);
    TC.startup_language := TC.gen_opt.lang;

    GWin_Util.Use_GUI_Font (Window);

    Small_Icon (Window, "AAA_Main_Icon");
    Large_Icon (Window, "AAA_Main_Icon");

    --  ** Menus and accelerators:

    for i in Window.MRU.ID_Menu'Range loop
      Window.MRU.ID_Menu (i) :=
        ID_custom (Custom_cmd'Val (Custom_cmd'Pos (mru1) + i - Window.MRU.ID_Menu'First));
    end loop;
    Create_Menus (Window);
    Accelerator_Table (Window, "Main_Menu");

    --  ** Status bar at bottom of the main window:

    Create (Window.Status_Bar, Window, "");
    Parts (Window.Status_Bar, (10, 210, 410, 640, 680, -1));
    Dock (Window.Status_Bar, GWindows.Base.At_Bottom);

    --  ** Main tool bar (new/open/save/...) at top left of the main window:

    TC.GWin.Toolbars.Init_Main_Tool_Bar (Window.Tool_Bar, Window);

    --  ** Floating tool bars:
    TC.GWin.Toolbars.Init_Floating_Tool_Bars (Window.Floating_toolbars, Window);

    --  ** Resize according to options:

    if Screen_Visibility ((wleft, wtop)) = Good then
      Left (Window, wleft);
      Top  (Window, wtop);
    end if;
    Size (Window, Integer'Max (400, wwidth), Integer'Max (200, wheight));
    Zoom (Window, wmaxi);

    Dock_Children (Window);
    Show (Window);
    --!!Redraw(Window.Drawing_toolbar.bar,True,True);

    if Argument_Count = 0 then
      On_File_New (Window, extra_first => True);
      --  ^ The MS Office-like first doc.
    end if;
    --  !! This works on 1st instance only:
    for I in 1 .. Argument_Count loop
      Open_Child_Window_And_Load_Picture
        (Window,
         G2GU (S2G (Argument (I))));
    end loop;
    Accept_File_Drag_And_Drop (Window);
    Window.Tool_Bar.Redraw;  --  2007: sometimes the buttons do not appear...
    Window.record_dimensions := True;
  end On_Create;

  overriding procedure On_Move (Window : in out MDI_Main_Type;
                                Left   : in     Integer;
                                Top    : in     Integer)
  is
  begin
    if Window.record_dimensions and
       not (Zoom (Window) or GWin_Util.Minimized (Window))
    then
      --  ^ Avoids recording dimensions before restoring them
      --    from previous session.
      --  We call the functions since the Top/Left arguments are reversed -
      --  bug of GWindows <= 2003; workaround compatible with fixes.
      --  bug fixed on 5-Jan-2012, gnavi rev. 109 !
      TC.GWin.wleft  := TC.GWin.MDI_Main.Left (Window);
      TC.GWin.wtop   := TC.GWin.MDI_Main.Top (Window);
      --  Will remember position if moved, maximized and closed
    end if;
  end On_Move;

  overriding procedure On_Size (Window : in out MDI_Main_Type;
                                Width  : in     Integer;
                                Height : in     Integer)
  is
  begin
    Dock_Children (Window);
    if Window.record_dimensions and
       not (Zoom (Window) or GWin_Util.Minimized (Window))
    then
      --  ^ Avoids recording dimensions before restoring them
      --    from previous session.
      TC.GWin.wwidth := Width;
      TC.GWin.wheight := Height;
      --  Will remember position if sized, maximized and closed
    end if;
  end On_Size;

  -----------------
  -- On_File_New --
  -----------------

  Current_MDI_Window : Natural := 0;

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first : Boolean)
  is
    New_Window : constant MDI_Picture_Child_Access :=
      new MDI_Picture_Child_Type;

    function Suffix return GWindows.GString is
    begin
      if Current_MDI_Window = 0 then
        return "";
      else
        return S2G (Current_MDI_Window'Image);
      end if;
    end Suffix;

    File_Title : constant GString := Lang.Msg (Lang.new_pic) & Suffix;

  begin
    New_Window.Extra_First_Doc := extra_first;
    user_maximize_restore := False;
    Create_MDI_Child (New_Window.all,
      Window, File_Title, Is_Dynamic => True);
    New_Window.Short_Name := G2GU (File_Title);

    MDI_Active_Window (Window, New_Window.all);

    --  Transfer user-defined default options:
    New_Window.Draw_Control.Picture.opt := gen_opt.options_for_new;
    Refresh_size_dependent_parameters
      (New_Window.Draw_Control.Picture,
       objects => True);

    Current_MDI_Window := Current_MDI_Window + 1;

    --  This is just to set the MRUs in the new window's menu:
    Update_Common_Menus (Window);

    --  Refresh File menu ("Save"), tool bar, ...
    Update_Information (New_Window.all);

    Finish_subwindow_opening (Window, New_Window.all);
  end On_File_New;

  ------------------
  -- On_File_Open --
  ------------------

  procedure On_File_Open (Window : in out MDI_Main_Type) is
    File_Name, File_Title : GString_Unbounded;
    Success : Boolean;
    use Lang;
  begin
    GWindows.Common_Dialogs.Open_File
      (Window, Msg (open),
       File_Name,
       ((G2GU (Msg (ltx_pic) & " (*." & S2G (Pic_suffix) & ")"),
           G2GU ("*." & S2G (Pic_suffix))),
         (G2GU (Msg (all_files) & " (*.*)"),
           G2GU ("*.*"))),
       '.' & S2G (Pic_suffix),
       File_Title,
       Success);

    if Success then
      Open_Child_Window_And_Load_Picture (Window, File_Name, File_Title);
    end if;
  end On_File_Open;

  procedure On_About (Window : in out MDI_Main_Type)
  is
    About     : GWindows.Windows.Window_Type;
    Oki       : GWindows.Buttons.Default_Button_Type;
    Result, w : Integer;
    Wmax      : constant := 550;
    use GWindows.Static_Controls, GWindows.Static_Controls.Web, Lang;
  begin
    About.Create_As_Dialog (Window, "TeXCAD", Width => Wmax + 50, Height => 300);
    About.Center;
    About.Small_Icon ("Grid_Icon");
    GWin_Util.Use_GUI_Font (About);
    w := About.Client_Area_Width - 32 - 10;

    Create_URL (About, "TeXCAD", S2G (web), 60, 15, 80, 16);

    Create_Label (About,
      S2G ("* Version: " & version & "   * Reference: " & reference),
      140, 15, w - 140 - 18, 25);

    Create_Icon (About, "AAA_Main_Icon", 10, 10, 32, 32);
    Create_Icon (About, "Picture_Icon",   w, 10, 32, 32);
    Create_Label (About, Msg (blurb), 60, 35, Wmax - 92, 25);
    Create_Label (About, S2G (TeXCAD_Resource_GUI.Version_info.LegalCopyright) & " (cf COPYING.TXT)",
      60, 55, Wmax - 92, 25);
    Create_URL (About, S2G ("Internet: " & web), S2G (web),
      60, 75, Wmax - 92, 25);
      --
    Create_Label (About, Msg (authors), 10, 95, Wmax, 25);
    Create_Label (About, "Georg Horn, Jörn Winkelmann: " --  &&
      & Msg (original), 30, 115, Wmax, 25);

    Create_URL
      (About,
       "Gautier de Montmollin: Ada-ptation, " &
       Msg (tc4) & ", " & Msg (windoze_version),
       "http://sf.net/users/gdemont/",
       30, 135, Wmax, 16);

    Create_Label (About, Msg (thanks), 10, 155, Wmax, 25);
    Create_URL
      (About,
       "David Botton: " & Msg (gwind),
       "http://sf.net/projects/gnavi/",
       30, 175, Wmax, 16);

    Oki.Create (About, "O&K", 20, About.Client_Area_Height - 40, 60, 25,
      ID => GWindows.Constants.IDOK);
    Show_Dialog_with_Toolbars_off (About, Window, Window, Result);
  end On_About;

  -------------------
  -- On_Menu_Hover --
  -------------------

  overriding procedure On_Menu_Hover
    (Window  : in out MDI_Main_Type;
     Item    : in     Integer;
     Kind    : in     GWindows.Windows.Hover_Item_Type)
  is
    use GWindows.Windows, Lang;
    m : Message := ready;
  begin
    if Kind = Menu_Item and Item > 0 then
      case Item is
        when ID_FILE_NEW =>
          m := new_pic;
        when others =>
          for cust in Custom_cmd loop
            if Item = ID_custom (cust) then
              case cust is
                when pick_obj =>
                  m := expl_pick;
                when others =>
                  null;
              end case;
            end if;
          end loop;
      end case;
    end if;
    Update_Status_Bar (Window, comment, Msg (m));
  end On_Menu_Hover;

  procedure Custom_Command (
        Window : in out MDI_Main_Type;
        C      :        Custom_cmd)
  is
  begin
    case C is
      when MDI_main_cmd =>
        case MDI_main_cmd (C) is
          when gen_opt_dialog =>
            TC.GWin.Options_Dialogs.On_General_Options (Window);
          when mru1 .. mru9 =>
            Open_Child_Window_And_Load_Picture
              (Window,
               Window.MRU.Item (1 + Custom_cmd'Pos (C) - Custom_cmd'Pos (mru1)).Name);
          when Floating_toolbar_categ =>
            Floating_Toolbars.Rotate_status (Window.Floating_toolbars (C));
            Update_Common_Menus (Window);
        end case;
      when others =>
        Message_Box (Window, "Main - Custom - not for main ",
          S2G (Custom_cmd'Image (C)));
    end case;
  end Custom_Command;

  ----------------------
  -- My_MDI_Close_All --
  ----------------------

  procedure My_Close_Win (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  --  Enumeration call back to close MDI child windows
  is
  begin
    if Window.all in MDI_Picture_Child_Type'Class and then
       MDI_Picture_Child.success_in_enumerated_close
    then -- no [cancel] up to now
      GWindows.Base.Close (Window.all);
    end if;
  end My_Close_Win;

  procedure My_MDI_Close_All (Window : in out MDI_Main_Type) is
  begin
    MDI_Picture_Child.success_in_enumerated_close := True;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Window).all,
                                      My_Close_Win'Access);
  end My_MDI_Close_All;

  --------------------
  -- On_Menu_Select --
  --------------------

  overriding procedure On_Menu_Select (Window : in out MDI_Main_Type;
                                       Item   : in     Integer)
  is
    use GWindows.Windows;
  begin
    case Item is
      when ID_FILE_NEW  =>
        On_File_New (Window, extra_first => False);
      when ID_FILE_OPEN =>
        On_File_Open (Window);
      when ID_APP_ABOUT =>
        On_About (Window);
      when ID_APP_EXIT  =>
        Close (Window);
      when ID_FILE_CLOSE =>
        if Window.Count_MDI_Children = 0 then
          Close (Window);  --  Ctrl-W when no subwindow is open.
        else
          On_Menu_Select (Window_Type (Window), Item);
        end if;
      when ID_WINDOW_CASCADE   =>
        MDI_Cascade (Window);
      when ID_WINDOW_TILE_HORZ =>
        MDI_Tile_Horizontal (Window);
      when ID_WINDOW_TILE_VERT =>
        MDI_Tile_Vertical (Window);
      when ID_WINDOW_CLOSE_ALL =>
        My_MDI_Close_All (Window);
      when others =>
        for c in MDI_main_cmd loop
          if Item = ID_custom (c) then
            Custom_Command (Window, c);
            exit;
          end if;
        end loop;
--  !!! test !!!
--  for c in MDI_child_cmd loop
--    if Item = Id_Custom (C) then
--      Message_Box (Window, "Main - Custom - Is for child " , Custom_Cmd'
--        Image (C));
--      exit;
--    end if;
--  end loop;
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  --------------------
  -- On_Right_Click --
  --------------------

  overriding procedure On_Right_Click (Control : in out MDI_Status_Bar_Type)
  is
    Parent : constant MDI_Main_Access :=
      MDI_Main_Access (Controlling_Parent (Control));
  begin
    On_About (Parent.all);
  end On_Right_Click;

  overriding procedure On_File_Drop
    (Window     : in out MDI_Main_Type;
     File_Names : in     GWindows.Windows.Array_Of_File_Names)
  is
  begin
    for I in File_Names'Range loop
      Open_Child_Window_And_Load_Picture (Window, File_Names (I));
    end loop;
  end On_File_Drop;

  overriding procedure On_Close (Window    : in out MDI_Main_Type;
                                 Can_Close :    out Boolean)
  is
  begin
    begin
      wmaxi := Zoom (Window);
      if not (wmaxi or GWin_Util.Minimized (Window)) then
        wleft   := Left (Window);
        wtop    := Top (Window);
        wwidth  := Width (Window);
        wheight := Height (Window);
      end if;
      for c in Floating_toolbar_categ loop
        TC_FT_memo (c).geom  := Window.Floating_toolbars (c).window.geom;
        TC_FT_memo (c).stat  := Window.Floating_toolbars (c).status;
      end loop;
    end;

    TC.GWin.Options.Save (Window.MRU);
    TC.GWin.Previewing.Cleanup;

    My_MDI_Close_All (Window);
    --  ^ Don't forget to save unsaved pictures !
    --    Operation can be cancelled by user for single unsaved pictures.
    Can_Close := MDI_Picture_Child.success_in_enumerated_close;
    --
    if Can_Close then
      GWindows.Base.On_Exception_Handler (Handler => null);
      --  !! Trick to remove a strange crash on Destroy_Children
      --  !! on certain Windows platforms - 29-Jun-2012
    end if;
  end On_Close;

  procedure Update_Status_Bar
    (Window    : in out MDI_Main_Type;
     Part      :        MDI_Status_bar_part;
     Content   :        GString := "")
  is
  begin
    Text (Window.Status_Bar, Content, MDI_Status_bar_part'Pos (Part));
  end Update_Status_Bar;

  procedure Toolbar_enabling
    (window    : in out MDI_Main_Type;
     switch    :        Boolean)
  is
  begin
    for t in Floating_toolbar_categ loop
      if switch then
        Floating_Toolbars.Enable (window.Floating_toolbars (t).window);
      else
        Floating_Toolbars.Disable (window.Floating_toolbars (t).window);
      end if;
    end loop;
  end Toolbar_enabling;

  procedure Show_Dialog_with_Toolbars_off
    (Window : in     GWindows.Base.Base_Window_Type'Class;
     Parent : in     GWindows.Base.Base_Window_Type'Class;
     Main   : in out MDI_Main_Type;
     Result :    out Integer
  )
  is
  begin
    Toolbar_enabling (Main, False);
    Result := GWindows.Application.Show_Dialog (Window, Parent);
    Toolbar_enabling (Main, True);
  end Show_Dialog_with_Toolbars_off;

end TC.GWin.MDI_Main;
