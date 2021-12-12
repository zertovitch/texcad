with TC.Input;

with TC.GWin.Lang;                      use TC.GWin.Lang;
with TC.GWin.MDI_Picture_Child;         use TC.GWin.MDI_Picture_Child;

with TC.GWin.Options,
     TC.GWin.Options_Dialogs,
     TC.GWin.Menus,
     TC.GWin.Toolbars,
     TC.GWin.Previewing;

with TeXCAD_Resource_GUI;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Menus;                    use GWindows.Menus;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Windows.MDI;              use GWindows.Windows.MDI;

with GWin_Util;                         use GWin_Util;
with GWindows.Static_Controls.Web;      use GWindows.Static_Controls.Web;

with Ada.Command_Line, Ada.Exceptions;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed, Ada.Strings;

package body TC.GWin.MDI_Main is

  procedure Update_MRU_Menu( m: in Menu_type) is
    cmd: Integer;
  begin
    for i in reverse mru'Range loop
      cmd:= ID_custom( Custom_cmd'Val( Custom_cmd'Pos(mru1) + i - mru'First));
      Text(
        m, Command, cmd,
         '&' & S2G (Trim(Integer'Image(i), Left)) &
         ' ' &
         Shorten_filename(To_GString_From_Unbounded(mru(i)))
      );
      --  State(m,command,cmd,Disabled);
    end loop;
  end Update_MRU_Menu;

  procedure Update_Toolbar_Menu( m: in Menu_type; tba: Floating_toolbar_array) is
    use Floating_toolbars;
  begin
    for c in Floating_toolbar_categ loop
      GWindows.Menus.Check(m, Command, ID_custom(c), tba(c).status /= invisible);
    end loop;
  end Update_Toolbar_Menu;

  procedure Update_Common_Menus_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Picture_Child_Type'Class then
      declare
        cw: MDI_Picture_Child_Type renames MDI_Picture_Child_Type(Window.all);
      begin
        Update_MRU_Menu(cw.File_menu);
        Update_Toolbar_Menu(cw.View_menu, cw.parent.Floating_toolbars);
      end;
    end if;
  end Update_Common_Menus_Child;

  procedure Update_Common_Menus(Window    : in out MDI_Main_Type;
                                top_entry : GString:= "" ) is
  begin
    if top_entry /= "" then
      Add_MRU(top_entry);
    end if;
    Update_MRU_Menu(Window.File_menu);
    Update_Toolbar_Menu(Window.View_Menu, Window.Floating_toolbars);
    GWindows.Base.Enumerate_Children(
      MDI_Client_Window (Window).all,
      Update_Common_Menus_Child'Access
    );
  end Update_Common_Menus;

  procedure Create_Menus (
        Window : in out MDI_Main_Type ) is
        Main: constant Menu_Type := Create_Menu;
  begin
    Window.File_menu:= TC.GWin.Menus.Create_File_Menu(is_child => False);
    Update_MRU_Menu(Window.File_menu);
    Append_Menu (Main, Msg(ffile), Window.File_menu);

    Window.View_Menu:= TC.GWin.Menus.Create_View_Menu;
    Append_Menu (Main, Msg(vview), Window.View_Menu);

    Append_Menu (Main, Msg(oopt), TC.GWin.Menus.Create_Options_Menu(is_child => False));
    Append_Menu (Main, Msg(wwindow), TC.GWin.Menus.Create_Wndw_Menu);
    Append_Menu (Main, Msg(hhelp), TC.GWin.Menus.Create_Help_Menu);

    MDI_Menu (Window, Main, Window_Menu => 4);
  end Create_Menus;

  procedure Focus_an_open_window(
    Window    : MDI_Main_Type;
    file_name : GString_Unbounded;
    is_open   : out Boolean )
  is
    procedure Identify (Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
      use type GString_Unbounded;
    begin
      if Window.all in MDI_Picture_Child_Type'Class then
        declare
          pw: MDI_Picture_Child_Type renames MDI_Picture_Child_Type(Window.all);
        begin
          if pw.file_name = file_name then
            is_open:= True;
            Focus(pw);
          end if;
        end;
      end if;
    end Identify;

  begin
    is_open:= False;
    GWindows.Base.Enumerate_Children(
      MDI_Client_Window (Window).all,
      Identify'Unrestricted_Access
    );
  end Focus_an_open_window;

  procedure Redraw_Child (Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Picture_Child_Type'Class then
      MDI_Picture_Child_Type(Window.all).Draw_Control.Picture.refresh:= full;
      --  ^ 17-Oct-2003 : otherwise the buffer is just copied to screen
      GWindows.Base.Redraw(Window.all);
    end if;
  end Redraw_Child;

  procedure Redraw_all(Window: in out MDI_Main_Type) is
  begin
    Redraw(Window);
    Redraw(Window.Tool_Bar);
    GWindows.Base.Enumerate_Children(MDI_Client_Window (Window).all,Redraw_Child'Access);
  end Redraw_all;

  procedure Close_extra_first_child(Window : GWindows.Base.Pointer_To_Base_Window_Class)
  is
  begin
    if Window.all in MDI_Picture_Child_Type'Class then
      declare
        w: MDI_Picture_Child_Type renames MDI_Picture_Child_Type(Window.all);
      begin
        if w.extra_first and w.draw_control.picture.saved then
          Close(Window.all);
        end if;
      end;
    end if;
  end Close_extra_first_child;

  procedure Close_extra_first_child(Window: in out MDI_Main_Type) is
  begin
    GWindows.Base.Enumerate_Children(MDI_Client_Window (Window).all,Close_extra_first_child'Access);
  end Close_extra_first_child;

  procedure Finish_subwindow_opening(
    m : in out MDI_Main_Type;
    c : in out MDI_Picture_Child_Type )
  is
  begin
    user_maximize_restore:= True;
    if MDI_childen_maximized then
      Zoom(c);
      Redraw_all(m);
    end if;
    --  Show things in the main status bar - effective only after Thaw!
    Zoom_picture(c,0);
    Show_Totals(c);
    Update_Permanent_Command(c);
  end Finish_subwindow_opening;

  procedure Open_Child_Window_And_Load_Picture (
        Window     : in out MDI_Main_Type;
        File_Name,
        File_Title :        Gwindows.Gstring_Unbounded ) is
    Candidate              : TC.Picture;
    is_open: Boolean;
  begin
    Focus_an_open_window( Window, File_Name, is_open );
    if is_open then
      return;        -- nothing to do, picture already in a window
    end if;
    TC.Input.Load( Candidate, False, G2S (To_GString_From_Unbounded (File_Name)));
    declare
      New_Window : constant MDI_Picture_Child_Access :=
        new MDI_Picture_Child_Type;
    begin
      -- We do here like Excel or Word: close the unused blank window
      Close_extra_first_child(Window);
      user_maximize_restore:= False;
      New_Window.Draw_Control.Picture:= Candidate;
      Refresh_size_dependent_parameters(
        New_Window.Draw_Control.Picture,
        objects => True
      );
      New_Window.File_Name:= File_Name;
      Create_MDI_Child (New_Window.all,
        Window,
        To_GString_from_Unbounded(File_Title),
        Is_Dynamic => True);
      New_Window.Short_Name:= File_Title;
      MDI_Active_Window (Window, New_Window.all);
      Update_Common_Menus(Window, To_GString_from_Unbounded(New_Window.File_Name));
      Finish_subwindow_opening(Window, New_Window.all);
    end;
  exception
    when E : TC.Input.Load_Error =>
      Message_Box(
        Window,
        "Error when loading picture data",
        S2G (Ada.Exceptions.Exception_Message(E)),
        Icon => Exclamation_Icon
      );
    when Ada.Text_IO.Name_Error =>
      Message_Box(Window, Msg(error), Msg(fnotfound), Icon => Exclamation_Icon);
  end Open_Child_Window_And_Load_Picture;

  procedure Open_Child_Window_And_Load_Picture (
        Window     : in out MDI_Main_Type;
        File_Name  :        Gwindows.Gstring_Unbounded ) is
  begin
    Open_Child_Window_And_Load_Picture(
      Window,
      File_Name,
      File_Name   -- file name used as title (could be nicer)
    );
  end Open_Child_Window_And_Load_Picture;

  procedure On_Button_Select (
        Control : in out MDI_Toolbar_Type;
        Item    : in     Integer           ) is
    Parent : constant MDI_Main_Access := MDI_Main_Access (Controlling_Parent (Control));
  begin
    On_Menu_Select (Parent.all, Item);
  end On_Button_Select;

  ---------------
  -- On_Create --
  ---------------

  procedure On_Create ( Window : in out MDI_Main_Type ) is
    use Ada.Command_Line;
  begin
    TC.GWin.Options.Load;
    TC.startup_language:= TC.gen_opt.lang;

    Gwin_Util.Use_Gui_Font(Window);

    Small_Icon (Window, "AAA_Main_Icon");
    Large_Icon (Window, "AAA_Main_Icon");

    -- ** Menus and accelerators:

    Create_Menus(Window);
    Accelerator_Table (Window, "Main_Menu");

    -- ** Status bar at bottom of the main window:

    Create(Window.Status_Bar, Window, "");
    Parts(Window.Status_Bar,(10,210,410,640,680,-1));
    Dock (Window.Status_Bar, Gwindows.Base.At_Bottom);

    -- ** Main tool bar (new/open/save/...) at top left of the main window:

    TC.GWin.Toolbars.Init_Main_toolbar(Window.Tool_Bar, Window.Images, Window);

    -- ** Floating tool bars:
    TC.GWin.Toolbars.Init_Floating_toolbars(Window.Floating_toolbars, Window);

    -- ** Resize according to options:

    if Valid_Left_Top(Wleft, Wtop) then
      Left(Window, Wleft);
      Top( Window, Wtop);
    end if;
    Size(Window, Integer'Max(400,Wwidth), Integer'Max(200,Wheight));
    Zoom(Window,Wmaxi);

    Dock_Children(Window);
    Show (Window);
    --!!Redraw(Window.Drawing_toolbar.bar,True,True);

    if Argument_count=0 then
      On_File_New (Window, extra_first => True);
      -- ^ The MS Office-like first doc.
    end if;
    -- !! This works on 1st instance only:
    for I in 1..Argument_Count loop
      Open_Child_Window_And_Load_Picture(
        Window,
        To_Gstring_Unbounded(S2G (Argument(I)))
      );
    end loop;
    Accept_File_Drag_And_Drop(Window);
    Redraw(Window.Tool_bar); -- 2007: sometimes the buttons do not appear...
    Window.record_dimensions:= True;
  end On_Create;

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer) is
  begin
    if Window.record_dimensions and
       not (Zoom(Window) or Minimized(Window))
    then
      -- ^ Avoids recording dimensions before restoring them
      --   from previous session.
      -- We call the function since the Top/Left arguments are reversed -
      -- bug of GWindows <= 2003; workaround compatible with fixes.
      -- bug fixed on 5-Jan-2012, gnavi rev. 109 !
      TC.Gwin.Wleft  := TC.GWin.MDI_Main.Left(Window);
      TC.Gwin.Wtop   := TC.GWin.MDI_Main.Top(Window);
      -- Will remember position if moved, maximized and closed
    end if;
  end On_Move;

  procedure On_Size (Window : in out MDI_Main_Type;
                     Width  : in     Integer;
                     Height : in     Integer) is
  begin
    Dock_Children(Window);
    if Window.record_dimensions and
       not (Zoom(Window) or Minimized(Window))
    then
      -- ^ Avoids recording dimensions before restoring them
      --   from previous session.
      TC.Gwin.Wwidth := Width;
      TC.Gwin.Wheight:= Height;
      -- Will remember position if sized, maximized and closed
    end if;
  end On_Size;

  -----------------
  -- On_File_New --
  -----------------

  Current_MDI_Window : Natural := 0;

  procedure On_File_New (Window : in out MDI_Main_Type; extra_first: Boolean)
  is
    New_Window : constant MDI_Picture_Child_Access := new MDI_Picture_Child_Type;

    function Suffix return GWindows.Gstring is
    begin
      if Current_MDI_Window = 0 then
        return "";
      else
        return S2G(Current_MDI_Window'Img);
      end if;
    end Suffix;

    File_Title: constant GString:= Msg(New_Pic) & Suffix;

  begin
    New_Window.extra_first:= extra_first;
    user_maximize_restore:= False;
    Create_MDI_Child (New_Window.all,
      Window, File_Title, Is_Dynamic => True);
    New_Window.Short_Name:= To_GString_Unbounded(File_Title);

    MDI_Active_Window (Window, New_Window.all);

    -- Transfer user-defined default options:
    New_Window.Draw_Control.Picture.Opt:= Gen_Opt.Options_For_New;
    Refresh_size_dependent_parameters(
      New_Window.Draw_Control.Picture,
      objects => True
    );

    Current_MDI_Window := Current_MDI_Window + 1;

    -- This is just to set the MRUs in the new window's menu:
    Update_Common_Menus(Window);

    Finish_subwindow_opening(Window, New_Window.all);
  end On_File_New;

  ------------------
  -- On_File_Open --
  ------------------

  procedure On_File_Open (
        Window : in out MDI_Main_Type ) is
    File_Name, File_Title : GString_Unbounded;
    Success    : Boolean;
  begin
    Open_File (Window, Msg(Open),
      File_Name,
      ((To_Gstring_Unbounded (Msg(Ltx_Pic) & " (*." & S2G (Pic_Suffix) & ")"),
          To_Gstring_Unbounded ("*." & S2G (Pic_Suffix) )),
        (To_Gstring_Unbounded (Msg(All_Files) & " (*.*)"),
          To_Gstring_Unbounded ("*.*"))),
      '.' & S2G (Pic_Suffix),
      File_Title,
      Success);

    if Success then
      Open_Child_Window_And_Load_Picture( Window, File_Name, File_Title );
    end if;
  end On_File_Open;

  procedure On_About(Window: in out MDI_Main_Type)
  is
    About     : Window_Type;
    Oki       : Default_Button_Type;
    Result,w  : Integer;
    Wmax      : constant := 550;
  begin
    Create_As_Dialog(About, Window, "TeXCAD", Width => Wmax + 50, Height => 300);
    Center(About);
    Small_Icon (About, "Grid_Icon");
    Gwin_Util.Use_Gui_Font(About);
    w:= Client_Area_Width (About)-32-10;

    GWindows.Static_Controls.Web.Create_URL(About, "TeXCAD", S2G (web), 60, 15, 80, 16);

    Create_Label(About,
      S2G ("* Version: " & Version & "   * Reference: " & Reference),
      140, 15, w-140-18, 25);

    Create_Icon (About, "AAA_Main_Icon", 10,10,32,32);
    Create_Icon (About, "Picture_Icon",   w,10,32,32);
    Create_Label (About, Msg(blurb), 60, 35, Wmax-92, 25);
    Create_Label (About, S2G (TeXCAD_Resource_GUI.Version_info.LegalCopyright) & " (cf COPYING.TXT)",
      60, 55, Wmax-92, 25);
    Create_URL (About, S2G ("Internet: " & web), S2G (web),
      60, 75, Wmax-92, 25);
      --
    Create_Label (About, Msg(Authors), 10, 95, Wmax, 25);
    Create_Label (About, "Georg Horn, Jörn Winkelmann: " --  &&
      & Msg(Original),       30,  115, Wmax, 25);

    Create_URL(
      About,
      "Gautier de Montmollin: Ada-ptation, " &
      Msg(tc4) & ", " & Msg(Windoze_Version),
      "http://sf.net/users/gdemont/",
      30, 135, Wmax, 16);

    Create_Label (About, Msg(Thanks), 10, 155, Wmax, 25);
    Create_URL(
      About,
      "David Botton: " & Msg(Gwind),
      "http://sf.net/projects/gnavi/",
      30, 175, Wmax, 16);

    Create (Oki, About, "O&K", 20, Client_Area_Height (About) - 40, 60, 25,
      Id => Idok);
    Show_Dialog_with_Toolbars_off(About, Window, Window, Result);
  end On_About;

  -------------------
  -- On_Menu_Hover --
  -------------------

  procedure On_Menu_Hover (
        Window : in out MDI_Main_Type;
        Item   : in     Integer;
        Kind   : in     Gwindows.Windows.Hover_Item_Type )
  is
    m: Message:= ready;
  begin
    if Kind = Gwindows.Windows.Menu_Item and Item > 0 then
      case Item is
        when ID_FILE_NEW =>
          m:= new_pic;
        when others =>
          for cust in custom_cmd loop
            if Item = ID_custom(cust) then
              case cust is
                when pick_obj =>
                  m:= expl_pick;
                when others =>
                  null;
              end case;
            end if;
          end loop;
      end case;
    end if;
    Update_Status_Bar(Window, comment, Msg(m));
  end On_Menu_Hover;

  procedure Custom_Command (
        Window : in out MDI_Main_Type;
        C      :        Custom_Cmd     ) is
  begin
    case C is
      when MDI_main_cmd =>
        case MDI_main_cmd(C) is
          when Gen_Opt_Dialog =>
            TC.GWin.Options_Dialogs.On_General_Options (Window);
          when mru1 .. mru9 =>
            Open_Child_Window_And_Load_Picture(
              Window,
              mru( 1 + Custom_Cmd'Pos(c)-Custom_Cmd'Pos(mru1) )
            );
          when Floating_toolbar_categ =>
            Floating_toolbars.Rotate_status( Window.Floating_toolbars(C) );
            Update_Common_Menus(Window);
        end case;
      when others =>
        Message_Box(Window, "Main - Custom - not for main " ,
          S2G (Custom_Cmd'Image(C)));
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
       MDI_Picture_Child.Success_In_Enumerated_Close
    then -- no [cancel] up to now
      GWindows.Base.Close (Window.all);
    end if;
  end My_Close_Win;

  procedure My_MDI_Close_All (Window : in out MDI_Main_Type) is
  begin
    MDI_Picture_Child.Success_In_Enumerated_Close:= True;
    GWindows.Base.Enumerate_Children (MDI_Client_Window (Window).all,
                                      My_Close_Win'Access);
  end My_MDI_Close_All;

  --------------------
  -- On_Menu_Select --
  --------------------

  procedure On_Menu_Select (
        Window : in out MDI_Main_Type;
        Item   : in     Integer        ) is
  begin
    case Item is
      when Id_File_New  =>
        On_File_New (Window, extra_first => False);
      when Id_File_Open =>
        On_File_Open (Window);
      when Id_App_About =>
        On_About (Window);
      when Id_App_Exit  =>
        Close (Window);
      when Id_File_Close =>
        if Window.Count_MDI_Children = 0 then
          Close (Window);  --  Ctrl-W when no subwindow is open.
        else
          On_Menu_Select (Window_Type (Window), Item);
        end if;
      when Id_Window_Cascade   =>
        MDI_Cascade (Window);
      when Id_Window_Tile_Horz =>
        MDI_Tile_Horizontal (Window);
      when Id_Window_Tile_Vert =>
        MDI_Tile_Vertical (Window);
      when Id_Window_Close_All =>
        My_MDI_Close_All(Window);
      when others =>
        for c in MDI_main_cmd loop
          if Item = ID_custom(c) then
            Custom_Command(Window, c);
            exit;
          end if;
        end loop;
-- !!! test !!!
--  for c in MDI_child_cmd loop
--    if Item = Id_Custom(C) then
--      Message_Box(Window, "Main - Custom - Is for child " , Custom_Cmd'
--        Image(C));
--      exit;
--    end if;
--  end loop;
        On_Menu_Select (Window_Type (Window), Item);
    end case;
  end On_Menu_Select;

  --------------------
  -- On_Right_Click --
  --------------------

  procedure On_Right_Click (
        Control : in out MDI_Status_Bar_Type ) is
    Parent : constant MDI_Main_Access := MDI_Main_Access (Controlling_Parent (Control));
  begin
    On_About (Parent.all);
  end On_Right_Click;

  procedure On_File_Drop (
        Window     : in out MDI_Main_Type;
        File_Names : in     GWindows.Windows.Array_Of_File_Names ) is
  begin
    for I in File_Names'Range loop
      Open_Child_Window_And_Load_Picture( Window, File_Names(I) );
    end loop;
  end On_File_Drop;

  procedure On_Close (
        Window    : in out MDI_Main_Type;
        Can_Close :    out Boolean        ) is
  begin
    begin
      wmaxi:= Zoom(Window);
      if not (wmaxi or Minimized(Window)) then
        wleft  := Left(Window);
        wtop   := Top(Window);
        wwidth := Width(Window);
        wheight:= Height(Window);
      end if;
      for c in Floating_toolbar_categ loop
        TC_FT_memo(c).geom  := Window.Floating_toolbars(c).window.geom;
        TC_FT_memo(c).stat  := Window.Floating_toolbars(c).status;
      end loop;
    end;

    TC.GWin.Options.Save;
    TC.GWin.Previewing.Cleanup;

    My_MDI_Close_All(Window);
    -- ^ Don't forget to save unsaved pictures !
    -- Operation can be cancelled by user for one unsaved picture.
    Can_Close:= MDI_Picture_Child.success_in_enumerated_close;
    --
    if Can_Close then
      GWindows.Base.On_Exception_Handler (Handler => null);
      -- !! Trick to remove a strange crash on Destroy_Children
      -- !! on certain Windows platforms - 29-Jun-2012
    end if;
  end On_Close;

  procedure Update_Status_Bar(
    Window    : in out MDI_Main_Type;
    Part      :        MDI_Status_bar_part;
    Content   :        GString:= "" ) is
  begin
    Text( Window.Status_Bar, Content, MDI_Status_bar_part'Pos(Part) );
  end Update_Status_Bar;

  procedure Toolbar_enabling(
    window    : in out MDI_Main_Type;
    switch    :        Boolean
  )
  is
  begin
    for t in Floating_toolbar_categ loop
      if switch then
        Floating_toolbars.Enable(window.Floating_toolbars(t).window);
      else
        Floating_toolbars.Disable(window.Floating_toolbars(t).window);
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
    Toolbar_enabling(Main, False);
    Result := GWindows.Application.Show_Dialog (Window, Parent);
    Toolbar_enabling(Main, True);
  end Show_Dialog_with_Toolbars_off;

end TC.GWin.MDI_Main;
