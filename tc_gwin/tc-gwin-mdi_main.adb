with TC.Input;

with TC.GWin.Lang;                      use TC.GWin.Lang;
with TC.GWin.MDI_Picture_Child;         use TC.GWin.MDI_Picture_Child;

with TC.GWin.Display,
     TC.GWin.Options,
     TC.GWin.Menus,
     TC.GWin.Toolbars,
     TC.GWin.Previewing;

with TeXCAD_Resource_GUI;

with GWindows.Application;              use GWindows.Application;
with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
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
      cmd:= Id_Custom( Custom_cmd'Val( Custom_cmd'Pos(mru1) + i - mru'First));
      Text(
        m, command, cmd,
         '&' & Trim(Integer'Image(i),left) &
         ' ' &
         Shorten_filename(To_GString_from_Unbounded(mru(i)))
      );
      --State(m,command,cmd,Disabled);
    end loop;
  end Update_MRU_Menu;

  procedure Update_Toolbar_Menu( m: in Menu_type; tba: Floating_toolbar_array) is
    use Floating_toolbars;
  begin
    for c in Floating_toolbar_categ loop
      Check(m, Command, Id_Custom(c), tba(c).status /= invisible);
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
    Update_Toolbar_Menu(Window.View_menu, Window.Floating_toolbars);
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
    Update_MRU_Menu(Window.file_menu);
    Append_Menu (Main, Msg(ffile), Window.file_menu);

    Window.View_menu:= TC.GWin.Menus.Create_View_Menu;
    Append_Menu (Main, Msg(vview), Window.View_menu);

    Append_Menu (Main, Msg(oopt), TC.GWin.Menus.Create_Options_Menu(is_child => False));
    Append_Menu (Main, Msg(wwindow), TC.GWin.Menus.Create_Wndw_Menu);
    Append_Menu (Main, Msg(hhelp), TC.GWin.Menus.Create_Help_Menu);

    MDI_Menu (Window, Main, Window_Menu => 4);
  end Create_Menus;

  procedure Focus_an_open_window(
    Window    : MDI_Main_Type;
    file_name : Gstring_Unbounded;
    is_open   : out Boolean )
  is
    procedure Identify (Window : GWindows.Base.Pointer_To_Base_Window_Class)
    is
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
      MDI_Picture_Child_Type(Window.all).Draw_Control.picture.refresh:= full;
      -- ^ 17-Oct-2003 : otherwise the buffer is just copied to screen
      GWindows.Base.Redraw(Window.all);
    end if;
  end Redraw_Child;

  procedure Redraw_all(Window: in out MDI_Main_Type) is
  begin
    Redraw(Window);
    Redraw(Window.Tool_bar);
    GWindows.Base.Enumerate_Children(MDI_Client_Window (Window).all,Redraw_child'Access);
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
    -- Show things in the main status bar - effective only after Thaw!
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
    TC.Input.Load( Candidate, False, To_GString_From_Unbounded (File_Name));
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
        Ada.Exceptions.Exception_Message(E),
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
        To_Gstring_Unbounded(Argument(I))
      );
    end loop;
    Accept_File_Drag_And_Drop(Window);
    Redraw(Window.Tool_bar); -- 2007: sometimes the buttons do not appear...
    Window.record_dimensions:= True;
  end On_Create;

  procedure On_Move (Window : in out MDI_Main_Type;
                     Left   : in     Integer;
                     Top    : in     Integer) is
    pragma Warnings(Off,Top);
    pragma Warnings(Off,Left);
  begin
    if Window.record_dimensions and
       not (Zoom(Window) or Minimized(Window)) then
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
       not (Zoom(Window) or Minimized(Window)) then
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
      ((To_Gstring_Unbounded (Msg(Ltx_Pic) & " (*." & Pic_Suffix & ")"),
          To_Gstring_Unbounded ("*." & Pic_Suffix )),
        (To_Gstring_Unbounded (Msg(All_Files) & " (*.*)"),
          To_Gstring_Unbounded ("*.*"))),
      '.' & Pic_Suffix,
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

    GWindows.Static_Controls.Web.Create_URL(About, "TeXCAD", web, 60, 15, 80, 16);

    Create_Label(About,
      "* Version: " & Version & "   * Reference: " & Reference,
      140, 15, w-140-18, 25);

    Create_Icon (About, "AAA_Main_Icon", 10,10,32,32);
    Create_Icon (About, "Picture_Icon",   w,10,32,32);
    Create_Label (About, Msg(blurb), 60, 35, Wmax-92, 25);
    Create_Label (About, TeXCAD_Resource_GUI.Version_info.LegalCopyright & " (cf COPYING.TXT)",
      60, 55, Wmax-92, 25);
    Create_URL (About, "Internet: " & web, web,
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

  ------------------------
  -- On_General_Options --
  ------------------------

  type Color_button is new Gwindows.Buttons.Button_Type with record
    z: Color_zone;
  end record;


  type Special_Check_Box_Type is new Check_Box_Type with record
    label   : Label_Type;
    edit_box: Edit_Box_Type;
  end record;

  procedure On_Click (Button : in out Special_Check_Box_Type);

  procedure State (Button : in out Special_Check_Box_Type;
                   State  : in     Check_State_Type)
  is
  begin
    GWindows.Buttons.State(Check_Box_Type(Button),State);
    Enabled(Button.label,State=Checked);
    Enabled(Button.edit_box,State=Checked);
  end State;

  procedure On_Click (Button : in out Special_Check_Box_Type) is
  begin
    On_Click(Check_Box_Type(Button));
    State(Button, State(Button));
  end On_Click;

  procedure On_General_Options (
        Window : in out MDI_Main_Type )
  is
    Pan                      : Window_Type;

    subtype Tab_subject is Lang.Message range
      gen_opt_tab_display .. gen_opt_tab_miscellaneous;
    package Tabbing is
      new GWin_Util.Property_Tabs_Package(Tab_subject,Lang.Msg,"O&K",Msg(mcancel));

    New_Pic_Opt              : Button_Type;
    Lang                     : Drop_Down_List_Box_Type;
    Result, i,x,y,yy         : Integer;
    D_tex_suff,
    D_mac_suff               : Edit_Box_Type;
    color_group, suffix_group: Group_Box_Type;
    bak_check_box            : Special_Check_Box_Type;
    g_group                  : Group_Box_Type;
    g_radio                  : array(Grid_Display) of Radio_Button_Type;
    redraw_again             : Boolean  := False;

    qbez_group : Group_Box_Type;
    q_radio    : array(Solid_Bezier_points_mode) of Radio_Button_Type;

    preview_group : Group_Box_Type;
    preview_radio : array(LaTeX_version) of Radio_Button_Type;
    latver: constant array(LaTeX_version) of String(1..5):=
      (v209 => "2.09 ", v2e => ">= 2e");
    preview_dir_group : Group_Box_Type;
    preview_dir_radio : array(Preview_directory_choice) of Radio_Button_Type;
    preview_dir_msg: constant array (Preview_directory_choice) of Message:=
      ( current   => mcurrent,
        temporary => mtemporary );

    wmax : constant:= 370;
    y0: constant:= 5;

    original  : constant TC.General_Options := gen_opt;
    candidate :          TC.General_Options := original;
    orig_col  : constant TC.GWin.Color_set  := color;

    procedure Get_Data ( pnl : in out Gwindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off,pnl);
    begin
      Candidate.tex_suff:= To_Unbounded_String(Text(d_tex_suff));
      Candidate.mac_suff:= To_Unbounded_String(Text(d_mac_suff));
      Candidate.bak_suff:= To_Unbounded_String(Text(bak_check_box.edit_box));
      candidate.bak_enabled:= State(bak_check_box) = Checked;
      for L in Language loop
        if Text(Lang) = Language_Rich_Image(L) then
          candidate.lang:= l;
          exit;
        end if;
      end loop;
      for g in Grid_Display loop
        if State(g_radio(g))=checked then
          if candidate.grid /= g then
            gen_opt.grid:= g;
            -- ^ On change aussi les options pour faire joli,
            --   mais original contient l'original
            redraw_again:= True;
            Redraw_all(Window);
            candidate.grid:= g;
          end if;
        end if;
      end loop;
      for q in Solid_Bezier_points_mode loop
        if State(q_radio(q))=checked then
          candidate.solid_bez:= q;
        end if;
      end loop;
      for l in LaTeX_version loop
        if State(preview_radio(l))=checked then
          candidate.preview_mode:= l;
        end if;
      end loop;
      for d in Preview_directory_choice loop
        if State(preview_dir_radio(d))=checked then
          candidate.preview_directory:= d;
        end if;
      end loop;
    exception
      when others =>
        Message_Box(
          pnl,
          "Invalid data", "Incomplete reading of your changes",
          OK_Box, Error_Icon);
    end Get_Data;

    procedure Do_Display (btn : in out Gwindows.Base.Base_Window_Type'Class)
    is
      modified: Boolean; -- dumped since not related to a specific picture
    begin
      On_Picture_Options(
        btn, Candidate.Options_For_New,
        window,
        modified,
        Msg(onewpicopt));
    end Do_Display;

    col_btn: array(Color_zone) of Color_button;

    procedure CColor (btn : in out Gwindows.Base.Base_Window_Type'Class ) is
      c : Color_Type;
      ok: Boolean;
      z : Color_zone;
    begin
      z:= Color_Button(btn).z;
      c:= color(z);
      Choose_Color( btn, c, ok );
      if ok and then color(z) /= c then
        color(z):= c;
        redraw_again:= True;
        TC.GWin.Display.recreate_drawing_objects:= True;
        Redraw_all(Window);
      end if;
    end CColor;

  begin
    Create_As_Dialog(Pan, Window, Msg(ogenopt), Width => wmax + 30, Height => 400);
    Center(Pan);
    Small_Icon (Pan, "Options_Icon");
    On_Destroy_Handler (Pan, Get_Data'Unrestricted_Access);

    GWin_Util.Use_Gui_Font(Pan);

    Tabbing.Create(pan);

    -- Misc tab / Suffix group --
    y:= y0;
    Create(suffix_group, Tabbing.tab(gen_opt_tab_miscellaneous), Msg(suffix),
      5,   y, wmax-5, 95);

    y:= y + 20;
    Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(Ltx_Pic),
      45,  y, 250, 20);
    Create (D_Tex_Suff, Tabbing.tab(gen_opt_tab_miscellaneous),
      To_GString_From_Unbounded(Candidate.Tex_Suff),
     300,  y, 50, 20);

    y:= y + 25;
    Create(bak_check_box.label, Tabbing.tab(gen_opt_tab_miscellaneous), Msg(Ltx_Pic_bak),
      45,  y, 250, 20);
    Create(bak_check_box.edit_box, Tabbing.tab(gen_opt_tab_miscellaneous),
      To_GString_From_Unbounded(Candidate.bak_suff),
     300,  y, 50, 20);
    Create(bak_check_box,Tabbing.tab(gen_opt_tab_miscellaneous), "",
      25,  y,  15, 15);
    State(bak_check_box, boolean_to_state(candidate.bak_enabled));

    y:= y + 25;
    Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(Tcd_Mac),
      45,  y, 250, 20);
    Create (D_Mac_Suff, Tabbing.tab(gen_opt_tab_miscellaneous),
      To_GString_From_Unbounded(Candidate.Mac_Suff),
     300,  y, 50, 20);

    -- Misc tab / Preview directory group
    y:= y + 35;
    Create(preview_dir_group, Tabbing.tab(gen_opt_tab_miscellaneous), Msg(preview_directory),
      5,   y, wmax-5, 65);
    for d in Preview_directory_choice loop
      x:= 10 + 180* Preview_directory_choice'Pos(d);
      Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(preview_dir_msg(d)),
        x,  y+40, 175, 22);
      Create(preview_dir_radio(d),Tabbing.tab(gen_opt_tab_miscellaneous), "",  x,  y+20,  60, 15);
      State(preview_dir_radio(d),boolean_to_state(d=candidate.preview_directory));
    end loop;

    -- Misc tab / Language ListBox
    y:= y + 70;
    Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(lng),
      10,  y, 150, 25);
    Create( Lang, Tabbing.tab(gen_opt_tab_miscellaneous),
     140,  y, 150, 200, Sort => False, Is_Dynamic => False);
    for L in Language loop
      Add( Lang, Language_Rich_Image(L) );
    end loop;
    Text(lang, Language_Rich_Image(candidate.lang));

    y:= y + 35;
    Create (New_Pic_Opt, Tabbing.tab(gen_opt_tab_miscellaneous), "&" & Msg(onewpicopt),
      10, y, 240, 30);
    On_Click_Handler (New_Pic_Opt, Do_Display'Unrestricted_Access);

    -- Display tab / Colours
    y:= y0;
    yy:= 30 * (2+Color_zone'Pos(Color_zone'Last));
    Create( color_group, Tabbing.tab(gen_opt_tab_display), Msg(gcolors), 5,   y, wmax-5, yy );

    for z in Color_zone loop
      i:= y + 30 * (Color_zone'Pos(z)+1);
      Create_Label(
        Tabbing.tab(gen_opt_tab_display),
        Msg(Message'Val(Message'Pos(background)+Color_zone'Pos(z))),
        25, i, 125, 25);
      Create (col_btn(z), Tabbing.tab(gen_opt_tab_display), Msg(choose), 190, i, 90, 22);
      On_Click_Handler (col_btn(z), CColor'Unrestricted_Access);
      col_btn(z).z:= z;
    end loop;

    -- Display tab / Grid
    y:= y + yy + 8;
    yy:= 65;
    Create(g_group, Tabbing.tab(gen_opt_tab_display), Msg(grid), 5, y, wmax-5, yy);
    for g in Grid_Display loop
      x:= 20 + 100* Grid_Display'Pos(g);
      Create_Label (Tabbing.tab(gen_opt_tab_display),
        Msg(message'Val(message'Pos(gridnone)+Grid_Display'Pos(g))),
        x,  y+40, 100, 22);
      Create(g_radio(g),Tabbing.tab(gen_opt_tab_display), "",  x,  y+20,  60, 15);
      On_Click_Handler(g_radio(g),Get_Data'Unrestricted_Access);
      State(g_radio(g),boolean_to_state(g=candidate.grid));
    end loop;

    -- LaTeX tab / Bezier group
    y:= y0;
    yy:= 65;
    Create(qbez_group, Tabbing.tab(gen_opt_tab_latex), Msg(bezpts), 5, y, wmax-5, yy);
    for q in Solid_Bezier_points_mode loop
      x:= 10 + 180* Solid_Bezier_points_mode'Pos(q);
      Create_Label (Tabbing.tab(gen_opt_tab_latex),
        Msg(message'Val(message'Pos(bezauto)+Solid_Bezier_points_mode'Pos(q))),
        x,  y+40, 175, 22);
      Create(q_radio(q),Tabbing.tab(gen_opt_tab_latex), "",  x,  y+20,  60, 15);
      State(q_radio(q),boolean_to_state(q=candidate.solid_bez));
    end loop;

    -- LaTeX tab / Preview LaTeX version group
    y:= y + yy + 8;
    Create(preview_group, Tabbing.tab(gen_opt_tab_latex), Msg(preview_latex_mode), 5, y, wmax-5, yy);
    for l in LaTeX_version loop
      x:= 10 + 180* LaTeX_version'Pos(l);
      Create_Label (Tabbing.tab(gen_opt_tab_latex), "LaTeX " & latver(l),
        x,  y+40, 175, 22);
      Create(preview_radio(l),Tabbing.tab(gen_opt_tab_latex), "",  x,  y+20,  60, 15);
      State(preview_radio(l),boolean_to_state(l=candidate.preview_mode));
    end loop;

    Show_Dialog_with_Toolbars_off(Pan, Window, Window, Result);

    case Result is
      when Idok     =>
        if Candidate.Lang /= Gen_Opt.Lang then
          Gwindows.Message_Boxes.Message_Box
            ( Window,
              Speak(Gen_Opt.Lang,lng_chg) & " - " &
              Speak(Candidate.Lang,lng_chg) ,
              Speak(Gen_Opt.Lang,fx_restrt) & ASCII.LF &
              Speak(Candidate.Lang,fx_restrt)
            );
        end if;
        gen_opt:= candidate;
        TC.GWin.Options.Save;
      when others   =>       -- contains Idcancel
        gen_opt:= original;
        if color /= orig_col then -- 17-Oct-2003: Cancel after colour change!
          TC.GWin.Display.recreate_drawing_objects:= True;
        end if;
        color:= orig_col;
    end case;
    if redraw_again then
      Redraw_all(Window);
    end if;
  end On_General_Options;

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
            On_General_Options(Window);
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
        Message_Box(Window, "Main - Custom - not for main " , Custom_Cmd'
          Image(C));
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
       MDI_Picture_Child.Success_In_Enumerated_Close then -- no [cancel] up to now
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
