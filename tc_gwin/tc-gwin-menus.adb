with TC.GWin.Lang;

with GWin_Util;

with Office_Applications;

package body TC.GWin.Menus is

  use GWindows.Menus, GWin_Util, Lang;

  procedure Append_Item (Menu         : in Menu_Type;
                         Menu_Command : in Custom_cmd)
  is
    i    : constant Integer := ID_custom (Menu_Command);
    s    : constant GWindows.GString := Msg (msg_for_command (Menu_Command));
    Keys : constant GWindows.GString := Keyboard_Shortcut (Menu_Command);
  begin
    if Keys = "" then
      Append_Item (Menu, s, i);
    else
      Append_Item (Menu, s & HT & Keys, i);
    end if;
  end Append_Item;

  function Create_File_Menu (is_child : Boolean) return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    Append_Item (m, Msg (fnew)  & HT & "Ctrl+N", ID_FILE_NEW);
    Append_Item (m, Msg (fopen) & HT & "Ctrl+O", ID_FILE_OPEN);
    Append_Separator (m);
    if is_child then
      Append_Item (m, open_containing_folder);
      Append_Item (m, save);
      Append_Item (m, save_as);
      Append_Item (m, close);
      Append_Separator (m);
      Append_Item (m, preview);
      Append_Separator (m);
    end if;
    for i in Office_Applications.MRU_Range loop
      Append_Item
        (m,
         "",
         ID_custom (Custom_cmd'Val (Custom_cmd'Pos (mru1) + i - Office_Applications.MRU_Range'First)));
    end loop;
    Append_Separator (m);
    if is_child then
      Append_Item (m, Msg (fexit) & HT & "Alt+F4", ID_APP_EXIT);
    else
      Append_Item (m, Msg (fexit) & HT & "Ctrl+W / Alt+F4", ID_APP_EXIT);
    end if;
    return m;
  end Create_File_Menu;

  function Create_Draw_Menu return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    Append_Item (m, text);
    Append_Item (m, "\&put", ID_custom (put));
    Append_Separator (m);  --  ^ 1-point commands
    Append_Item (m, line);
    Append_Item (m, framebox);
    Append_Item (m, filled_box);
    Append_Item (m, oval);
    Append_Item (m, circle);
    Append_Item (m, filled_circle);
    Append_Separator (m);  --  ^ 1-click, 2-point commands
    Append_Item (m, bez);
    Append_Item (m, par_cur_2d_cmd);
    return m;
  end Create_Draw_Menu;

  function Create_Line_Menu return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    for c in Line_setting_cmd loop
      Append_Item (m, c);
      if c = thick or c = dash_param then
        Append_Separator (m);
      end if;
    end loop;
    return m;
  end Create_Line_Menu;

  function Create_Edit_Menu return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    Append_Item (m, change_text);
    Append_Separator (m);
    Append_Item (m, pick_obj);
    Append_Item (m, select_all);
    Append_Item (m, unselect);
    Append_Separator (m);
    Append_Item (m, translate);
    Append_Item (m, mirror);
    Append_Item (m, rotate);
    Append_Item (m, homoth);
    Append_Separator (m);
    Append_Item (m, delete);
    Append_Item (m, copy_clip);
    Append_Item (m, cut_clip);
    Append_Item (m, paste_clip);
    Append_Item (m, save_macro);
    Append_Item (m, load_macro);
    return m;
  end Create_Edit_Menu;

  function Create_Tools_Menu return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    Append_Item (m, clean_pic);
    return m;
  end Create_Tools_Menu;

  function Create_View_Menu return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    for c in Floating_toolbar_categ loop
      Append_Item (m, c);
      if Windows_95 then -- Button sizes are wrong; Win 98, 2K, XP: OK
        State (m, Command, ID_custom (c), Grayed);
      end if;
    end loop;
    return m;
  end Create_View_Menu;

  function Create_Options_Menu (is_child : Boolean) return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    Append_Item (m, gen_opt_dialog);
    if is_child then
      Append_Item (m, pic_opt_dialog);
    end if;
    return m;
  end Create_Options_Menu;

  function Create_Wndw_Menu return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    Append_Item (m, Msg (wcascade), ID_WINDOW_CASCADE);
    Append_Item (m, Msg (wtilehor), ID_WINDOW_TILE_HORZ);
    Append_Item (m, Msg (wtilever), ID_WINDOW_TILE_VERT);
    Append_Item (m, Msg (wclosall), ID_WINDOW_CLOSE_ALL);
    return m;
  end Create_Wndw_Menu;

  function Create_Help_Menu return GWindows.Menus.Menu_Type is
    m : constant Menu_Type := Create_Popup;
  begin
    Append_Item (m, Msg (habout), ID_APP_ABOUT);
    return m;
  end Create_Help_Menu;

end TC.GWin.Menus;
