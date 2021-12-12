with TC.GWin.Lang;                      use TC.GWin.Lang;

with GWin_Util;

package body TC.GWin.Menus is

  use GWindows.Menus, GWin_Util;

  procedure Append_Item (Menu    : in Menu_Type;
                         Command : in Custom_cmd;
                         Keys    : in GString:= "")
  is
    i: constant Integer:= ID_custom(Command);
    s: constant GString:= Msg(msg_for_command(Command));
  begin
    if Keys = "" then
      Append_Item (Menu, s, i);
    else
      Append_Item (Menu, s & HT & Keys, i);
    end if;
  end Append_Item;

  function Create_File_Menu(is_child: Boolean) return GWindows.Menus.Menu_Type is
    m: constant Menu_Type:= Create_Popup;
  begin
    Append_Item (m, Msg(fnew)  & HT & "Ctrl+N", Id_File_New);
    Append_Item (m, Msg(fopen) & HT & "Ctrl+O", Id_File_Open);
    Append_Separator (m);
    if is_child then
      Append_Item (m, save, "Ctrl+S");
      Append_Item (m, save_as, "F12");
      Append_Item (m, close, "Ctrl+W / Ctrl+F4");
      Append_Separator (m);
      Append_Item (m, preview);
      Append_Separator (m);
    end if;
    for i in mru'Range loop
      Append_Item (m,
       "",
       Id_Custom( Custom_cmd'Val( Custom_cmd'Pos(mru1) + i - mru'First)) );
    end loop;
    Append_Separator (m);
    if is_child then
      Append_Item (m, Msg(fexit) & HT & "Alt+F4", ID_App_Exit);
    else
      Append_Item (m, Msg(fexit) & HT & "Ctrl+W / Alt+F4", ID_App_Exit);
    end if;
    return m;
  end Create_File_Menu;

  function Create_Draw_Menu return GWindows.Menus.Menu_Type is
  m: constant Menu_Type:= Create_Popup;
  begin
    Append_Item (m, text );
    Append_Item (m, "\&put", ID_custom(put) );
    Append_Separator (m); -- ^ 1-point commands
    Append_Item (m, line );
    Append_Item (m, framebox );
    Append_Item (m, filled_box );
    Append_Item (m, oval );
    Append_Item (m, circle );
    Append_Item (m, filled_circle );
    Append_Separator (m); -- ^ 1-click, 2-point commands
    Append_Item (m, bez );
    Append_Item (m, par_cur_2d_cmd );
    return m;
  end Create_Draw_Menu;

  function Create_Line_Menu return GWindows.Menus.Menu_Type is
    m: constant Menu_Type:= Create_Popup;
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
    m: constant Menu_Type:= Create_Popup;
  begin
    Append_Item (m, change_text );
    Append_Separator (m);
    Append_Item (m, pick_obj );
    Append_Item (m, select_all, "Ctrl+A" );
    Append_Item (m, unselect, "Ctrl+U" );
    Append_Separator (m);
    Append_Item (m, translate );
    Append_Item (m, mirror );
    Append_Item (m, rotate );
    Append_Item (m, homoth );
    Append_Separator (m);
    Append_Item (m, delete,     "[Ctrl+] Del" );
    Append_Item (m, copy_clip,  "Ctrl+C / Ctrl+Ins" );
    Append_Item (m, cut_clip,   "Ctrl+X / Shift+Del" );
    Append_Item (m, paste_clip, "Ctrl+V / Shift+Ins" );
    Append_Item (m, save_macro );
    Append_Item (m, load_macro );
    return m;
  end Create_Edit_Menu;

  function Create_Tools_Menu return GWindows.Menus.Menu_Type is
    m: constant Menu_Type:= Create_Popup;
  begin
    Append_Item (m, Msg(tclean), ID_custom(clean_pic));
    return m;
  end Create_Tools_Menu;

  function Create_View_Menu return GWindows.Menus.Menu_Type is
    m: constant Menu_Type:= Create_Popup;
  begin
    for c in Floating_toolbar_categ loop
      Append_Item (m, c);
      if Windows_95 then -- Button sizes are wrong; Win 98, 2K, XP: OK
        State(m,Command,ID_custom(c),Grayed);
      end if;
    end loop;
    return m;
  end Create_View_Menu;

  function Create_Options_Menu(is_child: Boolean) return GWindows.Menus.Menu_Type is
    m: constant Menu_Type:= Create_Popup;
  begin
    Append_Item (m, Msg(ogenopt), ID_custom(gen_opt_dialog));
    if is_child then
      Append_Item (m, Msg(opicopt), ID_custom(pic_opt_dialog));
    end if;
    return m;
  end Create_Options_Menu;

  function Create_Wndw_Menu return GWindows.Menus.Menu_Type is
    m: constant Menu_Type:= Create_Popup;
  begin
    Append_Item (m, Msg(wcascade), ID_WINDOW_CASCADE);
    Append_Item (m, Msg(wtilehor), ID_WINDOW_TILE_HORZ);
    Append_Item (m, Msg(wtilever), ID_WINDOW_TILE_VERT);
    Append_Item (m, Msg(wclosall), ID_WINDOW_CLOSE_ALL);
    return m;
  end Create_Wndw_Menu;

  function Create_Help_Menu return GWindows.Menus.Menu_Type is
    m: constant Menu_Type:= Create_Popup;
  begin
    Append_Item (m, Msg(habout), Id_App_About);
    return m;
  end Create_Help_Menu;

end TC.GWin.Menus;

