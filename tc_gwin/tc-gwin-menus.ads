-- Creation of various menus in TC.
-- Made common from MDI_Main and MDI_Child 10-Feb-2004

with GWindows.Menus;

package TC.GWin.Menus is
  -- File / Draw / Line / Edit / View / Options / Window / Help

  function Create_File_Menu(is_child: Boolean) return GWindows.Menus.Menu_Type;

  function Create_Draw_Menu return GWindows.Menus.Menu_Type;
  function Create_Line_Menu return GWindows.Menus.Menu_Type;
  function Create_Edit_Menu return GWindows.Menus.Menu_Type;
  function Create_View_Menu return GWindows.Menus.Menu_Type;

  function Create_Options_Menu(is_child: Boolean) return GWindows.Menus.Menu_Type;

  function Create_Wndw_Menu return GWindows.Menus.Menu_Type;
  function Create_Help_Menu return GWindows.Menus.Menu_Type;

end TC.GWin.Menus;