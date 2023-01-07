with GWindows.Base, GWindows.Image_Lists;
with Floating_Toolbars;

package TC.GWin.Toolbars is

  -- ** Main tool bar (new/open/save/...) at top left of the main window:

  procedure Init_Main_toolbar(
    tb    : in out Floating_Toolbars.GUI_toolbar'Class;
    il    : in out GWindows.Image_Lists.Image_List_Type;
    parent: in out GWindows.Base.Base_Window_Type'Class);

  -- ** Floating tool bars:

  procedure Init_Floating_toolbars(
    tbs   : in out Floating_toolbar_array;
    parent: in out GWindows.Base.Base_Window_Type'Class);

end TC.GWin.Toolbars;
