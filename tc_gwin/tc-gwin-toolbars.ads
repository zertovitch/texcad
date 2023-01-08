with GWindows.Base;
with Office_Applications;

package TC.GWin.Toolbars is

  -- ** Main tool bar (new/open/save/...) at top left of the main window:

  procedure Init_Main_Tool_Bar
    (tb     : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class;
     parent : in out GWindows.Base.Base_Window_Type'Class);

  -- ** Floating tool bars:

  procedure Init_Floating_Tool_Bars
    (tbs    : in out Floating_toolbar_array;
     parent : in out GWindows.Base.Base_Window_Type'Class);

end TC.GWin.Toolbars;
