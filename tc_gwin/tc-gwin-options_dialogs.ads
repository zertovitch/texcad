with TC.GWin.MDI_Main;
with GWindows.Base;

package TC.GWin.Options_Dialogs is

  ------------------------
  -- On_General_Options --
  ------------------------

  procedure On_General_Options (Window : in out TC.GWin.MDI_Main.MDI_Main_Type);

  ------------------------
  -- On_Picture_Options --
  ------------------------

  procedure On_Picture_Options
     (window      : in out GWindows.Base.Base_Window_Type'Class;
      pic_opt     : in out TC.Picture_Options;
      main        : in out TC.GWin.MDI_Main.MDI_Main_Type;
      is_modified :    out Boolean;
      title       : in     String);

end TC.GWin.Options_Dialogs;
