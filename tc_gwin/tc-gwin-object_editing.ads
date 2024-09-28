with TC.GWin.MDI_Main;

with GWindows.Base;

package TC.GWin.Object_Editing is

  procedure Change_Text
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     t        : in out Obj_Type;
     modified :    out Boolean);

  procedure Change_Oval
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     t        : in out Obj_Type;
     modified :    out Boolean);

  procedure Change_Bezier
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     ul_in_pt :        Real;
     t        : in out Obj_Type;
     modified :    out Boolean);

  procedure Change_Param_2D
    (parent   : in out GWindows.Base.Base_Window_Type'Class;
     main     : in out MDI_Main.MDI_Main_Type;
     t        : in out Obj_Type;
     modified :    out Boolean);

end TC.GWin.Object_Editing;
