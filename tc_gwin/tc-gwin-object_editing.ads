with TC.GWin.MDI_Main;                  use TC.GWin.MDI_Main;

with GWindows.Base;                     use GWindows.Base;

package TC.GWin.Object_editing is

  procedure Change_Text(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    t       : in out Obj_type;
    modified:    out Boolean );

  procedure Change_Oval(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    t       : in out Obj_type;
    modified:    out Boolean );

  procedure Change_Bezier(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    ul_in_pt:        Real;
    t       : in out Obj_type;
    modified:    out Boolean );

  procedure Change_Param_2D(
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    t       : in out Obj_type;
    modified:    out Boolean );

end TC.GWin.Object_editing;
