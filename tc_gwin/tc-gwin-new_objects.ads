with TC.GWin.MDI_Main;

with GWindows.Base;

package TC.GWin.New_objects is

  procedure New_text
    (p      : in out Picture;
     parent : in out GWindows.Base.Base_Window_Type'Class;
     main   : in out MDI_Main.MDI_Main_Type;
     P1     :        Point;
     art    :        Obj_art_type;
     ls     :        Line_settings);  --  Useless, just avoid thickness swapping

  procedure New_boxoval
    (p      : in out Picture;
     parent : in out GWindows.Base.Base_Window_Type'Class;
     main   : in out MDI_Main.MDI_Main_Type;
     P1, P2 :        Point;
     ls     : in out Line_settings;  --  Possible current dot/dash change
     cmd    :        Drawing_cmd);

  procedure New_linvec
    (p  : in out Picture;
     P1 :        Point;
     P2 : in out Point;        --  Moved by setting limited \line slope
     ls :        Line_settings);

  procedure New_bezier
    (p        : in out Picture;
     P1,PE,PG :        Point;
     ls       :        Line_settings);

  procedure New_circdisc
    (p      : in out Picture;
     P1, P2 :        Point;
     cmd    :        Drawing_cmd;
     ls     :        Line_settings);

  procedure New_paramcurve_2d
    (p      : in out Picture;
     parent : in out GWindows.Base.Base_Window_Type'Class;
     main   : in out MDI_Main.MDI_Main_Type;
     orig   :        Point;
     ls     : in out Line_settings);

end TC.GWin.New_objects;
