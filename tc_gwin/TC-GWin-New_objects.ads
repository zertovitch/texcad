with TC.GWin.MDI_Main;                  use TC.GWin.MDI_Main;
--with TC.GWin.MDI_Picture_Child;         use TC.GWin.MDI_Picture_Child;

with GWindows.Base;                     use GWindows.Base;

package TC.GWin.New_objects is

  procedure New_text(
    p       : in out Picture;
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    P1      :        Point;
    art     :        obj_art_type;
    ls      :        Line_settings  -- useless, just avoid thickness swapping
  );

  procedure New_boxoval(
    p           : in out Picture;
    parent      : in out Base_Window_Type'Class;
    main        : in out MDI_Main_Type;
    P1, P2      :        Point;
    ls          : in out Line_settings; -- possible current dot/dash change
    cmd         :        Drawing_cmd
  );

  procedure New_linvec(
    p           : in out Picture;
    P1          :        Point;
    P2          : in out Point;        -- moved by setting limited \line slope
    ls          :        Line_settings
  );

  procedure New_bezier(
    p        : in out Picture;
    P1,PE,PG :        Point;
    ls       :        Line_settings
  );

  procedure New_circdisc(
    p           : in out Picture;
    P1, P2      :        Point;
    cmd         :        Drawing_cmd;
    ls          :        Line_settings
  );

end TC.GWin.New_objects;