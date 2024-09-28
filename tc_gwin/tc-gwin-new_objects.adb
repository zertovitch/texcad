with TC.GWin.Object_Editing;

package body TC.GWin.New_objects is

  procedure New_text
    (p      : in out Picture;
     parent : in out GWindows.Base.Base_Window_Type'Class;
     main   : in out MDI_Main.MDI_Main_Type;
     P1     :        Point;
     art    :        Obj_Art_Type;
     ls     :        Line_Settings)
  is
    t : constant ptr_Obj_Type := new Obj_Type (art);
    modif : Boolean;
  begin
    t.P1 := P1;
    t.inhalt := Null_Unbounded_String;
    t.adjust_len := 0;
    t.solid := False;
    t.ls := ls;
    Object_Editing.Change_Text (parent, main, t.all, modif);
    if modif then
      Insert (p, t, at_end);
      p.refresh := only_last; -- 30-Apr-2004
    end if;
  end New_text;

  procedure New_boxoval
    (p           : in out Picture;
     parent      : in out GWindows.Base.Base_Window_Type'Class;
     main        : in out MDI_Main.MDI_Main_Type;
     P1, P2      :        Point;
     ls          : in out Line_Settings;  --  Possible current dot/dash change
     cmd         :        Drawing_cmd)
  is
    t : ptr_Obj_Type;
    modif : Boolean;
  begin
    if cmd = oval then
      t := new Obj_Type (oval);
      t.P1 := 0.5 * (P1 + P2);
      t.LL := P1;
      t.part := entire;
      t.osize := P2 - P1;
    else
      t := new Obj_Type (box);
      t.P1 := P1;
      t.inhalt := Null_Unbounded_String;
      t.adjust_len := 0;
      t.size := P2 - P1;
    end if;
    t.ls := ls;
    case cmd is
      when framebox   =>
        t.solid := False;
        Object_Editing.Change_Text (parent, main, t.all, modif);
      when filled_box =>
        t.solid := True;
      when oval =>
        Object_Editing.Change_Oval (parent, main, t.all, modif);
      when others =>
        null;
    end case;
    ls := t.ls; -- possible current dot/dash change by Text_Change
    --  Box is kept even if Change_Text cancelled (modif=False)
    Insert (p, t, at_end);
  end New_boxoval;

  procedure New_linvec
    (p  : in out Picture;
     P1 :        Point;
     P2 : in out Point;        --  Moved by setting limited \line slope
     ls :        Line_Settings)
  is
    t : constant ptr_Obj_Type := new Obj_Type (line);
  begin
    t.P1 := P1;
    t.P2 := P2;
    t.ls := ls;
    t.any_slope := p.opt.steigung or t.ls.pattern /= plain;
    Set_Slope_of_Linvec (t.all);
    Improve_Linvec (t.all, p.ul_in_pt); -- 30-Apr-2004
    Insert (p, t, at_end);
    P2 := t.P2;
  end New_linvec;

  procedure New_bezier
    (p          : in out Picture;
     P1, PE, PG :        Point;
     ls         :        Line_Settings)
  is
    t : constant ptr_Obj_Type := new Obj_Type (bezier);
  begin
    t.P1 := P1;
    t.PE := PE;
    t.ls := ls;
    Set_Control_Point (t.all, PG);
    case gen_opt.solid_bez is
      when auto    => t.num := 0;
      when suggest => t.num := Good_Num_of_Bezier_Points (t.all, p.ul_in_pt);
    end case;
    Set_Slope_of_Bezvec (t.all, p.ul_in_pt);
    Insert (p, t, at_end);
  end New_bezier;

  procedure New_circdisc
    (p      : in out Picture;
     P1, P2 :        Point;
     cmd    :        Drawing_cmd;
     ls     :        Line_Settings)
  is
    t : ptr_Obj_Type;
  begin
    case cmd is
      when circle        => t := new Obj_Type (circ);
      when filled_circle => t := new Obj_Type (disc);
      when others        => null;
    end case;
    t.ls := ls;
    t.P1 := P1;
    Set_Radius (t.all, df => P2 - P1);
    Insert (p, t, at_end);
  end New_circdisc;

  procedure New_paramcurve_2d
    (p      : in out Picture;
     parent : in out GWindows.Base.Base_Window_Type'Class;
     main   : in out MDI_Main.MDI_Main_Type;
     orig   :        Point;
     ls     : in out Line_Settings)
  is
    t : constant ptr_Obj_Type := new Obj_Type (paramcurve2d);
    modif : Boolean;
  begin
    t.P1 := orig;
    t.ls := ls;
    t.data_2d.segments := 0;
    t.data_2d.scale    := 1.0;
    t.data_2d.min_t    := 0.0;
    t.data_2d.max_t    := 1.0;
    Object_Editing.Change_Param_2D (parent, main, t.all, modif);
    if modif then
      Insert (p, t, at_end);
      p.refresh := only_last;
    end if;
  end New_paramcurve_2d;

end TC.GWin.New_objects;
