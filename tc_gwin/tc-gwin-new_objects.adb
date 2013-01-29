with TC.GWin.Object_editing;            use TC.GWin.Object_editing;

package body TC.GWin.New_objects is

  procedure New_boxoval(
    p           : in out Picture;
    parent      : in out Base_Window_Type'Class;
    main        : in out MDI_Main_Type;
    P1, P2      :        Point;
    ls          : in out Line_settings; -- possible current dot/dash change
    cmd         :        Drawing_cmd )
  is
    t : ptr_Obj_type;
    modif: Boolean;
  begin
    if cmd = oval then
      t:= new Obj_type(oval);
      t.P1:= 0.5 * (P1+P2);
      t.LL:= P1;
      t.part:= entire;
      t.osize := P2-P1;
    else
      t:= new Obj_type(box);
      t.P1:= P1;
      t.inhalt:= Null_Unbounded_String;
      t.adjust_len:= 0;
      t.size := P2-P1;
    end if;
    t.ls:= ls;
    case cmd is
      when framebox   =>
        t.solid:= False;
        Change_Text(parent, main, t.all, modif);
      when filled_box =>
        t.solid:= True;
      when oval =>
        Change_Oval(parent, main, t.all, modif);
      when others =>
        null;
    end case;
    ls:= t.ls; -- possible current dot/dash change by Text_Change
    -- Box is kept even if Change_Text cancelled (modif=False)
    Insert(p, t, at_end);
  end New_Boxoval;

  procedure New_linvec(
    p           : in out Picture;
    P1          :        Point;
    P2          : in out Point;        -- moved by setting limited \line slope
    ls          :        Line_settings)
  is
    t : constant ptr_Obj_type:= new Obj_type(line);
  begin
    t.P1:= P1;
    t.P2:= P2;
    t.ls:= ls;
    t.any_slope:= p.opt.steigung or t.ls.pattern /= plain;
    Set_slope_of_linvec(t.all);
    Improve_linvec(t.all, p.ul_in_pt); -- 30-Apr-2004
    Insert(p, t, at_end);
    P2:= t.P2;
  end New_linvec;

  procedure New_bezier(
    p        : in out Picture;
    P1,PE,PG :        Point;
    ls       :        Line_settings)
  is
    t : constant ptr_Obj_type:= new Obj_type(bezier);
  begin
    t.P1:= P1;
    t.PE:= PE;
    t.ls:= ls;
    Set_control_point( t.all, PG );
    case gen_opt.solid_bez is
      when auto    => t.num:= 0;
      when suggest => t.num:= Good_num_of_bezier_points(t.all,p.ul_in_pt);
    end case;
    Set_slope_of_bezvec(t.all,p.ul_in_pt);
    Insert(p, t, at_end);
  end New_bezier;

  procedure New_circdisc(
    p           : in out Picture;
    P1, P2      :        Point;
    cmd         :        Drawing_cmd;
    ls          :        Line_settings
  )
  is
    t : ptr_Obj_type;
  begin
    case cmd is
      when circle        => t:= new Obj_type(circ);
      when filled_circle => t:= new Obj_type(disc);
      when others        => null;
    end case;
    t.ls:= ls;
    t.P1:= P1;
    Set_radius( t.all, df=> P2 - P1 );
    Insert(p, t, at_end);
  end New_circdisc;

  procedure New_text(
    p       : in out Picture;
    parent  : in out Base_Window_Type'Class;
    main    : in out MDI_Main_Type;
    P1      :        Point;
    art     :        obj_art_type;
    ls      :        Line_settings  -- useless, just avoid thickness swapping
  )
  is
    t : constant ptr_Obj_type:= new Obj_type(art);
    modif: Boolean;
  begin
    t.P1:= P1;
    t.inhalt:= Null_Unbounded_String;
    t.adjust_len:= 0;
    t.solid:= False;
    t.ls:= ls;
    Change_Text(parent, main, t.all, modif);
    if modif then
      Insert(p, t, at_end);
      p.refresh:= only_last; -- 30-Apr-2004
    end if;
  end New_text;

end TC.GWin.New_objects;
