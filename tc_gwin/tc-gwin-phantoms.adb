with TC.GWin.Display;                   use TC.GWin.Display;

with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;

pragma Elaborate_All (GWindows.Drawing_Objects);  --  For Create_Pen

package body TC.GWin.Phantoms is

  phantom_pen : array (Pen_Style_Type) of Pen_Type;

  procedure Invert_Rubber_Box
    (w  : in out MDI_Picture_Child.TC_Picture_Panel;
     cz :        Color_Zone)
  is
    pragma Unreferenced (cz);
    X1 : constant Integer := w.Xs;
    Y1 : constant Integer := w.Ys;
    X2 : constant Integer := w.X;
    Y2 : constant Integer := w.Y;
  begin
    Set_Mix_Mode (w.Drawing_Area, R2_NOTXORPEN);
    Select_Object (w.Drawing_Area, phantom_pen (Dot));
    Line (w.Drawing_Area, X1, Y1, X2, Y1);
    Line (w.Drawing_Area, X2, Y1, X2, Y2);
    Line (w.Drawing_Area, X1, Y2, X2, Y2);
    Line (w.Drawing_Area, X1, Y1, X1, Y2);
  end Invert_Rubber_Box;

  procedure Invert_Phantom (w : in out MDI_Picture_Child.TC_Picture_Panel) is
    procedure Draw_one (p : ptr_Obj_type) is
    begin
      Draw (w.Drawing_Area, w.Picture, p, w.Width, w.Height);
    end Draw_one;

    o, l : ptr_Obj_type;

    procedure Ortholine (P, D : Point) is
      V  : Point;
      nV : Real;
    begin
      V := Ortho (D);
      nV := Norm (V) * w.Picture.opt.zoom_fac;
      if nV > 0.01 then
        V := 15.0 * (1.0 / nV) * V;
        l.P1 := P - V;
        l.P2 := P + V;
        Draw_one (l);
      end if;
    end Ortholine;

    procedure Circle_or_Oval_frame_and_cross is -- 14-Oct-2005
      f : constant := 0.05;
      fx, fy : Real;
      hd, vd : Point;
    begin
      if o.art = oval then
        fx := o.osize.x * 0.5;
        fy := o.osize.y * 0.5;
      else
        fx := o.rad;
        fy := fx;
      end if;
      hd := (f * fx, 0.0);
      vd := (0.0, f * fy);
      --  Frame:
      l.P1:= o.P1 + (-fx,-fy);
      l.P2:= l.P1;       Draw_one( l ); -- 1 pixel (for 3x pixel inversion)
      l.P2:= l.P1 + hd;  Draw_one( l );
      l.P2:= l.P1 + vd;  Draw_one( l );
      l.P1:= o.P1 + (+fx,-fy);
      l.P2:= l.P1;       Draw_one( l ); -- 1 pixel (for 3x pixel inversion)
      l.P2:= l.P1 - hd;  Draw_one( l );
      l.P2:= l.P1 + vd;  Draw_one( l );
      l.P1:= o.P1 + (+fx,+fy);
      l.P2:= l.P1;       Draw_one( l ); -- 1 pixel (for 3x pixel inversion)
      l.P2:= l.P1 - hd;  Draw_one( l );
      l.P2:= l.P1 - vd;  Draw_one( l );
      l.P1:= o.P1 + (-fx,+fy);
      l.P2:= l.P1;       Draw_one( l ); -- 1 pixel (for 3x pixel inversion)
      l.P2:= l.P1 + hd;  Draw_one( l );
      l.P2:= l.P1 - vd;  Draw_one( l );
      -- Cross:
      l.P1:= o.P1 + hd;
      l.P2:= o.P1 - hd;
      Draw_one( l );
      l.P1:= o.P1 + vd;
      l.P2:= o.P1 - vd;
      Draw_one( l );
      l.P1:= o.P1;
      l.P2:= o.P1;
      Draw_one( l ); -- 1 pixel (for 3x pixel inversion)
    end Circle_or_Oval_frame_and_cross;

    more : constant :=  1.0;  --  Extend the line from a Bezier control point
    rbd  : constant Real := 7.0 / w.Picture.opt.zoom_fac;  --  Small circle
    size : Point;
  begin
    case w.phantomart is
      when txt | putaux | disc =>
        o := new Obj_type (circ);
      when others =>
        o := new Obj_type (w.phantomart);
    end case;
    o.next := null;
    o.P1 := w.PS;
    o.picked := False;
    o.ls := w.phantom_ls;
    case w.phantomart is
      when aux | txt | putaux =>
        o.rad := rbd;
      when box =>
        o.size   := w.PU - w.PS;
        o.solid  := False;
        o.inhalt := Null_Unbounded_String;
      when line =>
        o.P2 := w.PU;
        o.any_slope :=
          --  any slope for any line type
          w.Picture.opt.steigung or
          --  no real \line, \vector etc.
          w.current_cmd in Deformation_cmd or
          --  dot & dash lines are always unlimited
          w.current_ls.pattern /= plain;
        Set_slope_of_linvec (o.all);
      when circ | disc =>
        Set_radius (o.all, df => o.P1 - w.PU);
      when oval =>
        size := w.PU - w.PS;
        o.osize := (abs (size.x), abs (size.y));
        o.LL := (Real'Min (w.PS.x, w.PU.x),
                 Real'Min (w.PS.y, w.PU.y));
        o.P1 := 0.5 * (w.PS + w.PU);
        o.part := entire;
      when bezier =>
        o.PE := w.PE;
        Set_control_point (o.all, w.PU);
        o.num := 0;
        Set_slope_of_bezvec (o.all, w.Picture.ul_in_pt);
        o.num := Good_num_of_bezier_points (o.all, w.Picture.ul_in_pt) / 9;
      when others => null;
    end case;
    Set_Mix_Mode (w.Drawing_Area, R2_NOTXORPEN);
    Select_Object (w.Drawing_Area, phantom_pen (Solid));
    Draw_one (o);
    case w.phantomart is
      when circ | oval =>
        l := new Obj_type (line);
        l.ls := normal_line_settings;
        Circle_or_Oval_frame_and_cross; -- 14-Oct-2005, show frame
        Dispose (l);
      when disc =>
        --  14-Oct-2005, a diagonal cross for showing difference with circle:
        l := new Obj_type (line);
        l.ls := normal_line_settings;
        declare
          x : constant Real := o.rad / 1.4142135624;
        begin
          l.P1:= o.P1 - (x,x);
          l.P2:= o.P1 + (x,x);
          Draw_one( l );
          l.P1:= o.P1 - (x,-x);
          l.P2:= o.P1 + (x,-x);
          Draw_one( l );
        end;
        Circle_or_Oval_frame_and_cross; -- 14-Oct-2005, show frame
        Dispose(l);
      when bezier =>
        Select_Object( w.Drawing_Area, phantom_pen( Dash_Dot_Dot ) );
        l:= new Obj_type(line);
        l.ls:= normal_line_settings;
        l.P1:= o.PC;
        l.P2:= o.P1 + more * (o.P1-o.PC);
        Draw_one( l );
        l.P2:= o.PE + more * (o.PE-o.PC);
        Draw_one( l );
        Select_Object( w.Drawing_Area, phantom_pen( Solid ) );
        Ortholine(o.P1, o.P1-o.PC);
        Ortholine(o.PE, o.PE-o.PC);
        Dispose(l);
        l:= new Obj_type(circ);
        l.P1:= o.P1;
        l.rad:= rbd;
        Draw_one( l );
        l.P1:= o.PE;
        Draw_one( l );
        Dispose(l);
      when others=> null;
    end case;
    Dispose(o);
  end Invert_Phantom;

begin
  -- 17-Jun-2003 : moved from Invert_[rubber_]box and Invert_phantom
  -- to avoid GDI leak under Windows 95/98/ME. We simply create all
  -- possible pens, once for all.
  for s in Pen_Style_Type loop
    Create_Pen( phantom_pen(s), s, 1, Black );
  end loop;
end TC.GWin.Phantoms;
