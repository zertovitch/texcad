package body TC.Display is
  use TC.REF, TC.Graphics, Ada.Numerics;

  P0   : Point; --  Linke untere Ecke des angezeigten Bildausschnittes
  ulpt : Real;  --  size of unitlength, for qBezier density

  procedure Set_Origin( P: Point ) is
  begin
    P0:= P;
  end Set_Origin;

  procedure Set_ul( pt: Real ) is -- size of unitlength, for qBezier density
  begin
    ulpt:= pt;
  end Set_ul;

  ldots: constant String:= "...";
  tail: constant:= text_cutting / 3;

  function Abbr( s:String ) return String is

    function Cut( s:String ) return String is
    begin
      if s'Length <= text_cutting then
        return s;
      else
        return
          s(s'First .. s'First + text_cutting - 1 - ldots'Length - tail)
          & ldots &
          s(s'Last - tail + 1 .. s'Last);
      end if;
    end Cut;

    e: Integer:= 0;

  begin
    if s'Length=0 then
      return "(?)";
    end if;
    if s'Length > 2 and then
         ( (s(s'First)='{' and s(s'Last)='}') or
           (s(s'First)='$' and s(s'Last)='$') ) then
      return Abbr(s( s'First+1 .. s'Last-1 ));
    elsif s'Length > 1 and then
       s(s'First)='\' then
      for i in reverse s'Range loop
        if s(i)=' ' then e:= i; end if;
      end loop;
      if e = 0 then      -- test is a \command only
        return Cut(s);
      else               -- skip \command
        return Abbr(s(e+1 .. s'Last));
      end if;
    else
      return Cut(s);
    end if;
  end Abbr;

  -- 2007:
  function Abbr( s: Unbounded_String ) return String is
  begin
    return Abbr(To_String(s));
  end Abbr;

  function TransX( rx: Real ) return Integer is
  pragma Inline(TransX);
  begin
    return Integer(h_mag*(rx-P0.x)); --  + 0.5
  end TransX;

  function TransY( ry: Real ) return Integer is
  pragma Inline(TransY);
  begin
    return m_y - Integer(v_mag*(ry-P0.y) + 0.5);
  end TransY;

  procedure Trans( P: Point; sx,sy: out Integer) is
  pragma Inline(Trans);
  begin
    sx:= TransX( P.x );
    sy:= TransY( P.y );
  end Trans;

  procedure Draw_unknown_put(o: Obj_type) is
    sx,sy: Integer;
  begin
    Trans( o.P1, sx,sy );
    SetTextJustify(centertext,centertext);
    OutTextXY(sx,sy,Abbr(o.inhalt));
  end Draw_unknown_put;

  procedure Draw_text(o: Obj_type) is
    h_just: H_Justify;
    v_just: V_Justify;
    sx,sy: Integer;
  begin
    if Length(o.inhalt) > 0 then
      Trans( Position_of_text(o), sx,sy );
      Values( o.adjust(1..o.adjust_len), h_just, v_just );
      SetTextJustify(h_just,v_just);
      OutTextXY( sx,sy, Abbr(o.inhalt) );
    end if;
  end Draw_text;

  procedure PlotPoint( P: Point ) is
    x,y: Integer;
  begin
    Trans( P, x,y );
    PutPoint( x,y );
  end PlotPoint;

  angle_vect: constant Real:= pi - 0.25;
  cos_angle_vect: constant Real:= Cos(angle_vect);
  sin_angle_vect: constant Real:= Sin(angle_vect);

  -- Draws an arrow pointing to (a,b), in direction of (u,v)
  procedure Arrow( a,b, u,v: Integer) is
    -- GM
    un,vn,luv, iv_luv,absc,ordo: Real;
    p: constant Real:= h_mag * arrow_length_pt / ulpt;
    -- ^ Corrected 11-Jan-2004 (was an absolute constant)
    c,d, e,f: Integer;
    fs: constant:= 8;
    f_shrink: constant:= 1.0/16.0;
  begin
    luv:= Sqrt(Real(u*u+v*v));
    if Almost_zero(luv * f_shrink) then
      Ellipse(a,b, 3,3, Fill=> False);
    else
      iv_luv:= 1.0 / luv;
      un:= iv_luv * Real(u);
      vn:= iv_luv * Real(v);
      absc:= cos_angle_vect*p;
      ordo:= sin_angle_vect*p;
      c:= Integer(Real(a) + un*absc + vn*ordo);
      d:= Integer(Real(b) - un*ordo + vn*absc);
      e:= Integer(Real(a) + un*absc - vn*ordo);
      f:= Integer(Real(b) + un*ordo + vn*absc);
      Line( c,d, e,f );   -- corde
      for i in 0..fs loop -- faisceau
        Line( (c*i+e*(fs-i))/fs,(d*i+f*(fs-i))/fs, a,b );
      end loop;
    end if;
  end Arrow;

  procedure Draw_line(P1,P2: Point; ls: Line_settings) is
  -- Internal - 20-Feb-2004
    x1,x2,y1,y2, ns, xc,yc: Integer;
    tr_x1x2: Boolean:= False;
    D, Pa, Pb: Point;
  begin
    case ls.pattern is
      when plain =>
        Trans(P1, x1,y1);
        Trans(P2, x2,y2);
        tr_x1x2:= True;
        Line(x1,y1,x2,y2);
      when dot =>
        D:= P2 - P1;
        ns:= TC.epic_calc.Num_segments(D, ls.dot_gap);
        D:= 1.0 / Real(ns) * D;
        for i in 0..ns loop
          PlotPoint( P1 + Real(i) * D );
        end loop;
      when dash =>
        D:= P2 - P1;
        ns:= TC.epic_calc.Num_segments(D, ls.dash_length);
        D:= 1.0 / Real(ns) * D;
        Pa:= P1;
        for i in 1..ns loop
          Pb:= P1 + Real(i) * D;
          if i mod 2 =1 then
            Trans(Pa, x1,y1);
            Trans(Pb, x2,y2);
            Line(x1,y1,x2,y2);
          end if;
          Pa:= Pb;
        end loop;
    end case;
    if not (ls.arrows = no_arrow) or tr_x1x2 then
      Trans(P1, x1,y1);
      Trans(P2, x2,y2);
    end if;
    case ls.arrows is
      when no_arrow =>
        null;
      when head =>
        Arrow(x2,y2, x2-x1, y2-y1);
      when both =>
        Arrow(x2,y2, x2-x1, y2-y1);
        Arrow(x1,y1, x1-x2, y1-y2);
      when middle =>
        Trans(0.5 * (P1+P2), xc,yc);
        Arrow(xc,yc, x2-x1, y2-y1);
    end case;
  end Draw_line;

  procedure Draw_box(o: Obj_type) is  --  GH
    sx1,sx2,sy1,sy2: Integer;
    P2: Point;
  begin
     if o.solid then
       Trans(o.P1,sx1,sy1);
       Trans(o.P1 + o.size, sx2,sy2);
       Full_rectangle(sx1,sy1,sx2,sy2);
     else
       P2:= o.P1 + o.size;
       Draw_line(o.P1, (P2.x,o.P1.y),  o.ls);
       Draw_line((o.P1.x, P2.y), P2,   o.ls);
       Draw_line((o.P1.x, P2.y), o.P1, o.ls);
       Draw_line(P2, (P2.x,o.P1.y),    o.ls);
       -- Draw_line(P2, o.P1, o.ls); -- Diagonal for test
       Draw_text(o);
     end if;
  end Draw_box;

  procedure Draw_line(o: Obj_type) is --  GH
  begin
    Draw_line(o.P1, o.P2, o.ls);
  end Draw_line;

  procedure Draw_circ(o: Obj_type) is  --  GH
    sx,sy: Integer;
  begin
    Trans(o.P1, sx,sy);
    Ellipse(sx,sy,Integer(h_mag*o.rad+0.5),Integer(v_mag*o.rad+0.5), Fill=> o.art=disc);
  end Draw_circ;

  procedure Draw_oval(o: Obj_type) is  --  GH
    x1,x2,x3,x4,y1,y2,y3,y4,xp,yp, arc_rad: Integer;
    arc_rad_f: Real;
    arc_arc: Point;
    arc_rad_max: constant Real:= 20.0 / ulpt; -- 20pt is the max radius
    -- ^ Corrected 13-Jan-2004 (was an absolute constant, 7.0)
  begin
    arc_rad_f:= Real'Min(Real'Min(o.osize.x,o.osize.y) * 0.5, arc_rad_max);
    arc_arc:= (arc_rad_f,arc_rad_f);
    Trans( o.LL,                     x1,y1 );
    Trans( o.LL + arc_arc,           x2,y2 );
    Trans( o.LL + o.osize - arc_arc, x3,y3 );
    Trans( o.LL + o.osize,           x4,y4 );
    arc_rad:= Integer(h_mag * arc_rad_f);
    Trans( o.LL + 0.5 * o.osize, xp,yp );
    case o.part is
      when entire =>
        line(x2,y1,x3,y1);
        line(x4,y2,x4,y3);
        line(x3,y4,x2,y4);
        line(x1,y3,x1,y2);
        arc(x2,y2,180,270,arc_rad);
        arc(x3,y2,270,360,arc_rad);
        arc(x3,y3,0,90,arc_rad);
        arc(x2,y3,90,180,arc_rad);
      when L =>
        line(x2,y1,xp,y1); line(xp,y4,x2,y4); line(x1,y3,x1,y2);
        arc(x2,y2,180,270,arc_rad); arc(x2,y3,90,180,arc_rad);

      when R =>
        line(xp,y1,x3,y1); line(x4,y2,x4,y3); line(x3,y4,xp,y4);
        arc(x3,y2,270,360,arc_rad); arc(x3,y3,0,90,arc_rad);

      when T =>
        line(x4,yp,x4,y3); line(x3,y4,x2,y4); line(x1,y3,x1,yp);
        arc(x3,y3,0,90,arc_rad); arc(x2,y3,90,180,arc_rad);

      when B =>
        line(x2,y1,x3,y1); line(x4,y2,x4,yp); line(x1,yp,x1,y2);
        arc(x2,y2,180,270,arc_rad); arc(x3,y2,270,360,arc_rad);

      when LT =>
        line(xp,y4,x2,y4); line(x1,y3,x1,yp);
        arc(x2,y3,90,180,arc_rad);

      when LB =>
        line(x2,y1,xp,y1); line(x1,yp,x1,y2);
        arc(x2,y2,180,270,arc_rad);

      when RT =>
        line(x4,yp,x4,y3); line(x3,y4,xp,y4);
        arc(x3,y3,0,90,arc_rad);

      when RB =>
        line(xp,y1,x3,y1); line(x4,y2,x4,yp);
        arc(x3,y2,270,360,arc_rad);

    end case;
  end Draw_oval;

  procedure Draw_grid is
    x,y,cx,cy: Integer;
    small: constant:= 3.9;
    stepx: constant:= 1.0;
    stepy: constant:= 1.0;
  begin
    if gen_opt.grid /= none and then
       h_mag > small and then v_mag > small then
      cx:= Integer(Real(m_x)/h_mag);
      cy:= Integer(Real(m_y)/v_mag);
      SetColor( shadow );
      case gen_opt.grid is
        when none => null; -- Normal, isn't it ?

        -- points / lines: We need to Trans* to cope with rounding
        when points =>
          for i in reverse 0..cx loop
            x:= TransX( P0.x + stepx * Real(i) );
            for j in reverse 0..cy loop
              PutPoint( x, TransY( P0.y + stepy * Real(j) ) );
            end loop;
          end loop;
        when lines =>
          for i in reverse 0..cx loop
            x:= TransX( P0.x + stepx * Real(i) );
            Line( x, 0, x, m_y );
          end loop;
          for j in reverse 0..cy loop
            y:= TransY( P0.y + stepy * Real(j) );
            Line( 0, y, m_x, y );
          end loop;
      end case;
    end if;
  end Draw_grid;

  procedure Draw_Bezier is new Bezier_curve(PlotPoint);

  procedure Bezier_and_arrows(o: Obj_type) is
    x,y: Integer;
  begin
    Draw_bezier(o, ulpt);
    case o.ls.arrows is
      when no_arrow =>
        null;
      when head =>
        Trans(o.PE, x,y);
        Arrow(x,y,  o.bez_slope(1)(h), -o.bez_slope(1)(v));
      when both =>
        Trans(o.PE, x,y);
        Arrow(x,y,  o.bez_slope(1)(h), -o.bez_slope(1)(v));
        Trans(o.P1, x,y);
        Arrow(x,y,  o.bez_slope(2)(h), -o.bez_slope(2)(v));
      when middle =>
        Trans(o.Pmiddle, x,y);
        Arrow(x,y,  o.bez_slope(1)(h), -o.bez_slope(1)(v));
    end case;
  end Bezier_and_arrows;


  procedure Shadow (o: Obj_type) is
    x,y,x1,y1: Integer;
    procedure Cross( P: Point; size: Natural ) is
    begin
      Trans(P, x,y);
      Line(x,y-size,x,y+size);
      Line(x-size,y,x+size,y);
    end Cross;
    ovc: constant:= 5;
    pix: Integer;
  begin
    SetColor( shadow );
    case o.art is
      when txt | putaux | box =>
        -- We show the anchor to text
        if Length(o.inhalt) > 0 then
          Cross( Position_of_text(o),12 );
        end if;
      when line => null;
      when circ | disc => -- (also disc: + 14-Oct-2005)
        pix:= abs(TransX(o.rad)-TransX(0.0));
        -- We show the centre
        Cross(o.P1,Integer'Min(6,pix));
        -- We show the corners (+ 14-Oct-2005)
        if pix > ovc then
          Trans(o.P1 + (-o.rad,-o.rad), x,y);
          Line(x,y,x+ovc,y); Line(x,y,x,y-ovc);
          Trans(o.P1 + (+o.rad,-o.rad), x,y);
          Line(x,y,x-ovc,y); Line(x,y,x,y-ovc);
          Trans(o.P1 + (-o.rad,+o.rad), x,y);
          Line(x,y,x+ovc,y); Line(x,y,x,y+ovc);
          Trans(o.P1 + (+o.rad,+o.rad), x,y);
          Line(x,y,x-ovc,y); Line(x,y,x,y+ovc);
        end if;
      when oval =>
        -- We show the corners
        pix:= abs(TransX(o.osize.x)-TransX(0.0));
        if pix > ovc then -- (test: + 14-Oct-2005)
          Trans(o.LL, x,y);
          Line(x,y,x+ovc,y); Line(x,y,x,y-ovc);
          Trans(o.LL + (o.osize.x,0.0), x,y);
          Line(x,y,x-ovc,y); Line(x,y,x,y-ovc);
          Trans(o.LL + (0.0,o.osize.y), x,y);
          Line(x,y,x+ovc,y); Line(x,y,x,y+ovc);
          Trans(o.LL + o.osize, x,y);
          Line(x,y,x-ovc,y); Line(x,y,x,y+ovc);
        end if;
      when bezier =>
        -- We show the lines between the control point and the ends
        Trans(o.PC, x,y);
        Trans(o.P1, x1,y1); Line(x,y,x1,y1);
        Trans(o.PE, x1,y1); Line(x,y,x1,y1);
      when others=> null;
    end case;
  end Shadow;

  pick_to_zone: constant array( Boolean ) of Color_zone:=
    (False=> normal, True=> picked);

  procedure Draw (o: Obj_type) is
  begin
    SetColor( pick_to_zone( o.picked ) );
    case o.art is
      when txt         => Draw_text(o);
      when box         => Draw_box(o);
      when line        => Draw_line(o);
      when circ | disc => Draw_circ(o);
      when oval        => Draw_oval(o);
      when bezier      => Bezier_and_arrows(o);
      when putaux      => Draw_unknown_put(o);
      when others=> null;
    end case;
  end Draw;

  procedure Draw (p: in out Picture) is
    o: ptr_Obj_type;
  begin
    if p.refresh = no then return; end if;
    -- ^ NB: for performance sake, it is a good idea to treat that case
    -- as soon as possible (before calling Draw or instantiating TC.display)
    Set_origin( p.opt.P0 );
    Set_ul( p.ul_in_pt );
    if p.refresh = full then
      -- Rulers; -- !!
      ClearScreen;
      Draw_grid;
    end if;
    if p.refresh >= shadows_and_objects then
      -- ** Shadows:
      o:= p.root;
      while o/=null loop
        Shadow(o.all);
        o:= o.next;
      end loop;
    end if; -- p.refresh = full
    -- ** Figures:
    o:= p.root;
    while o/=null loop
      case p.refresh is
        when picked =>
          if o.picked then
            Draw(o.all);
            p.refresh:= every; -- for preserving overlapping
          end if;
        when unpicked =>
          if not o.picked then
            Draw(o.all);
            p.refresh:= every; -- for preserving overlapping
          end if;
        when only_last =>      -- New object
          if o.next = null then
            Shadow(o.all);     -- this should be overlapped...
            Draw(o.all);
          end if;
        when others => -- "no" included, but never comes here
          Draw(o.all);
      end case;
      o:= o.next;
    end loop;
    p.refresh:= no;
  end Draw;

end TC.Display;
