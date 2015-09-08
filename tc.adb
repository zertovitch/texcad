package body TC is

  use REF;

  function "+"(P1,P2: Point) return Point is
  begin
    return ( P1.x+P2.x, P1.y+P2.y );
  end "+";

  function "-"(P1,P2: Point) return Point is
  begin
    return ( P1.x-P2.x, P1.y-P2.y );
  end "-";

  function "-"(P1: Point) return Point is
  begin
    return ( -P1.x, -P1.y );
  end "-";

  function "*"(f: Real; P: Point) return Point is
  begin
    return ( f * P.x, f * P.y );
  end "*";

  function Norm2(P:Point) return Real is
  begin
    return P.x**2 + P.y**2;
  end Norm2;

  function Norm(P:Point) return Real is
  begin
    return Sqrt(Norm2(P));
  end Norm;

  function Ortho(P:Point) return Point is
  begin
    return (P.y,-P.x);
  end Ortho;

  function Pic_suffix return String is
  begin
    return To_String(gen_opt.tex_suff);
  end Pic_suffix;

  function Mac_suffix return String is
  begin
    return To_String(gen_opt.mac_suff);
  end Mac_suffix;

  package body Graphics is

    procedure Images( hj: H_justify; vj: V_justify; s: out String; sl: out Natural) is
      ch: constant array(H_justify) of Character:= "lcr";
      cv: constant array(V_justify) of Character:= "tcb";
    begin
      s(s'First)  := ch(hj);
      s(s'First+1):= cv(vj);
      sl:= s'First+1;
    end Images;

    procedure Values( s: String; hj: out H_justify; vj: out V_justify) is
    begin
      hj:= centertext;
      vj:= centertext;
      for i in 1..s'Length loop -- admits partial options
        case s(i) is
          when 'l'=> hj:= lefttext;
          when 'r'=> hj:= righttext;
          when 't'=> vj:= toptext;
          when 'b'=> vj:= bottomtext;
          when 'c'=> null;  -- default is centered
          when others=> null;
        end case;
      end loop;
    end Values;

    function Position_of_text( o: Obj_type ) return Point is
      h_just: H_justify;
      v_just: V_justify;
      sz, P: Point;
    begin
      case o.art is
        when txt | putaux =>
          return o.P1;
        when box =>
          sz:= o.size;
        when others =>
          return (0.0,0.0); -- indeed, an error...
      end case;
      Values( o.adjust(1..o.adjust_len), h_just, v_just );
      case h_just is
        when lefttext   => P.x:= o.P1.x;
        when centertext => P.x:= o.P1.x + 0.5 * sz.x;
        when righttext  => P.x:= o.P1.x + sz.x;
      end case;
      case v_just is
        when toptext    => P.y:= o.P1.y + sz.y;
        when centertext => P.y:= o.P1.y + 0.5 * sz.y;
        when bottomtext => P.y:= o.P1.y;
      end case;
      return P;
    end Position_of_text;

  end Graphics;

  procedure Bezier_curve( o: Obj_type; pt_scale: Real ) is
    sc, scp, nt: Natural;
    isc, tr: Real;
    PA,PB: Point;
  begin
    sc:= o.num;
    if sc < 1 then -- \qbezier - autom
      sc:= Good_num_of_bezier_points(o, pt_scale);
    end if;
    isc:= 1.0 / Real(sc);
    scp:= sc+1;
    PB:= 2.0 * (o.PC-o.P1);
    PA:= isc * ((o.PE-o.P1)-PB);
    nt:= 0;
    while nt < scp loop
      tr:= Real(nt);
      Action( isc * tr * (tr * PA + PB) + o.P1 );
      nt:= nt + 2;
    end loop;
  end Bezier_curve;

  function Evaluate_variable (name : String; t: Real) return Real is
  begin
    if name'Length = 1 and then (name(name'First) = 't' or name(name'First) = 'T') then
      return t;
    end if;
    return 0.0;
  end Evaluate_variable;

  function Evaluate_param_curve_2D( o: Obj_type; t: Real ) return Point is
    x: constant Real:= TC_Formulas.Evaluate(o.parsed_2d_x, t);
    y: constant Real:= TC_Formulas.Evaluate(o.parsed_2d_y, t);
  begin
    -- RIO.Put(x); RIO.Put(16.0 * sin(t)**3); Ada.Text_IO.New_Line;
    -- RIO.Put(y); RIO.Put(13.0 * cos(t) - 5.0 * cos(2.0 * t) - 2.0 * cos(3.0 * t) - cos(4.0 * t)); Ada.Text_IO.New_Line;
    return o.P1 + o.data_2d.scale * (x, y);
  end Evaluate_param_curve_2D;

  procedure Parametric_curve_2D( o: Obj_type; pt_scale: Real ) is
    sc, nt: Natural;
    isc, t: Real;
    len: constant Real:= o.data_2d.max_t - o.data_2d.min_t;
    P1, P2, P3, P: Point;
    density: Real;
    function Convert_0_1_to_min_t_max_t(z: Real) return Real is
    begin
      return z * len + o.data_2d.min_t;  --  t in [o.min_t, o.max_t]
    end;
    sing: Boolean;
  begin
    sc:= o.data_2d.segments;
    if sc = 0 then  --  Automatically compute number of segments
      density:= 16.0 * Real'Max(1.0, pt_scale);
      --  We pick 3 points, and hope there is no singularity there
      P1:= Evaluate_param_curve_2D(o, Convert_0_1_to_min_t_max_t(0.01));
      P2:= Evaluate_param_curve_2D(o, Convert_0_1_to_min_t_max_t(0.49));
      P3:= Evaluate_param_curve_2D(o, Convert_0_1_to_min_t_max_t(0.99));
      sc:= 1 + Integer(density * ( Norm(P1 - P2) + Norm(P2 - P3) ));
    end if;
    isc:= 1.0 / Real(sc);
    nt:= 0;
    while nt <= sc loop
      t:= (Real(nt) * isc) * len + o.data_2d.min_t;  --  t in [o.min_t, o.max_t]
      begin
        P:= Evaluate_param_curve_2D(o, t);
        sing:= False;
      exception
        when others =>
          sing:= True;
      end;
      if sing then
        Singularity;
      else
        Action(P);
      end if;
      nt:= nt + 1;
    end loop;
  end Parametric_curve_2D;

  -- For Get_Slope

  rad2deg: constant:= 57.295779513082320877;

  type angle_table is record
    x,y  : Slope_value;
    angle: Real;
  end record;

  line_angles: constant array (1 .. 25) of  angle_table:=(
       (x=>0, y=>1, angle=>90.0),
       (x=>1, y=>0, angle=>0.0),
       (x=>1, y=>1, angle=>45.0),
       (x=>1, y=>2, angle=>63.434948822922010648),
       (x=>1, y=>3, angle=>71.565051177077989351),
       (x=>1, y=>4, angle=>75.963756532073521417),
       (x=>1, y=>5, angle=>78.690067525979786913),
       (x=>1, y=>6, angle=>80.537677791974382609),
       (x=>2, y=>1, angle=>26.565051177077989351),
       (x=>2, y=>3, angle=>56.309932474020213086),
       (x=>2, y=>5, angle=>68.198590513648188229),
       (x=>3, y=>1, angle=>18.434948822922010648),
       (x=>3, y=>2, angle=>33.690067525979786913),
       (x=>3, y=>4, angle=>53.130102354155978703),
       (x=>3, y=>5, angle=>59.036243467926478582),
       (x=>4, y=>1, angle=>14.036243467926478588),
       (x=>4, y=>3, angle=>36.869897645844021297),
       (x=>4, y=>5, angle=>51.340191745909909396),
       (x=>5, y=>1, angle=>11.309932474020213086),
       (x=>5, y=>2, angle=>21.801409486351811770),
       (x=>5, y=>3, angle=>30.963756532073521417),
       (x=>5, y=>4, angle=>38.659808254090090604),
       (x=>5, y=>6, angle=>50.194428907734805993),
       (x=>6, y=>1, angle=>9.4623222080256173906),
       (x=>6, y=>5, angle=>39.805571092265194006));

  arrow_angles: constant array (1 .. 13) of  angle_table:=(
       (x=>0, y=>1, angle=>90.0),
       (x=>1, y=>0, angle=>0.0),
       (x=>1, y=>1, angle=>45.0),
       (x=>1, y=>2, angle=>63.434948822922010648),
       (x=>1, y=>3, angle=>71.565051177077989351),
       (x=>1, y=>4, angle=>75.963756532073521417),
       (x=>2, y=>1, angle=>26.565051177077989351),
       (x=>2, y=>3, angle=>56.309932474020213086),
       (x=>3, y=>1, angle=>18.434948822922010648),
       (x=>3, y=>2, angle=>33.690067525979786913),
       (x=>3, y=>4, angle=>53.130102354155978703),
       (x=>4, y=>1, angle=>14.036243467926478588),
       (x=>4, y=>3, angle=>36.869897645844021297));

  procedure Get_slope(df: Point; sl: out LaTeX_slope; vector: Boolean) is
    --  JW,GH
    d,d1,angle: Real;
    s: Positive;
  begin
    if Almost_zero(df.x) then
      sl(h):= 0;
      if df.y<0.0 then
        sl(v):=-1;
      else
        sl(v):=1;
      end if;
    else
      s:= 1; -- Calm down Aonix OA 7.2.2 warning
      angle:= Arctan(abs df.y /abs df.x ) * rad2deg;
      d:= 180.0;
      if vector then   -- \vector
         for i in 1 .. 13 loop
            d1:= abs(angle-arrow_angles(i).angle);
            if  d1<d then
              s:=i;
              d:=d1;
            end if;
         end loop;
         if df.x<0.0 then
           sl(h):= -arrow_angles(s).x;
         else
           sl(h):=  arrow_angles(s).x;
         end if;
         if df.y<0.0 then
           sl(v):= -arrow_angles(s).y;
         else
           sl(v):=  arrow_angles(s).y;
         end if;
       else            -- \line
         for i in 1 .. 25 loop
            d1:=abs(angle-line_angles(i).angle);
            if d1<d then
              s:=i;
              d:=d1;
            end if;
         end loop;
         if df.x<0.0 then
           sl(h):= -line_angles(s).x;
         else
           sl(h):=  line_angles(s).x;
         end if;
         if df.y<0.0 then
           sl(v):= -line_angles(s).y;
         else
           sl(v):=  line_angles(s).y;
         end if;
      end if;
    end if;
  end Get_slope;

  procedure Set_slope_of_bezvec(o: in out Obj_type; pt_scale: Real) is

    procedure Find_good_slope(
      s                 : in out LaTeX_slope;
      inf,start,top,sup:        Real;
      emergency_vector  :        Point
    )
    is
      l2, tm, f, fp: Real;
      PA,PB,PV,Ptop: Point;
      it: Natural;
      itmax: constant:= 100;
      tol: constant:= 0.01; -- tolerance, in pt units
      part_of_arrow: constant:= 0.9;
      tol2: constant:= tol ** 2;
      ko: Boolean;
      sc2: Real;
    begin
      -- We try to find a point on the curve to coincide
      -- with a convenient on the axis of the arrow
      ko:= False;
      sc2:= pt_scale ** 2;
      PB:= 2.0 * (o.PC-o.P1);
      PA:= (o.PE-o.P1)-PB;
      --C:= o.P1 - o.PE;
      l2:= (arrow_length_pt * part_of_arrow / pt_scale)**2;
      Ptop:= top* (top * PA + PB) + o.P1;
      if o.ls.arrows = middle then
        o.Pmiddle:= Ptop;
      end if;
      -- Point of arrow's top. We are looking for some distance from it.
      tm:= start; -- t=1 -> f'(1) = 0 -> exit!
      it:= 0;
      loop
        PV:= (tm * (tm * PA + PB) + o.P1) - Ptop; -- V = vector from target point to point on curve
        f:= Norm2(PV) - l2;
        -- f(t) = ||P(t)-PE||^2 - l^2.
        --   We are looking for t s.t. f(t)=0. It's a quartic.
        --   Interesting for us is a root in [0.5;1] (if exist).
        exit when sc2 * abs f < tol2; -- root found
        -- Newton's algorithm: t_{n+1} = t_n - f(t_n) / f'(t_n)
        fp:= 2.0 * PV.x * (2.0 * tm * PA.x + PB.x) +
             2.0 * PV.y * (2.0 * tm * PA.y + PB.y);
        ko:= Almost_zero(fp); -- algo not usable
        exit when ko;
        tm:= tm - f / fp;
        it:= it + 1;
        ko:= it = itmax or tm < inf or tm > sup; -- too long or outside range
        exit when ko;
      end loop;
      if ko then
        -- Not convergent or solution outside -> set vector along tangent
        Get_slope( emergency_vector, s, vector => True );
      else
        Get_slope( -PV, s, vector => True );
      end if;
    end Find_good_slope;

  begin
    if o.art = bezier then
      case o.ls.arrows is
        when no_arrow => null;
        when head     => Find_good_slope(o.bez_slope(1), 0.4, 0.77, 1.0, 1.0, o.PE-o.PC);
        when both     => Find_good_slope(o.bez_slope(1), 0.4, 0.77, 1.0, 1.0, o.PE-o.PC);
                         Find_good_slope(o.bez_slope(2), 0.0, 0.23, 0.0, 0.6, o.P1-o.PC);
        when middle   => Find_good_slope(o.bez_slope(1), 0.0, 0.27, 0.5, 0.5, o.PE-o.P1);
      end case;
    end if;
  end Set_slope_of_bezvec;

  procedure Set_slope_of_linvec(o: in out Obj_type) is
    s: LaTeX_slope;
  begin
    if not o.any_slope then
      Get_slope( o.P2-o.P1, s, o.ls.arrows /= no_arrow );
      o.line_slope:= s;
      if  abs s(h) >= abs s(v) then
        if  s(h)=0 then
          o.P2.y:= o.P1.y;
        else
          o.P2.y:= o.P1.y + (o.P2.x-o.P1.x) * (Real(s(v))/Real(s(h)));
        end if;
      else
        o.P2.x:= o.P1.x + (o.P2.y-o.P1.y)*(Real(s(h))/Real(s(v)));
      end if;
    end if;
  end Set_slope_of_linvec;

  procedure Set_radius(o: in out Obj_type; df: Point)
  is
    -- art: constant array(Boolean) of Obj_art_type:= (False=>circ, True=>disc);
    -- max_rad: constant Real:= Max_radius(art(filled),ul_in_pt); -- was 7.0
  begin
    o.rad:= Sqrt(df.x**2+df.y**2);
    --  if filled and o.rad > max_rad then
    --    o.rad:= max_rad;
    --  end if;
    -- ^(no more limit!)
  end Set_radius;

  function Good_num_of_bezier_points(o: Obj_type; pt_scale: Real) return Positive is
    density: constant Real:= 8.0 * Real'Max(1.0,pt_scale);
  begin
    return 1 + Integer(density * ( Norm(o.P1-o.PC) + Norm(o.PC - o.PE) ));
  end Good_num_of_bezier_points;

  procedure Set_control_point(o: in out Obj_type; TG: Point) is
    M: Point;
  begin
    -- M = midpoint
    M:= 0.5 * (o.P1 + o.PE);
    -- Set control point such that TG is on the middle
    -- of the parametric curve: P(1/2)=TG
    o.PC:= M + 2.0 * (TG - M);
  end Set_control_point;

  procedure Delete_object_list(root: in out ptr_Obj_type) is -- JW
    mem: ptr_Obj_type;
  begin
    while root /= null loop
      mem:= root.next;
      Dispose(root);
      root:= mem;
    end loop;
  end Delete_object_list;

  -- Same algo as read_real
  function TeX_Number( s: String ) return Real is
    neg: Boolean;
    -- i: Integer; -- 10-May-2005: replaced by d (Real); overflow by kazr@waseda.jp
    rv,d: Real;
    n: Integer;
  begin
    rv:= 0.0;
    n:= s'First;
    -- 1/ Skip spaces
    while n <= s'Last and then s(n)=' ' loop
      n:= n+1;
    end loop;
    if n > s'Last then
      return 0.0;
    end if;
    -- 2/ Skip non-number
    if s(n)/='-' and s(n)/='.' and s(n) not in '0' ..'9' then
      return 0.0;
    end if;
    -- 3/ Treat minus
    if s(n)='-' then
      neg:= True;
      n:= n+1;
    else
      neg:= False;
    end if;
    if n > s'Last then
      return 0.0;
    end if;
    -- 4/ Treat integer part
    while n <= s'Last and then s(n) in '0'..'9' loop
      rv:= 10.0 * rv + Real(Character'Pos(s(n))-Character'Pos('0'));
      n:= n+1;
    end loop;
    -- 5/ Treat decimal part
    if n <= s'Last and then s(n)='.' then
      d:= 1.0;
      n:= n+1;
      while n <= s'Last and then s(n) in '0'..'9' loop
        d:= d * 0.1;
        rv:= rv + Real(Character'Pos(s(n))-Character'Pos('0')) * d;
        n:= n+1;
      end loop;
    end if;
    if neg then
      rv:= -rv;
    end if;
    return rv;
  end TeX_Number;

  function TeX_Number( x: Real; prec: Positive:= Real'Digits ) return String is
    s: String(1..30);
    na,nb,np:Natural;
  begin
    RIO.Put(s,x,prec,0);
    -- return Trim(s,left);
    na:= s'First;
    nb:= s'Last;
    np:= 0;
    for i in s'Range loop
      case s(i) is
        when '.' => np:= i; exit;  --   Find a decimal point
        when ' ' => na:= i+1;      -- * Trim spaces on left
        when others => null;
      end case;
    end loop;
    if np>0 then
      while nb > np and then s(nb)='0' loop
        nb:= nb-1;                 -- * Remove extra '0's
      end loop;
      if nb = np then
        nb:= nb-1;                 -- * Remove '.' if it is at the end
      elsif s(na..np-1)= "-0" then
        na:= na+1;
        s(na):= '-';               -- * Reduce "-0.x" to "-.x"
      elsif s(na..np-1)= "0" then
        na:= na+1;                 -- * Reduce "0.x" to ".x"
      end if;
    end if;
    return s(na..nb);
  end Tex_Number;

  function Image( o:Ovop ) return String is
  begin
    if o = entire then
      return "";
    else
      return Ovop'Image(o);
    end if;
  end Image;

  package body Units is

    unit_in_pt: constant array( Unit ) of Real:=
      ( pt => 1.0,
        pc => 12.0,
        im => 72.27,
        bp => 72.27/72.0,
        cm => 72.27/2.54,
        mm => 72.27/25.4,
        dd => 0.376*72.27/25.4,
        cc => 12.0 * 0.376*72.27/25.4,
        sp => 1.0/65536.0, -- 5-Jun-2003: was 65536.0 !
        unknown => 1.0
      );

    function Convert( x: Real; from,to: Unit ) return Real is
    begin
      return x * unit_in_pt(from) / unit_in_pt(to);
    end Convert;

    function Unit_Value( s: String ) return Unit is
      su: String:= s;
      u: Unit;
    begin
      if su="in" then
        su:= "im";
      end if;
      begin
        u:= Unit'Value(su);
      exception
        when others =>
          u:= unknown;
      end;
      return u;
    end Unit_Value;

    function Convert( s: String; to: Unit ) return Real is
      n: Natural:= 0;
    begin
      for i in reverse s'Range loop
        if s(i) in 'a'..'z' then
          n:= i;
        end if;
      end loop;
      if n=0 then
        return 0.0; -- tolerant
      else
        return Convert( TeX_Number( s(s'First..n-1) ),
                        Unit_Value( s(n..s'Last) ),
                        to);
      end if;
    end Convert;

  end Units;

  -- Ada 95 Quality and Style Guide, 7.2.7:
  -- Tests for
  --
  -- (1) absolute "equality" to 0 in storage,
  -- (2) absolute "equality" to 0 in computation,
  -- (3) relative "equality" to 0 in storage, and
  -- (4) relative "equality" to 0 in computation:
  --
  --  abs X <= Float_Type'Model_Small                      -- (1)
  --  abs X <= Float_Type'Base'Model_Small                 -- (2)
  --  abs X <= abs X * Float_Type'Model_Epsilon            -- (3)
  --  abs X <= abs X * Float_Type'Base'Model_Epsilon       -- (4)

  function Almost_zero(X: Real) return Boolean is
  begin
    return  abs X <= Real'Base'Model_Small;
  end Almost_zero;

  procedure Improve_linvec(o: in out Obj_type; ul_pt: Real) is
    dx,dy: Real;
    threshold: constant:= 14.142136;
    -- ^ Above Sqrt(2) * 10pt, LaTeX dares to display
    LaTeX_displays: Boolean;
  begin
    if o.art = line and then
       o.ls.pattern = plain and then
       o.ls.arrows <= head
    then
      dx:= abs(o.P2.x-o.P1.x);
      dy:= abs(o.P2.y-o.P1.y);
      if Almost_zero(dx) or Almost_zero(dy) then
         -- Horizontal or vertical are always OK
        LaTeX_displays:= True;
      else
        LaTeX_displays:= Norm(o.P2 - o.P1) * ul_pt > threshold;
      end if;
      if o.any_slope then                     -- 1/ Current is an "anyline"
        if LaTeX_displays then -- A too short \line will not be displayed!
          declare
            olatex: Obj_type:= o;
            dpt: Real;
          begin
            olatex.any_slope:= False;
            Set_slope_of_linvec(olatex);
            dpt:= Norm(olatex.P2 - o.P2) * ul_pt;
            -- Distance between end points
            if Almost_zero(dpt) then
              -- not too different -> change to \line or \vector
              o:= olatex;
            end if;
          end;
        end if;
      else                                    -- 2/ Current is \line, \vector
        if o.ls.arrows = no_arrow and not LaTeX_displays then
           -- A too short \line is not displayed!
          o.any_slope:= True;      -- -> to "anyline"
        end if;
      end if;
    end if;
  end Improve_linvec;

  procedure Insert( p: in out Picture; t: ptr_Obj_type; where: Insert_location ) is
    a: ptr_Obj_type;
  begin
    case where is
      when at_begin =>
        t.next:= p.root;
        p.root:= t;
      when at_end =>
        t.next:= null;
        if p.root = null then
          p.root:= t;
        else
          -- invariant: a /= null
          a:= p.root;
          loop
            if a.next = null then
              a.next:= t;
              exit;
            end if;
            a:= a.next;
          end loop;
        end if;

        p.saved:= False;
        p.total:= p.total + 1;
        if hidden( t.art ) then
          p.total_hidden:= p.total_hidden + 1;
        end if;
    end case;
  end Insert;

  procedure Refresh_size_dependent_parameters(p: in out Picture; objects: Boolean) is
    ul: constant String:= To_String(p.opt.unitlength);
    lw: constant String:= To_String(p.opt.linewidth);
    o: ptr_Obj_type:= p.root;
  begin
    p.ul_in_pt:= Units.Convert(ul,Units.pt);
    p.lw_in_pt:= Units.Convert(lw,Units.pt);
    if objects then -- Correct now size-dependent features of objects:
      while o /= null loop
        Set_slope_of_bezvec(o.all, p.ul_in_pt);
        Improve_linvec(o.all, p.ul_in_pt);
        o:= o.next;
      end loop;
    end if;
  end Refresh_size_dependent_parameters;

  -- 10-Jun-2003: maximal radius for LaTeX
  max_radius_pt: constant array(circ..disc) of Real:= (20.0,7.5);
  function Max_radius(a: Obj_art_type; ul_in_pt: Real) return Real is
  begin
    if Almost_zero(ul_in_pt) then
      return max_radius_pt(a);
    else
      return max_radius_pt(a) / ul_in_pt;
    end if;
  end Max_radius;

  package body epic_calc is

    function Num_segments (D_lta: Point; dotgap: Real) return Positive is
      done: Boolean;
      nx, ny, ns, tmp: Integer;
      gap: Real;
    begin
      if Almost_zero(dotgap) then
        gap:= 1.0;
      else
        gap:= dotgap;
      end if;

      -- \sqrtandstuff
      nx:= Integer(abs D_lta.x / gap + 0.5) ;
      ny:= Integer(abs D_lta.y / gap + 0.5);

      done:= False;
      if nx < 2 then
        if ny < 2 then
          ns:= nx + ny;
          done:= True;
        else
          ns:= ny;
          done:= True;
        end if;
      else
        if ny < 2 then
          ns:= nx;
          done:= True;
        end if;
      end if;
      if done then
        if ns = 0 then
          ns:= 1;
        end if;
      else
        if ny > nx then
          tmp:= nx; nx:= ny; ny:= tmp;
        end if; -- exchange nx & ny, so now nx > ny
        ns:= nx + (750 + (457*ny**2) / nx) / 1000;
        -- This is the approximation of sqrt(nx**2 + ny**2),
        -- rounded (+0.75), as in epic.sty. See sqrt.tex for the maths.
        -- You may wonder about using that instead of the sqrt function...
        -- Reasons:
        --   1/ display & emulation give same output as epic
        --   2/ it's charming!
      end if;
      return ns;
    end Num_segments;

  end epic_calc;

  function Sty_title(s: Supposing_sty) return String is
  begin
    case s is
      when bezier  => return "bezier (\in LaTeX > 2.09)";
      when emlines => return "emlines (EmTeX, DOS)";
      when epic    => return "epic";
    end case;
  end Sty_title;

end TC;
