with Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with Ada.Numerics;

with TC.IO_Commands;                    use TC.IO_Commands;

package body TC.Output is

  function Max(a,b,c:Real) return Real is --  GH
  begin
    if  a>b then
      if  a>c then
        return a;
      else
        return c;
      end if;
    else
      if b>c then
        return b;
      else
        return c;
      end if;
    end if;
  end Max;

  function Min(a,b:Real) return Real renames Real'Min;

  function Vorz(a:Real) return Real is --  JW
  begin
    if  a < 0.0 then
      return -1.0;
    else
      return 1.0;
    end if;
  end Vorz;

  function On_off( s: Boolean ) return String is
  begin
    if s then
      return "{\on}";
    else
      return "{\off}";
    end if;
  end On_off;

  default_precision: constant:= 3;
  current_precision: Positive;

  function R( x: Real; prec: Positive ) return String
    renames TeX_Number;

  function R( x: Real ) return String is
  begin
    return R(x,current_precision);
  end R;

  function I( x: Integer ) return String is
  begin
    return Trim(Integer'Image(x), Left);
  end I;

  function I( x: Integer; prec: Positive ) return String is
    pragma Warnings(off,prec); -- prec is fake here
  begin
    return I(x);
  end I;

  generic
    type Num is private;
    with function Im( x: Num ) return String;
    with function Im( x: Num; prec: Positive ) return String;
  package Put_Args is
    function Br( x: Num ) return String;                   -- {x}
    function Pt( x,y: Num ) return String;                 -- (x,y)
    function Pt( x,y: Num; prec: Positive ) return String; -- (x,y)
  end Put_Args;

  package body Put_Args is

    function Br( x: Num ) return String is
    begin
      return '{' & Im(x) & '}';
    end Br;

    function Pt( x,y: Num ) return String is
    begin
      return '(' & Im(x) & ',' & Im(y) & ')';
    end Pt;

    function Pt( x,y: Num; prec: Positive ) return String is
    begin
      return '(' & Im(x,prec) & ',' & Im(y,prec) & ')';
    end Pt;

  end Put_Args;

  package PA_Real is new Put_Args(Real,R,R); use PA_Real;
  package PA_Integer is new Put_Args(Integer,I,I); use PA_Integer;

  -- Some numbers are saved in Ada/Pascal format "0.01" instead of ".01"
  -- for compatibility TC 3.2
  function Ada_Pas_Real( x: Real; prec: Positive ) return String is
    s: String(1..30);
  begin
    RIO.Put(s,x,prec,0);
    return Trim(s,Left);
  end Ada_Pas_Real;

  function Ada_Pas_Real( x: Real ) return String is
  begin
    return Ada_Pas_Real(x,current_precision);
  end Ada_Pas_Real;

  package PA_AdaPas_Real is new Put_Args(Real,Ada_Pas_Real,Ada_Pas_Real);

  function Pt( P: Point) return String is
  begin
    return Pt( P.x, P.y );
  end Pt;

  function Pt( P: Point; prec: Positive) return String is
  begin
    return Pt( P.x, P.y, prec );
  end Pt;

  ------------------------------------------------------
  -- Buffering of output - 26-Jan-2007                --
  -- Only the pending end-of-line is buffered for now --
  ------------------------------------------------------

  package Buffered_Text_Output is
    -- Mimic Ada.Text_IO :
    procedure Put( f: Ada.Text_IO.File_Type; c: Character );
    procedure Put( f: Ada.Text_IO.File_Type; s: String );
    procedure Put_Line( f: Ada.Text_IO.File_Type; s: String );
    procedure New_Line( f: Ada.Text_IO.File_Type );
    -- What this package is for :
    procedure Forget_Last_New_Line( f: Ada.Text_IO.File_Type );
    procedure Flush( f: Ada.Text_IO.File_Type );
  end Buffered_Text_Output;

  package body Buffered_Text_Output is
    -- Not nice, but fine here: common buffering, whatever file
    pending_new_Line: Boolean:= False;
    procedure Flush( f: Ada.Text_IO.File_Type ) is
    begin
      if pending_new_Line then
        Ada.Text_IO.New_Line(f);
        pending_new_Line:= False;
      end if;
    end Flush;
    --
    procedure Put( f: Ada.Text_IO.File_Type; c: Character ) is
    begin
      Flush(f);
      Ada.Text_IO.Put(f,c);
    end Put;
    procedure Put( f: Ada.Text_IO.File_Type; s: String ) is
    begin
      Flush(f);
      Ada.Text_IO.Put(f,s);
    end Put;
    procedure Put_Line( f: Ada.Text_IO.File_Type; s: String ) is
    begin
      Flush(f);
      Ada.Text_IO.Put(f,s);
      pending_new_Line:= True;
    end Put_Line;
    procedure New_Line( f: Ada.Text_IO.File_Type ) is
    begin
      Flush(f);
      pending_new_Line:= True;
    end New_Line;
    --
    procedure Forget_Last_New_Line( f: Ada.Text_IO.File_Type ) is
    pragma Warnings( Off, f ); -- f not used : not-so-nice...
    begin
      pending_new_Line:= False;
    end Forget_Last_New_Line;
  end Buffered_Text_Output;

  procedure Insert( pic           : in Picture;
                    macro         :    Boolean;
                    file          :    Ada.Text_IO.File_Type;
                    displayed_name:    String
  )
  is
    --  JW,GH (Save)
    tf: Ada.Text_IO.File_Type renames file;
    o: ptr_Obj_type;
    s1,s2,Pmin,Pmax: Point;
    pointnum: Natural;

    use REF;

    current_LaTeX_ls: Line_settings:= normal_line_settings;
    -- ^ 2-Mar-2004. Tracks changes in settings (esp. thickness) in the
    -- order they will be parsed by LaTeX.

    -- 25-Jan-2007: track last command, in order to allow
    -- chaining of \drawline, \dashline, \dottedline
    type Command_tracker is record
       -- default: bogus values in case the tracker
       -- is used at first epic command
      k      : Kom_type        := czoom;
      P      : Point           := (0.0,0.0);
      stretch: Integer         := -101;
      symbol : Unbounded_String:= To_Unbounded_String("z314");
      gap    : Real            := -1.0;
      length : Real            := -1.0;
    end record;
    last_command: Command_tracker;
    function Img_track(k: Kom_type) return String is
    begin
      last_command.k:= k;
      return Img(k);
    end Img_track;

    use Buffered_Text_Output;

    procedure Pack_Line is
    begin
      if Ada.Text_IO."<"(Ada.Text_IO.Col(tf), 75) then
        Forget_Last_New_Line(tf);
      end if;
    end Pack_Line;

    procedure End_of_emulation is
    begin
      Put_Line(tf,"%\end");
    end End_of_emulation;

    procedure Write_line_any_slope(
      M       : in out Point;
      PP      :        Point;
      stretch :        Integer)
    is
    --  JW
      genauigkeit: constant:= 0.12;
      pixfac     : constant:= 0.5;
      DQ,D: Point;
      mini,vx,vy,f,xf:Real;
      numprec: Positive;
    begin
      if pic.opt.sty(epic) then
        if last_command.k = cdrawline
          and then Almost_Zero(Norm2(M - last_command.P))
          and then last_command.stretch = stretch
        then -- can chain, then just output end point
          Pack_Line;
          Put_Line(tf, Pt(PP));
          last_command.P:= PP;
        elsif last_command.k = cdrawline
          and then Almost_Zero(Norm2(PP - last_command.P))
          and then last_command.stretch = stretch
        then -- can chain this segment reversed
          Pack_Line;
          Put_Line(tf, Pt(M));
          last_command.P:= M;
        else
          Put(tf, Img_track(cdrawline));
          -- \drawline[stretch](x1,y1)(x2,y2)...(xn,yn)
          if stretch /= 0 then
            Put(tf,'[' & I(stretch) & ']');
          end if;
          Put_Line(tf, Pt(M) & Pt(PP));
          last_command.P:= PP;
          last_command.stretch:= stretch;
        end if;
      elsif pic.opt.sty(emlines) then --  and (mx <> xx) and (my <> yy)
        pointnum:= pointnum + 1;
        Put(tf, Img_track(cemline1) & Br(M.x) & Br(M.y) & Br(pointnum) );
        pointnum:= pointnum + 1;
        Put_Line(tf, Br(PP.x) & Br(PP.y) & Br(pointnum) );
      else
        DQ:= PP - M;
        vx:= Vorz(DQ.x);
        vy:= Vorz(DQ.y);
        mini:= Min(abs DQ.x, abs DQ.y);
        numprec:= current_precision + 1;
        if  mini > genauigkeit then
          f:= mini/genauigkeit;
          -- 3-Jun-2003: ensure good rendering for large-scale pictures
          if pic.ul_in_pt > 0.0 and then pic.lw_in_pt > 0.0 then
            xf:= pixfac * pic.ul_in_pt / pic.lw_in_pt;
            if xf > 1.0 then
              f:= f * xf;
            end if;
          end if;
          f:= Real'Floor(f + 0.99999999999);
          -- Pascal: f:=round(min/genauigkeit+0.5);
          D:= (1.0/f) * DQ;
          numprec:= numprec + Integer'Max(0,Integer(Log(f)));
          Put(tf,
            "\multiput" & Pt(M) & Pt(D,numprec) & Br(Integer(f)) &
            "{\line");
        else -- mini <= genauigkeit
          D:= PP - M;
          Put(tf, Img_track(cput) & Pt(M) & "{\line");
        end if;
        if abs D.x > abs D.y then
          Put_Line(tf, Pt(Integer(vx),0) & '{' & R(abs D.x, numprec) & "}}");
        else
          Put_Line(tf, Pt(0,Integer(vy)) & '{' & R(abs D.y, numprec) & "}}");
        end if;
      end if;
      M:= PP;
    end Write_line_any_slope;

    procedure Write_reduced_any_lines(
      M, Q : in out Point;
      PP: Point; no_start: Boolean)
    is
      --  JW, GM
      --  Fasst EMlinien mit fast gleicher Steigung zusammen
      s1,s2,df: Real;
    begin
      if no_start then
        if not pic.opt.reduce then
          Write_line_any_slope( M, Q, 0);
        elsif
          abs(PP.x-M.x) > 0.01  and  abs(PP.y-M.y) > 0.01 and
          -- Bug found thanks to GNAT's validity checks:
          abs(Q.x-M.x) > 0.01  and  abs(PP.x-Q.x) > 0.01
        then
          s1:=(Q.y-M.y)  / (Q.x-M.x);
          s2:=(PP.y-Q.y) / (PP.x-Q.x);
          df:= abs(s1-s2);
          if df > pic.opt.stdiff then
            Write_line_any_slope( M, Q, 0);
          end if;
        end if;
      end if;
      Q:= PP;
    end Write_reduced_any_lines;

    procedure Write_emulated_bezier(o: Obj_type) is
      --  JW , GM
      M, Q: Point;
      no_start: Boolean:= False;

      procedure PlotPoint( P: Point ) is
      begin
        Write_reduced_any_lines( M, Q, P, no_start );
        no_start:= True;
      end PlotPoint;

      procedure Draw_Bezier is new Bezier_curve(PlotPoint);

    begin
      M:= o.P1;
      Q:= (0.0,0.0);
      -- ^ Unused at start, just calms down
      -- validity check (-gnatVa) + pragma Initialize_Scalars
      Draw_Bezier(o,pic.ul_in_pt);
      Write_line_any_slope( M, o.PE, 0);
    end Write_emulated_bezier;

    procedure Write_emulated_paramcurve2d(o: Obj_type) is
      M, Q, PE: Point;
      no_start, restart: Boolean:= False;

      procedure PlotPoint( P: Point ) is
      begin
        if restart then
          M:= P;
          restart:= False;
        else
          Write_reduced_any_lines( M, Q, P, no_start );
          no_start:= True;
        end if;
      end PlotPoint;

      procedure Singularity is
      begin
        no_start:= False;
        restart:= True;
      end Singularity;

      procedure Draw_Paramcurve is new Parametric_curve_2D(PlotPoint, Singularity);

    begin
      M:= Evaluate_param_curve_2D(o, o.data_2d.min_t);
      Q:= (0.0,0.0);
      Draw_Paramcurve(o,pic.ul_in_pt);
      PE:= Evaluate_param_curve_2D(o, o.data_2d.max_t);
      Write_line_any_slope( M, PE, 0);
    end Write_emulated_paramcurve2d;

    procedure Write_kreis( o: Obj_type; Pmin: Point ) is

      use Ada.Numerics;

      procedure Write_circle_with_lines(C: Point; as_command: Boolean) is
        --  JW, GM
        pid: constant:= 2.0 * Pi;
        M, PP, Q: Point;
        t,dt : Real;
      begin
        if as_command then
          Put_Line(tf, Img_track(ccircle2) & Pt(C) & Br(2.0*o.rad));
        end if;
        dt:= pid / ((12.0 + 0.25 * o.rad) * pic.opt.quality);
        Q:= (0.0,0.0);
        -- ^ Unused at start, just calms down
        -- validity check (-gnatVa) + pragma Initialize_Scalars
        t:= 0.0;
        M:= C + (o.rad,0.0);
        while t < pid loop
          PP:= o.rad * ( Cos(t), Sin(t) ) + o.P1;
          Write_reduced_any_lines( M, Q, PP, not Almost_Zero(t) );
          t:= t+dt;
        end loop;
        Write_line_any_slope( M, o.P1 - Pmin + (o.rad,0.0), 0);
        if as_command then
          End_of_emulation;
        end if;
      end Write_circle_with_lines;

      -- 10-Jun-2003: Draw fractal for filled circles above max LaTeX size
      procedure Write_disc_with_boxes(C: Point) is
        -- GM
        eps: Real:= 0.16; -- limit to the algorithm
        pad: Real:= 0.16; -- padding around rectangles
        type Multi is (single, double, quadruple);

        -- Write a rectangle, relative to C
        procedure Rect( P1,P2:Point; rep: Multi:= single; P1b,P1c: Point:= C ) is
          prec: constant Integer:= current_precision + 1;
          with_frames: constant Boolean:= False; -- For seeing what's done
        begin
          if rep = single then
            Put(tf, Img_track(cput));
          else
            Put(tf, "\multiput");
          end if;
          Put(tf, Pt(C+P1-(pad,pad),prec));
          if rep /= single then
            Put(tf, Pt(P1b-P1,prec) & "{2}");
            if rep = quadruple then
              Put(tf,"{\multiput(0,0)" & Pt(P1c-P1,prec) & "{2}");
            end if;
          end if;
          if with_frames then
            Put(tf,
              "{\framebox" & Pt(P2-P1+2.0*(pad,pad),prec) & "[]{}}");
          else
            Put(tf,
              "{\rule{" &
              R(P2.x-P1.x+2.0*pad,prec) & "\unitlength}{" &
              R(P2.y-P1.y+2.0*pad,prec) & "\unitlength}}");
          end if;
          if rep = quadruple then
            Put(tf,'}');
          end if;
          New_Line(tf);
        end Rect;

        -- P1 is the lower left corner in the first quadrant
        -- We fractally "fill" the area limited by (1) the vertical
        -- and (2) the horizontal lines passing through P1,
        -- and (3) the circle.

        procedure Fill( P1: Point; a_beg,a_end: Real ) is
          a_mid: constant Real:= 0.5 * (a_beg+a_end);
          P2: constant Point:= o.rad * (Cos(a_mid), Sin(a_mid));
        begin
          if Real'Min(abs(P1.x-P2.x),abs(P1.y-P2.y)) >= pad then
            -- Draw rectangles by symmetry on the 4 quadrants,
            -- avoiding drawing twins:
            if Almost_Zero( Norm2(P1) ) then
              Rect( -P2, P2 );
            elsif Almost_Zero(P1.x) then
              Rect( (-P2.x, P1.y), (P2.x,P2.y), double, (-P2.x,-P2.y) );
            elsif Almost_Zero(P1.y) then
              Rect( (P1.x,-P2.y), (P2.x,P2.y), double, (-P2.x,-P2.y) );
            else
              Rect( P1,P2, quadruple, (-P2.x,P1.y), (P1.x,-P2.y));
            end if;
            -- Recursively fill the rest of the area:
            Fill( (P1.x,P2.y), a_mid, a_end ); -- above the rectangle
            Fill( (P2.x,P1.y), a_beg, a_mid ); -- right to the rectangle
          end if;
        end Fill;

      begin
        if pic.ul_in_pt > 0.0 then
          eps:= eps / pic.ul_in_pt;
          pad:= pad / pic.ul_in_pt;
        end if;
        Put_Line(tf,"%\circle*" & Pt(C) & Br(2.0*o.rad));
        Fill( (0.0,0.0), 0.0, Pi / 2.0 );
        Write_circle_with_lines(o.P1 - Pmin, as_command => False);
        -- For disc: just a fine outline, no command
        End_of_emulation;
      end Write_disc_with_boxes;

    begin
      if o.rad <= Max_radius(o.art,pic.ul_in_pt) then -- was: 7.0
        Put(tf, Img_track(cput) & Pt( o.P1 - Pmin ) & "{\circle");
        if o.art=disc then
          Put(tf,'*');
        end if;
        Put_Line(tf, Br(2.0*o.rad) & '}');
      elsif o.art = disc then
        Write_disc_with_boxes(o.P1 - Pmin);
      else
        Write_circle_with_lines(o.P1 - Pmin, as_command => True);
      end if;
    end Write_kreis;

    function Arrows_option_image( ls: Line_settings ) return String is
    begin
      case ls.arrows is
        when no_arrow => return "";         -- never happens
        when head     => return "";         -- "normal" case
        when both     => return "[both]";
        when middle   => return "[middle]";
      end case;
    end Arrows_option_image;

    -- 24-Apr-2003
    procedure Write_bezier_command(o: Obj_type; force_no_vec: Boolean:= False) is
      bez_choice: constant array(Boolean, Boolean) of Kom_type:=
        ( True=>   (True=> cqbezvec, False=> cqbezier1),
          False => (True=> cbezvec,  False=> cbezier1)
        );
      q  : constant Boolean:= o.num=0;
      vec: constant Boolean:= o.ls.arrows /= no_arrow and not force_no_vec;
    begin
      Put(tf, Img_track( bez_choice(q, vec) ));
      if not q then
        Put(tf, '{' & I(o.num) & '}');
        -- NB: different syntaxes: \bezier: {n}, \qbezier: [n]
      end if;
      if vec then
        Put(tf,Arrows_option_image(o.ls));
      end if;
      Put_Line(tf, Pt(o.P1 - Pmin) & Pt(o.PC - Pmin) & Pt(o.PE - Pmin) );
    end Write_bezier_command;

    procedure Write_arrow(P: Point; s: LaTeX_slope) is
      minipt: constant:= 0.2;
      al: Real; -- 4-Jun-2003: minimal length to obtain an arrow, was {0.2}
    begin
      if pic.ul_in_pt > 0.0 then
        al:= minipt / pic.ul_in_pt;
      else
        al:= minipt;
      end if;
      Put(tf, Img_track(cput) & Pt(P) & "{\vector" & Pt(s(h),s(v)) & Br(al) & '}');
    end Write_arrow;

    procedure Write_bezier(o: Obj_type) is -- 24-Feb-2004
    begin
      if o.ls.arrows = no_arrow then -- \bezier or %\bezier
        if pic.opt.sty(bezier) then
          Write_bezier_command(o);
        else
          Put(tf,'%');
          Write_bezier_command(o);
          Write_emulated_bezier(o);
          End_of_emulation;
        end if;
      else                           -- %\bezvec or %\qbezvec
        Write_bezier_command(o);
        case With_arrows(o.ls.arrows) is -- no_arrow treated above
          when head     => Write_arrow(o.PE - Pmin, o.bez_slope(1));
          when both     => Write_arrow(o.PE - Pmin, o.bez_slope(1));
                           Write_arrow(o.P1 - Pmin, o.bez_slope(2));
          when middle   => Write_arrow(o.Pmiddle - Pmin, o.bez_slope(1));
        end case;
        if pic.opt.sty(bezier) then
          Write_bezier_command(o, force_no_vec => True);
        else
          Write_emulated_bezier(o);
        end if;
        End_of_emulation;
      end if;
    end Write_bezier;

    procedure Write_paramcurve2d(o: Obj_type) is
      long: constant Boolean:= Length(o.data_2d.form_x) + Length(o.data_2d.form_y) > 40;
      procedure Spacing is
      begin
        if long then
          New_Line(tf);
          Put(tf, "%   ");
        end if;
      end Spacing;
    begin
      Put(tf, "%\paramcurvexy");
      if o.data_2d.segments > 0 then
        Put(tf, '[' & I(o.data_2d.segments) & ']');
      end if;
      Put(tf, Pt(o.P1 - Pmin) & '(' & R(o.data_2d.scale) & ")(");
      Spacing;
      Put(tf, To_String(o.data_2d.form_x) & ", ");
      Spacing;
      Put(tf, To_String(o.data_2d.form_y) & ", ");
      Spacing;
      Put_Line(tf, R(o.data_2d.min_t) & ", " & R(o.data_2d.max_t) & ')');
      Write_emulated_paramcurve2d(o);
      End_of_emulation;
    end Write_paramcurve2d;

    procedure Write_vector_arrows(P1,P2: Point; arrows: Line_arrows) is
      s: LaTeX_slope;
    begin
      Get_slope( P2-P1, s, True );
      case With_arrows(arrows) is
        when head   => Write_arrow(P2, s);
        when both   => Write_arrow(P2, s);
                       Write_arrow(P1, (-s(h),-s(v)));
        when middle => Write_arrow(0.5*(P1+P2), s);
      end case;
    end Write_vector_arrows;

    procedure Write_plain_line_any_slope(o: Obj_type) is
      s1,s2: Point;
      needs_emulation: constant Boolean:=
        not (pic.opt.sty(emlines) or pic.opt.sty(epic));
    begin
      s1:= o.P1 - Pmin;
      s2:= o.P2 - Pmin;
      case o.ls.arrows is
        when no_arrow =>
          if needs_emulation then
            -- line, but no emlines.sty or epic.sty
            Put_Line(tf, Img_track(cemline2) & Pt(s1) & Pt(s2));
          end if;
        when With_arrows =>
          Put_Line( tf,
             Img_track(cvector2) &                -- %\vector
             Arrows_option_image(o.ls) &    -- [m]
             Pt(s1) & Pt(s2) );             -- (0,0)(10,10)
          Write_vector_arrows(s1,s2,o.ls.arrows);
      end case;
      Write_line_any_slope( s1, s2, o.ls.stretch );
      -- ^ emline, epic or emulation
      case o.ls.arrows is
        when no_arrow =>
          if needs_emulation then
            End_of_emulation;
          end if;
        when With_arrows =>
          End_of_emulation;
      end case;
    end Write_plain_line_any_slope;

    procedure Write_dotted_line(
      E1, E2   : Point;
      gap      : Real;
      sym      : String;
      epic     : Boolean;
      put_cmd  : Boolean;
      thick    : Boolean;
      can_chain: Boolean)
    is
      E1p: Point;
      D_lta: constant Point:= E2-E1;
      np, prec: Positive;
      cgap, w: Real;
      procedure Put_options_and_parameters is
      begin
        -- Options: \dottedline[$\bullet$]{3}(0,0)(70,0)
        if sym /= "" then
          Put(tf,'[' & sym & ']');
        end if;
        if gap > 0.0 then
          Put(tf, Br(gap));
        end if;
        Put_Line(tf, Pt(E1) & Pt(E2));
      end Put_options_and_parameters;
    begin
      if epic then
        if put_cmd then
          if can_chain
            and then last_command.k = cdottedline1
            and then Almost_Zero(Norm2(E1 - last_command.P))
            and then Almost_Zero(last_command.gap - gap)
            and then last_command.symbol = sym
          then -- can chain, then just output end point
            Pack_Line;
            Put_Line(tf, Pt(E2));
            last_command.P:= E2;
          else
            Put(tf, Img_track(cdottedline1));
            Put_options_and_parameters;
            last_command.P:= E2;
            last_command.gap:= gap;
            last_command.symbol:= To_Unbounded_String(sym);
          end if;
        else
          -- No command, but options and parameters are to
          -- be put to complete another command
          Put_options_and_parameters;
        end if;
      else -- emulation of epic.sty
        np:= TC.epic_calc.Num_segments(D_lta, gap);
        prec:= current_precision + 1 + Integer'Max(0, Integer(Log(Real(np),10.0)));
        cgap:= 1.0 / Real(np); -- the corrected gap s.t. the last point is at end
        E1p:= E1 - pic.lw_in_pt / pic.ul_in_pt * (0.5,0.5);
        Put(tf,
          "\multiput" & Pt(E1p) & Pt(cgap * D_lta, prec) & Br(np+1) & '{');
        if sym = "" then
          w:= pic.lw_in_pt;
          if thick then
            w:= w * 2.0;
          end if;
          Put(tf,"{\rule{" & R(w, prec) & "pt}{" & R(w, prec) & "pt}}");
        else
          Put(tf,"\makebox(0,0)[cc]{" & sym & '}');
        end if;
        Put_Line(tf,"}");
      end if;
    end Write_dotted_line;

    -- 23-Feb-2004: %\dottedbox
    procedure Write_dotted_box(o: Obj_type) is
      C1,C2: Point; -- corners
      gap: constant Real:= o.ls.dot_gap;
      sym: constant String:= To_String(o.ls.dot_symbol);
      procedure Spit(E1, E2: Point) is
      begin
        Write_dotted_line(
          E1, E2, gap, sym,
          epic      => pic.opt.sty(epic),
          put_cmd   => True,
          thick     => o.ls.thickness=thick,
          can_chain => True
        );
      end Spit;
    begin
      C1:= o.P1 - Pmin;
      C2:= o.P1 + o.size - Pmin;
      Put(tf, Img_track(cdottedbox) & Pt(C1) & Pt(o.size));
      if sym /= "" then
        Put(tf,'[' & sym & ']');
      end if;
      if gap > 0.0 then
        Put(tf, Br(gap));
      end if;
      New_Line(tf);
      -- The emulation itself:
      -- 1/ Text (if any), with \makebox(w,h)[align]{Text} :
      if o.inhalt /= "" then
        Put(tf,
          Img_track(cput) & Pt( o.P1 - Pmin ) & '{' & Img_track(cmakebox) &
          Pt( o.size ) &
          '[' & o.adjust(1..o.adjust_len) & ']');
        if Length(o.inhalt) >= 50 then
          New_Line(tf);
        end if;
        Put_Line(tf,'{' & To_String(o.inhalt) & "}}");
      end if;
      -- 2/ Dotted frame (parallels in same direction):
      Spit( (C1.x,C2.y),  C1         ); -- ver
      Spit(  C1,         (C2.x,C1.y) ); -- hor
      Spit( (C1.x,C2.y),  C2         ); -- hor
      Spit(  C2,         (C2.x,C1.y) ); -- ver
      End_of_emulation;
    end Write_dotted_box;

    procedure Write_dash_line(
      E1, E2               : Point;
      stretch              : Integer;
      length, gap          : Real;
      epic                 : Boolean;
      put_cmd              : Boolean;
      can_chain            : Boolean)
    is
      E1p, Pa,Pb: Point;
      D: Point:= E2-E1;
      ns: Positive;
      -- prec: Positive;
      procedure Put_options_and_parameters is
      begin
        -- \dashline[stretch]{dash-length}
        -- [inter-dot-gap for dash](x1,y1)(x2,y2)...(xn,yn)
        if stretch /= 0 then
          Put(tf,'[' & I(stretch) & ']');
        end if;
        if not Almost_Zero(length) then
          Put(tf, Br(length));
        end if;
        if not Almost_Zero(gap) then
          Put(tf, Br(gap));
        end if;
        Put_Line(tf, Pt(E1) & Pt(E2));
      end Put_options_and_parameters;
    begin
      if epic then
        if put_cmd then
          if can_chain
            and then last_command.k = cdashline1
            and then Almost_Zero(Norm2(E1 - last_command.P))
            and then Almost_Zero(last_command.gap - gap)
            and then Almost_Zero(last_command.length - length)
          then -- can chain, then just output end point
            Pack_Line;
            Put_Line(tf, Pt(E2));
            last_command.P:= E2;
          else
            Put(tf, Img_track(cdashline1));
            Put_options_and_parameters;
            last_command.P:= E2;
            last_command.gap:= gap;
            last_command.length:= length;
          end if;
        else
          -- No command, but options and parameters are to
          -- be put to complete another command
          Put_options_and_parameters;
        end if;
      else -- emulation of epic.sty
        -- !! emulate gap, stretch !!
        ns:= TC.epic_calc.Num_segments(D, length);
        -- prec:= precision + 1 + Integer'Max(0, Integer(log(Real(ns),10.0)));
        E1p:= E1 - pic.lw_in_pt / pic.ul_in_pt * (0.5,0.5);
        D:= 1.0 / Real(ns) * D;
        Pa:= E1p;
        for i in 1..ns loop
          Pb:= E1p + Real(i) * D;
          if i mod 2 =1 then
            Write_line_any_slope( Pa, Pb, 0 );
            -- ^ emline or emulation
          end if;
          Pa:= Pb;
        end loop;
      end if;
    end Write_dash_line;

    -- 20-Jan-2004: epic's \dottedline, 24-Feb-2004: epic's \dashline

    procedure Write_dot_dash_line_command(o: Obj_type) is
      procedure Spit(epic, put_cmd, can_chain: Boolean) is
      begin
        case o.ls.pattern is
          when plain => null; -- not here
          when dot =>
            Write_dotted_line(
              o.P1 - Pmin,
              o.P2 - Pmin,
              o.ls.dot_gap,
              To_String(o.ls.dot_symbol),
              epic      => epic,
              put_cmd   => put_cmd,
              thick     => o.ls.thickness=thick,
              can_chain => can_chain
            );
          when dash =>
            Write_dash_line(
              o.P1 - Pmin,
              o.P2 - Pmin,
              o.ls.stretch,
              o.ls.dash_length,
              o.ls.dash_dot_gap,
              epic      => epic,
              put_cmd   => put_cmd,
              can_chain => can_chain
            );
        end case;
      end Spit;
      emul_epic: constant Boolean:= not pic.opt.sty(epic);
    begin
      case o.ls.arrows is
        when no_arrow =>
          if emul_epic then -- emulated, command as TeX comment
            Put(tf,'%');
          end if;
          -- Spit the epic command, even as comment:
          Spit(epic => True, put_cmd => True, can_chain => not emul_epic);
          if emul_epic then
            Spit(epic => False, put_cmd => False, can_chain => False);
            -- put_cmd, can_chain are bogus there.
            End_of_emulation;
          end if;
        when others   =>
          -- all other syntaxes are %\vector{dot}... %\end
          Put(tf, Img_track(cvector2) & Arrows_option_image(o.ls));
          case o.ls.pattern is
            when plain => null; -- not here
            when dot   => Put(tf, "{dot}");
            when dash  => Put(tf, "{dash}");
          end case;
          Spit(epic => True, put_cmd => False, can_chain => False);
          -- epic's options, points, no command
          Write_vector_arrows(o.P1 - Pmin, o.P2 - Pmin, o.ls.arrows);
          Spit(epic => pic.opt.sty(epic), put_cmd => True, can_chain => False);
          -- Spit the epic command for segment, as epic or emulated.
          -- We don't chain: the eol forgetting puts an eventual point
          -- at the end of the abov TeX comment!
          End_of_emulation;
      end case;
    end Write_dot_dash_line_command;

    procedure Write_LaTeX_put_figure(o: Obj_type) is
      o_len: Real;
      special_arrows_limited_slope: constant Boolean:=
        o.art = line and o.ls.arrows in both .. middle;
      use Ada.Characters.Handling;
    begin
      if special_arrows_limited_slope then
        -- %\vector command around!
        Put_Line(tf, Img_track(cvector2) & Arrows_option_image(o.ls) & "{\line}");
      end if;
      -- The \put command:
      Put(tf, Img_track(cput) & Pt( o.P1 - Pmin ) & '{');
      case  o.art  is
        when txt=>
          Put(tf,"\makebox(0,0)[" & o.adjust(1..o.adjust_len) & ']');
          if Length(o.inhalt) >=50 then
            New_Line(tf);
          end if;
          Put(tf,'{' & To_String(o.inhalt) & '}');
        when box=>
          if  o.solid then
            Put(tf,
              "\rule{" &
              R(o.size.x) & "\unitlength}{" &
              R(o.size.y) & "\unitlength}");
          else
            case o.ls.pattern is
              when plain => Put(tf, Img_track(cframebox));
              when dot   => null; -- not a \put{...}
              when dash  => Put(tf, Img_track(cdashbox) & Br(o.ls.dash_length) );
            end case;
            Put(tf,
              Pt( o.size ) &
              '[' & o.adjust(1..o.adjust_len) & ']'
            );
            if Length(o.inhalt) >=50 then
              New_Line(tf);
            end if;
            Put(tf,'{' & To_String(o.inhalt) & '}');
          end if;

        when line =>
          case o.ls.arrows is
            when head   => Put(tf, Img_track(cvector1));
            when others => Put(tf, Img_track(cline));
            -- \line has more slopes than \vector -> OK for middle & both
          end case;
          o_len:= abs(o.P2.x-o.P1.x);
          if o_len = 0.0 then
            o_len:= abs(o.P2.y-o.P1.y);
          end if;
          Put(tf, Pt(o.line_slope(h), o.line_slope(v)) & Br(o_len));
          case o.ls.arrows is
            when both | middle =>
              Put(tf,"}");
              -- TC "%\" command already added, then th \put command.
              -- Now emulation of arrows
              Write_vector_arrows(o.P1 - Pmin, o.P2 - Pmin, o.ls.arrows);
              New_Line(tf);
              End_of_emulation;
            when others   => null;
          end case;

        when oval=>
         Put(tf,
           Img_track(coval) & Pt( o.osize ) &
           '[' & To_Lower(Image(o.part)) & ']');
        when putaux=> Put(tf,To_String(o.inhalt));
        when others=> null;  -- [P2Ada]: no otherwise / else in Pascal
      end case;
      if not special_arrows_limited_slope then
        Put_Line(tf,"}");
      end if;
    end Write_LaTeX_put_figure;

    inf: constant Real:= Real(Integer'Last);
    Pinf: constant Point:= (inf,inf);

    function Short_name(s: String) return String is
      fs: Integer:= s'First;
    begin
      for i in s'Range loop
        case s(i) is
          when '\' | '/' | '|' | ':' => fs:= i+1;
          when others => null;
        end case;
      end loop;
      return s(fs..s'Last);
    end Short_name;

  begin -- Insert
    o:= pic.root;
    if o = null then -- empty picture
      Pmax:= (0.0, 0.0);
      Pmin:= (0.0, 0.0);
    else
      Pmax:= (-1.0)*Pinf;
      Pmin:=        Pinf;
      while o/=null loop
        if  (not macro)  or  o.picked then
          case  o.art  is
            when
             txt | putaux =>
                s1:=o.P1;
                s2:=o.P1;

             when box =>
                s1:= o.P1 + o.size;
                s2:= o.P1;

             when line =>
                if o.P2.x > o.P1.x then
                   s1.x:=o.P2.x; s2.x:=o.P1.x;
                 else
                   s1.x:=o.P1.x; s2.x:=o.P2.x;
                end if;
                if  o.P2.y > o.P1.y then
                   s1.y:=o.P2.y; s2.y:=o.P1.y;
                else
                   s1.y:=o.P1.y; s2.y:=o.P2.y;
                end if;

             when circ | disc =>
                s1:= o.P1 + (o.rad,o.rad);
                s2:= o.P1 - (o.rad,o.rad);

             when oval =>
                s1:= o.P1 + 0.5 * o.osize;
                s2:= o.P1 - 0.5 * o.osize;

             when bezier =>
                s1.x:=  Max( o.P1.x, o.PC.x, o.PE.x);
                s2.x:= -Max(-o.P1.x,-o.PC.x,-o.PE.x);
                s1.y:=  Max( o.P1.y, o.PC.y, o.PE.y);
                s2.y:= -Max(-o.P1.y,-o.PC.y,-o.PE.y);

             when paramcurve2d =>
                s1:= o.P1;  --  We should compute a bounding box !!
                s2:= s1;

             when others => -- includes aux
                s1:= (-1.0)*Pinf;
                s2:=        Pinf;
           end case;

           if  s1.x > Pmax.x then Pmax.x:=s1.x; end if;
           if  s1.y > Pmax.y then Pmax.y:=s1.y; end if;
           if  s2.x < Pmin.x then Pmin.x:=s2.x; end if;
           if  s2.y < Pmin.y then Pmin.y:=s2.y; end if;
         end if;
         o:= o.all.next;

       end loop;
       if Pmin = Pinf then -- 4-Jun-2003: only "aux" objects
         Pmax:= (0.0,0.0);
         Pmin:= (0.0,0.0);
       end if;
     end if; -- measuring limits.

     if  macro then
       Pmax:= Pmax - Pmin;
     else
       Pmin:= (0.0,0.0);
     end if;

     Put_Line(tf, "% This is a LaTeX picture output by TeXCAD.");
     Put_Line(tf, "% File name: [" & Short_name(displayed_name) & "].");
     Put_Line(tf, "% Version of TeXCAD: " & version);
     Put_Line(tf, "% Reference / build: " & reference);
     Put_Line(tf, "% For new versions, check: " & web);
     Put_Line(tf, "% Options are on the following lines.");
     -- 8-Jul-2004: + file name
     Put_Line(tf,Img(cgrade)  & On_off(pic.opt.steigung) );
     Put_Line(tf,Img(clines)  & On_off(pic.opt.sty(emlines)) );
     Put_Line(tf,Img(cepic)   & On_off(pic.opt.sty(epic))     );
     Put_Line(tf,Img(cbezmac) & On_off(pic.opt.sty(bezier))   );
     Put_Line(tf,Img(creduce) & On_off(pic.opt.reduce)   );
     Put_Line(tf,Img(csnap)   & On_off(pic.opt.snapping) );
     -- Preview insertions (Jan-2007):
     declare
       insertion: constant String:= To_String(pic.opt.pv_insert);
     begin
       for i in insertion'Range loop
         if i = insertion'First then
           Put(tf,Img(cmd_pv_insert) & '{');
         end if;
         case insertion(i) is
           when ASCII.LF =>
             Put_Line(tf,"}");
             Put(tf,Img(cmd_pv_insert) & '{');
           when ASCII.CR =>
             null;
           when others =>
             Put(tf,insertion(i));
         end case;
         if i = insertion'Last then
           Put_Line(tf,"}");
         end if;
       end loop;
     end;

     current_precision:= default_precision;
     Put_Line(tf,Img(cqual)    & PA_AdaPas_Real.Br(pic.opt.quality));
     Put_Line(tf,Img(cgdiff)   & PA_AdaPas_Real.Br(pic.opt.stdiff));
     Put_Line(tf,Img(csnapasp) & Br(pic.opt.snap_asp));
     Put_Line(tf,Img(czoom)    & '{' & Ada_Pas_Real(pic.opt.zoom_fac,4) & '}');

     if pic.opt.sty(emlines) then
       Put_Line(tf,"\special{em:linewidth " & To_String(pic.opt.linewidth) & '}');
     end if;
     Put_Line(tf,
       "\unitlength " & To_String(pic.opt.unitlength) &
       " % = " & R(pic.ul_in_pt) & "pt");
     Put_Line(tf,"\linethickness{" & To_String(pic.opt.linewidth) & '}');

     Put_Line(tf,"\ifx\plotpoint\undefined\newsavebox{\plotpoint}\fi % GNUPLOT compatibility");

     if pic.ul_in_pt > 0.0 then
       current_precision:= default_precision +
         Integer'Max(0, Integer(Log(pic.ul_in_pt) / Log(10.0) - 0.5));
       -- Add extra precision when 1 UL is a "big" distance
     else
       current_precision:= default_precision;
     end if;
     Put_Line(tf,"\begin{picture}" & Pt( Pmax - pic.opt.P0) & Pt( pic.opt.P0 ));

     o:= pic.root;
     pointnum:= 0;

    while o/= null loop
      --Put("#" & obj_art_type'image(o.art));

      if o.art = aux then
        Put_Line(tf,To_String(o.inhalt));
      elsif o.picked or not macro then
        if lined(o.art) and current_LaTeX_ls.thickness /= o.ls.thickness then
          -- ^ Change of thin/thick useful only when the figure
          --   is composed by lines and then thickness is different
          --   than the previous lined figure's.
          current_LaTeX_ls.thickness:= o.ls.thickness;
          case current_LaTeX_ls.thickness is
            when thin  => Put_Line(tf,Img_track(cthinlines));
            when thick => Put_Line(tf,Img_track(cthicklines));
          end case;
        end if;

        if o.art = line and then o.any_slope then
          case o.ls.pattern is
            when plain       => Write_plain_line_any_slope(o.all);
            when dot | dash  => Write_dot_dash_line_command(o.all);
          end case;
        else  -- Something else (not an em-line/vector)
          case o.art is
            when bezier      => Write_bezier(o.all);
            when paramcurve2d=> Write_paramcurve2d(o.all);
            when circ | disc => Write_kreis(o.all, Pmin);
            when aux         => Put(tf,To_String(o.inhalt));
            when others =>
              if o.art = box and o.ls.pattern=dot then
                Write_dotted_box(o.all);
              else
                Write_LaTeX_put_figure(o.all); -- e.g. \put{\dashbox...}
              end if;
          end case;
        end if; -- picked or not macro
      end if; -- aux or not
      o:= o.next;
    end loop;

    Put_Line(tf,"\end{picture}");
    Flush(tf);
  end Insert;

  procedure Save( pic           : in out Picture;
                  macro         :        Boolean;
                  file_name     :        String;
                  displayed_name:        String
  )
  is
    use Ada.Text_IO;
    tf: File_Type;
  begin
    Create(tf,Out_File,file_name);
    Insert(pic,macro,tf,displayed_name);
    Close(tf);
    if not macro then pic.saved:= True; end if;
  end Save;

  procedure Insert_and_Wrap_in_document(
    pic  : in Picture;
    macro:    Boolean;
    file :    Ada.Text_IO.File_Type;
    title:    String )
  is
    use Ada.Text_IO;
  begin
    case gen_opt.preview_mode is
      when v209 =>
        Put(file,"\documentstyle");
        if pic.opt.sty(epic) then
          Put(file,"[epic]");
        end if;
      when v2e  =>
        Put(file,"\documentclass");
    end case;
    Put_Line(file,"{article}\pagestyle{empty}");
    Put_Line(file,To_String(pic.opt.pv_insert));
    case gen_opt.preview_mode is
      when v209 =>
        null;
      when v2e  =>
        if pic.opt.sty(epic) then
          Put_Line(file,"\usepackage{epic}");
        end if;
    end case;
    Put_Line(file,"\begin{document}\parindent 0pt\TeX CAD preview (");
    Put_Line(file,"\verb/" & title & "/ ) :\\[1.3ex]\framebox{");
    Insert( pic, macro, file, "" );
    Put_Line(file,"}\end{document}");
  end Insert_and_Wrap_in_document;

end TC.Output;
