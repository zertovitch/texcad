with Ada.Characters.Handling;

package body TC.Morphing is

  procedure Translate( o: in out Obj_type; M: Point ) is
  begin
    if o.art /= aux then  -- 27-May-2003
      o.P1:= o.P1 + M;
    end if;
    case o.art is
      when aux | txt | putaux | box =>
        null;
      when line =>
        o.P2:= o.P2 + M;
      when circ | disc =>
        null;
      when oval =>
        o.LL:= o.LL + M;
      when bezier =>
        o.PE:= o.PE + M;
        o.PC:= o.PC + M;
      when others=> null;
    end case;
  end Translate;

  -- 23-May-2003: we adapt text alignment to symmetry and rotations.
  -- The oval corner classification is the good tool for that.

  function Has_text_alignment(o: Obj_type) return Boolean is
  begin
    case o.art is
      when txt    => return True;
      when box    => return not o.solid;
      when others => return False;
    end case;
  end Has_text_alignment;

  function Text_align_to_oval_corner(o: Obj_type) return Ovop is
    use TC.Graphics;
    conv: constant array( H_justify, V_justify ) of Ovop :=
      ( lefttext   => (toptext => LT, centertext => L,      bottomtext => LB),
        centertext => (toptext =>  T, centertext => entire, bottomtext =>  B),
        righttext  => (toptext => RT, centertext => R,      bottomtext => RB) );
    h: H_justify;
    v: V_justify;
  begin
    Values( o.adjust(1..o.adjust_len), h, v );
    return conv( h,v );
  end Text_align_to_oval_corner;

  procedure Oval_corner_to_text_align(o: in out Obj_type; corner: Ovop) is
    a: constant String:= Ada.Characters.Handling.To_Lower(Ovop'Image(corner));
  begin
    case corner is
      when entire => o.adjust_len:= 0; -- same as "cc"
      when others => o.adjust(1..a'Length):= a;
                     o.adjust_len:= a'Length;
    end case;
  end Oval_corner_to_text_align;

  -- Symmetry scheme:
  --    4  3  2
  --     \ | /
  --  1 -- * -- 1
  --     / | \
  --    2  3  4

  procedure Symmetry( o: in out Obj_type; M: Point; subcmd: Natural; pt_scale: Real ) is
    hr,x,y: Real; hi: Integer;
    ovsym: constant array( 1..4, Ovop ) of Ovop:=
      (
        1=> ( LB,   B,  RB,
              L, entire, R,
              LT,   T,  RT ),
        2=> ( RB,   R,  RT,
              B, entire, T,
              LB,   L,  LT ),
        3=> ( RT,   T,  LT,
              R, entire, L,
              RB,   B,  LB ),
        4=> ( LT,   L,  LB,
              T, entire, B,
              RT,   R,  RB )
      );

    procedure Sym2P(P: in out Point) is
      mu: Real;
    begin
      mu:= (-x+y+P.x-P.y);
      P:= P + (-mu,mu);
    end Sym2P;

    procedure Sym4P(P: in out Point) is
      mu: Real;
    begin
      mu:= (x+y-P.x-P.y);
      P:= P + (mu,mu);
    end Sym4P;

  begin
    x:= M.x;
    y:= M.y;

    if subcmd in 1..4 then
      if o.art = oval then
        o.part:= ovsym( subcmd, o.part );
      elsif Has_text_alignment(o) then
        Oval_corner_to_text_align(
          o,
          ovsym( subcmd, Text_align_to_oval_corner(o) )
        );
      end if;
    end if;

    case subcmd is
      when 1=>
        case o.art is
          when txt | circ | disc | putaux =>
            o.P1.y:= 2.0*y - o.P1.y;
          when box =>
            o.P1.y:= 2.0*y - o.P1.y - o.size.y;
          when line =>
            o.P1.y:= 2.0*y-o.P1.y;
            o.P2.y:= 2.0*y-o.P2.y;
            if not o.any_slope then o.line_slope(v):=-o.line_slope(v);end if;
          when oval =>
            o.P1.y:= 2.0*y - o.P1.y;
            o.LL.y:= 2.0*y - o.LL.y - o.osize.y;
          when bezier =>
            o.P1.y:= 2.0*y-o.P1.y;
            o.PE.y:= 2.0*y-o.PE.y;
            o.PC.y:= 2.0*y-o.PC.y;
            Set_slope_of_bezvec(o,pt_scale);
          when others=> null;
        end case;

      when 2=>
        case  o.art  is
          when txt | circ | disc| putaux=>
            Sym2P(o.P1);
          when box=>
            Sym2P(o.P1);
            hr:=o.size.x; o.size.x:=o.size.y; o.size.y:=hr;

          when line=>
            Sym2P(o.P1);
            Sym2P(o.P2);
            if not o.any_slope then
              hi:=o.line_slope(v); o.line_slope(v):=o.line_slope(h); o.line_slope(h):=hi;
            end if;

          when oval=>
            Sym2P(o.P1);
            Sym2P(o.LL);
            hr:=o.osize.x; o.osize.x:=o.osize.y; o.osize.y:=hr;

          when bezier =>
            Sym2P(o.P1);
            Sym2P(o.PE);
            Sym2P(o.PC);
            Set_slope_of_bezvec(o,pt_scale);

          when others=> null;
        end case;

      when 3=>
        case  o.art  is
          when txt | circ | disc | putaux=>
            o.P1.x:= 2.0*x-o.P1.x;
          when box=>
            o.P1.x:= 2.0*x-o.P1.x-o.size.x;
          when line =>
            o.P1.x:= 2.0*x-o.P1.x;
            o.P2.x:= 2.0*x-o.P2.x;
            if not o.any_slope then o.line_slope(h):=-o.line_slope(h); end if;
          when oval=>
            o.P1.x:= 2.0*x-o.P1.x;
            o.LL.x:= 2.0*x-o.LL.x-o.osize.x;
          when bezier =>
            o.P1.x:= 2.0*x-o.P1.x;
            o.PE.x:= 2.0*x-o.PE.x;
            o.PC.x:= 2.0*x-o.PC.x;
            Set_slope_of_bezvec(o,pt_scale);
          when others=> null;
        end case;

      when 4=>
        case  o.art  is
          when txt | circ | disc | putaux=>
            Sym4P(o.P1);
          when box=>
            hr:=o.P1.x+o.size.x;
            o.P1.x:=x+y-o.P1.y-o.size.y; o.P1.y:=x+y-hr;
            hr:=o.size.x; o.size.x:=o.size.y; o.size.y:=hr;

          when line =>
            Sym4P(o.P1);
            Sym4P(o.P2);
            if not o.any_slope then
              hi:=o.line_slope(v); o.line_slope(v):=-o.line_slope(h); o.line_slope(h):=-hi;
            end if;

          when oval=>
            Sym4P(o.P1);
            hr:=o.LL.x+o.osize.x;
            o.LL.x:=x+y-o.LL.y-o.osize.y; o.LL.y:=x+y-hr;
            hr:=o.osize.x; o.osize.x:=o.osize.y; o.osize.y:=hr;

          when bezier =>
            Sym4P(o.P1);
            Sym4P(o.PE);
            Sym4P(o.PC);
            Set_slope_of_bezvec(o,pt_scale);

          when others=> null;
        end case;
      when others=> null;
    end case;
  end Symmetry;

  -- Rotation scheme:
  --    0
  --  1   3
  --    2

  procedure Rotate( o: in out Obj_type; M: Point; subcmd: Natural; pt_scale: Real ) is
    hr,x,y: Real; hi: Integer;

    function Rotate_oval( scheme: Integer; ov: Ovop ) return Ovop is
      circular: constant array( 0..7 ) of Ovop:= (LT, L, LB, B, RB, R, RT, T);
      shifting: constant array( 1..3 ) of Integer:= ( 2, 4, -2 );
    begin
      for i in circular'Range loop
        if circular(i) = ov then
          return circular( (i+shifting(scheme)) mod 8 );
        end if;
      end loop;
      return ov; -- should not happen
    end Rotate_oval;

  begin
    x:= M.x;
    y:= M.y;

    if subcmd in 1..3 then
      if o.art = oval then
        o.part:= Rotate_oval( subcmd, o.part );
      elsif Has_text_alignment(o) then
        Oval_corner_to_text_align(
          o,
          Rotate_oval( subcmd, Text_align_to_oval_corner(o) )
        );
      end if;
    end if;

    case subcmd is
      when 1=>
        case  o.art  is
          when txt | circ | disc | putaux =>
             hr:=o.P1.x;
             o.P1.x:=x+(y-o.P1.y); o.P1.y:=y+(hr-x);

          when box=>
             o.P1.y:=o.P1.y+o.size.y; hr:=o.P1.x;
             o.P1.x:=x+(y-o.P1.y); o.P1.y:=y+(hr-x);
             hr:=o.size.x; o.size.x:=o.size.y; o.size.y:=hr;

          when line =>
             hr:=o.P1.x;
             o.P1.x:=x+(y-o.P1.y); o.P1.y:=y+(hr-x);
             hr:=o.P2.x;
             o.P2.x:=x+(y-o.P2.y); o.P2.y:=y+(hr-x);
             if not o.any_slope then
               hi:=o.line_slope(h); o.line_slope(h):=-o.line_slope(v); o.line_slope(v):=hi;
             end if;

          when oval=>
             hr:=o.P1.x;
             o.P1.x:=x+(y-o.P1.y); o.P1.y:=y+(hr-x);
             o.LL.y:=o.LL.y+o.osize.y; hr:=o.LL.x;
             o.LL.x:=x+(y-o.LL.y); o.LL.y:=y+(hr-x);
             hr:=o.osize.x; o.osize.x:=o.osize.y; o.osize.y:=hr;

          when bezier =>
             hr:=o.P1.x;
             o.P1.x:=x+(y-o.P1.y); o.P1.y:=y+(hr-x);
             hr:=o.PE.x;
             o.PE.x:=x+(y-o.PE.y); o.PE.y:=y+(hr-x);
             hr:=o.PC.x;
             o.PC.x:=x+(y-o.PC.y); o.PC.y:=y+(hr-x);
             Set_slope_of_bezvec(o,pt_scale);
          when others => null;
        end case;

      when 2=>
        case o.art is
          when txt | circ | disc | putaux=>
             o.P1:= 2.0*M - o.P1;

          when box=>
             o.P1:= 2.0*M - o.P1 - o.size;

          when line =>
             o.P1:= 2.0*M - o.P1;
             o.P2:= 2.0*M - o.P2;
             if not o.any_slope then
               o.line_slope(h):= -o.line_slope(h);
               o.line_slope(v):= -o.line_slope(v);
             end if;

          when oval=>
             o.P1:= 2.0*M - o.P1;
             o.LL:= 2.0*M - o.LL - o.osize;

          when bezier =>
             o.P1:= 2.0*M - o.P1;
             o.PE:= 2.0*M - o.PE;
             o.PC:= 2.0*M - o.PC;
             Set_slope_of_bezvec(o,pt_scale);
          when others=> null;
        end case;

      when 3=>
        case  o.art  is
          when txt | circ | disc | putaux=>
             hr:=o.P1.x;
             o.P1.x:=x+(o.P1.y-y); o.P1.y:=y-(hr-x);

          when box=>
             o.P1.x:=o.P1.x+o.size.x; hr:=o.P1.x;
             o.P1.x:=x+(o.P1.y-y); o.P1.y:=y-(hr-x);
             hr:=o.size.x; o.size.x:=o.size.y; o.size.y:=hr;

          when line =>
             hr:=o.P1.x;
             o.P1.x:=x+(o.P1.y-y); o.P1.y:=y-(hr-x);
             hr:=o.P2.x;
             o.P2.x:=x+(o.P2.y-y); o.P2.y:=y-(hr-x);
             if not o.any_slope then
               hi:=o.line_slope(h); o.line_slope(h):=o.line_slope(v); o.line_slope(v):=-hi;
             end if;

          when oval=>
             hr:=o.P1.x;
             o.P1.x:=x+(o.P1.y-y); o.P1.y:=y-(hr-x);
             o.LL.x:=o.LL.x+o.osize.x; hr:=o.LL.x;
             o.LL.x:=x+(o.LL.y-y); o.LL.y:=y-(hr-x);
             hr:=o.osize.x; o.osize.x:=o.osize.y; o.osize.y:=hr;

          when bezier =>
             hr:=o.P1.x;
             o.P1.x:=x+(o.P1.y-y); o.P1.y:=y-(hr-x);
             hr:=o.PE.x;
             o.PE.x:=x+(o.PE.y-y); o.PE.y:=y-(hr-x);
             hr:=o.PC.x;
             o.PC.x:=x+(o.PC.y-y); o.PC.y:=y-(hr-x);
             Set_slope_of_bezvec(o,pt_scale);
          when others=> null;
        end case;
      when others=> null; -- include 0 (no rotation!)
    end case;
  end Rotate;

  -- 28-May-2003 -- Affine transformation where Diag.x and Diag.y are >= 0
  procedure Affine_positive( o: in out Obj_type; Center, Diag: Point; pt_scale: Real ) is
    D1: constant Point:= (1.0,1.0) - Diag;
    is_an_homothethy: constant Boolean:= Almost_zero( Diag.y - Diag.x );

    procedure Move( P: in out Point ) is
    begin
      P:= ( D1.x * Center.x + Diag.x * P.x,
            D1.y * Center.y + Diag.y * P.y );
    end Move;

  begin
    if o.art /= aux then
      Move(o.P1);
    end if;
    case  o.art  is
      when txt | putaux =>
        null;
      when circ | disc =>
        o.rad:= o.rad * Norm(Diag) / REF.Sqrt(2.0);
        -- ^ Not ideal (should be an ellipse) but preserves homothethy
      when box=>
        o.size:= (Diag.x * o.size.x, Diag.y * o.size.y);

      when line =>
        Move(o.P2);
        if not is_an_homothethy then
          o.any_slope:= True;
        end if;

      when oval=>
        Move(o.LL);
        o.osize:= (Diag.x * o.osize.x, Diag.y * o.osize.y);

      when bezier =>
        Move(o.PE);
        Move(o.PC);
        Set_slope_of_bezvec(o,pt_scale);
      when others => null;
    end case;
  end Affine_positive;

  -- 28-May-2003 -- Affine transformation with factors Diag.x, Diag.y
  procedure Affine( o: in out Obj_type; Center, Diag: Point; pt_scale: Real ) is
    Dabs: constant Point:= (abs Diag.x, abs Diag.y);
  begin
    Affine_positive( o, Center, Dabs, pt_scale );
    if Diag.x < 0.0 then
      Symmetry( o, Center, 3, pt_scale );
    end if;
    if Diag.y < 0.0 then
      Symmetry( o, Center, 1, pt_scale );
    end if;
  end Affine;

  procedure Morphit(
    p     : in out Picture;
    m     :        Morphart;
    M1,M2 :        Point;
    keep  :        Boolean;
    iter  :        Positive;
    subcmd:        Positive
  )
  is
    need_copied_model: constant Boolean:= iter > 1 and not keep;
    o, n, model: ptr_Obj_type;
    f: Real;
    bilan: Integer;

    procedure Copy_of_model_after_o is
      c: ptr_Obj_type:= new Obj_type'(model.all);
    begin
      c:= new Obj_type'(model.all); -- c.all is a new object, a copy of model.all
      o.next  := c; -- we insert just after o (no need to change root).
      c.picked:= True;
      c.next  := n;
      o:= c; -- the previous o.all is behind, o points on the copy.
    end Copy_of_model_after_o;

  begin
    -- Compute total of creations per object:
    if keep then
      bilan:= iter;
    else
      bilan:= iter-1;
    end if;
    o:= p.root;
    while o /= null loop
      n:= o.next;
      if o.picked then
        if need_copied_model then -- we need a model that is a copy of the original
          model:= new Obj_type'(o.all);
        else
          model:= o;
        end if;
        if keep then
          Copy_of_model_after_o; -- we let the original behind
          model.picked:= False;  -- original is not picked
        end if;
        for i in 1 .. iter loop
          if i > 1 then
            Copy_of_model_after_o; -- copies 2,3,...
          end if;
          f:= Real(i);
          case m is
            when translation => Translate( o.all, f * (M2-M1) );
            when symmetry    => Symmetry( o.all, M1, subcmd, p.ul_in_pt );
            when rotation    => Rotate( o.all, M1, (i * subcmd) mod 4, p.ul_in_pt );
            when homothethy  => Affine( o.all, M1, (M2.x ** i, M2.y ** i), p.ul_in_pt );
          end case;
        end loop;
        p.picked:= p.picked + iter-1;
        p.total := p.total  + bilan;
        if hidden(o.art) then
          p.picked_hidden:= p.picked_hidden + iter - 1;
          p.total_hidden := p.total_hidden  + bilan;
        end if;
        p.saved:= False;
        if need_copied_model then
          Dispose(model);
        end if;
      end if;
      o:= n;
    end loop;
  end Morphit;

end TC.Morphing;
