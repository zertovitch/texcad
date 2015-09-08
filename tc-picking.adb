package body TC.Picking is

  -- Behaviour changed 12-Aug-2007:
  -- For single pick (resp. unpick), only the unselected
  -- (resp. selected) object nearest to M1 and under the max distance,
  -- if any, is being selected (resp. unselected) at a time.
  -- Previously, all objects under the max distance were
  -- selected at the same time, it was nasty (required zooming a lot).
  -- Now, when you click on overlapping objects, they are selected
  -- one per click

  procedure PicPic(
    p     : in out Picture;
    op    :        Pick_operation;
    M1,M2 : Point:= (0.0,0.0)
  )
  is
    dist_max, dist_max_2, txt_dist2: Real;
    a: ptr_Obj_type:= p.root;

    last_near_dist: Real; -- 2007

    function Near_Point return Boolean is
      use REF, TC.Graphics;

      function Near_Text( T: Point ) return Boolean is
        x_adj: Natural;
        dist2: Real;
      begin
        x_adj:= Integer'Min(Integer'Max(1,Length(a.inhalt)),TC.Graphics.text_cutting);
        dist2:= ((M1.x-T.x)/Real(x_adj))**2 + (M1.y-T.y)**2;
        if dist2 <= txt_dist2 then
          last_near_dist:= Sqrt(dist2);
          return True;
        else
          return False;
        end if;
      end Near_Text;

      -- 25-Apr-2003
      -- Change from TeXCAD 3 ergonomy: we check the distance to the
      -- frame instead of distance to the full box.
      -- Reason: with no [y/n] choice for picking, then a box is always
      -- selected with the old method while selecting object inside of it.

      function Near_Box( P1, P2: Point; frame: Boolean) return Boolean is
        dist: Real;
      begin
        if frame then -- Distance to frame:
          -- vertical range
          if M1.y >= P1.y - dist_max and then M1.y <= P2.y + dist_max then
            if abs(M1.x - P1.x) <= dist_max then     -- left side
              last_near_dist:= abs(M1.x - P1.x);
              return True;
            elsif abs(M1.x - P2.x) <= dist_max then  -- right side
              last_near_dist:= abs(M1.x - P2.x);
              return True;
            end if;
          end if;
          -- horizontal range
          if  M1.x >= P1.x - dist_max and then M1.x <= P2.x + dist_max then
            if abs(M1.y - P1.y) <= dist_max then     -- top side
              last_near_dist:= abs(M1.y - P1.y);
              return True;
            elsif abs(M1.y - P2.y) <= dist_max then  -- bottom side
              last_near_dist:= abs(M1.y - P2.y);
              return True;
            end if;
          end if;
          return a.art=box and then Near_Text( Position_of_text( a.all ));
        end if;
        -- Distance to full box:
        dist:= Real'Max(P1.x - M1.x, M1.x - P2.x);
        dist:= Real'Max(dist, P1.y - M1.y);
        dist:= Real'Max(dist, M1.y - P2.y);
        if dist <= dist_max then
          last_near_dist:= dist;
          return True;
        end if;
        return False;
      end Near_Box;

    function Near_Line return Boolean is
      slope,h_slope,v_slope,dx,dy,aux: Real;
      P,A1,A2: Point;
    begin
      if a.any_slope then
        h_slope:= a.P2.x - a.P1.x;
        v_slope:= a.P2.y - a.P1.y;
      else
        h_slope:= Real(a.line_slope(h));
        v_slope:= Real(a.line_slope(v));
      end if;
      if abs(h_slope) < 1.0 then
        if (abs(M1.x-a.P1.x) <= dist_max) and then
           (M1.y > Real'Min(a.P1.y,a.P2.y)) and then
           (M1.y < Real'Max(a.P1.y,a.P2.y))
        then
          last_near_dist:= abs(M1.x-a.P1.x);
          return True;
        end if;
      else
        slope:= v_slope / h_slope;
        A1:= a.P1;
        A2:= a.P2;
        P:= M1;
        if abs(slope)>1.0 then
          aux:=A1.x; A1.x:= A1.y; A1.y:=aux;
          aux:=A2.x; A2.x:= A2.y; A2.y:=aux;
          aux:= P.x;  P.x:=  P.y;  P.y:=aux;
          slope:= 1.0 / slope;
        end if;
        if  (P.x > Real'Min(A1.x,A2.x))  and  (P.x < Real'Max(A1.x,A2.x)) then
          dx:= P.x - A1.x;
          dy:= dx * slope;
          if abs(P.y-A1.y-dy) <= dist_max then
            last_near_dist:= abs(P.y-A1.y-dy);
            return True;
          end if;
        end if;
      end if;
      return False;
    end Near_Line;

    near: Boolean:= False;
    procedure Near_to_target_Point(P:Point) is
      dist: Real;
    begin
      if Norm2(M1-P) <= dist_max_2 then
        dist:= Sqrt(Norm2(M1-P));
        if near then
          last_near_dist:= Real'Min(dist, last_near_dist);
        else
          last_near_dist:= dist;
        end if;
        near:= True;
      end if;
    end Near_to_target_Point;

    procedure Scout_Bezier is new Bezier_curve(Near_to_target_Point);

    procedure Do_Nothing is begin null; end;

    procedure Scout_Param_2D is new Parametric_curve_2D(Near_to_target_Point, Do_Nothing);

    begin -- Near_Point
      case a.art  is
        when txt | putaux=>
          return Near_Text( a.P1 );
        when box =>
          return Near_Box( a.P1, a.P1 + a.size, frame => not a.solid );
        when line =>
          return Near_Line;
        when disc =>
          if Norm(M1 - a.P1) - a.rad <= dist_max then
            last_near_dist:= Real'Max(0.0, Norm(M1 - a.P1) - a.rad);
            return True;
          end if;
        when circ  =>
          if abs(Norm(M1 - a.P1) - a.rad) <= dist_max then
            last_near_dist:= abs(Norm(M1 - a.P1) - a.rad);
            return True;
          end if;
        when oval  =>
          return Near_Box( a.LL, a.LL + a.osize, frame => True );
        when bezier =>
          Scout_Bezier(a.all, p.ul_in_pt);
          return near;
        when paramcurve2d =>
          Scout_Param_2D(a.all, p.ul_in_pt);
          return near;
        when others =>
          return False; -- incl.: aux
      end case;
      return False;
    end Near_Point;

    function In_Area return Boolean is
      MI,MA: Point;
    begin
      MI.x:= Real'Min(M1.x,M2.x) - dist_max;
      MI.y:= Real'Min(M1.y,M2.y) - dist_max;
      MA.x:= Real'Max(M1.x,M2.x) + dist_max;
      MA.y:= Real'Max(M1.y,M2.y) + dist_max;
      case  a.art  is
        when txt | putaux =>
          return a.P1.x >= MI.x and then a.P1.x <= MA.x  and then
                 a.P1.y >= MI.y and then a.P1.y <= MA.y;
        when box =>
          return a.P1.x >= MI.x  and then  a.P1.x+a.size.x <= MA.x  and then
                 a.P1.y >= MI.y  and then  a.P1.y+a.size.y <= MA.y;
        when line =>
          return a.P1.x >= MI.x  and then  a.P1.x <= MA.x  and then
                 a.P1.y >= MI.y  and then  a.P1.y <= MA.y  and then
                 a.P2.x >= MI.x  and then  a.P2.x <= MA.x  and then
                 a.P2.y >= MI.y  and then  a.P2.y <= MA.y;
        when circ | disc =>
          return a.P1.x-a.rad >= MI.x  and then  a.P1.x+a.rad <= MA.x  and then
                 a.P1.y-a.rad >= MI.y  and then  a.P1.y+a.rad <= MA.y;
        when oval =>
          return a.LL.x>=MI.x  and then  a.LL.x + a.osize.x <=MA.x  and then
                 a.LL.y>=MI.y  and then  a.LL.y + a.osize.y <=MA.y;
        when bezier =>
          return a.P1.x >= MI.x and then  --  xc>=P1.x and then
                 a.PE.x >= MI.x and then
                 a.P1.x <= MA.x and then  --  xc<=xx and then
                 a.PE.x <= MA.x and then
                 a.P1.y >= MI.y and then  --  yc>=P1.y and then
                 a.PE.y >= MI.y and then
                 a.P1.y <= MA.y and then  --  yc<=yy and then
                 a.PE.y <= MA.y;        --  xc,yc kann ausserhalb Screen liegen
        when paramcurve2d =>
          return a.P1.x >= MI.x and then a.P1.x <= MA.x  and then
                 a.P1.y >= MI.y and then a.P1.y <= MA.y;
          -- !! Cheap solution (catch only 1st point)...
          -- We should compute a bounding box !!
        when others =>
          return False; -- incl.: aux
      end case;
    end In_Area;

    found: Boolean;

    has_modifiable_info: constant array(Obj_art_type) of Boolean:=
      (txt | putaux | box | oval | bezier | paramcurve2d => True,
       others => False);

    min_dist: Real:= Real'Last;

  begin -- PicPic
    if op = pick_text then
      PicPic( p, unpick_all );
      p.memo:= null;
    end if;

    -- 27-Apr-2003
    -- Change from TeXCAD 3 ergonomy: distance is pixel-based
    -- -> Adds precision with zooming
    -- -> Better chance to pick something on large pictures
    dist_max   := px_dist / p.opt.zoom_fac;
    dist_max_2 := dist_max ** 2;
    txt_dist2  := px_txt_dist2 / (p.opt.zoom_fac**2);

    -- 1/ Nobody is candidate a priori
    a:= p.root;
    while a /= null loop
      a.pick_swap_candidate:= False;
      a:= a.next;
    end loop;
    -- 2/ Identify the candidates for being picked / unpicked
    a:= p.root;
    while a /= null loop
      case op is
        when pick      | unpick      =>
          found:= Near_Point;
        when pick_area | unpick_area =>
          found:= In_Area;
        when pick_all  | unpick_all  =>
          found:= True;
        when pick_text =>
          if has_modifiable_info(a.art) then
            if (a.art=box and then a.solid) then
              found:= False;
            else
              found:= Near_Point;
            end if;
          else
            found:= False;
          end if;
      end case;
      case op is
        when pick | pick_area | pick_all | pick_text =>
          if found and not a.picked then
            a.pick_swap_candidate:= True;
            if op=pick then
              a.pick_distance:= last_near_dist;
              min_dist:= Real'Min(min_dist, last_near_dist);
            end if;
            exit when op = pick_text;
          end if;
        when unpick | unpick_area | unpick_all =>
          if found and a.picked then
            a.pick_swap_candidate:= True;
            if op=unpick then
              a.pick_distance:= last_near_dist;
              min_dist:= Real'Min(min_dist, last_near_dist);
            end if;
          end if;
      end case;
      a:= a.next;
    end loop;
    -- 3/ Re-scan the picture to set/cancel the picked status definitively
    a:= p.root;
    while a /= null loop
      if a.pick_swap_candidate then
        case op is
          when pick | pick_area | pick_all | pick_text =>
            if op /= pick or else Almost_zero(a.pick_distance - min_dist) then
              a.picked:= True;
              p.picked:= p.picked + 1;
              if hidden( a.art ) then
                p.picked_hidden:= p.picked_hidden + 1;
              end if;
              p.memo:= a;
              exit when op = pick_text or op = pick;
            end if;
        when unpick | unpick_area | unpick_all =>
          if op /= unpick or else Almost_zero(a.pick_distance - min_dist) then
            a.picked:= False;
            p.picked:= p.picked - 1;
            if hidden( a.art ) then
              p.picked_hidden:= p.picked_hidden - 1;
            end if;
            exit when op = pick_text or op = unpick;
          end if;
        end case;
      end if;
      a:= a.next;
    end loop;
  end PicPic;

  procedure Del_picked( p: in out Picture ) is
    procedure Stat_and_del( o: in out ptr_Obj_type ) is
    begin
      p.saved := False;
      p.picked:= p.picked - 1;
      p.total:= p.total - 1;
      if hidden( o.art ) then
        p.picked_hidden:= p.picked_hidden - 1;
        p.total_hidden:= p.total_hidden - 1;
      end if;
      Dispose(o);
    end Stat_and_del;
    a,b: ptr_Obj_type;
  begin
    while p.root/=null and then p.root.picked loop
      a:= p.root;
      p.root:= p.root.next;
      Stat_and_del(a);
    end loop;
    if p.root/=null then
      a:= p.root;
      b:= a.next;
      while b/=null loop
        -- Invariant here:
        -- a.all exists, a.picked=False, b=a.next, b.all exists
        if b.picked then
          a.next:= b.next; -- b skipped
          Stat_and_del(b); -- b deleted
        else
          a:= b;
        end if;
        b:= a.next;
      end loop;
    end if;
  end Del_picked;

end TC.Picking;
