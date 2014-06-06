-- The generic display package for TeXCAD.
-- On a multi-window implementation, please use one instance per window

generic

  m_x, m_y : Natural;  --  Maximales x und y im Zeichenfenster
  h_mag, v_mag: Real;

  with procedure ClearScreen;
  with procedure SetColor( zone: Graphics.Color_zone );
  with procedure PutPoint( x,y: Integer );
  with procedure Line( x1,y1,x2,y2: Integer );
  with procedure Full_rectangle( x1,y1,x2,y2: Integer );
  with procedure Ellipse( x,y, rx,ry: Integer; fill: Boolean );
  with procedure Arc( x,y, a1,a2, r: Integer );
  with procedure SetTextJustify( h: Graphics.H_justify; v: Graphics.V_justify );
  with procedure OutTextXY( x,y: Integer; t: String );

package TC.Display is

  procedure Draw( p: in out Picture ); -- in out: p.refresh is changed
  procedure Set_Origin( P: Point ); -- the (x0,y0) in a Picture
  procedure Set_ul( pt: Real ); -- size of unitlength, for qBezier density
  procedure Draw( o: Obj_type );

end TC.Display;
