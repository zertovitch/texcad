--  This is the generic display package for TeXCAD.
--
--  On a multi-window implementation, please use one instance per window.

generic

  m_x, m_y : Natural;  --  Maximal x and y in the drawing area.
  h_mag, v_mag : Real;

  with procedure ClearScreen;
  with procedure SetColor (zone : Graphics.Color_Zone);
  with procedure PutPoint (x, y : Integer);
  with procedure Line (x1, y1, x2, y2 : Integer; thickness : Line_thickness);
  with procedure Full_rectangle (x1, y1, x2, y2 : Integer);
  with procedure Ellipse (x, y, rx, ry : Integer; fill : Boolean; thickness : Line_thickness);
  with procedure Arc (x, y, a1, a2, r : Integer);
  with procedure SetTextJustify (h : Graphics.H_Justify; v : Graphics.V_Justify);
  with procedure OutTextXY (x, y : Integer; t : String);

package TC.Display is

  procedure Draw (p : in out Picture); --  Reason of "in out": p.refresh is changed
  procedure Set_Origin (P : Point);    --  Set the (x0, y0) point in a Picture
  procedure Set_ul (pt : Real);        --  Size of unitlength, for qBezier density
  procedure Draw (o : Obj_type);

end TC.Display;
