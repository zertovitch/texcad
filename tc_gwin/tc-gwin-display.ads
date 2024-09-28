with GWindows.Drawing;

package TC.GWin.Display is

  recreate_drawing_objects : Boolean := True;

  procedure Draw
    (Canvas : in out GWindows.Drawing.Canvas_Type'Class;
     p      : in out TC.Picture;       --  in out: p.refresh is changed
     an_obj :        TC.ptr_Obj_Type;  --  null for whole picture redraw
     width  :        Integer;
     height :        Integer);

  procedure Pixels_to_Units
    (p          :     TC.Picture;
     px, py, my :     Integer;
     PU         : out TC.Point);

end TC.GWin.Display;
