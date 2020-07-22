with TC.Display;

with GWindows.Application;              use GWindows.Application;
with GWindows.Drawing_Objects;          use GWindows.Drawing_Objects;

with Ada.Numerics;                      use Ada.Numerics;

pragma Elaborate_All(GWindows.Drawing_Objects); -- For GUI_Font initialisation

package body TC.GWin.Display is
  use GWindows.Drawing;

  zcount: Natural:= 0;

  -- 16-Jun-2003 : moved brushes, pens, fonts to package level from Draw.
  -- Ressources shared by all windows, to be renewed as few as possible
  -- due to brush / pen leak in Windows 95/98/ME.

  brush, hatch: array( Color_zone ) of Brush_Type;
  pen         : array( Color_zone ) of Pen_Type;
  GUI_Font    : Font_Type;

  -- Conversions from TC.Graphics styles to GWindows.Drawing styles :

  h_align: constant array(H_justify) of Horizontal_Alignment_Type:=
    (Left,Center,Right);

  v_align: constant array(V_justify) of Vertical_Alignment_Type:=
    (Top,Base_Line,Bottom);

  procedure Draw(
    Canvas : in out GWindows.Drawing.Canvas_Type'Class;
    p      : in out TC.Picture; -- in out: p.refresh is changed
    an_obj : TC.ptr_Obj_type;   -- null for whole picture redraw
    width  : Integer;
    height : Integer)
  is
    current_color    : Color_Type;
    current_zone     : TC.Graphics.Color_zone:= normal;

    whole_picture: constant Boolean:= an_obj = null;

    procedure ClearScreen is
    begin
      Fill_Rectangle (
        Canvas,
        (0, 0, Desktop_Width, Desktop_Height), -- !!
        brush( background ) );
    end ClearScreen;

    procedure SetColor( zone: TC.Graphics.Color_zone ) is
    begin
      if whole_picture then
        current_zone:= zone;
        current_color:= color( current_zone );
        Text_Color(Canvas,current_color);
      end if;
    end SetColor;

    procedure PutPoint( x,y: Integer ) is
    begin
      GWindows.Drawing.Point(Canvas,x,y, current_color);
    end PutPoint;

    procedure Line(x1,y1,x2,y2: Integer) is
    begin
      if whole_picture then
       Select_Object( Canvas, pen(current_zone) );
      end if;
      if x1 /= x2 or y1 /= y2 then
        Line(Canvas,x1,y1,x2,y2);
      else
        GWindows.Drawing.Point(Canvas,x1,y1, current_color);
      end if;
    end Line;

    procedure Full_rectangle( x1,y1,x2,y2: Integer ) is
    begin
      Background_Mode(Canvas, Transparent);
      Fill_Rectangle( Canvas, (x1,y1,x2,y2), hatch(current_zone) );
    end Full_rectangle;

    procedure Ellipse( x,y, rx,ry: Integer; fill: Boolean ) is
      l,t,r,b: Integer;
    begin
      l:= x-rx;
      t:= y-ry;
      r:= x+rx;
      b:= y+ry;
      if whole_picture then
        Select_Object( Canvas, pen(current_zone) );
      end if;
      if fill then
        Background_Mode(Canvas, Transparent);
        Select_Object( Canvas, hatch(current_zone) );
        Ellipse(Canvas, l,t,r,b);
      else
        Arc(Canvas, l,t,r,b, r,y,r,y);
      end if;
    end Ellipse;

    procedure Arc( x,y, a1,a2, r: Integer ) is
      use TC.REF;
      f: constant:= 2.0 * Pi / 360.0;
    begin
      Arc(Canvas, x-r,y-r,x+r,y+r,
          Integer(Real(x)+Real(r)*Cos(Real(a1)*f)),
          Integer(Real(y)-Real(r)*Sin(Real(a1)*f)),
          Integer(Real(x)+Real(r)*Cos(Real(a2)*f)),
          Integer(Real(y)-Real(r)*Sin(Real(a2)*f))
          );
    end Arc;

    procedure SetTextJustify( h: H_justify; v: V_justify ) is
    begin
      Horizontal_Text_Alignment( Canvas, h_align(h) );
      Vertical_Text_Alignment  ( Canvas, v_align(v) );
    end SetTextJustify;

    procedure OutTextXY( x,y: Integer; t: String ) is
    begin
      Background_Mode(Canvas, Transparent);
      Select_Object (Canvas, GUI_Font);
      Put(Canvas,x,y,t);
    end OutTextXY;

    package TC_Display_for_Windows is
      new TC.Display(
        m_x            => width,
        m_y            => height,
        h_mag          => p.opt.zoom_fac,
        v_mag          => p.opt.zoom_fac * p.aspect,
        ClearScreen    => ClearScreen,
        SetColor       => SetColor,
        PutPoint       => PutPoint,
        Line           => Line,
        Full_rectangle => Full_rectangle,
        Ellipse        => Ellipse,
        Arc            => Arc,
        SetTextJustify => SetTextJustify,
        OutTextXY      => OutTextXY
      );

  begin
    if whole_picture then

      Set_Mix_Mode(Canvas,R2_COPYPEN);

      if recreate_drawing_objects then
        for z in Color_zone loop
          Create_Solid_Brush( brush(z), color(z) );
          if Windows_95 then
            -- Windows 95 has a bug that makes the hatch
            -- displaced depending on the colour
            Create_Solid_Brush( hatch(z), color(z) );
          else
            Create_Hatch_Brush( hatch(z), Diagonal_Cross, color(z) );
          end if;
        end loop;
        for z in Color_zone loop
          Create_Pen( pen(z), Solid, 1, color(z) );
        end loop;
        recreate_drawing_objects:= False;
      end if;

      TC_Display_for_Windows.Draw(p);
    else
      current_color:= Black;
      TC_Display_for_Windows.Set_Origin( p.opt.P0 );
      TC_Display_for_Windows.Set_ul( p.ul_in_pt );
      TC_Display_for_Windows.Draw(an_obj.all);
    end if;

    -- Put(Canvas,0,0,integer'image(zcount));
    zcount:= zcount+1;
  end Draw;

  procedure Pixels_to_Units(
    p       : TC.Picture;
    px,py,my: Integer;
    PU      : out TC.Point)
  is
    h_mag: constant TC.Real:= p.opt.zoom_fac;
    v_mag: constant TC.Real:= p.opt.zoom_fac * p.aspect;
  begin
    PU:= p.opt.P0 +
         ( TC.Real(px) / h_mag,
           TC.Real(my-py) / v_mag ); -- On screen Y is reversed.
  end Pixels_to_Units;

begin
  Create_Stock_Font(GUI_Font, Default_GUI);
end TC.GWin.Display;
