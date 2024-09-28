--        ________          _   ____   ____  ___
--           /    ____  \ _/   /      /   / /   \
--          /    /___/  _\    /      /---/ /    /
--         /    /___   /  \  /____  /   / /____/

-----------------------------
-- Root package for TeXCAD --
-----------------------------

with Ada.Text_IO,
     Ada.Unchecked_Deallocation,
     Ada.Numerics.Generic_Elementary_Functions,
     Ada.Strings.Unbounded;

with Formulas;

package TC is

  use Ada.Strings.Unbounded;

  version   : constant String := "4.6 preview 1";
  reference : constant String := "28-Sep-2024 (revision a120)";
  web1      : constant String := "https://texcad.sourceforge.io/";
  web2      : constant String := "https://github.com/zertovitch/texcad";
  web3      : constant String := "https://alire.ada.dev/crates/texcad";
  mail      : constant String := "gdemont@users.sf.net";

  subtype Real is Long_Float;
  package REF is new Ada.Numerics.Generic_Elementary_Functions (Real);
  package RIO is new Ada.Text_IO.Float_IO (Real);

  type Point is record
    x, y : Real;
  end record;

  function "+"(P1, P2 : Point) return Point;       pragma Inline ("+");
  function "-"(P1, P2 : Point) return Point;       pragma Inline ("-");
  function "-"(P1 : Point) return Point;           pragma Inline ("-");
  function "*"(f : Real; P : Point) return Point;  pragma Inline ("*");
  function Norm2 (P : Point) return Real;          pragma Inline (Norm2);
  function Norm (P : Point) return Real;           pragma Inline (Norm);
  function Ortho (P : Point) return Point;         pragma Inline (Ortho);

  --  5-Mar-2004: grouping the ".sty"'s used by the picture.
  type Supposing_Sty is (bezier, epic, emlines);
  type Supposing_Sty_Set is array (Supposing_Sty) of Boolean;

  --  17-Feb-2003:
  --    Options_type replaced by { General_options, Picture_options }
  --    Cleaner and allows a multi-document context.

  type Picture_Options is record
    sty       : Supposing_Sty_Set := (emlines => False,
                                      bezier  => True,
                                      epic    => False);
    steigung  : Boolean := True;   --  Linien mit bel. Steigung?
    quality   : Real    := 8.0;    --  Qualitaet von bezier/Kreis durch emline
    reduce    : Boolean := True;   --  Verbundene linien bei gleicher Steigung
                                   --  zusammen fassen
    stdiff    : Real    := 0.005;  --  Steigungs-Differenz fuer REDUCE = TRUE

    snapping  : Boolean  := False;  --  Schnapp-Funktion an oder aus
    snap_asp  : Positive := 1;      --  Rasterbreite fuer Schnapp-Funktion
    zoom_fac  : Real     := 4.0;    --  Vergroesserungsfaktor
    --  LaTeX-Einheitslaenge der Bilder
    unitlength : Unbounded_String := To_Unbounded_String ("1mm");
    --  Linienbreite
    linewidth : Unbounded_String := To_Unbounded_String ("0.4pt");
    --  Linke untere Ecke des angezeigten Bildausschnittes
    P0        : Point  := (0.0, 0.0);
    --  LaTeX code to be inserted for preview
    pv_insert : Unbounded_String :=
      To_Unbounded_String ("% Your \input, \def, etc. here");
  end record;

  type Language is (l_english, l_french);
  type Grid_Display is (none, points, lines);
  type Solid_Bezier_Points_Mode is (auto, suggest);
  --  ^ "auto" is for \qbezier only,
  --    "suggest" uses \bezier and a proposed amount of points

  type LaTeX_Version is
    (v209,  --             \documentstyle
     v2e);  --  (or later) \documentclass

  type Preview_Directory_Choice is
    (temporary,
     current);

  type General_Options is record
    tex_suff          : Unbounded_String := To_Unbounded_String ("pic");
                        --  Suffix fuer TeX-Dateien
    bak_suff          : Unbounded_String := To_Unbounded_String ("bak");
                        --  Backup suffix, to be appended to original name
                        --  E.g. name.pic.bak. Added 12-Jan-2007
    bak_enabled       : Boolean := True;
    mac_suff          : Unbounded_String := To_Unbounded_String ("mac");
                        --  Suffix fuer Macro-Dateien
    lang              : Language := l_english;
    grid              : Grid_Display := points;
    solid_bez         : Solid_Bezier_Points_Mode := auto;
    options_for_new   : Picture_Options;
    preview_mode      : LaTeX_Version := v2e;  --  +10-Jan-2007
    preview_directory : Preview_Directory_Choice := temporary;  --  +22-Jan-2007
  end record;

  gen_opt : General_Options;

  startup_language : Language;
  --  On start, startup_language is equal to loaded gen_opt.lang .
  --  After that, gen_opt.lang may change.

  function Pic_Suffix return String;
  function Mac_Suffix return String;

  type Obj_Art_Type is
    (txt, box, line,
     circ, disc, oval, aux, putaux,
     bezier,
     paramcurve2d,
     unitl, spez, beginn, ende1, ende2, point0, option);

  type Art_Set is array (Obj_Art_Type) of Boolean;

  hidden : constant Art_Set := (aux => True, others => False);

  lined : constant Art_Set :=
    (box | line | circ | oval | bezier => True,
     others => False);

  type Ovop is -- Options for \oval
    (LT,   T,  RT,
     L, entire, R,
     LB,   B,  RB);

  type Obj_Type;
  type ptr_Obj_Type is access Obj_Type;

  type Line_Pattern is (plain, dot, dash);
  type Line_Thickness is (thin, thick);    --  'thin' is normal (\linewidth)
  type Line_Arrows is (no_arrow, head, both, middle);  --  ( ---, -->, <->, ->- )

  subtype With_Arrows is Line_Arrows range head .. Line_Arrows'Last;

  type Line_Settings is record  --  15-Feb-2004
    ---  *** Width ***  ---
    thickness    : Line_Thickness;
    stretch      : Integer;           -- (epic) \in -100..\infty, default: 0
    ---  *** Pattern ***  ---
    pattern      : Line_Pattern;
    dot_gap      : Real;              --  gap in ul; 0.0 -> not specified
    dot_symbol   : Unbounded_String;  --  when "", default dot
    dash_length  : Real;
    dash_dot_gap : Real;              --  default is 0
    ---  *** Arrows ***  ---
    arrows       : Line_Arrows;
  end record;

  normal_line_settings : constant Line_Settings :=
    (thickness    => thin,
     stretch      => 0,
     pattern      => plain,
     dot_gap      => 0.0,
     dot_symbol   => Null_Unbounded_String,
     dash_length  => 1.0,
     dash_dot_gap => 0.0,
     arrows       => no_arrow);

  type Hor_Ver is (h, v);
  subtype Slope_Value is Integer range -6 .. 6; -- for \line; for \vector: -4..4

  type LaTeX_Slope is array (Hor_Ver) of Slope_Value;
  --  25-Feb-2004: for storing fractional LaTeX slope values
  type LaTeX_Slopes is array (1 .. 2) of LaTeX_Slope;
  --  25-Feb-2004: line, vector, bezvec,... up to 2 non-parallel arrows

  function Evaluate_Variable (name : String; t : Real) return Real;
  package TC_Formulas is new Formulas (Real, Real, Evaluate_variable);

  type Param_Curve_2D_Data is record
    segments  : Natural;  --  if 0: automatic
    scale     : Real;
    form_x    : Unbounded_String;  --  x(t)
    form_y    : Unbounded_String;  --  y(t)
    min_t     : Real;
    max_t     : Real;
  end record;

  type Obj_Type (art : Obj_Art_Type) is record  --  JW,GH
    P1 : Point;
    next : ptr_Obj_Type := null;
    picked : Boolean := False;
    pick_swap_candidate : Boolean;
    pick_distance : Real;
    ls : Line_Settings;
    case art is
       when aux | txt | putaux | box =>
          size          : Point;
          inhalt        : Unbounded_String;
          adjust        : String (1 .. 2);
          adjust_len    : Natural;
          solid         : Boolean;
       when line =>       -- \vector is line with ls.arrows = head.
          P2            : Point;
          any_slope     : Boolean;
          line_slope    : LaTeX_Slope;
       when circ | disc =>
          rad :  Real;
       when oval =>
          LL    : Point;  -- left-lower
          osize : Point;  -- (width, height)
          part  : Ovop;
       when bezier =>
          PE        : Point; -- end point
          PC        : Point; -- control point
          Pmiddle   : Point; -- midpoint (for arrows = middle only)
          bez_slope : LaTeX_Slopes;
          num       : Natural;
       when paramcurve2d =>  --  Check Read_paramcurve2d in TC.Input for syntax
          data_2d      : Param_Curve_2D_Data;
          parsed_2d_x  : TC_Formulas.Formula;  --  Needs refresh after each modification
          parsed_2d_y  : TC_Formulas.Formula;  --  Needs refresh after each modification
       when others => null;
    end case;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation (Obj_Type, ptr_Obj_Type);

  package Graphics is

    type Color_Zone is (background, normal, picked, shadow);

    type H_Justify is (lefttext, centertext, righttext);
    type V_Justify is (toptext,  centertext, bottomtext);

    procedure Images (hj : H_Justify; vj : V_Justify; s : out String; sl : out Natural);
    procedure Values (s : String; hj : out H_Justify; vj : out V_Justify);

    text_cutting : constant := 15;

    function Position_of_Text (o : Obj_Type) return Point;

  end Graphics;

  type Refresh_mode is
    (no,                       -- nothing
     only_last,                -- only last object (when a new one is added)
     picked, unpicked, every,  -- only objects
     shadows_and_objects,      -- for when new objects are added
     full                      -- background, grid, shadow, objects
    );

  type Picture is record
    root   : ptr_Obj_Type := null;
    opt    : Picture_Options;
    saved  : Boolean := True;  --  Bild gespeichert?
    aspect : Real    := 1.0;   --  Faktoren und Aspect-ratio um
                               --  Welt- in Bildschirmkoordinaten umzurechnen
    total,
    total_hidden,
    picked,
    picked_hidden : Natural := 0;    --  total / picked objects, visible / hidden
    memo    : ptr_Obj_Type := null;  --  pointer to a certain object (e.g. text to change)
    refresh : Refresh_mode;   --  9-May-2003: which sort of refresh is needed?

    ul_in_pt : Real;
    --  \unithlength expressed in pt, used to convert sizes not in unitlength
    --  e.g. rules from GNUPlot., writing lines with \multiput, etc.
    lw_in_pt : Real;
    --  \linewidth expressed in pt
  end record;

  type Insert_Location is (at_begin, at_end);

  procedure Insert (p : in out Picture; t : ptr_Obj_Type; where : Insert_Location);

  --  Refresh_size_dependent_parameters (was: Set_pt_lengths), sets:
  --    - ul_in_pt, lw_in_pt
  --    - arrow directions (15-Jan-2004), midpoint (Feb-2004) in bezvec's
  procedure Refresh_Size_Dependent_Parameters (p : in out Picture; objects : Boolean);

  function Max_Radius (a : Obj_Art_Type; ul_in_pt : Real) return Real;

  generic
    with procedure Action (P : Point);
  procedure Bezier_Curve (o : Obj_Type; pt_scale : Real);

  generic
    with procedure Action (P : Point);
    with procedure Singularity;  --  Lift pen of drawing, for instance
  procedure Parametric_Curve_2D (o : Obj_Type; pt_scale : Real);

  procedure Get_Slope (df : Point; sl : out LaTeX_Slope; vector : Boolean);
  procedure Set_Slope_of_Bezvec (o : in out Obj_Type; pt_scale : Real);
  --  ^ finds the arrow slope(s) a bezvec, sets midpoint for arrow at middle

  procedure Set_Slope_of_Linvec (o : in out Obj_Type);
  --  ^ finds len; if any_slope: finds h_slope, v_slope; corrects x2,y2

  --  10-Mar-2004: improvement of plain lines
  --    1/ changes to \line, \vector when possible
  --    2/ changes to any_slope when \line is too short - see LineThrs.tcp
  --    (1/ is for shorter output, 2/ ensures display)
  procedure Improve_Linvec (o : in out Obj_Type; ul_pt : Real);

  procedure Set_Radius (o : in out Obj_Type; df : Point);
  --  ^ finds radius

  --  "Good" number of points for a \bezier ; \qbezier has an automatism
  function Good_Num_of_Bezier_Points (o : Obj_Type; pt_scale : Real) return Positive;
  --  Set control points with extrema points on curve
  procedure Set_Control_Point (o : in out Obj_Type; TG : Point);

  procedure Delete_Object_List (root : in out ptr_Obj_Type);

  function TeX_Number (s : String) return Real;
  --  Tolerant parsing of a TeX number
  function TeX_Number (x : Real; prec : Positive := Real'Digits) return String;
  --  Minimal representation of a TeX number

  function Image (o : Ovop) return String;

  package Units is

    --  pt  point         (1 in = 72.27 pt)
    --  pc  pica          (1 pc = 12 pt)
    --  in  inch          (1 in = 25.4 mm)
    --  bp  big point     (1 in = 72 bp), also PostScript point
    --  cm  centimeter    (1 cm = 10 mm)
    --  mm  millimeter
    --  dd  didot point   (1157 dd = 1238 pt) 1/72 of a French inch, 0.376 mm
    --  cc  cicero        (1 cc = 12 dd)
    --  sp  scaled point  (65536 sp = 1 pt) TeX's smallest unit

    type Unit is (pt, pc, im, bp, cm, mm, dd, cc, sp, unknown); -- "im" is "in"

    function Convert (x : Real; from, to : Unit) return Real;
    function Convert (s : String; to : Unit) return Real;
    --  l is a string like "123pt" or ".55mm"

  end Units;

  function Almost_Zero (X : Real) return Boolean;
  pragma Inline (Almost_Zero);

  arrow_length_pt : constant := 4.0;  --  in pt (measured 11-Jan-2004)

  function Sty_Title (s : Supposing_Sty) return String;

  --  A few algorithms from epic.sty.
  --  Enhancements to Picture Environment. Version 1.2 - Released June 1, 1986
  --  by Sunil Podar, Dept. of Computer Science, SUNY at Stony Brook, NY 11794.

  package Epic_Calc is
    function Num_Segments (D_lta : Point; dotgap : Real) return Positive;
    --  For dotgap = 0.0 (<=> no parameter), 1 is taken.
  end Epic_Calc;

private

  function Evaluate_Param_Curve_2D (o : Obj_Type; t : Real) return Point;

end TC;
