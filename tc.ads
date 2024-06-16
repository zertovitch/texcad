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

  version   : constant String := "4.53";
  reference : constant String := "16-Jun-2024 (revision a111)";
  web       : constant String := "http://texcad.sf.net/";
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
  type Supposing_sty is (bezier, epic, emlines);
  type Supposing_sty_set is array (Supposing_sty) of Boolean;

  --  17-Feb-2003:
  --    Options_type replaced by { General_options, Picture_options }
  --    Cleaner and allows a multi-document context.

  type Picture_Options is record
    sty       : Supposing_sty_set := (emlines => False,
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

  type LaTeX_version is
    (v209,  --            \documentstyle
     v2e);  -- (or later) \documentclass

  type Preview_directory_choice is
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
    preview_mode      : LaTeX_version := v2e;  --  +10-Jan-2007
    preview_directory : Preview_directory_choice := temporary;  --  +22-Jan-2007
  end record;

  gen_opt : General_Options;

  startup_language : Language;
  --  On start, startup_language is equal to loaded gen_opt.lang .
  --  After that, gen_opt.lang may change.

  function Pic_suffix return String;
  function Mac_suffix return String;

  type Obj_art_type is
    (txt, box, line,
     circ, disc, oval, aux, putaux,
     bezier,
     paramcurve2d,
     unitl, spez, beginn, ende1, ende2, point0, option);

  type Art_set is array (Obj_art_type) of Boolean;

  hidden : constant Art_set := (aux => True, others => False);

  lined : constant Art_set :=
    (box | line | circ | oval | bezier => True,
     others => False);

  type Ovop is -- Options for \oval
    (LT,   T,  RT,
     L, entire, R,
     LB,   B,  RB);

  type Obj_type;
  type ptr_Obj_type is access Obj_type;

  type Line_pattern is (plain, dot, dash);
  type Line_thickness is (thin, thick);    --  'thin' is normal (\linewidth)
  type Line_arrows is (no_arrow, head, both, middle);  --  ( ---, -->, <->, ->- )

  subtype With_arrows is Line_arrows range head .. Line_arrows'Last;

  type Line_Settings is record  --  15-Feb-2004
    ---  *** Width ***  ---
    thickness    : Line_thickness;
    stretch      : Integer;           -- (epic) \in -100..\infty, default: 0
    ---  *** Pattern ***  ---
    pattern      : Line_pattern;
    dot_gap      : Real;              --  gap in ul; 0.0 -> not specified
    dot_symbol   : Unbounded_String;  --  when "", default dot
    dash_length  : Real;
    dash_dot_gap : Real;              --  default is 0
    ---  *** Arrows ***  ---
    arrows       : Line_arrows;
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

  type Hor_ver is (h, v);
  subtype Slope_value is Integer range -6 .. 6; -- for \line; for \vector: -4..4

  type LaTeX_slope is array (Hor_ver) of Slope_value;
  --  25-Feb-2004: for storing fractional LaTeX slope values
  type LaTeX_slopes is array (1 .. 2) of LaTeX_slope;
  --  25-Feb-2004: line, vector, bezvec,... up to 2 non-parallel arrows

  function Evaluate_variable (name : String; t : Real) return Real;
  package TC_Formulas is new Formulas (Real, Real, Evaluate_variable);

  type Param_curve_2D_data is record
    segments  : Natural;  --  if 0: automatic
    scale     : Real;
    form_x    : Unbounded_String;  --  x(t)
    form_y    : Unbounded_String;  --  y(t)
    min_t     : Real;
    max_t     : Real;
  end record;

  type Obj_type (art : Obj_art_type) is record  --  JW,GH
    P1 : Point;
    next : ptr_Obj_type := null;
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
          line_slope    : LaTeX_slope;
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
          bez_slope : LaTeX_slopes;
          num       : Natural;
       when paramcurve2d =>  --  Check Read_paramcurve2d in TC.Input for syntax
          data_2d      : Param_curve_2D_data;
          parsed_2d_x  : TC_Formulas.Formula;  --  Needs refresh after each modification
          parsed_2d_y  : TC_Formulas.Formula;  --  Needs refresh after each modification
       when others => null;
    end case;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation (Obj_type, ptr_Obj_type);

  package Graphics is

    type Color_Zone is (background, normal, picked, shadow);

    type H_Justify is (lefttext, centertext, righttext);
    type V_Justify is (toptext,  centertext, bottomtext);

    procedure Images (hj : H_Justify; vj : V_Justify; s : out String; sl : out Natural);
    procedure Values (s : String; hj : out H_Justify; vj : out V_Justify);

    text_cutting : constant := 15;

    function Position_of_text (o : Obj_type) return Point;

  end Graphics;

  type Refresh_mode is
    (no,                       -- nothing
     only_last,                -- only last object (when a new one is added)
     picked, unpicked, every,  -- only objects
     shadows_and_objects,      -- for when new objects are added
     full                      -- background, grid, shadow, objects
    );

  type Picture is record
    root   : ptr_Obj_type := null;
    opt    : Picture_Options;
    saved  : Boolean := True;  --  Bild gespeichert?
    aspect : Real    := 1.0;   --  Faktoren und Aspect-ratio um
                               --  Welt- in Bildschirmkoordinaten umzurechnen
    total,
    total_hidden,
    picked,
    picked_hidden : Natural := 0;    --  total / picked objects, visible / hidden
    memo    : ptr_Obj_type := null;  --  pointer to a certain object (e.g. text to change)
    refresh : Refresh_mode;   --  9-May-2003: which sort of refresh is needed?

    ul_in_pt : Real;
    --  \unithlength expressed in pt, used to convert sizes not in unitlength
    --  e.g. rules from GNUPlot., writing lines with \multiput, etc.
    lw_in_pt : Real;
    --  \linewidth expressed in pt
  end record;

  type Insert_location is (at_begin, at_end);

  procedure Insert (p : in out Picture; t : ptr_Obj_type; where : Insert_location);

  --  Refresh_size_dependent_parameters (was: Set_pt_lengths), sets:
  --    - ul_in_pt, lw_in_pt
  --    - arrow directions (15-Jan-2004), midpoint (Feb-2004) in bezvec's
  procedure Refresh_size_dependent_parameters (p : in out Picture; objects : Boolean);

  function Max_radius (a : Obj_art_type; ul_in_pt : Real) return Real;

  generic
    with procedure Action (P : Point);
  procedure Bezier_curve (o : Obj_type; pt_scale : Real);

  generic
    with procedure Action (P : Point);
    with procedure Singularity;  --  Lift pen of drawing, for instance
  procedure Parametric_curve_2D (o : Obj_type; pt_scale : Real);

  procedure Get_slope (df : Point; sl : out LaTeX_slope; vector : Boolean);
  procedure Set_slope_of_bezvec (o : in out Obj_type; pt_scale : Real);
  --  ^ finds the arrow slope(s) a bezvec, sets midpoint for arrow at middle

  procedure Set_slope_of_linvec (o : in out Obj_type);
  --  ^ finds len; if any_slope: finds h_slope, v_slope; corrects x2,y2

  --  10-Mar-2004: improvement of plain lines
  --    1/ changes to \line, \vector when possible
  --    2/ changes to any_slope when \line is too short - see LineThrs.tcp
  --    (1/ is for shorter output, 2/ ensures display)
  procedure Improve_linvec (o : in out Obj_type; ul_pt : Real);

  procedure Set_radius (o : in out Obj_type; df : Point);
  --  ^ finds radius

  --  "Good" number of points for a \bezier ; \qbezier has an automatism
  function Good_num_of_bezier_points (o : Obj_type; pt_scale : Real) return Positive;
  --  Set control points with extrema points on curve
  procedure Set_control_point (o : in out Obj_type; TG : Point);

  procedure Delete_object_list (root : in out ptr_Obj_type);

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

  function Sty_title (s : Supposing_sty) return String;

  --  A few algorithms from epic.sty.
  --  Enhancements to Picture Environment. Version 1.2 - Released June 1, 1986
  --  by Sunil Podar, Dept. of Computer Science, SUNY at Stony Brook, NY 11794.

  package epic_calc is
    function Num_segments (D_lta : Point; dotgap : Real) return Positive;
    --  For dotgap = 0.0 (<=> no parameter), 1 is taken.
  end epic_calc;

private

  function Evaluate_param_curve_2D (o : Obj_type; t : Real) return Point;

end TC;
