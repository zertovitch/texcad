---------------------------------------------------------------------------
-- GUI contents of resource script file: TeXCAD.rc
-- Transcription time: 2015/09/05  12:49:14
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 05-Apr-2015
---------------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;
with GWindows.Buttons.Owner_drawn;      use GWindows.Buttons.Owner_drawn;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;
with Interfaces.C;                      use Interfaces.C;

package TeXCAD_Resource_GUI is

  type Cleanup_Dialog_Type is new Window_type with record

    IDCANCEL: Dialog_Button_Type;    -- closes parent window after click
    IDCANCEL_permanent: Button_Type; -- doesn't close parent window after click
    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
    Detection_List: List_View_Control_Type;
  end record; -- Cleanup_Dialog_Type

  -- Dialog at resource line 71

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Cleanup_Dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "CLEANUP";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Cleanup_Dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );

  type Param_Curve_2D_Dialog_Type is new Window_type with record

    -- Label: 0
    -- Label: 0
    X_Form_Box: Edit_Box_Type;
    Y_Form_Box: Edit_Box_Type;
    -- Label: 0
    -- Label: 0
    T_Min_Box: Edit_Box_Type;
    T_Max_Box: Edit_Box_Type;
    Segments_Label: Label_Type;
    Segments_Box: Edit_Box_Type;
    Scale_Label: Label_Type;
    Scale_Box: Edit_Box_Type;
    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
    IDCANCEL: Dialog_Button_Type;    -- closes parent window after click
    IDCANCEL_permanent: Button_Type; -- doesn't close parent window after click
  end record; -- Param_Curve_2D_Dialog_Type

  -- Dialog at resource line 93

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Param_Curve_2D_Dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "2D PARAMETRIC CURVE";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Param_Curve_2D_Dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );

  package Version_info is
    Comments: constant String:= "TeXCAD is a program for drawing or retouching {picture}s in LaTeX.";
    CompanyName: constant String:= "Free Software Foundation, Inc.";
    Authors: constant String:= "Georg Horn, Jörn Winkelmann, Gautier de Montmollin";
    FileDescription: constant String:= "TeXCAD, a LaTeX {picture} drawing program";
    FileVersion: constant String:= "Ver. 4.3";
    InternalName: constant String:= "TeXCAD";
    LegalCopyright: constant String:= "© 2003 .. 2015 Free Software Foundation";
    OriginalFilename: constant String:= "TeXCAD.exe";
    ProductName: constant String:= "TeXCAD";
    ProductVersion: constant String:= "4.3";
    Translation: constant:= 1033;
  end Version_info;


  ------------------------------------------------
  -- Defined resource symbols --> Ada constants --
  ------------------------------------------------

  -- NB: only items with a defined symbol get a constant here
  -- These constants are needed for getting button and menu feedbacks.

  IDC_STATIC           : constant:=     -1;
  Cleanup_Dialog       : constant:=    101;
  Param_Curve_2D_Dialog: constant:=    102;
  X_Form_Box           : constant:=  40000;
  Y_Form_Box           : constant:=  40001;
  Detection_List       : constant:=  40002;
  T_Max_Box            : constant:=  40002;
  T_Min_Box            : constant:=  40003;
  Segments_Label       : constant:=  40004;
  Segments_Box         : constant:=  40005;
  Scale_Label          : constant:=  40006;
  Scale_Box            : constant:=  40007;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return GString;  --  Just turn 123 into "#123".


  -- Last line of resource script file: 148

end TeXCAD_Resource_GUI;
