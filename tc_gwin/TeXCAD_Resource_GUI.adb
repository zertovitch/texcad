---------------------------------------------------------------------------
--  GUI contents of resource script file: TeXCAD.rc
--  Transcription time: 2024/09/28  20:01:44
--  GWenerator project file: TeXCAD.gwen
--
--  Translated by the RC2GW or by the GWenerator tool.
--  URL: http://sf.net/projects/gnavi
--
--  This file contains only automatically generated code. Do not edit this.
--  Rework the resource script instead, and re-run the translator.
--  RC Grammar version: 29-Jul-2022
---------------------------------------------------------------------------

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.GStrings;                 use GWindows.GStrings;
with System;

pragma Warnings ("U");  --  turn off warnings for unused entity

package body TeXCAD_Resource_GUI is

  --  ** Generated code begins here \/ \/ \/.

  --  Dialog at resource line 67

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Cleanup_Dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "CLEANUP";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h : Integer;
  begin
    Dlg_to_Scn (0, 0, 298, 110, x, y, w, h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog
     (Window => Window_Type (Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then  Client_Area_Width (Window, w); end if;
    if Height = Use_Default then Client_Area_Height (Window, h); end if;
    Use_GUI_Font (Window);
    Create_Contents (Window, True);
  end Create_Full_Dialog;  --  Cleanup_Dialog_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Cleanup_Dialog_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     )
  is
    x, y, w, h : Integer;
  begin
    if resize then
    Dlg_to_Scn (0, 0, 298, 110, x, y, w, h);
      Move (Window, x, y);
      Client_Area_Size (Window, w, h);
    end if;
    Use_GUI_Font (Window);
    Dlg_to_Scn (237, 90, 50, 14, x, y, w, h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "CANCEL", x, y, w, h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "CANCEL", x, y, w, h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
    Dlg_to_Scn (117, 90, 114, 14, x, y, w, h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "&CLEANUP SELECTED", x, y, w, h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "&CLEANUP SELECTED", x, y, w, h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn (9, 7, 277, 77, x, y, w, h);
    Create (Window.Detection_List, Window, x, y, w, h, Multiple, Report_View, No_Sorting, False, Align_Left);
  end Create_Contents;  --  Cleanup_Dialog_Type

  --  Dialog at resource line 78

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Param_Curve_2D_Dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "2D PARAMETRIC CURVE";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h : Integer;
  begin
    Dlg_to_Scn (0, 0, 331, 118, x, y, w, h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
    Create_As_Dialog
     (Window => Window_Type (Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then  Client_Area_Width (Window, w); end if;
    if Height = Use_Default then Client_Area_Height (Window, h); end if;
    Use_GUI_Font (Window);
    Create_Contents (Window, True);
  end Create_Full_Dialog;  --  Param_Curve_2D_Dialog_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Param_Curve_2D_Dialog_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     )
  is
    x, y, w, h : Integer;
  begin
    if resize then
    Dlg_to_Scn (0, 0, 331, 118, x, y, w, h);
      Move (Window, x, y);
      Client_Area_Size (Window, w, h);
    end if;
    Use_GUI_Font (Window);
    Dlg_to_Scn (8, 12, 15, 12, x, y, w, h);
    Create_Label (Window, "x(t)", x, y, w, h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn (8, 31, 15, 12, x, y, w, h);
    Create_Label (Window, "y(t)", x, y, w, h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn (26, 10, 297, 15, x, y, w, h);
    Create (Window.X_Form_Box, Window, "", x, y, w, h, Horizontal_Scroll => True, Read_Only => False, ID => X_Form_Box);
    Dlg_to_Scn (26, 29, 297, 15, x, y, w, h);
    Create (Window.Y_Form_Box, Window, "", x, y, w, h, Horizontal_Scroll => True, Read_Only => False, ID => Y_Form_Box);
    Dlg_to_Scn (14, 52, 25, 13, x, y, w, h);
    Create_Label (Window, "t_min", x, y, w, h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn (14, 71, 25, 13, x, y, w, h);
    Create_Label (Window, "t_max", x, y, w, h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn (48, 50, 70, 14, x, y, w, h);
    Create (Window.T_Min_Box, Window, "", x, y, w, h, Horizontal_Scroll => True, Read_Only => False, ID => T_Min_Box);
    Dlg_to_Scn (48, 69, 70, 14, x, y, w, h);
    Create (Window.T_Max_Box, Window, "", x, y, w, h, Horizontal_Scroll => True, Read_Only => False, ID => T_Max_Box);
    Dlg_to_Scn (126, 52, 104, 10, x, y, w, h);
    Create (Window.Segments_Label, Window, "SEGMENTS (0=auto)", x, y, w, h, GWindows.Static_Controls.Right, None, ID => Segments_Label);
    Dlg_to_Scn (239, 50, 37, 12, x, y, w, h);
    Create (Window.Segments_Box, Window, "", x, y, w, h, Horizontal_Scroll => True, Read_Only => False, ID => Segments_Box);
    Dlg_to_Scn (126, 70, 104, 10, x, y, w, h);
    Create (Window.Scale_Label, Window, "SCALE (0=auto)", x, y, w, h, GWindows.Static_Controls.Right, None, ID => Scale_Label);
    Dlg_to_Scn (239, 68, 37, 12, x, y, w, h);
    Create (Window.Scale_Box, Window, "", x, y, w, h, Horizontal_Scroll => True, Read_Only => False, ID => Scale_Box);
    Dlg_to_Scn (210, 99, 50, 14, x, y, w, h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "O&K", x, y, w, h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "O&K", x, y, w, h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn (267, 99, 50, 14, x, y, w, h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "Cancel", x, y, w, h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "Cancel", x, y, w, h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
  end Create_Contents;  --  Param_Curve_2D_Dialog_Type

  --  ** Generated code ends here /\ /\ /\.

  --  ** Some helper utilities (body).

  procedure Dlg_to_Scn (  --  Converts dialog coords to screen (pixel) coordinates.
    xd, yd, wd, hd :  in Integer;
    xs, ys, ws, hs : out Integer)
  is
    --  function GetDialogBaseUnits return Integer;
    --  pragma Import (StdCall, GetDialogBaseUnits, "GetDialogBaseUnits");
    --  baseunit, baseunitX, baseunitY: Integer;
    baseunitX : constant := 6;
    baseunitY : constant := 13;
  begin
    --  baseunit := GetDialogBaseUnits; -- this gives X=8, Y=16 (SYSTEM font)
    --  baseunitX := baseunit mod (2 ** 16);
    --  baseunitY := baseunit  / (2 ** 16);
    --  NB: the other way with MapDialogRect works only
    --    by full moon, hence the user-defined units.
    xs := (xd * baseunitX) / 4;
    ws := (wd * baseunitX) / 4;
    ys := (yd * baseunitY) / 8;
    hs := (hd * baseunitY) / 8;
  end Dlg_to_Scn;

  package Common_Fonts is
    GUI_Font : GWindows.Drawing_Objects.Font_Type;
    URL_Font : GWindows.Drawing_Objects.Font_Type;
    --  ^ These fonts are created once, at startup
    --    it avoid GUI resource leak under Windows 95/98/ME
    procedure Create_Common_Fonts;
    --  in initialisation part if this pkg becomes standalone
  end Common_Fonts;

  procedure Use_GUI_Font (Window : in out GWindows.Base.Base_Window_Type'Class)
  is
  begin
    --  Use Standard Windows GUI font instead of system font
    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);
  end Use_GUI_Font;

  function Num_resource (id : Natural) return GString is
    img : constant String := Integer'Image (id);
  begin
    return To_GString_From_String ('#' & img (img'First + 1 .. img'Last));
  end Num_resource;

  package body Common_Fonts is

    procedure Create_Common_Fonts is

     type Face_Name_Type is array (1 .. 32) of GWindows.GChar_C;

     type LOGFONT is record
       lfHeight         : Interfaces.C.long;
       lfWidth          : Interfaces.C.long;
       lfEscapement     : Interfaces.C.long;
       lfOrientation    : Interfaces.C.long;
       lfWeight         : Interfaces.C.long;
       lfItalic         : Interfaces.C.char;
       lfUnderline      : Interfaces.C.char;
       lfStrikeOut      : Interfaces.C.char;
       lfCharSet        : Interfaces.C.char;
       lfOutPrecision   : Interfaces.C.char;
       lfClipPrecision  : Interfaces.C.char;
       lfQuality        : Interfaces.C.char;
       lfPitchAndFamily : Interfaces.C.char;
       lfFaceName       : Face_Name_Type;
     end record;

     Log_of_current_font : aliased LOGFONT;

     subtype PVOID   is System.Address;                      --  winnt.h
     subtype LPVOID  is PVOID;                               --  windef.h

     function GetObject
       (hgdiobj   : GWindows.Types.Handle := GWindows.Drawing_Objects.Handle (GUI_Font);
        cbBufferl : Interfaces.C.int      := LOGFONT'Size / 8;
        lpvObject : LPVOID                := Log_of_current_font'Address)
       return Interfaces.C.int;
     pragma Import (StdCall, GetObject,
                      "GetObject" & Character_Mode_Identifier);

     function CreateFontIndirect
       (lpvObject : LPVOID                := Log_of_current_font'Address)
       return GWindows.Types.Handle;
     pragma Import (StdCall, CreateFontIndirect,
                      "CreateFontIndirect" & Character_Mode_Identifier);

    begin
      GWindows.Drawing_Objects.Create_Stock_Font
        (GUI_Font,
         GWindows.Drawing_Objects.Default_GUI);
      if GetObject = 0 then
        GWindows.Drawing_Objects.Create_Font (URL_Font,
          "MS Sans Serif",
          14, Underline => True);
            --  !! ^ Not so nice (non-unsharpened font, size ~..., color ?)
      else
        Log_of_current_font.lfUnderline := Interfaces.C.char'Val (1);
        GWindows.Drawing_Objects.Handle (URL_Font, CreateFontIndirect);
      end if;
    end Create_Common_Fonts;

  end Common_Fonts;

begin
  Common_Fonts.Create_Common_Fonts;

  --  Last line of resource script file: 149

end TeXCAD_Resource_GUI;
