---------------------------------------------------------------------------
-- GUI contents of resource script file: TeXCAD.rc
-- Transcription time: 2013/03/28   14:37:32
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 25-Nov-2012
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

  package Version_info is
    Comments: constant String:= "TeXCAD is a program for drawing or retouching {picture}s in LaTeX.";
    CompanyName: constant String:= "Free Software Foundation, Inc.";
    Authors: constant String:= "Georg Horn, Jörn Winkelmann, Gautier de Montmollin";
    FileDescription: constant String:= "TeXCAD, a LaTeX {picture} drawing program";
    FileVersion: constant String:= "Ver. 4.3 (rev. a34)";
    InternalName: constant String:= "TeXCAD";
    LegalCopyright: constant String:= "© 2003 .. 2013 Free Software Foundation";
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

  IDC_STATIC             : constant:=     -1;
  ID_FILE_NEW            : constant:=  57600;
  ID_FILE_OPEN           : constant:=  57601;
  ID_FILE_CLOSE          : constant:=  57602;
  ID_FILE_SAVE           : constant:=  57603;
  ID_FILE_SAVE_AS        : constant:=  57604;
  ID_FILE_PAGE_SETUP     : constant:=  57605;
  ID_FILE_PRINT_SETUP    : constant:=  57606;
  ID_FILE_PRINT          : constant:=  57607;
  ID_FILE_PRINT_DIRECT   : constant:=  57608;
  ID_FILE_PRINT_PREVIEW  : constant:=  57609;
  ID_FILE_UPDATE         : constant:=  57610;
  ID_FILE_SAVE_COPY_AS   : constant:=  57611;
  ID_FILE_SEND_MAIL      : constant:=  57612;
  ID_FILE_MRU_FIRST      : constant:=  57616;
  ID_FILE_MRU_FILE1      : constant:=  57616;
  ID_FILE_MRU_FILE2      : constant:=  57617;
  ID_FILE_MRU_FILE3      : constant:=  57618;
  ID_FILE_MRU_FILE4      : constant:=  57619;
  ID_FILE_MRU_FILE5      : constant:=  57620;
  ID_FILE_MRU_FILE6      : constant:=  57621;
  ID_FILE_MRU_FILE7      : constant:=  57622;
  ID_FILE_MRU_FILE8      : constant:=  57623;
  ID_FILE_MRU_FILE9      : constant:=  57624;
  ID_FILE_MRU_FILE10     : constant:=  57625;
  ID_FILE_MRU_FILE11     : constant:=  57626;
  ID_FILE_MRU_FILE12     : constant:=  57627;
  ID_FILE_MRU_FILE13     : constant:=  57628;
  ID_FILE_MRU_FILE14     : constant:=  57629;
  ID_FILE_MRU_FILE15     : constant:=  57630;
  ID_FILE_MRU_FILE16     : constant:=  57631;
  ID_FILE_MRU_LAST       : constant:=  57631;
  ID_EDIT_CLEAR          : constant:=  57632;
  ID_EDIT_CLEAR_ALL      : constant:=  57633;
  ID_EDIT_COPY           : constant:=  57634;
  ID_EDIT_CUT            : constant:=  57635;
  ID_EDIT_FIND           : constant:=  57636;
  ID_EDIT_PASTE          : constant:=  57637;
  ID_EDIT_PASTE_LINK     : constant:=  57638;
  ID_EDIT_PASTE_SPECIAL  : constant:=  57639;
  ID_EDIT_REPEAT         : constant:=  57640;
  ID_EDIT_REPLACE        : constant:=  57641;
  ID_EDIT_SELECT_ALL     : constant:=  57642;
  ID_EDIT_UNDO           : constant:=  57643;
  ID_EDIT_REDO           : constant:=  57644;
  ID_WINDOW_NEW          : constant:=  57648;
  ID_WINDOW_ARRANGE      : constant:=  57649;
  ID_WINDOW_CASCADE      : constant:=  57650;
  ID_WINDOW_TILE_HORZ    : constant:=  57651;
  ID_WINDOW_TILE_VERT    : constant:=  57652;
  ID_WINDOW_SPLIT        : constant:=  57653;
  ID_WINDOW_CLOSE_ALL    : constant:=  57654;
  ID_APP_ABOUT           : constant:=  57664;
  ID_APP_EXIT            : constant:=  57665;
  ID_HELP_INDEX          : constant:=  57666;
  ID_HELP_FINDER         : constant:=  57667;
  ID_HELP_USING          : constant:=  57668;
  ID_CONTEXT_HELP        : constant:=  57669;
  ID_HELP                : constant:=  57670;
  ID_DEFAULT_HELP        : constant:=  57671;
  ID_NEXT_PANE           : constant:=  57680;
  ID_PREV_PANE           : constant:=  57681;
  ID_FORMAT_FONT         : constant:=  57696;
  ID_OLE_INSERT_NEW      : constant:=  57856;
  ID_OLE_EDIT_LINKS      : constant:=  57857;
  ID_OLE_EDIT_CONVERT    : constant:=  57858;
  ID_OLE_EDIT_CHANGE_ICON: constant:=  57859;
  ID_OLE_EDIT_PROPERTIES : constant:=  57860;
  ID_OLE_VERB_FIRST      : constant:=  57872;
  ID_OLE_VERB_LAST       : constant:=  57887;
  ID_VIEW_TOOLBAR        : constant:=  59392;
  ID_VIEW_STATUS_BAR     : constant:=  59393;
  ID_VIEW_REBAR          : constant:=  59396;
  ID_VIEW_AUTOARRANGE    : constant:=  59397;
  ID_VIEW_SMALLICON      : constant:=  59408;
  ID_VIEW_LARGEICON      : constant:=  59409;
  ID_VIEW_LIST           : constant:=  59410;
  ID_VIEW_DETAILS        : constant:=  59411;
  ID_VIEW_LINEUP         : constant:=  59412;
  ID_VIEW_BYNAME         : constant:=  59413;
  ID_RECORD_FIRST        : constant:=  59648;
  ID_RECORD_LAST         : constant:=  59649;
  ID_RECORD_NEXT         : constant:=  59650;
  ID_RECORD_PREV         : constant:=  59651;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return GString;


  -- Last line of resource script file: 106

end TeXCAD_Resource_GUI;
