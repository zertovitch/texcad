--        ________          _   ____   ____  ___
--           /    ____  \ _/   /      /   / /   \
--          /    /___/  _\    /      /---/ /    /
--         /    /___   /  \  /____  /   / /____/

----------------------------------------------------
-- Root package of the MS Windows hull for TeXCAD --
----------------------------------------------------

with GWindows;                          use GWindows;
with GWindows.Constants;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Colors;                   use GWindows.Colors;

with Standard_IDs;                      use Standard_IDs;

with Floating_toolbars;

pragma Elaborate_All(GWindows.GStrings); -- "To_GString_Unbounded"

package TC.GWin is

  pragma Elaborate_Body(TC.GWin);

  use TC.Graphics;

  function S2G (Value : String) return GString renames To_GString_From_String;
  function G2S (Value : GString) return String renames To_String;
  function GU2G (Value : GString_Unbounded) return GString renames To_GString_From_Unbounded;
  function G2GU (Value : GString) return GString_Unbounded renames To_GString_Unbounded;

  type Color_Set is array( Color_zone ) of Color_Type;

  color: Color_Set:=
    ( background => White,
      normal     => Black,
      picked     => Red,
      shadow     => Light_Gray );

  -- Main window dimensions:
  wleft, wtop, wheight, wwidth: Integer:= GWindows.Constants.Use_Default;
  wmaxi: Boolean:= True;

  -- Under MS Windows, the children are either all maximized, or none.
  MDI_childen_maximized: Boolean:= True;
  -- We are only interested in user-caused maximize-restore operations
  user_maximize_restore: Boolean:= True;

  -- MRU (Most recently used) files:
  mru: array(1..9) of GString_Unbounded:=
    (others=> To_GString_Unbounded(""));

  procedure Add_MRU( name: GString );

  function Shorten_filename( s: GString ) return GString;

  ----------------
  -- Commands : --
  ----------------

  type Custom_cmd is
    ( -- Drawing (permanent):
      text,
      put,
      line,
      framebox, filled_box,
      oval,
      circle, filled_circle,
      bez,
      par_cur_2d_cmd,
      -- Editing (permanent):
      pick_obj,
      change_text,
      translate, mirror, rotate, homoth,
      delete, cut_clip, copy_clip, save_macro,
      unselect, select_all,
      tc_undo, tc_redo,
      paste_clip, load_macro,
      -- Line settings:
      thin, thick,
      plain, dot, dot_param, dash, dash_param,
      no_arrow, head, both, middle,
      -- Misc.:
      pic_opt_dialog,
      zoom_plus, zoom_minus,
      clean_pic,
      preview,
      save, save_as, close,
      -- Only for MDI main:
      gen_opt_dialog,
      TB_Drawing, TB_Line_settings,
      mru1, mru2, mru3, mru4, mru5, mru6, mru7, mru8, mru9
    );

  subtype Drawing_cmd is Custom_cmd range text .. par_cur_2d_cmd;
  -- ^ All drawing commands
  subtype Box_cmd is Custom_cmd range framebox .. filled_box;
  -- ^ Box-type drawing commands
  subtype Permanent_cmd is Custom_cmd range text .. homoth;
  -- ^ All drawing commands, plus the pick_obj and deformations
  subtype Line_setting_cmd is Custom_cmd range thin .. middle;
  -- ^ Line Settings
  subtype Line_thickness_cmd is Line_setting_cmd range thin .. thick;
  -- ^ Line Settings: thickness
  subtype Line_pattern_cmd is Line_setting_cmd range plain .. dash_param;
  -- ^ Line Settings: pattern
  subtype Line_arrows_cmd is Line_setting_cmd range no_arrow .. middle;
  -- ^ Line Settings: arrows
  subtype Deformation_cmd is Custom_cmd range translate .. homoth;
  -- ^ All deformations commands (requires a previous picking)
  subtype Permanent_direct_cmd is Custom_cmd range text .. change_text;
  -- ^ All permanent commands not involving picked objects
  subtype Select_cmd is Custom_cmd range unselect .. select_all;
  -- ^ All select commands, minus the pick_obj
  subtype Action_on_picked_cmd is Custom_cmd range translate .. save_macro;
  -- ^ All commands requiring a previous picking
  subtype Removes_picked_cmd is Action_on_picked_cmd range delete .. cut_clip;
  -- ^ All commands that end up removing all picked items

  subtype MDI_child_cmd is Custom_cmd range Custom_cmd'First .. close;
  -- Commands only for MDI child
  subtype MDI_main_cmd is Custom_cmd range gen_opt_dialog .. Custom_cmd'Last;
  -- Commands only for MDI mani

  ID_custom : array (Custom_cmd) of Integer; -- initialized by this package

  no_std_id : constant := -1; -- Trap to avoid unexpectedly activating a command

  --  These are standard Windows ID's that may do the same
  --  thing as ID_custom above. Used in On_Menu_Select.
  --  Probably it is not needed (those ID are not used for setting up
  --  menus or toolbars). Note: 04-Sep-2018.
  ID_std : constant array (Custom_cmd) of Integer:=
    (  select_all => ID_EDIT_SELECT_ALL,
       unselect   => ID_EDIT_CLEAR_ALL,
       delete     => ID_EDIT_CLEAR,
       copy_clip  => ID_EDIT_COPY,
       cut_clip   => ID_EDIT_CUT,
       paste_clip => ID_EDIT_PASTE,
       tc_undo    => ID_EDIT_UNDO,
       tc_redo    => ID_EDIT_REDO,
       save       => ID_FILE_SAVE,
       save_as    => ID_FILE_SAVE_AS,
       close      => ID_FILE_CLOSE,
       others     => no_std_id );

  function Keyboard_Shortcut (c : Custom_cmd) return GString;

  Windows_95: Boolean; -- has a bug with hatch brush

  -- Floating toolbar window dimensions and status:

  subtype Floating_toolbar_categ is Custom_cmd range TB_Drawing .. TB_Line_settings;
  type FT_memo is record
    geom: Floating_toolbars.LTWH_Rectangle;
    stat: Floating_toolbars.Floating_TB_status;
  end record;

  TC_FT_memo: array(Floating_toolbar_categ) of FT_memo;
  -- ^ Initialized by TC.GWin.Toolbars .

  type Floating_toolbar_array is
    array(Floating_toolbar_categ) of Floating_toolbars.Floating_toolbar;

end TC.GWin;
