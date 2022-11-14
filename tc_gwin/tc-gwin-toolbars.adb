with GWin_Util;                         use GWin_Util;

with TC.GWin.Lang;                      use TC.GWin.Lang;
with TC.GWin.MDI_Main;

pragma Elaborate(TC.Gwin); -- Windows_95 flag needed

with Interfaces.C;

package body TC.GWin.Toolbars is

  use Floating_toolbars, GWindows.Image_Lists, Interfaces.C;

  procedure Add_Button_with_Tip
    (Control     : in out GUI_toolbar'Class;
     Image_Index : in     Natural;
     Command_ID  : in     Integer;
     Tip         : in     GString)
  is
  begin
    Control.Add_String(Tip);
    Control.Add_button(Image_Index, Command_ID, Control.string_count);
    Control.string_count:= Control.string_count + 1;
  end Add_Button_with_Tip;

  procedure Add_Button_with_Tip
    (Control     : in out GUI_toolbar'Class;
     Image_Index : in     Natural;
     Cmd_enu     : in     Custom_cmd)
  is
    function Extended_Message return GString is
      menu_msg : constant GString := Msg (msg_for_command (Cmd_enu));
      keys     : constant GString := Keyboard_Shortcut (Cmd_enu);
    begin
      if keys = "" then
        return menu_msg;
      else
        --  "GNAT-Studio-style" toolbar label.
        return menu_msg & NL & NL & "Shortcut: " & keys;
      end if;
    end Extended_Message;
  begin
    Add_Button_with_Tip
      (Control, Image_Index, ID_custom (Cmd_enu), Extended_Message);
  end Add_Button_with_Tip;

  procedure Add_Button_for_Floating
    (Control     : in out Floating_Toolbar;
     Image_Index : in     Natural;
     Cmd_enu     : in     Custom_cmd)
  is
  begin
    Control.bar.Add_Button(Image_Index, ID_custom(Cmd_enu));
  end Add_Button_for_Floating;

  TBSTYLE_TOOLTIPS            : constant:= 16#00000100#;
  TBSTYLE_FLAT                : constant:= 16#00000800#;
  TBSTYLE_LIST                : constant:= 16#00001000#;
  TBSTYLE_EX_MIXEDBUTTONS     : constant:= 16#00000008#;
  sep_w: constant:= 8;

  procedure Init_Main_toolbar(
    tb    : in out Floating_toolbars.GUI_toolbar'Class;
    il    : in out GWindows.Image_Lists.Image_List_Type;
    parent: in out GWindows.Base.Base_Window_Type'Class)
  is
--    use GWindows.Common_Controls;
    st: Interfaces.C.unsigned;
  begin
    Create (tb, parent, 0, 0, 0, 40);
    Dock (tb, GWindows.Base.At_Top);

    Create (il, "Toolbar_Bmp", 32);
    Set_Image_List (tb, il);
    st:= Get_Style(tb);
    Set_Style(tb, TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST or st);
    -- Attempted TBSTYLE_AUTOSIZE to stop flickering, in vain...
    Set_Extended_Style(tb, TBSTYLE_EX_MIXEDBUTTONS);

    Add_Button_with_Tip (tb,  0, ID_File_New, Msg(fnew));
    Add_Button_with_Tip (tb,  1, ID_File_Open, Msg(fopen));
    Add_Button_with_Tip (tb,  2, save);

    Add_Separator(tb, sep_w);

    Add_Button_with_Tip (tb, 13, pick_obj);
    Add_Button_with_Tip (tb, 18, change_text);
    Add_Button_with_Tip (tb,  3, cut_clip);
    Add_Button_with_Tip (tb,  4, copy_clip);
    Add_Button_with_Tip (tb,  5, paste_clip);

    Add_Separator (tb, sep_w);

    Add_Button_with_Tip (tb, 19, tc_undo);
    Add_Button_with_Tip (tb, 20, tc_redo);

    Add_Separator (tb, sep_w);

    Add_Button_with_Tip (tb, 12, preview);
    Add_Button_with_Tip (tb,  9, zoom_plus);
    Add_Button_with_Tip (tb, 10, zoom_minus);

    Add_Separator(tb, sep_w);

    Add_Button_with_Tip (tb, 15, pic_opt_dialog);

    Add_Separator(tb, sep_w);

    Add_Button_with_Tip (tb, 14, ID_App_About, Msg(habout));

  end Init_Main_toolbar;

  procedure Reset_Drawing_toolbar(tb: in out Floating_toolbar) is
  begin
    Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT);
    -- Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST);
    -- Set_Extended_Style(tb.bar, TBSTYLE_EX_MIXEDBUTTONS);
    -- The wrapping with tool tip doesn't work (Windows 7)
    Dock(tb.bar, Gwindows.Base.At_Top);
    Create(tb.images, "Drawing_Toolbar_Bmp", 28);
    Set_Image_List(tb.bar, tb.images);
    -- Order: as in menu.
    for c in Drawing_cmd loop
      Add_Button_for_Floating(tb,
        Custom_cmd'Pos(c) - Custom_cmd'Pos(Drawing_cmd'First), -- 0,1,2,...
        c
      );
      if c = put or c = filled_circle then
        Add_Separator(tb.bar,10);
      end if;
    end loop;
  end Reset_Drawing_toolbar;

  procedure Reset_Line_toolbar(tb: in out Floating_toolbar) is
  begin
    Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT);
    -- Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST);
    -- Set_Extended_Style(tb.bar, TBSTYLE_EX_MIXEDBUTTONS);
    -- The wrapping with tool tip doesn't work (Windows 7)
    Dock(tb.bar, Gwindows.Base.At_Top);
    Create(tb.images, "Line_Toolbar_Bmp", 56);
    Set_Image_List(tb.bar, tb.images);
    -- Order: as in menu.
    for c in Line_setting_cmd loop
      Add_Button_for_Floating(tb,
        Custom_cmd'Pos(c) - Custom_cmd'Pos(Line_setting_cmd'First),
        c
      );
      if c = thick or c = dash_param then
        Add_Separator(tb.bar,16);
      end if;
    end loop;
  end Reset_Line_toolbar;

  procedure Notify_any_toolbar(tb: in Floating_toolbar) is
    use TC.GWin.MDI_Main;
  begin
    if tb.parent.all in MDI_Main_Type'Class then
      Update_Common_Menus(MDI_Main_Type(tb.parent.all));
    end if;
  end Notify_any_toolbar;

  procedure Init_Floating_toolbars(
    tbs   : in out Floating_toolbar_array;
    parent: in out GWindows.Base.Base_Window_Type'Class)
  is
    min_width: constant array(Floating_toolbar_categ) of Integer:=
      (TB_Drawing       => 28+7,
       TB_Line_settings => 56+7);

    max_height: constant array(Floating_toolbar_categ) of Integer:=
      (TB_Drawing       => 322+28+12,
       TB_Line_settings => 224+8);

    title: constant array(Floating_toolbar_categ) of Message:=
      (TB_Drawing       => vtogdtb,
       TB_Line_settings => vtogltb);

  begin
    for cat in tbs'Range loop
      Create(tbs(cat), parent, Filter_Amp(Msg(title(cat))),
        TC_FT_memo(cat).geom.l,
        TC_FT_memo(cat).geom.t,
        TC_FT_memo(cat).geom.w,
        min_width(cat),
        TC_FT_memo(cat).geom.h,
        max_height(cat),
        Notify_any_toolbar'Access
      );
    end loop;
    Reset_Drawing_toolbar(tbs(TB_Drawing));
    Reset_Line_toolbar(tbs(TB_Line_settings));
    for cat in tbs'Range loop
      Change_status(tbs(cat), TC_FT_memo(cat).stat);
    end loop;
  end Init_Floating_toolbars;

  default_stat: Floating_TB_status;

begin
  if Windows_95 then
    default_stat:= invisible; -- Button sizes are wrong (16x16)
  else
    default_stat:= windowed;  -- Win 98, 2K, XP, Vista are OK
  end if;

  TC_FT_memo:=
    ( TB_Drawing =>
        ( geom =>
           (l => 310,
            t => 2,
            w => 20 + (28+8)*10,
            h => 40 + 28),
          stat => default_stat
        ),
      TB_Line_settings=>
        ( geom =>
           (l => 727,
            t => 2,
            w => 20 + (56+8)*5,
            h => 20 + 12*3),
          stat => default_stat
        )
     );
end TC.GWin.Toolbars;

