with GWin_Util;
with GWindows.Image_Lists;

with TC.GWin.Lang;                      use TC.GWin.Lang;
with TC.GWin.MDI_Main;

pragma Elaborate (TC.Gwin);  --  Windows_95 flag needed

with Interfaces.C;

package body TC.GWin.Toolbars is

  use Floating_Toolbars, GWin_Util, Interfaces.C;

  procedure Add_Button_with_Tip
    (Control     : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class;
     Image_Index : in     Natural;
     Command_ID  : in     Integer;
     Tip         : in     GString)
  is
  begin
    Control.Add_String(Tip);
    Control.Add_Button(Image_Index, Command_ID, Control.String_Count);
    Control.String_Count:= Control.String_Count + 1;
  end Add_Button_with_Tip;

  procedure Add_Button_with_Tip
    (Control     : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class;
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

  procedure Init_Main_Tool_Bar
    (tb     : in out Office_Applications.Classic_Main_Tool_Bar_Type'Class;
     parent : in out GWindows.Base.Base_Window_Type'Class)
  is
    st : Interfaces.C.unsigned;
  begin
    tb.Create (parent, 0, 0, 0, 40);
    tb.Dock (GWindows.Base.At_Top);

    tb.Images.Create ("Toolbar_Bmp", 32);
    tb.Set_Image_List (tb.Images);
    st := tb.Get_Style;
    tb.Set_Style (TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST or st);
    -- Attempted TBSTYLE_AUTOSIZE to stop flickering, in vain...
    tb.Set_Extended_Style (TBSTYLE_EX_MIXEDBUTTONS);

    Add_Button_with_Tip (tb,  0, ID_FILE_NEW, Msg(fnew));
    Add_Button_with_Tip (tb,  1, ID_FILE_OPEN, Msg(fopen));
    Add_Button_with_Tip (tb,  2, save);

    tb.Add_Separator (sep_w);

    Add_Button_with_Tip (tb, 13, pick_obj);
    Add_Button_with_Tip (tb, 18, change_text);
    Add_Button_with_Tip (tb,  3, cut_clip);
    Add_Button_with_Tip (tb,  4, copy_clip);
    Add_Button_with_Tip (tb,  5, paste_clip);

    tb.Add_Separator (sep_w);

    Add_Button_with_Tip (tb, 19, tc_undo);
    Add_Button_with_Tip (tb, 20, tc_redo);

    tb.Add_Separator (sep_w);

    Add_Button_with_Tip (tb, 12, preview);
    Add_Button_with_Tip (tb,  9, zoom_plus);
    Add_Button_with_Tip (tb, 10, zoom_minus);

    tb.Add_Separator (sep_w);

    Add_Button_with_Tip (tb, 15, pic_opt_dialog);

    tb.Add_Separator (sep_w);

    Add_Button_with_Tip (tb, 14, ID_APP_ABOUT, Msg(habout));

  end Init_Main_Tool_Bar;

  procedure Reset_Drawing_toolbar(tb: in out Floating_Toolbar) is
  begin
    Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT);
    -- Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST);
    -- Set_Extended_Style(tb.bar, TBSTYLE_EX_MIXEDBUTTONS);
    -- The wrapping with tool tip doesn't work (Windows 7)
    Dock(tb.bar, GWindows.Base.At_Top);
    tb.bar.Images.Create ("Drawing_Toolbar_Bmp", 28);
    Set_Image_List (tb.bar, tb.bar.Images);
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

  procedure Reset_Line_toolbar(tb: in out Floating_Toolbar) is
  begin
    Set_Style (tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT);
    -- Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST);
    -- Set_Extended_Style(tb.bar, TBSTYLE_EX_MIXEDBUTTONS);
    -- The wrapping with tool tip doesn't work (Windows 7)
    Dock (tb.bar, GWindows.Base.At_Top);
    tb.bar.Images.Create ("Line_Toolbar_Bmp", 56);
    Set_Image_List (tb.bar, tb.bar.Images);
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

  procedure Notify_any_toolbar(tb: in Floating_Toolbar) is
    use TC.GWin.MDI_Main;
  begin
    if tb.parent.all in MDI_Main_Type'Class then
      Update_Common_Menus(MDI_Main_Type(tb.parent.all));
    end if;
  end Notify_any_toolbar;

  procedure Init_Floating_Tool_Bars
    (tbs    : in out Floating_toolbar_array;
     parent : in out GWindows.Base.Base_Window_Type'Class)
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
      Create(tbs(cat), parent, Filter_amp(Msg(title(cat))),
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
  end Init_Floating_Tool_Bars;

  default_stat: Floating_TB_Status;

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

