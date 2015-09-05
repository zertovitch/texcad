with GWin_Util;                         use GWin_Util;

with TC.GWin.Lang;                      use TC.GWin.Lang;
with TC.GWin.MDI_Main;

pragma Elaborate(TC.Gwin); -- Windows_95 flag needed

with Interfaces.C;

package body TC.GWin.Toolbars is

  use Floating_toolbars, GWindows.Image_Lists, Interfaces.C;

  procedure Add_Button
    (Control     : in out GUI_toolbar'Class;
     Image_Index : in     Natural;
     Command     : in     Custom_cmd)
  is
    i: constant Integer:= ID_custom(Command);
    s: constant GString:= Msg(msg_for_command(Command));
  begin
    -- The tool tip is the menu text.
    Control.Add_String(s);
    Control.Add_button(Image_Index, i, Control.string_count);
    Control.string_count:= Control.string_count + 1;
  end Add_Button;

  TBSTYLE_TOOLTIPS            : constant:= 16#00000100#;
  TBSTYLE_FLAT                : constant:= 16#00000800#;
  TBSTYLE_LIST                : constant:= 16#00001000#;
  TBSTYLE_EX_MIXEDBUTTONS     : constant:= 16#00000008#;
  sep_w: constant:= 8;

  procedure Add_Button
    (Control     : in out Floating_Toolbar;
     Image_Index : in     Natural;
     Command     : in     Custom_cmd)
  is
  begin
    Add_button(Control.bar, Image_Index, Command);
  end Add_Button;

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

    Create (il, "Toolbar_Bmp", 16);
    Set_Image_List (tb, il);
    st:= Get_Style(tb);
    Set_Style(tb, TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST or st);
    -- Attempted TBSTYLE_AUTOSIZE to stop flickering, in vain...
    Set_Extended_Style(tb, TBSTYLE_EX_MIXEDBUTTONS);

    Add_Button (tb,  0, ID_File_New);
    Add_Button (tb,  1, ID_File_Open);
    Add_Button (tb,  2, save);

    Add_Separator(tb, sep_w);

    Add_Button (tb, 13, pick_obj);
    Add_Button (tb,  3, cut_clip);
    Add_Button (tb,  4, copy_clip);
    Add_Button (tb,  5, paste_clip);

    Add_Separator(tb, sep_w);

    Add_Button (tb, 12, preview);
    Add_Button (tb,  9, zoom_plus);
    Add_Button (tb, 10, zoom_minus);

    Add_Separator(tb, sep_w);

    Add_Button (tb, 15, pic_opt_dialog);

    Add_Separator(tb, sep_w);

    Add_Button (tb, 14, ID_App_About); -- 7 = help

  end Init_Main_toolbar;

  procedure Reset_Drawing_toolbar(tb: in out Floating_toolbar) is
  begin
    Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST);
    Set_Extended_Style(tb.bar, TBSTYLE_EX_MIXEDBUTTONS);
    Dock(tb.bar, Gwindows.Base.At_Top);
    Create(tb.images, "Drawing_Toolbar_Bmp", 28);
    Set_Image_List(tb.bar, tb.images);
    -- Order: as in menu.
    for c in Drawing_cmd loop
      Add_Button(tb,
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
    Set_Style(tb.bar, TBSTYLE_WRAPABLE or TBSTYLE_FLAT or TBSTYLE_TOOLTIPS or TBSTYLE_LIST);
    Set_Extended_Style(tb.bar, TBSTYLE_EX_MIXEDBUTTONS);
    Dock(tb.bar, Gwindows.Base.At_Top);
    Create(tb.images, "Line_Toolbar_Bmp", 56);
    Set_Image_List(tb.bar, tb.images);
    -- Order: as in menu.
    for c in Line_setting_cmd loop
      Add_Button(tb,
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
      (TB_Drawing       => 28+6,
       TB_Line_settings => 56+6);

    max_height: constant array(Floating_toolbar_categ) of Integer:=
      (TB_Drawing       => 322+6,
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
            w => 20 + (28+8)*9,
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

