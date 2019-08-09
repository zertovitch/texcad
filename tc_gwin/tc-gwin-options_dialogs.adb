with TC.GWin.Lang;                      use TC.GWin.Lang;
with TC.GWin.MDI_Picture_Child;         use TC.GWin.MDI_Picture_Child;

with TC.GWin.Display,
     TC.GWin.Options;

with GWindows.Base;                     use GWindows.Base;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Common_Dialogs;           use GWindows.Common_Dialogs;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Windows;                  use GWindows.Windows;

with GWin_Util;                         use GWin_Util;

package body TC.GWin.Options_Dialogs is

  type Color_button is new Gwindows.Buttons.Button_Type with record
    z: Color_zone;
  end record;

  ------------------------------
  --  Special_Check_Box_Type  --
  ------------------------------

  type Special_Check_Box_Type is new Check_Box_Type with record
    label   : Label_Type;
    edit_box: Edit_Box_Type;
  end record;

  procedure On_Click (Button : in out Special_Check_Box_Type);

  procedure State (Button : in out Special_Check_Box_Type;
                   State  : in     Check_State_Type)
  is
  begin
    GWindows.Buttons.State(Check_Box_Type(Button),State);
    Enabled(Button.label,State=Checked);
    Enabled(Button.edit_box,State=Checked);
  end State;

  procedure On_Click (Button : in out Special_Check_Box_Type) is
  begin
    On_Click(Check_Box_Type(Button));
    State(Button, State(Button));
  end On_Click;

  ------------------------
  -- On_General_Options --
  ------------------------

  procedure On_General_Options (Window : in out TC.GWin.MDI_Main.MDI_Main_Type)
  is
    Pan                      : Window_Type;

    subtype Tab_subject is Lang.Message range
      gen_opt_tab_display .. gen_opt_tab_miscellaneous;
    package Tabbing is
      new GWin_Util.Property_Tabs_Package(Tab_subject,Lang.Msg,"O&K",Msg(mcancel));

    New_Pic_Opt              : Button_Type;
    Lang                     : Drop_Down_List_Box_Type;
    Result, i,x,y,yy         : Integer;
    D_tex_suff,
    D_mac_suff               : Edit_Box_Type;
    color_group, suffix_group: Group_Box_Type;
    bak_check_box            : Special_Check_Box_Type;
    g_group                  : Group_Box_Type;
    g_radio                  : array(Grid_Display) of Radio_Button_Type;
    redraw_again             : Boolean  := False;

    qbez_group : Group_Box_Type;
    q_radio    : array(Solid_Bezier_points_mode) of Radio_Button_Type;

    preview_group : Group_Box_Type;
    preview_radio : array(LaTeX_version) of Radio_Button_Type;
    latver: constant array(LaTeX_version) of String(1..5):=
      (v209 => "2.09 ", v2e => ">= 2e");
    preview_dir_group : Group_Box_Type;
    preview_dir_radio : array(Preview_directory_choice) of Radio_Button_Type;
    preview_dir_msg: constant array (Preview_directory_choice) of Message:=
      ( current   => mcurrent,
        temporary => mtemporary );

    wmax : constant:= 370;
    y0: constant:= 5;

    original  : constant TC.General_Options := gen_opt;
    candidate :          TC.General_Options := original;
    orig_col  : constant TC.GWin.Color_set  := color;

    procedure Get_Data ( pnl : in out Gwindows.Base.Base_Window_Type'Class ) is
      pragma Warnings(off,pnl);
    begin
      Candidate.tex_suff:= To_Unbounded_String(Text(d_tex_suff));
      Candidate.mac_suff:= To_Unbounded_String(Text(d_mac_suff));
      Candidate.bak_suff:= To_Unbounded_String(Text(bak_check_box.edit_box));
      candidate.bak_enabled:= State(bak_check_box) = Checked;
      for L in Language loop
        if Text(Lang) = Language_Rich_Image(L) then
          candidate.lang:= l;
          exit;
        end if;
      end loop;
      for g in Grid_Display loop
        if State(g_radio(g))=checked then
          if candidate.grid /= g then
            gen_opt.grid:= g;
            -- ^ On change aussi les options pour faire joli,
            --   mais original contient l'original
            redraw_again:= True;
            Window.Redraw_all;
            candidate.grid:= g;
          end if;
        end if;
      end loop;
      for q in Solid_Bezier_points_mode loop
        if State(q_radio(q))=checked then
          candidate.solid_bez:= q;
        end if;
      end loop;
      for l in LaTeX_version loop
        if State(preview_radio(l))=checked then
          candidate.preview_mode:= l;
        end if;
      end loop;
      for d in Preview_directory_choice loop
        if State(preview_dir_radio(d))=checked then
          candidate.preview_directory:= d;
        end if;
      end loop;
    exception
      when others =>
        Message_Box(
          pnl,
          "Invalid data", "Incomplete reading of your changes",
          OK_Box, Error_Icon);
    end Get_Data;

    procedure Do_Display (btn : in out Gwindows.Base.Base_Window_Type'Class)
    is
      modified: Boolean; -- dumped since not related to a specific picture
    begin
      On_Picture_Options(
        btn, Candidate.Options_For_New,
        window,
        modified,
        Msg(onewpicopt));
    end Do_Display;

    col_btn: array(Color_zone) of Color_button;

    procedure CColor (btn : in out Gwindows.Base.Base_Window_Type'Class ) is
      c : Color_Type;
      ok: Boolean;
      z : Color_zone;
    begin
      z:= Color_Button(btn).z;
      c:= color(z);
      Choose_Color( btn, c, ok );
      if ok and then color(z) /= c then
        color(z):= c;
        redraw_again:= True;
        TC.GWin.Display.recreate_drawing_objects:= True;
        Window.Redraw_all;
      end if;
    end CColor;

  begin
    Create_As_Dialog(Pan, Window, Msg(ogenopt), Width => wmax + 30, Height => 400);
    Center(Pan);
    Small_Icon (Pan, "Options_Icon");
    On_Destroy_Handler (Pan, Get_Data'Unrestricted_Access);

    GWin_Util.Use_Gui_Font(Pan);

    Tabbing.Create(pan);

    -- Misc tab / Suffix group --
    y:= y0;
    Create(suffix_group, Tabbing.tab(gen_opt_tab_miscellaneous), Msg(suffix),
      5,   y, wmax-5, 95);

    y:= y + 20;
    Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(Ltx_Pic),
      45,  y, 250, 20);
    Create (D_Tex_Suff, Tabbing.tab(gen_opt_tab_miscellaneous),
      To_GString_From_Unbounded(Candidate.Tex_Suff),
     300,  y, 50, 20);

    y:= y + 25;
    Create(bak_check_box.label, Tabbing.tab(gen_opt_tab_miscellaneous), Msg(Ltx_Pic_bak),
      45,  y, 250, 20);
    Create(bak_check_box.edit_box, Tabbing.tab(gen_opt_tab_miscellaneous),
      To_GString_From_Unbounded(Candidate.bak_suff),
     300,  y, 50, 20);
    Create(bak_check_box,Tabbing.tab(gen_opt_tab_miscellaneous), "",
      25,  y,  15, 15);
    State(bak_check_box, boolean_to_state(candidate.bak_enabled));

    y:= y + 25;
    Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(Tcd_Mac),
      45,  y, 250, 20);
    Create (D_Mac_Suff, Tabbing.tab(gen_opt_tab_miscellaneous),
      To_GString_From_Unbounded(Candidate.Mac_Suff),
     300,  y, 50, 20);

    -- Misc tab / Preview directory group
    y:= y + 35;
    Create(preview_dir_group, Tabbing.tab(gen_opt_tab_miscellaneous), Msg(preview_directory),
      5,   y, wmax-5, 65);
    for d in Preview_directory_choice loop
      x:= 10 + 180* Preview_directory_choice'Pos(d);
      Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(preview_dir_msg(d)),
        x,  y+40, 175, 22);
      Create(preview_dir_radio(d),Tabbing.tab(gen_opt_tab_miscellaneous), "",  x,  y+20,  60, 15);
      State(preview_dir_radio(d),boolean_to_state(d=candidate.preview_directory));
    end loop;

    -- Misc tab / Language ListBox
    y:= y + 70;
    Create_Label (Tabbing.tab(gen_opt_tab_miscellaneous), Msg(lng),
      10,  y, 150, 25);
    Create( Lang, Tabbing.tab(gen_opt_tab_miscellaneous),
     140,  y, 150, 200, Sort => False, Is_Dynamic => False);
    for L in Language loop
      Add( Lang, Language_Rich_Image(L) );
    end loop;
    Text(lang, Language_Rich_Image(candidate.lang));

    y:= y + 35;
    Create (New_Pic_Opt, Tabbing.tab(gen_opt_tab_miscellaneous), "&" & Msg(onewpicopt),
      10, y, 240, 30);
    On_Click_Handler (New_Pic_Opt, Do_Display'Unrestricted_Access);

    -- Display tab / Colours
    y:= y0;
    yy:= 30 * (2+Color_zone'Pos(Color_zone'Last));
    Create( color_group, Tabbing.tab(gen_opt_tab_display), Msg(gcolors), 5,   y, wmax-5, yy );

    for z in Color_zone loop
      i:= y + 30 * (Color_zone'Pos(z)+1);
      Create_Label(
        Tabbing.tab(gen_opt_tab_display),
        Msg(Message'Val(Message'Pos(background)+Color_zone'Pos(z))),
        25, i, 125, 25);
      Create (col_btn(z), Tabbing.tab(gen_opt_tab_display), Msg(choose), 190, i, 90, 22);
      On_Click_Handler (col_btn(z), CColor'Unrestricted_Access);
      col_btn(z).z:= z;
    end loop;

    -- Display tab / Grid
    y:= y + yy + 8;
    yy:= 65;
    Create(g_group, Tabbing.tab(gen_opt_tab_display), Msg(grid), 5, y, wmax-5, yy);
    for g in Grid_Display loop
      x:= 20 + 100* Grid_Display'Pos(g);
      Create_Label (Tabbing.tab(gen_opt_tab_display),
        Msg(message'Val(message'Pos(gridnone)+Grid_Display'Pos(g))),
        x,  y+40, 100, 22);
      Create(g_radio(g),Tabbing.tab(gen_opt_tab_display), "",  x,  y+20,  60, 15);
      On_Click_Handler(g_radio(g),Get_Data'Unrestricted_Access);
      State(g_radio(g),boolean_to_state(g=candidate.grid));
    end loop;

    -- LaTeX tab / Bezier group
    y:= y0;
    yy:= 65;
    Create(qbez_group, Tabbing.tab(gen_opt_tab_latex), Msg(bezpts), 5, y, wmax-5, yy);
    for q in Solid_Bezier_points_mode loop
      x:= 10 + 180* Solid_Bezier_points_mode'Pos(q);
      Create_Label (Tabbing.tab(gen_opt_tab_latex),
        Msg(message'Val(message'Pos(bezauto)+Solid_Bezier_points_mode'Pos(q))),
        x,  y+40, 175, 22);
      Create(q_radio(q),Tabbing.tab(gen_opt_tab_latex), "",  x,  y+20,  60, 15);
      State(q_radio(q),boolean_to_state(q=candidate.solid_bez));
    end loop;

    -- LaTeX tab / Preview LaTeX version group
    y:= y + yy + 8;
    Create(preview_group, Tabbing.tab(gen_opt_tab_latex), Msg(preview_latex_mode), 5, y, wmax-5, yy);
    for l in LaTeX_version loop
      x:= 10 + 180* LaTeX_version'Pos(l);
      Create_Label (Tabbing.tab(gen_opt_tab_latex), "LaTeX " & latver(l),
        x,  y+40, 175, 22);
      Create(preview_radio(l),Tabbing.tab(gen_opt_tab_latex), "",  x,  y+20,  60, 15);
      State(preview_radio(l),boolean_to_state(l=candidate.preview_mode));
    end loop;

    TC.GWin.MDI_Main.Show_Dialog_with_Toolbars_off(Pan, Window, Window, Result);

    case Result is
      when Idok     =>
        if Candidate.Lang /= Gen_Opt.Lang then
          Gwindows.Message_Boxes.Message_Box
            ( Window,
              Speak(Gen_Opt.Lang,lng_chg) & " - " &
              Speak(Candidate.Lang,lng_chg) ,
              Speak(Gen_Opt.Lang,fx_restrt) & ASCII.LF &
              Speak(Candidate.Lang,fx_restrt),
              Icon => Information_Icon
            );
        end if;
        gen_opt:= candidate;
        TC.GWin.Options.Save;
      when others   =>       -- contains Idcancel
        gen_opt:= original;
        if color /= orig_col then -- 17-Oct-2003: Cancel after colour change!
          TC.GWin.Display.recreate_drawing_objects:= True;
        end if;
        color:= orig_col;
    end case;
    if redraw_again then
      Window.Redraw_all;
    end if;
  end On_General_Options;

end TC.GWin.Options_Dialogs;
