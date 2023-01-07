with GWindows.Registry;

package body TC.GWin.Options is
  use GWindows.Registry;

  kname: constant GString:= "Software\TeXCAD";

  -- Ensures that all keys are processed in each operation: Clear/Load/Save

  type Key is
    ( -- General TC options:
      pic, mac, lng, grid, bez,
      preview_latex_version,
      preview_insert,
      preview_directory,
      bak,
      bak_active,
      -- Default TC picture options:
      snapping,
      snap_asp,
      zoom_fac,
      quality,
      reduce,
      stdiff,
      steigung,
      ul,
      lw,
      style_switch_bezier,
      style_switch_epic,
      style_switch_emlines,
      -- ** Informations specific to TC for Windows:
      -- Main window:
      kleft,
      ktop,
      kwidth,
      kheight,
      kmaxi,
      kchildmaxi,
      -- Drawing toolbar window:
      dtbl, dtbt, dtbw, dtbh, dtbs,
      -- Line settings toolbar window:
      ltbl, ltbt, ltbw, ltbh, ltbs,
      -- Menus: Most Recent Units
      mru1, mru2, mru3, mru4, mru5, mru6, mru7, mru8, mru9,
      cobackground, conormal, coselected, coshadow
    );

  subtype Colors is Key range cobackground .. coshadow;
  subtype TB_Key is Key range dtbl .. ltbs;
  subtype Style_switch is Key range style_switch_bezier .. style_switch_emlines;

  style_switch_prefix: constant String:= "style_switch_";
  style_switch_offset: constant Integer:= style_switch_prefix'Length;

  -----------
  -- Clear --
  -----------

  procedure Clear is

    procedure Clear_for_one_user(user_key_name: GString) is
    begin
      for k in Key loop
        Delete_Value( user_key_name, S2G (Key'Image(k)), HKEY_USERS );
      end loop;
      Unregister( user_key_name, HKEY_USERS );
    exception
      when REGISTRY_ERROR => raise Clear_failed;
    end Clear_for_one_user;

    list: constant Key_Name_Array:= Get_Sub_Keys ("", HKEY_USERS);
  begin
    for n in list'Range loop
      declare
        user   : constant GString:= To_GString_From_Unbounded(list(n));
        user_tc: constant GString:= user & '\' & kname;
      begin
        declare
          test: Key_Name_Array:= Get_Sub_Keys (user_tc, HKEY_USERS);
          pragma Warnings(off,test);
        begin
          Clear_for_one_user(user_tc);
        end;
      exception
        when REGISTRY_ERROR => null; -- None of ours keys for that user
      end;
    end loop;
  end Clear;

  ----------
  -- Load --
  ----------

  procedure Load is
    o: General_Options renames TC.gen_opt;
    p: Picture_Options renames o.options_for_new;
  begin
    for k in Key loop
      begin
        declare
          ks: constant GString := S2G (Key'Image(k));
          s : constant String  := G2S (Get_Value( kname, ks, HKEY_CURRENT_USER ));
        begin
          case k is
            when pic  =>
              if s /= "" then
                -- ^ Don't erase the default value when this key is missing
                --   (no exception raised in that case)
                o.tex_suff := To_Unbounded_String (s);
              end if;
            when bak  =>
              if s /= "" then
                -- ^ Don't erase the default value when this key is missing
                --   (no exception raised in that case)
                o.bak_suff := To_Unbounded_String (s);
              end if;
            when bak_active =>
              o.bak_enabled:= Boolean'Value(s);
            when mac  =>
              if s /= "" then
                -- ^ Don't erase the default value when this key is missing
                --   (no exception raised in that case)
                o.mac_suff:= To_Unbounded_String(s);
              end if;
            when lng  => o.lang:= TC.Language'Value(s);
            when grid => o.grid:= TC.Grid_Display'Value(s);
            when bez  => o.solid_bez:= TC.Solid_Bezier_Points_Mode'Value(s);
            when preview_latex_version =>
              o.preview_mode:= LaTeX_version'Value(s);
            when preview_insert =>
              if s /= "" then
                -- ^ Don't erase the default value when this key is missing
                --   (no exception raised in that case)
                p.pv_insert:= To_Unbounded_String(s);
              end if;
            when preview_directory =>
              o.preview_directory:= Preview_directory_choice'Value(s);
            when snapping => p.snapping:= Boolean'Value(s);
            when snap_asp => p.snap_asp:= Integer'Value(s);
            when zoom_fac => p.zoom_fac:= Real'Value(s);
            when quality  => p.quality:= Real'Value(s);
            when reduce   => p.reduce:= Boolean'Value(s);
            when stdiff   => p.stdiff:= Real'Value(s);
            when steigung => p.steigung:= Boolean'Value(s);
            when ul       =>
              if s /= "" then
                -- ^ Don't erase the default value when this key is missing
                --   (no exception raised in that case)
                p.unitlength:= To_Unbounded_String(s);
              end if;
            when lw       =>
              if s /= "" then
                -- ^ Don't erase the default value when this key is missing
                --   (no exception raised in that case)
                p.linewidth:= To_Unbounded_String(s);
              end if;
            when Style_switch =>
              p.sty(Supposing_sty'Value(
                GWindows.GStrings.To_String (ks(ks'First+style_switch_offset..ks'Last))
              )) := Boolean'Value(s);
            when kleft    => wleft  := Integer'Value(s);
            when ktop     => wtop   := Integer'Value(s);
            when kwidth   => wwidth := Integer'Value(s);
            when kheight  => wheight:= Integer'Value(s);
            when kmaxi    => wmaxi:= Boolean'Value(s);
            when kchildmaxi => MDI_childen_maximized:= Boolean'Value(s);
            when TB_Key =>  -- Floating toolbar subcase
              declare
                TB_Key_name: constant String(1..4):= Key'Image(k);
                cat: Floating_toolbar_categ;
              begin
                -- example "DTBL" -> Drawing toolbar Left
                case TB_Key_name(1) is
                  when 'D' => cat:= TB_Drawing;
                  when 'L' => cat:= TB_Line_settings;
                  when others => null; -- Should not happen !
                end case;
                case TB_Key_name(4) is
                  when 'L' => TC_FT_memo(cat).geom.l:= Integer'Value(s);
                  when 'T' => TC_FT_memo(cat).geom.t:= Integer'Value(s);
                  when 'W' => TC_FT_memo(cat).geom.w:= Integer'Value(s);
                  when 'H' => TC_FT_memo(cat).geom.h:= Integer'Value(s);
                  when 'S' => TC_FT_memo(cat).stat:= Floating_Toolbars.Floating_TB_Status'Value(s);
                  when others => null; -- Should not happen !
                end case;
              end;
            when mru1..mru9 => mru( Key'Pos(k)-Key'Pos(mru1)+1 ):= To_GString_Unbounded(S2G(s));
            when Colors   => color(
                               Color_Zone'Val(Key'Pos(k)-Key'Pos(Colors'First))
                             ):= Color_Type'Value(s);
          end case;
        end;
      exception
        when REGISTRY_ERROR => null; -- This key is missing
        when others         => null; -- Data_Error or something else
      end;
    end loop;
  end Load;

  ----------
  -- Save --
  ----------

  procedure Save is
    o: General_Options renames TC.gen_opt;
    p: Picture_Options renames o.options_for_new;
    zekey: Key;

    procedure R( v: String ) is
    begin
      Register( kname, S2G (Key'Image(zekey)), S2G (v), HKEY_CURRENT_USER );
    end R;

  begin
    for k in Key loop
      zekey:= k;
      declare
        ks: constant String:= Key'Image(k);
      begin
        case k is
          when pic  => R( To_String(o.tex_suff) );
          when bak  => R( To_String(o.bak_suff) );
          when bak_active => R( Boolean'Image(o.bak_enabled) );
          when mac  => R( To_String(o.mac_suff) );
          when lng  => R( TC.Language'Image(o.lang) );
          when grid => R( TC.Grid_Display'Image(o.grid) );
          when bez  => R( TC.Solid_Bezier_Points_Mode'Image(o.solid_bez) );
          when preview_latex_version =>
                       R( LaTeX_version'Image(o.preview_mode) );
          when preview_insert =>
                       R( To_String(p.pv_insert) );
          when preview_directory =>
                       R( Preview_directory_choice'Image(o.preview_directory) );
          when snapping => R( Boolean'Image(p.snapping) );
          when snap_asp => R( Integer'Image(p.snap_asp) );
          when zoom_fac => R( Real'Image(p.zoom_fac) );
          when quality  => R( Real'Image(p.quality) );
          when reduce   => R( Boolean'Image(p.reduce) );
          when stdiff   => R( Real'Image(p.stdiff) );
          when steigung => R( Boolean'Image(p.steigung) );
          when ul       => R( To_String(p.unitlength) );
          when lw       => R( To_String(p.linewidth) );
          when Style_switch =>
            R(Boolean'Image(p.sty(Supposing_sty'Value(ks(ks'First+style_switch_offset..ks'Last)))));
          when kleft    => R( Integer'Image(wleft) );
          when ktop     => R( Integer'Image(wtop) );
          when kwidth   => R( Integer'Image(wwidth) );
          when kheight  => R( Integer'Image(wheight) );
          when kmaxi    => R( Boolean'Image(wmaxi) );
          when kchildmaxi => R( Boolean'Image(MDI_childen_maximized ) );
          when TB_Key =>  -- Floating toolbar subcase
            declare
              TB_Key_name: constant String(1..4):= Key'Image(k);
              cat: Floating_toolbar_categ;
            begin
              -- example "DTBL" -> Drawing toolbar Left
              case TB_Key_name(1) is
                when 'D' => cat:= TB_Drawing;
                when 'L' => cat:= TB_Line_settings;
                when others => raise Constraint_Error; -- Should not happen !
              end case;
              case TB_Key_name(4) is
                when 'L' => R( Integer'Image(TC_FT_memo(cat).geom.l) );
                when 'T' => R( Integer'Image(TC_FT_memo(cat).geom.t) );
                when 'W' => R( Integer'Image(TC_FT_memo(cat).geom.w) );
                when 'H' => R( Integer'Image(TC_FT_memo(cat).geom.h) );
                when 'S' => R( Floating_Toolbars.Floating_TB_Status'Image(TC_FT_memo(cat).stat) );
                when others => null; -- Should not happen !
              end case;
            end;
          when mru1..mru9 =>
            R(
              GWindows.GStrings.To_String (
                To_GString_From_Unbounded(mru( Key'Pos(k)-Key'Pos(mru1)+1 ))
              )
            );
          when Colors   =>
            R( Color_Type'Image(
                 color(Color_Zone'Val(Key'Pos(k)-Key'Pos(Colors'First)))
               )
            );
        end case;
      end;
    end loop;
  end Save;

end TC.GWin.Options;
