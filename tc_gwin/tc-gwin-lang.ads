with TC.Tools;

with Ada.Strings.Unbounded;

package TC.GWin.Lang is

  type Message is
   (empty,
    --  Status bar
    ready,
    --  Menus
    ffile,
      fnew, fopen, fsave, fsaveas, fclose, fexit,
    ddraw,
      dtext,
      dframebox, dfilledbox,
      dline,
      dcircle, dfcircle, doval, dbezchain,
    lline,
      lthin, lthick,
      lplain, ldot, ldotp, ldash, ldashp,
      lnone, lhead, lboth, lmiddle,
    eedit,
      echgtxt, epickobj, epickall, eunpickall,
      etranslate, emirror, erotate, emorphing,
      edelete, ecopyclip, ecutclip, epasteclip,
      eundo, eredo,
      esavemac, eloadmac,
    ttools,
      tclean,
    vview,
      vtogdtb, vtogltb,
    oopt,
      opicopt, onewpicopt, ogenopt,
    wwindow,
      wcascade, wtilehor, wtilever, wclosall,
    hhelp,
      habout,
    --  Various panels
    exists, replace, cannotsave, cannotbackup,
    mcancel,
    open,
    save,
    save_as, save_macro,
    preview, prev_fail,
    close_not_saved,
    do_you_want_to_save,
    the_changes_you_made_to,
    lng_chg,
    fx_restrt,
    no_picked,
    --  General options panel
    gen_opt_tab_display,
    gen_opt_tab_latex,
    gen_opt_tab_miscellaneous,
    lng,
    gcolors, background, normal, picked, shadow,
    grid, gridnone, gridpts, gridlin,
    bezpts, bezauto, bezsugg,
    param2d_title, param2d_segments, param2d_scale,
    preview_latex_mode,
    preview_directory,
    mcurrent,
    mtemporary,
    --  Picture options panel
    pic_opt_tab_drawing,
    pic_opt_tab_latex,
    dimensions, unitlength, linewidth,
    linechain, reduchain, slopetol,
    zoom_fac, qualcirc, origin,
    choose,
    snapping, activated, stepping,
    slopes, anyslope, txslopes,
    preview_insertions,
    compat,
    --  Object edition
    left, hcenter, right,
    top, vcenter, bottom,
    dash_size,
    --  Misc
    dot_gap, dot_symbol,
    new_pic,
    ltx_pic,
    ltx_pic_bak,
    tcd_mac,
    suffix,
    all_files,
    noemlines1,
    noemlines2,
    keeporig,
    numiter,
    error,
    fnotfound,
    expl_pick,
    --  Tools
    cleanup,
    cleanup_selected,
    topic,
    occurrences,
    first_pos,
    clean_empty_text,
    clean_zero_sized_object,
    clean_unknown_command,
    clean_comment,
    --  About
    blurb,
    authors,
    original,
    tc4,
    windoze_version,
    thanks,
    gwind,
    --  Mouse
    mouse_drag,
    bez_pt2,
    bez_ptc,
    m_paste
  );

  --  NB: String might become Wide_String

  --  Gives message m in language l
  function Speak (l : Language; m : Message) return GWindows.GString;
  --  Gives message m in startup_language
  function Msg (m : Message) return GWindows.GString;

  --  Filter the '&'-s
  function Filter_amp (s : GWindows.GString) return GWindows.GString;

  function Language_rich_image (l : Language) return GWindows.GString;

  msg_for_command : constant array (Custom_cmd) of Message :=
     (preview          => preview,
      save             => fsave,
      save_as          => fsaveas,
      close            => fclose,
      text             => dtext,
      framebox         => dframebox,
      filled_box       => dfilledbox,
      line             => dline,
      circle           => dcircle,
      filled_circle    => dfcircle,
      put              => empty, --!!
      oval             => doval,
      bez              => dbezchain,
      par_cur_2d_cmd   => param2d_title,
      pick_obj         => epickobj,
      select_all       => epickall,
      unselect         => eunpickall,
      change_text      => echgtxt,
      translate        => etranslate,
      mirror           => emirror,
      rotate           => erotate,
      homoth           => emorphing,
      delete           => edelete,
      paste_clip       => epasteclip,
      copy_clip        => ecopyclip,
      cut_clip         => ecutclip,
      tc_undo          => eundo,
      tc_redo          => eredo,
      save_macro       => esavemac,
      load_macro       => eloadmac,
      thin             => lthin,
      thick            => lthick,
      plain            => lplain,
      dot              => ldot,
      dot_param        => ldotp,
      dash             => ldash,
      dash_param       => ldashp,
      no_arrow         => lnone,
      head             => lhead,
      both             => lboth,
      middle           => lmiddle,
      gen_opt_dialog   => ogenopt,
      pic_opt_dialog   => opicopt,
      clean_pic        => tclean,
      TB_Drawing       => vtogdtb,
      TB_Line_settings => vtogltb,
      others           => empty);

  msg_for_cleanup : constant array (Tools.Detection) of Message :=
    (Tools.empty_text         =>  clean_empty_text,
     Tools.zero_sized_object  =>  clean_zero_sized_object,
     Tools.unknown_command    =>  clean_unknown_command,
     Tools.comment            =>  clean_comment);

  function U (Source : String) return Ada.Strings.Unbounded.Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  type Message_set is
    array (Message) of Ada.Strings.Unbounded.Unbounded_String;

end TC.GWin.Lang;
