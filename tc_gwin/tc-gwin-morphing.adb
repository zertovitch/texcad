with TC.Morphing;

with TC.GWin.Lang,
     TC.GWin.MDI_Main;

with GWindows.Base,
     GWindows.Buttons,
     GWindows.Constants,
     GWindows.Edit_Boxes,
     GWindows.Message_Boxes,
     GWindows.Static_Controls,
     GWindows.Windows;

with GWin_Util;

package body TC.GWin.Morphing is

  use TC.Morphing;
  use GWindows.Message_Boxes;

  procedure Deformation_Dialog
    (parent : in out MDI_Main.MDI_Main_Type;
     m      :        Morphart;
     subcmd :    out Natural;
     iter   :    out Positive;
     keep_o :    out Boolean;
     L      :    out Point)      --  Diagonal linear transformation
  is
    biter, bfx, bfy : GWindows.Edit_Boxes.Edit_Box_Type;
    keep            : GWindows.Buttons.Check_Box_Type;
    oki             : GWindows.Buttons.Default_Button_Type;
    cancel          : GWindows.Buttons.Button_Type;
    pan             : GWindows.Windows.Window_Type;
    bt_choix        : array (1 .. 4) of GWindows.Buttons.Dialog_Button_Type;
    result, x       : Integer;
    aborting        : Boolean := False;

    ID_offset : constant := 10000;

    use GWindows.Buttons;

    procedure Get_Data
      (window : in out GWindows.Base.Base_Window_Type'Class)
    is
      pragma Warnings (off, window);
    begin
      if m = homothethy then
        L := (Real'Value (G2S (bfx.Text)), Real'Value (G2S (bfy.Text)));
      end if;
      if m /= symmetry then
        iter := Positive'Value (G2S (biter.Text));
      end if;
      keep_o := State (keep) = Checked;
      --  Bug spotted by Dmitry Chistyakov, 2007-01-25-17-08-58:
      --  numbers can be invalid, thus the following exception part
    exception
      when others =>
        Message_Box
          (window,
           "Invalid number(s) given", "Operation cancelled",
           OK_Box, Error_Icon);
        aborting := True;
    end Get_Data;

    max_sub  : constant array (Morphart) of Integer :=
      (translation => 0, symmetry => 4, rotation => 3, homothethy => 3);

    m_angle : constant array (symmetry .. rotation, 1 .. 4) of String (1 .. 8) :=
        (("  0° (-)",
          " 45° (/)",
          " 90° (|)",
          "135° (\)"),
         (" <- 90° ",
          "  180°  ",
          " 90° -> ",
          "        "));
    use GWindows.Constants, GWindows.Edit_Boxes, GWindows.Static_Controls, Lang;
  begin
    pan.Create_As_Dialog (parent, "", Width => 450, Height => max_sub (m) * 30 + 130);
    pan.Center;
    pan.Small_Icon ("Options_Icon");
    GWin_Util.Use_GUI_Font (pan);

    case m is
      when translation =>
        null;
      when symmetry .. rotation =>
        for i in 1 .. max_sub (m) loop
          Create
            (bt_choix (i), pan, S2G (m_angle (m, i)),
             20, 50 + (i - 1) * 30,
             80, 25, ID => i + ID_offset);
        end loop;
        bt_choix (1).Focus;
      when homothethy =>
        Create (bfx,  pan, "1.0",   20,   60, 60, 22);
        Create_Label (pan, "0.0",   20,  110, 60, 22);
        Create_Label (pan, "0.0",   90,   60, 60, 22);
        Create (bfy,  pan, "1.0",   90,  110, 60, 22);
        bfx.Focus;
    end case;

    Create_Label (pan, Msg (keeporig),  20, 20, 110, 25);
    Create (keep, pan, "",             140, 20,  20, 18);
    State (keep, Checked);

    if m = symmetry then -- A**2=I -> iter>1 useless...
      iter := 1;
    else
      Create_Label (pan, Msg (numiter), 200, 20, 150, 25);
      Create (biter, pan, "1",          360, 20,  40, 25);
    end if;

    if m in symmetry .. rotation then -- specific exit buttons
      x := 20;
    else
      oki.Create (pan, "O&K", 20,
              pan.Client_Area_Height - 40, 60, 25, ID => IDOK);
      x := 100;
    end if;
    cancel.Create (pan, Msg (mcancel), x,
            pan.Client_Area_Height - 40, 60, 25, ID => IDCANCEL);

    pan.On_Destroy_Handler (Get_Data'Unrestricted_Access);

    MDI_Main.Show_Dialog_with_Toolbars_off (pan, parent, parent, result);

    if aborting then
      subcmd := 0;
    else
      x := result - ID_offset;
      if x in 1 .. max_sub (m) then
        subcmd := x;
      else
        case result is
          when IDOK     => subcmd := 1;
          when others   => subcmd := 0;  --  Contains the IDCANCEL case.
        end case;
      end if;
    end if;
  end Deformation_Dialog;

  procedure Deformation (w : in out MDI_Picture_Child.TC_Picture_Panel) is
    m : constant array (Deformation_cmd) of Morphart :=
       (translate => translation,
        mirror    => symmetry,
        rotate    => rotation,
        homoth    => homothethy);
    mo     : Morphart;
    iter   : Integer;
    subcmd : Natural;
    keep_o : Boolean;
    L      : Point;    --  Diagonal linear transformation
    use Lang;
  begin
    if w.Picture.picked = 0 then
      Message_Box (w, "", Msg (no_picked), OK_Box, Error_Icon);  --  usually doesn't happen...
    else
      mo := m (w.current_cmd);
      Deformation_Dialog (w.main.all, mo, subcmd, iter, keep_o, L);
      if mo /= homothethy then
        L := w.PU; -- End point of mouse
      end if;
      --  Message_Box(w,"",
      --    "subcmd=" & integer'image(subcmd) & NL &
      --    "iter=" & integer'image(iter) & NL &
      --    "keep_o=" & boolean'image(keep_o) );
      if subcmd /= 0 then
        Morphit
          (w.Picture, mo,
           w.PS, L,
           keep_o,
           iter,
           subcmd);
        --  Refresh Bezier midpoints:
        Refresh_size_dependent_parameters (w.Picture, objects => True);
        if keep_o then -- 14-Oct-2003
          w.Picture.refresh := shadows_and_objects;  --  Only added objects
        else
          w.Picture.refresh := full;                 --  Original disappears
        end if;
        w.Redraw;
      end if;
    end if;
  end Deformation;

end TC.GWin.Morphing;
