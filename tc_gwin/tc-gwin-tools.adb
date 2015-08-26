--  Picture tools

with TC.Tools;                          use TC.Tools;
with TC.GWin.Lang;                      use TC.GWin.Lang;
with TeXCAD_Resource_GUI;               use TeXCAD_Resource_GUI;

with GWindows.Application;              use GWindows.Application;
with GWindows.Constants;                use GWindows.Constants;

package body TC.GWin.Tools is

  procedure Cleanup_dialog(Window: in out MDI_Picture_Child_Type) is
    d: TeXCAD_Resource_GUI.Cleanup_Dialog_Type;
    Result: Integer;
    stat: Detection_stat;
  begin
    Create_Full_Dialog(d, Window, Msg(cleanup));
    d.IDOK.Text(Msg(cleanup));
    d.IDCANCEL.Text(Msg(mcancel));
    d.Center;
    d.Small_Icon("Options_Icon");
    Detect(Window.Draw_Control.picture, stat);
    d.Detection_List.Insert_Column(Msg(topic), 0, 210);
    d.Detection_List.Insert_Column(Msg(occurrences), 1, 90);
    d.Detection_List.Insert_Column(Msg(first_pos), 2, 110);
    for topic in Detection loop
      d.Detection_List.Insert_Item(Msg(msg_for_cleanup(topic)), Detection'Pos(topic));
      d.Detection_List.Set_Sub_Item(Integer'Image(stat(topic).number), Detection'Pos(topic), 1);
      if stat(topic).number > 0 then
        d.Detection_List.Set_Sub_Item(Integer'Image(stat(topic).first_obj_pos), Detection'Pos(topic), 2);
      end if;
    end loop;
    Result:= Show_Dialog(d, Window);
    case Result is
      when IDOK     =>
        Clean(Window.Draw_Control.picture, (others => True));  --  !! must be selectable
        Window.Draw_Control.picture.saved:= False;
        Window.Draw_Control.picture.refresh:= full;
        Show_Totals(Window); -- show the '*' for modified
      when others   =>
        null; -- Contains IDCANCEL
    end case;
  end;

end TC.GWin.Tools;
