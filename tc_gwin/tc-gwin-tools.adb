--  Picture tools

with TC.Tools;

with TC.GWin.Lang;

with TeXCAD_Resource_GUI;

with GWindows.Application,
     GWindows.Base,
     GWindows.Constants;

package body TC.GWin.Tools is

  procedure Cleanup_Dialog (Window : in out MDI_Picture_Child.MDI_Picture_Child_Type) is
    d : TeXCAD_Resource_GUI.Cleanup_Dialog_Type;
    Result : Integer;
    use TC.Tools;
    stat : Detection_stat;
    action : Cleanup_action := no_cleanup_action;

    procedure Get_Data
      (Some_Window : in out GWindows.Base.Base_Window_Type'Class)
    is
    pragma Unreferenced (Some_Window);
    begin
      for topic in Detection loop
        action (topic) := d.Detection_List.Is_Selected (Detection'Pos (topic));
      end loop;
    end Get_Data;

    use Lang;

  begin
    TeXCAD_Resource_GUI.Create_Full_Dialog (d, Window, Msg (cleanup));
    d.IDOK.Text (Msg (cleanup_selected));
    d.IDCANCEL.Text (Msg (mcancel));
    d.Center;
    d.Small_Icon ("Tools_Icon");
    d.On_Destroy_Handler (Get_Data'Unrestricted_Access);
    Detect (Window.Draw_Control.Picture, stat);
    d.Detection_List.Insert_Column (Msg (topic), 0, 210);
    d.Detection_List.Insert_Column (Msg (occurrences), 1, 90);
    d.Detection_List.Insert_Column (Msg (first_pos), 2, 110);
    for detection_topic in Detection loop
      d.Detection_List.Insert_Item
        (Msg (msg_for_cleanup (detection_topic)),
         Detection'Pos (detection_topic));
      d.Detection_List.Set_Sub_Item
        (S2G (stat (detection_topic).number'Image),
         Detection'Pos (detection_topic), 1);
      if stat (detection_topic).number > 0 then
        d.Detection_List.Set_Sub_Item
          (S2G (stat (detection_topic).first_obj_pos'Image),
           Detection'Pos (detection_topic), 2);
      end if;
    end loop;
    Result := GWindows.Application.Show_Dialog (d, Window);
    case Result is
      when GWindows.Constants.IDOK =>
        if action /= no_cleanup_action then
          Clean (Window.Draw_Control.Picture, action);
          Window.Draw_Control.Picture.saved := False;
          Window.Draw_Control.Picture.refresh := full;
          Window.Update_Information;  --  Show the '*', for modified
        end if;
      when others =>
        null;  --  Contains the IDCANCEL case
    end case;
  end Cleanup_Dialog;

end TC.GWin.Tools;
