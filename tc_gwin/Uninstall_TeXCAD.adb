with GWindows.Message_Boxes;

with TC.GWin.Options;

procedure Uninstall_TeXCAD is
begin
  TC.GWin.Options.Clear;
  GWindows.Message_Boxes.Message_Box("TeXCAD","Uninstalled, you can remove TeXCAD.exe");
exception
  when TC.GWin.Options.Clear_failed =>
    GWindows.Message_Boxes.Message_Box("TeXCAD","Uninstalled failed. Do you have administrator rights ?");
end Uninstall_TeXCAD;
