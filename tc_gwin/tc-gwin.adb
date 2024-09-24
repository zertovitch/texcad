with GWin_Util;

pragma Elaborate (GWin_Util);  --  For: Get_Windows_Version

package body TC.GWin is

  function Keyboard_Shortcut (c : Custom_cmd) return GWindows.GString is
  begin
    case c is
      when save       => return "Ctrl+S";
      when save_as    => return "F12";
      when close      => return "Ctrl+W / Ctrl+F4";
      when delete     => return "[Ctrl+] Del";
      when copy_clip  => return "Ctrl+C / Ctrl+Ins";
      when cut_clip   => return "Ctrl+X / Shift+Del";
      when paste_clip => return "Ctrl+V / Shift+Ins";
      when select_all => return "Ctrl+A";
      when unselect   => return "Ctrl+U";
      when others     => return "";
    end case;
  end Keyboard_Shortcut;

  procedure Determine_version is
    minor, major : Integer;
    use type GWin_Util.Windows_family;
    f : GWin_Util.Windows_family;
  begin
    GWin_Util.Get_Windows_version (major, minor, f);
    Windows_95 := f = GWin_Util.Win9x and major = 4 and minor = 0;
  end Determine_version;

  function Equivalent (Id_1, Id_2 : ID_Type) return Boolean is
    use GWindows, GWindows.GStrings;
    --  Copied from LEA.
    --
    F1 : GString := GU2G (Id_1.file_name);
    F2 : GString := GU2G (Id_2.file_name);
    S1 : GString := GU2G (Id_1.short_name);
    S2 : GString := GU2G (Id_2.short_name);
    trace : constant Boolean := False;
    result : Boolean;
    use Ada.Text_IO;
  begin
    if trace then
      Put_Line ("F1 = [" & G2S (F1) & ']');
      Put_Line ("F2 = [" & G2S (F2) & ']');
      Put_Line ("S1 = [" & G2S (S1) & ']');
      Put_Line ("S2 = [" & G2S (S2) & ']');
    end if;
    if F1 = "" or else F2 = "" then
      To_Upper (S1);
      To_Upper (S2);
      result := S1 = S2;
    else
      To_Upper (F1);
      To_Upper (F2);
      result := F1 = F2;
    end if;
    if trace then
      Put_Line ("Equivalent: " & result'Image);
    end if;
    return result;
  end Equivalent;

  function Simple_Name (path : GWindows.GString) return GWindows.GString is
    start : Natural := path'First;
  begin
    for i in reverse path'Range loop
      if path (i) = '\' then
        start := i + 1;
        exit;
      end if;
    end loop;
    return path (start .. path'Last);
  end Simple_Name;

begin
  for c in Custom_cmd loop
    ID_custom (c) := 101 + Custom_cmd'Pos (c);
  end loop;
  Determine_version;
end TC.GWin;
