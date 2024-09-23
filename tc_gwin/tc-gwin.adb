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

begin
  for c in Custom_cmd loop
    ID_custom (c) := 101 + Custom_cmd'Pos (c);
  end loop;
  Determine_version;
end TC.GWin;
