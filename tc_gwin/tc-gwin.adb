with GWindows.Application;
with GWin_Util;

pragma Elaborate(GWin_Util); -- Get_Windows_Version

package body TC.GWin is

  -------------
  -- Add_MRU --
  -------------

  procedure Add_MRU (name : GString) is
    x : Integer := mru'First - 1;
    up_name : GString := name;
  begin
    --  Add name to the list in task bar or
    --  elsewhere in Windows Explorer or Desktop.
    GWindows.Application.Add_To_Recent_Documents (name);

    To_Upper (up_name);

    --  Search for name in the list.
    for m in mru'Range loop
      declare
        up_mru_m : GString := To_GString_From_Unbounded (mru (m));
      begin
        To_Upper (up_mru_m);
        if up_mru_m = up_name then -- case insensitive comparison (Jan-2007)
          x := m;
          exit;
        end if;
      end;
    end loop;

    --  Does item's name exist in list ?
    if x /= 0 then
      --  Roll up entries after the item, erasing it.
      for i in x .. mru'Last - 1 loop
        mru (i) := mru (i+1);
      end loop;
      mru (mru'Last) := Null_GString_Unbounded;
    end if;

    --  Roll down the full list.
    for i in reverse mru'First .. mru'Last-1 loop
      mru (i+1) := mru(i);
    end loop;

    --  At latest now, name will exist in the list.
    mru (mru'First) := To_GString_Unbounded (name);

  end Add_MRU;

  function Shorten_filename( s: GString ) return GString is
    max: constant:= 33;
    beg: constant:= 6;
  begin
    if s'Length < max then
      return s;
    else
      return
        s(s'First .. s'First + beg-1) &       -- beg
        "..." &                               -- 3
        s(s'Last - max + beg + 1 .. s'Last);  -- max - beg - 3
    end if;
  end Shorten_filename;

  procedure Determine_version is
    minor, major : Integer;
    use GWin_Util;
    f : Windows_family;
  begin
    Get_Windows_version (major, minor, f);
    Windows_95 := f = Win9x and major = 4 and minor = 0;
  end Determine_version;

begin
  for c in Custom_cmd loop
    ID_custom(c):= 101 + Custom_cmd'Pos(c);
  end loop;
  Determine_version;
end TC.GWin;
