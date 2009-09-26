-- 22-Jan-2007: allows preview in current directory
--  5-Mar-2004: separated from TC.GWin.MDI_Picture_Child.
--  Fix enabled: temporary files left in temp dir.

package TC.GWin.Previewing is

  procedure Create_files(pic: Picture; title: String);

  procedure Start;
  Preview_error: exception;

  procedure Cleanup;

end TC.GWin.Previewing;