--  5-Mar-2004: separated from TC.GWin.MDI_Picture_Child.
--  Fix enabled: temporary files left in temp dir.

with TC.Output;

with Ada.Directories,
     Ada.Environment_Variables,
     Ada.Text_IO;

with GWin_Util;
pragma Elaborate_All (GWin_Util);

package body TC.GWin.Previewing is

  tmp_prefix : constant String := GWin_Util.Temp_dir;

  function name return String is
    file_name_dot : constant String := "TeXCADpv.";
  begin
    case gen_opt.preview_directory is
      when temporary =>
        return tmp_prefix & file_name_dot;
      when current =>
        return file_name_dot;
    end case;
  end name;

  function tex return String is
  begin
    return name & "tex";
  end tex;

  function dvi return String is
  begin
    return name & "dvi";
  end dvi;

  function bat return String is
  begin
    return name & "bat";
  end bat;

  procedure Create_files (pic : Picture; title : String) is
    use Ada.Text_IO;
    pf, bf : File_Type;
  begin
    Cleanup;
    --  Create a LaTeX file with the picture inside:
    Create (pf, Out_File, tex);
    TC.Output.Insert_and_Wrap_in_document (pic, False, pf, title);
    Close (pf);
    --  Preview file is made
    --  Create batch file:
    Create (bf, Out_File, TC.GWin.Previewing.bat);
    case gen_opt.preview_directory is
      when temporary =>
        if tmp_prefix /= "" then
          Put_Line (bf, tmp_prefix (1 .. 2));                           -- change drive
          Put_Line (bf, "cd " & tmp_prefix (3 .. tmp_prefix'Last - 1)); -- change dir
        end if;
      when current =>
        null;
    end case;
    Put_Line (bf, "call latex " & TC.GWin.Previewing.tex); -- latex could be a batch-file
    Put_Line (bf, "start " & TC.GWin.Previewing.dvi);
    Close (bf);
  end Create_files;

  procedure Start is
    params : constant String := "/C " & TC.GWin.Previewing.bat;
  begin
    declare
      cmd : constant String := Ada.Environment_Variables.Value ("COMSPEC");
      --  usually, cmd.exe
    begin
      GWin_Util.Start (cmd, params, As_Minimized => True);
    end;
  exception
    when Constraint_Error => -- no "COMSPEC" set
      begin
        GWin_Util.Start ("cmd.exe", params, As_Minimized => True);
      exception
        when others =>
          raise Preview_Error;
      end;
  end Start;

  procedure Cleanup is
    use Ada.Directories;
    --
    procedure Delete_if_any (File_Name : String) is
    begin
      Delete_File (File_Name);
    exception
      when Name_Error => -- Doesn't exist, perhaps already deleted
        null;
      when Use_Error =>  -- Exists, but locked: e.g. a .tex file, locked by TeX
        null;
    end Delete_if_any;
    --
  begin
    Delete_if_any (tex);
    Delete_if_any (dvi);
    Delete_if_any (bat);
    Delete_if_any (name & "aux");
    Delete_if_any (name & "log");
  end Cleanup;

end TC.GWin.Previewing;
