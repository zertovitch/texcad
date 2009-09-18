--        ________          _   ____   ____  ___
--           /    ____  \ _/   /      /   / /   \
--          /    /___/  _\    /      /---/ /    /
--         /    /___   /  \  /____  /   / /____/

------------------------------------------
-- Incarnation as a Windows application --
------------------------------------------
-- 17-Feb-2003, GdM

with Ada.Calendar;                      use Ada.Calendar;
with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;                       use Ada.Text_IO;

with GNAT.Traceback.Symbolic;

with TC.GWin.MDI_Main;                  use TC.GWin.MDI_Main;
with TC.GWin.Options;

with GWindows;                          use GWindows;
with GWindows.Application;              use GWindows.Application;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Message_Boxes;            use GWindows.Message_Boxes;

with GWin_Util;                         use GWin_Util;

procedure TeXCAD is

  Top: TC.GWin.MDI_Main.MDI_Main_Type;

  function Time_id return String is
    T     : constant Time:= Clock;
    x, sc : Natural;

  begin
    x := Natural( Seconds(T) );
    sc:= x mod 60;
    x := x  /  60;

    declare
      -- + 100: trick for obtaining 0x
      sY : constant String:= Integer'Image( Year(T)       );
      sM : constant String:= Integer'Image( Month(T) + 100);
      sD : constant String:= Integer'Image(  Day(T)  + 100);
      ssc: constant String:= Integer'Image( sc       + 100);
      smn: constant String:= Integer'Image( x mod 60 + 100);
      shr: constant String:= Integer'Image( x  /  60 + 100);

    begin
      return
        sY( sY'Last-3 .. sY'Last ) & '-' &
        sM( sM'Last-1 .. sM'Last ) & '-' &
        sD( sD'Last-1 .. sD'Last ) & '-' &
        shr( shr'Last-1 .. shr'Last ) & '-' &
        smn( smn'Last-1 .. smn'Last ) & '-' &
        ssc( ssc'Last-1 .. ssc'Last );
    end;

  end Time_id;

  procedure Interactive_crash(E: Ada.Exceptions.Exception_Occurrence) is
    small_insult: constant String:=
        Ada.Exceptions.Exception_Name (E) & NL &
        Ada.Exceptions.Exception_Message (E);
    insult: constant String:=
        small_insult & NL &
        GNAT.Traceback.Symbolic.Symbolic_Traceback(E);
    tid: constant String:= Time_id;
    file_name: constant String:=
      "TeXCAD_crash_report_" & tid & ".txt";
    f: File_Type;
    pedigree: constant GString:=
      "TeXCAD/Windows, v." &
      TC.version   & ", ref. " &
      TC.reference;
    report: constant GString:=
      pedigree & NL &
      "Crash occurence: " & S2G(tid) & NL &
      S2G(insult) & NL & NL &
      "I'm using Windows 95, 98, ME, NT, 2K, XP or ? :__." & NL &
      "I was doing ______ (with TeXCAD) when the crash happened" & NL &
      "and append files ______ to help you reproducing the bug.";
  begin
    case Message_Box
      ("TeXCAD (Windows) version " & TC.version &
        ", reference " & TC.reference,
        S2G(insult) & NL &
        "Do you want to try to e-mail a report ?" & NL &
        "In any case, the report is in file: " & S2G(file_name),
        Yes_No_Box
      ) is
      when Yes =>
        Start( To_URL_Encoding(
          "mailto:gdm@dplanet.ch?Subject=Bug in " &
          pedigree & "&Body="  & report
          )
        );
      when others => null;
    end case;
    Create(f,Out_File,file_name);
    Put(f, To_String(report));
    Close(f);
  end Interactive_crash;

  uninst: constant GString:= "Uninstall TeXCAD";

begin
  if Argument_Count=1 and then Argument(1)="/UNINSTALL" then
    if Message_Box(
      uninst,
      "This will clear all user-related options." &
      NL & "Continue ?", Yes_No_Box) = Yes
    then
      TC.GWin.Options.Clear;
      Message_Box(uninst,"Options cleared. You can remove TeXCAD.exe");
    end if;
  else
    Create_MDI_Top (Top, "TeXCAD");
    Message_Loop;
  end if;
exception
  when TC.GWin.Options.Clear_failed =>
    Message_Box(uninst,"Clearing failed. Do you have administrator rights ?");
  when E : others =>
    Interactive_crash(E);
end TeXCAD;
