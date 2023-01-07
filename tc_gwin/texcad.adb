--        ________          _   ____   ____  ___
--           /    ____  \ _/   /      /   / /   \
--          /    /___/  _\    /      /---/ /    /
--         /    /___   /  \  /____  /   / /____/

------------------------------------------------------
--  Incarnation of TeXCAD as a Windows application  --
------------------------------------------------------
--  17-Feb-2003, GdM

with TC.GWin.MDI_Main,
     TC.GWin.Options;

with GWindows.Application,
     GWindows.Base,
     GWindows.GStrings,
     GWindows.Message_Boxes;

with GWin_Util;

with Ada.Calendar,
     Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Exceptions,
     Ada.Text_IO;

with GNAT.Traceback.Symbolic;

procedure TeXCAD is

  Top: TC.GWin.MDI_Main.MDI_Main_Type;

  function Time_id return String is
    use Ada.Calendar;
    T     : constant Time:= Clock;
    x, sc : Natural;

  begin
    x := Natural( Seconds(T) );
    sc:= x mod 60;
    x := x  /  60;

    declare
      --  + 100: trick for obtaining 0x
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

  use GWindows.Message_Boxes, GWin_Util;

  procedure Interactive_crash(
    Window : in out GWindows.Base.Base_Window_Type'Class;
    E: Ada.Exceptions.Exception_Occurrence)
  is
    pragma Unreferenced (Window);
    use Ada.Text_IO, GWindows;
    small_insult: constant GString:=
        S2G (Ada.Exceptions.Exception_Name (E)) & NL &
        S2G (Ada.Exceptions.Exception_Message (E));
    insult: constant GString:=
        small_insult & NL &
        S2G (GNAT.Traceback.Symbolic.Symbolic_Traceback(E));
    tid: constant String:= Time_id;
    file_name: constant String:=
      "TeXCAD_crash_report_" & tid & ".txt";
    f: File_Type;
    pedigree: constant GString:=
      "TeXCAD/Windows, v." &
      S2G (TC.version   & ", ref. " & TC.reference);
    report: constant GString:=
      pedigree & NL &
      "Crash occurence: " & S2G(tid) & NL &
      insult & NL & NL &
      "I'm using Windows 95, 98, ME, NT, 2K, XP or ? :__." & NL &
      "I was doing ______ (with TeXCAD) when the crash happened" & NL &
      "and append files ______ to help you reproducing the bug.";
  begin
    Create(f,Out_File,file_name);
    Put(f, GWindows.GStrings.To_String(report));
    Close(f);
    GWindows.Base.On_Exception_Handler (Handler => null); -- Avoid infinite recursion!
    case Message_Box
      ("Crash in TeXCAD (Windows) version " & S2G(TC.version) &
        ", reference " & S2G(TC.reference),
        insult & NL &
        "Do you want to try to e-mail a report ?" & NL &
        "In any case, the report is in file: " & S2G(file_name),
        Yes_No_Box
      )
    is
      when Yes =>
        begin
          Start( To_URL_Encoding(
            "mailto:" & TC.mail & "?Subject=Bug in " &
            GWindows.GStrings.To_String (pedigree) & "&Body="  &
            GWindows.GStrings.To_String (report)
            )
          );
        exception
          when others =>
            Message_Box(
              "E-mail of report",
              "Call of e-mail client failed, please send " &
              S2G(file_name) & "."
            );
        end;
      when others => null;
    end case;
  end Interactive_crash;

  uninst: constant GWindows.GString:= "Uninstall TeXCAD";

  use Ada.Characters.Handling, Ada.Command_Line;

begin
  if Argument_Count=1 and then To_Upper(Argument(1))="/UNINSTALL" then
    if Message_Box(
      uninst,
      "This will clear all user-related options stored" & NL &
      "in the Windows Registry, for all users." &
      NL & "Continue ?", Yes_No_Box) = Yes
    then
      TC.GWin.Options.Clear;
      Message_Box(uninst,"Options cleared. You can remove TeXCAD.exe");
    end if;
  else
    GWindows.Base.On_Exception_Handler (Handler => Interactive_crash'Unrestricted_Access);
    Top.Create_MDI_Top ("TeXCAD");
    Top.Focus;
    GWindows.Application.Message_Loop;
  end if;
exception
  when TC.GWin.Options.Clear_failed =>
    Message_Box(uninst,"Clearing failed. Do you have administrator rights ?");
  when E : others =>
    Message_Box("Uncaught exception","An exception occured in TeXCAD - sorry!");
    declare
      dummy: GWindows.Base.Base_Window_Type;
    begin
      Interactive_crash(dummy, E);
    end;
end TeXCAD;
