with TC.Input, TC.Output, TC.Tools;

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;

procedure Test_TC_IO is
  use TC.RIO;

  function Short(s: String) return String is
    f: Integer:= s'First;
  begin
    for i in s'Range loop
      if s(i)='\' or s(i)='/' then
        f:= i+1;
      end if;
    end loop;
    return s(f..s'Last);
  end Short;

  procedure Text_compare(n1,n2: String) is
    f1,f2: File_Type;
    s1,s2,os1,os2: String(1..1000);
    l1,l2,ol1,ol2: Natural;
    r1,r2: Positive;
  begin
    Open(f1,In_File,n1);
    Open(f2,In_File,n2);
    l1:= 0;
    l2:= 0;
    s1(1):= ' '; -- just calm down a warning
    s2(1):= ' '; -- just calm down a warning
    r1:= 1;
    r2:= 1;
    while not End_Of_File(f1) loop
      ol1:= l1;
      ol2:= l2;
      os1(1..ol1):= s1(1..l1);
      os2(1..ol2):= s2(1..l2);
      Get_Line(f1,s1,l1);
      begin
        Get_Line(f2,s2,l2);
      exception
        when End_Error =>
          Put_Line('[' & n2 & "]: EOF !");
          exit;
      end;
      if s1(1..l1) /= s2(1..l2) then
        if os1(1..ol1) = s2(1..l2) then
          -- Try to synchronize with previous line of s1
          null;
          -- should brake reading of the other line !!
        elsif s1(1..l1) = os2(1..ol2) then
          -- Try to synchronize with previous line of s2
          null;
          -- should brake reading of the other line !!
        else
          New_Line;
          Put_Line(Short(n1) & ':' & Integer'Image(r1) & ": " & s1(1..l1));
          Put_Line(Short(n2) & ':' & Integer'Image(r2) & ": " & s2(1..l2));
        end if;
      end if;
      r1:= r1 + 1;
      r2:= r2 + 1;
    end loop;
    Close(f2);
    Close(f1);
  end Text_compare;

  procedure Test_clean ( name: String ) is
    use TC.Tools;
    pic: TC.Picture;
    stat: Detection_stat;
    action: Cleanup_action;
  begin
    for topic in Detection loop
      TC.Input.Load( pic, False, name & ".tcp");
      action:= (others => False);
      action(topic):= True;
      TC.Tools.Detect( pic, stat );
      if stat(topic).number > 0 then
        TC.Tools.Clean( pic, action );
        TC.Output.Save( pic, False, name & "_Clean_" & Detection'Image(topic) & ".out", "");
      end if;
    end loop;
  end Test_clean;

  procedure Test_one( name: String; across_sty: Boolean ) is
    pic: TC.Picture;

    Sty_set_combs: constant:= 2**TC.Supposing_sty_set'Length;
    type Comb_range is range 0..Sty_set_combs-1;

    procedure Comb_number_to_set(c: Comb_range; ss: out TC.Supposing_sty_set) is
      i: Comb_range:= c;
    begin
      for s in TC.Supposing_sty loop
        ss(s):= i mod 2 = 1;
        i:= i / 2;
      end loop;
    end Comb_number_to_set;

  begin
    TC.Input.Load( pic, False, name & ".tcp");
    Put(Short(name) & " ok/");
    if across_sty then
      Comb_number_to_set(0, pic.opt.sty);
      Put("Save]");
      TC.Output.Save( pic, False, name & ".out", "");
      -- Test all transfers between combination i and combination j.
      Put(
        "[" & Integer'Image(Sty_set_combs**2) &
        " pairs of Change/Save/Load"
      );
      for i in Comb_range loop
        for j in Comb_range loop
          -- Set to style combination i
          Comb_number_to_set(i, pic.opt.sty);
          TC.Output.Save( pic, False, name & ".ou2", "");
          TC.Input.Load( pic, False, name & ".ou2");
          -- Set to style combination j
          Comb_number_to_set(j, pic.opt.sty);
          TC.Output.Save( pic, False, name & ".ou2", "");
          TC.Input.Load( pic, False, name & ".ou2");
        end loop;
      end loop;
      Comb_number_to_set(0, pic.opt.sty);
      TC.Output.Save( pic, False, name & ".ou3", "");
      Put("][Comparing v. 1 & v." & Integer'Image(1+2*Sty_set_combs**2) & ']');
      Text_compare(name & ".out", name & ".ou3");
    else
      Put("..2S.out");
      TC.Output.Save( pic, False, name & ".out", "");
      Put("][3L.out");
      TC.Input.Load( pic, False, name & ".out");
      Put("..4S.ou2,Comp");
      TC.Output.Save( pic, False, name & ".ou2", "");
      Text_compare(name & ".out", name & ".ou2");
      Put("][5L.ou2");
      TC.Input.Load( pic, False, name & ".ou2");
      Put("..6S.ou3,Comp...]");
      TC.Output.Save( pic, False, name & ".ou3", "");
      Text_compare(name & ".ou2", name & ".ou3");
    end if;
    Put_Line("[Last comparison done]");
  end Test_one;

  procedure Test( name1: String; across_sty: Boolean:= False ) is
    name: constant String:= "./Test_IO/" & name1;
  begin
    Put(" [Load... ");
    Test_one(name, across_sty);
    Test_clean(name);
  exception
    when Name_Error =>
      Test_one(To_Lower(name), across_sty);
  end;

  use TC.Units;

begin
  for u2 in TC.Units.Unit'First .. TC.Units.sp loop
    Ada.Text_IO.Put("      " & TC.Units.Unit'Image(u2));
  end loop;
  Ada.Text_IO.New_Line;
  for u1 in TC.Units.Unit'First .. TC.Units.sp loop
    Ada.Text_IO.Put(TC.Units.Unit'Image(u1));
    for u2 in TC.Units.Unit'First .. TC.Units.sp loop
      if u2 = TC.Units.sp then
        TC.RIO.Put(Convert(1.0,u1,u2),9,0,0);
      else
        if u1 = sp then
          TC.RIO.Put(Convert(1.0,u1,u2),3,1,2);
        else
          TC.RIO.Put(Convert(1.0,u1,u2),3,4,0);
        end if;
      end if;
    end loop;
    Ada.Text_IO.New_Line;
  end loop;

  Put( TC.Units.Convert("123pt", TC.Units.pt),4,4,0 );
  New_Line;
  Put( TC.Units.Convert("456.0pt", TC.Units.pt),4,4,0 );
  New_Line;
  Put( TC.Units.Convert("1.0 in", TC.Units.cm),4,4,0 );
  New_Line;

  Test("LineThrs");
  Test("Glocke2");
  Test("CAGlob");
  Test("ModuFT", True);
  Test("Cellule");
  Test("PlugFlow", True);
  Test("O2R");
  Test("OmegaK");
  Test("IntrCara");
  Test("Smodele");
  Test("Burgchoc");
  Test("Burgrare");
  Test("Burgsinu");
  Test("Tutti");
  Test("Bezoff");
  for i in 1..5 loop
    declare
      n: constant String:= Integer'Image(1000+i);
    begin
      Test("burg" & n(5), True);
    end;
  end loop;
  for i in 1..8 loop
    declare
      n: constant String:= Integer'Image(1000+i);
    begin
      Test("petri" & n(4..5), True);
    end;
  end loop;
  for i in 1..41 loop
    declare
      n: constant String:= Integer'Image(1000+i);
    begin
      Test("web" & n(3..5));
    end;
  end loop;
  Test("rubbish");  --  Testing TC.Tools.Clean
  Test("gnuplot1");
  Test("gnuplot2");
  Test("gnuplot3", True);
  Test("Fig_TC41", True); -- new figures
  Test("FigeTC41", True); -- same, using epic
  Test("Fig_TC42", True); -- insertions for preview
  Test("op_amp", True);   -- epic, output from jPicEdt
  Test("diode", True);    -- epic, output from jPicEdt
  Test("eepfig4c", True); -- epic with chained \drawline, \dashline
  Test("gesicht", True);  -- an example in the old DOS TeXCAD...
  Test("tanne", True);    -- an example in the old DOS TeXCAD...
  Test("texcads", True);  -- TeXCAD genealogy, appearing in the doc
  Put("---Done--- Last chance to check memory usage... press Return");
  Skip_Line;
end Test_TC_IO;
