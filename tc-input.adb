with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;

with Ada.Exceptions;

with TC.IO_Commands;                    use TC.IO_Commands;

package body TC.Input is

  use RIO;

  procedure Which_command(com: String; art: out Obj_art_type;
                          options: Boolean; k: out Kom_type) is --  JW
    start,ende: Kom_type;
  begin
    art:= aux;
    if options then
      start := K_option'First;
      ende  := cputaux;
    else
      start := cmakebox;
      ende  := caux;
    end if;

    for i in  start .. ende loop
      if Img(i) = com then
        art:= kommando_art(i);
        k := i;
        exit;
      end if;
    end loop;

    if  art = aux then
      k := caux;
    end if;
  end Which_command;

  procedure Load( pic      : in out Picture;
                  macro    :        Boolean;
                  file_name:        String )
  is

    subtype Small_String is String(1..255);
    subtype Big_String is String(1..4090);

    --  JW,GH
    tf: File_Type;
    line_buf, com, arg  : Big_String;
    dum_str             : Small_String;
    line_n, line_len,
    com_len, arg_len,
    p, q, dumi          : Natural:= 0;
    ch                  : Character;
    stop                : Boolean; -- succ
    end_of_parsing      : Boolean:= False;
    Pdum                : Point;
    cur_obj, o, obj_ptr2: ptr_Obj_type;
    kommando            : Kom_type;
    TC_option,
    ziart               : Obj_art_type;
    ls                  : Line_settings:= normal_line_settings;
    type mode_int is range 0..2;
    mode: mode_int;
    -- Fix 23-Apr-2003: End_of_File(tf) before the end of parsing (prefetch)

    use TC.Units;

    procedure Error(msg_0: String) is --  JW,GH
      h: constant String:= Integer'Image(line_n);
      msg: constant String:=
        msg_0 & ASCII.LF &
        " in file :" & file_name & ASCII.LF &
        " at line :" & h & ASCII.LF &
        " command :" & '[' & com(1..com_len) & ']' & ASCII.LF &
        " --> ";
      procedure Raise_it( content: String ) is
      begin
        Ada.Exceptions.Raise_Exception(
          Load_error'Identity,
          msg & content
        );
      end Raise_it;
    begin
      Close(tf);
      case mode is
        when 0|2 =>  Raise_it( line_buf(1..line_len) );
        when 1 =>    Raise_it( arg(1..arg_len) );
      end case;
    end Error;

    procedure Read_line is --  GH
    begin
       if End_Of_File(tf) then
         if mode = 2 then
           line_len:= 0;
           end_of_parsing:= True;
         else
           Error("Unexpected end of file");
         end if;
       else
         if End_Of_Line(tf) then
           Skip_Line(tf);
         end if;
         Get_Line(tf, line_buf, line_len);
         if line_len > 0 and then line_buf(line_len) = ASCII.CR then
           -- Linux & Co has only ASCII.LF as line terminator, then
           -- the CR of a DOS/Windows file appears at line end.
           line_len:= line_len - 1;
         end if;
         line_n:= line_n + 1;
       end if;
       p:=1;
    end Read_line;

    procedure Read_ch is --  JW,GH
    begin
      if mode = 0 or mode = 2 then
        if p > line_len then
          Read_line;
        end if;
        if end_of_parsing then
          ch:= ' ';
        else
          ch:= line_buf(p);
          p:= p + 1;
        end if;
      elsif q > arg_len then
        ch:= Character'Val(255);
      else
        ch:= arg(q);
        q:= q + 1;
      end if;
    end Read_ch;

    procedure Skip_blanks is -- GM 24-Jan-2007
    begin
      while (ch = ' ' or ch = ASCII.HT) and not end_of_parsing loop
        Read_ch;
      end loop;
    end Skip_blanks;

    procedure Seek_ch(x:Character) is --  JW,GH
    begin
      while ch /= x and not end_of_parsing loop
        if ch=' ' then
          Read_ch;
        else
          Error(''' & x & "' expected");
        end if;
      end loop;
      Read_ch;
    end Seek_ch;

    procedure Read_word is --  JW
    begin
      if ch = '\' then
        com_len:= com_len + 1;
        com(com_len):= '\';
        Read_ch;
      end if;
      if ch /= Character'Val(255) then
        loop
          com_len:= com_len + 1;
          com(com_len):= ch;
          Read_ch;
          exit when ch not in 'a'..'z';
        end loop;
      end if;
    end Read_word;

    procedure read_com is --  JW
      empty_comment: Boolean:= False;
    begin
      com_len:= 0;
      Skip_blanks;
      if end_of_parsing then
        return;
      end if;
      if  ch = '%' then
        com_len:= 1;
        com(1):= '%';
        empty_comment:= p > line_len;
        -- put(boolean'image(empty_comment));
        if empty_comment then
          ch:='-';
          -- bogus, avoid skipping line (otherwise: glue next!) 1-Mar-2003
        else
          Read_ch;
        end if;
      end if;
      if ch='\' and not empty_comment then
        Read_word;
      end if;
    end read_com;

    procedure Read_real( r: out Real) is
      -- JW,GH
      -- GM: accepts ".123"
      neg: Boolean:= False;
      f10: Real; -- 5-Jun-2003: Float for big numbers after '.'
    begin
      r:= 0.0;
      Skip_blanks;
      case ch is
        when '-' =>
          neg:= True;
          Read_ch;
        when '+' => -- +27-Jan-2007
          Read_ch;
        when '.' | '0'..'9' =>
          null;
        when others =>
          Error("Number expected");
      end case;

      while ch in '0'..'9' and not end_of_parsing loop
        r:= 10.0 * r + Real(Character'Pos(ch)-Character'Pos('0'));
        Read_ch;
      end loop;
      if ch='.' then
        f10:= 1.0;
        Read_ch;
        while ch in '0'..'9' and not end_of_parsing loop
          f10:= f10 * 0.1;
          r:= r + Real(Character'Pos(ch)-Character'Pos('0')) * f10;
          Read_ch;
        end loop;
      end if;
      if neg then
        r:=-r;
      end if;
    end Read_real;

    function Read_real return Real is
      r: Real;
    begin
      Read_real(r);
      return r;
    end Read_real;

    procedure Read_coords( P: out Point ) is --  GH
    begin
      Seek_ch('('); Read_real(P.x);
      Seek_ch(','); Read_real(P.y);
      Seek_ch(')');
    end Read_coords;

    procedure Read_coords_and_adjust( P: out Point ) is -- 1-Mar-2004
    begin
      Read_coords(P);
      if macro then
        P:= P + pic.opt.P0;
      end if;
    end Read_coords_and_adjust;

    procedure Read_slope( s: out Slope_value ) is --  JW,GH
      si,i: Integer;
    begin
      --  read_ch;
      Skip_blanks;
      if ch/='-' and ch not in '0'..'9' then
        Error("Number expected");
      else
        if ch='-' then
          Read_ch;
          si:= -1;
        else
          si:= 1;
        end if;
        i:= si * (Character'Pos(ch)-Character'Pos('0'));
        if i not in Slope_value then
          Error("Wrong slope value");
        end if;
        Read_ch;
        s:= i;
      end if;
    end Read_slope;

    procedure Read_arg(op, cl  :     Character;
                       arg_1   : out String;
                       len     : out Natural;
                       optional:     Boolean)
    is
      --  JW,GH
      count: Integer;
      a2: String(arg_1'Range);
      l2: Natural;
    begin
      -- Added 6-Jul-2012: there may be blanks before arguments!
      while ch = ' ' and not end_of_parsing loop
        Read_ch;
      end loop;
      --
      if optional and ch/= op then
        len:= 0;
        return;
      end if;
      Seek_ch(op);
      l2 := 0;
      count:=1;
      loop
        if ch=op then
          count:=count+1;
        elsif ch=cl then
          count:=count-1;
        end if;
        if count > 0 then
          l2:= l2 + 1;
          a2(l2):= ch;
        end if;
        Read_ch;
        exit when count=0 or ch = Character'Val(255) or end_of_parsing;
      end loop;
      if count /= 0 then
        Error(''' & cl & "' expected");
      end if;
      arg_1(arg_1'First..l2):= a2(1..l2);
      len:= l2;
    end Read_arg;

    -- Added Jan 2007:
    procedure Read_arg(op, cl  :     Character;
                       arg_1   : out Unbounded_String;
                       optional:     Boolean)
    is
      arg_fixed : Big_String;
      arg_length: Natural;
    begin
      Read_arg(op,cl,arg_fixed,arg_length,optional);
      arg_1:= To_Unbounded_String(arg_fixed(1..arg_length));
    end Read_arg;

    procedure Read_real_arg(
      op, cl: Character;
      wert: out Real;
      optional: Boolean:= False)
    is
    begin
      if optional and ch/= op then
        wert:= 0.0;
      else
        Seek_ch(op); Read_real(wert); Seek_ch(cl);
      end if;
    end Read_real_arg;

    function Read_real_arg(
      op, cl: Character;
      optional: Boolean:= False
    ) return Real
    is
      r: Real;
    begin
      Read_real_arg(op,cl,r,optional);
      return r;
    end Read_real_arg;

    procedure Read_real_arg( wert: out Real; optional: Boolean:= False) is
    begin
      Read_real_arg('{','}',wert,optional);
    end Read_real_arg;

    function Read_real_arg(optional: Boolean:= False) return Real is
      r: Real;
    begin
      Read_real_arg(r,optional);
      return r;
    end Read_real_arg;

    procedure Skip_until_end_of_emulation is
      endcom: constant String:= "%\end";
    begin
      if com(1) = '%' then
        while not end_of_parsing loop
          Read_line;
          exit when Index(line_buf,endcom) /= 0;
        end loop;
        Read_line;
        ch:=line_buf(p);
        p:= p + 1;
      end if;
    end Skip_until_end_of_emulation;

    -- \unitlength passed
    procedure Read_unitlength is
    begin
      while (ch='=' or ch=' ') and not end_of_parsing loop
        Read_ch;
      end loop;
      --  GdM 27-nov-2000: compat. TC90 ('='), 2003: ch=' '
      declare
        ul: constant String:= TeX_Number( Read_real, 10 );
      begin
        pic.opt.unitlength:= To_Unbounded_String(ul);
        Skip_blanks;
        while ch in 'a'..'z' and not end_of_parsing loop
          pic.opt.unitlength := pic.opt.unitlength & ch;
          Read_ch;
        end loop;
        Refresh_size_dependent_parameters(pic, objects => False);
        -- corrects ul_in_pt
      end;
    end Read_unitlength;

    -- \setlength passed
    procedure Read_setlength is
    -- \setlength{\unitlength}{2pt}, (web001), 1-Mar-2003
    begin
      Read_arg('{','}',arg,arg_len, optional => False);
      if arg(1..arg_len) = "\unitlength" then
        Seek_ch('{');
        Read_unitlength;
        Seek_ch('}');
      end if;
    end Read_setlength;

    procedure Read_size(v: out Real) is
    -- Reads {3.4\unitlength}, {2pt} and converts to \unitlength
    -- Needed for GNUPlot's rule 3-Mar-2003
      vi: Small_String;
      pts: Real;
    begin
      Seek_ch('{');
      Read_real(v);
      com_len:= 0;
      Read_word;
      if com(1..com_len) /= "\unitlength" then
        Put(vi,v,5,0);
        pts:= Convert( Trim(vi,Left) & com(1..com_len), pt );
        v:= pts / pic.ul_in_pt;
        -- Put('[' & Trim(vi,left) & com(1..com_len) & ',');
        -- Put(pts,0,4,0); Put(','); Put(ul_in_pt,0,4,0);
        -- Put(','); Put(v,0,4,0); Put_Line("]");
      end if;
      Seek_ch('}');
    end Read_size;

    procedure Star_to_disc is
    begin
      if ch='*' then
        Read_ch;
        obj_ptr2:= new Obj_type(disc);
        obj_ptr2.P1:= o.P1;
        obj_ptr2.picked:= o.picked;
        obj_ptr2.ls:= o.ls;
        Dispose(o);
        o:= obj_ptr2;
      end if;
    end Star_to_disc;

    function Is_com_on return Boolean is
    begin return com(1..com_len)="\on"; end Is_com_on;

    procedure Read_arrows_option( ls: in out Line_settings ) is
      arrs: Small_String;
      arrl: Natural;
    begin
      Read_arg('[',']', arrs, arrl, optional => True);
      ls.arrows:= head;
      if arrl > 0 then
        case arrs(1) is
          when 'b' => ls.arrows:= both;
          when 'm' => ls.arrows:= middle;
          when others => null;
        end case;
      end if;
    end Read_arrows_option;

    procedure Read_drawline_options( ls: in out Line_settings ) is
    begin
      -- \drawline[stretch](x1,y1)(x2,y2)...(xn,yn)
      ls.stretch:= Integer(Read_real_arg('[',']', optional => True));
    end Read_drawline_options;

    procedure Read_dashline_options( ls: in out Line_settings ) is
    begin
      ls.pattern:= dash;
      -- \dashline[stretch]{dash-length}[inter-dot-gap for dash](x1,y1)(x2,y2)...(xn,yn)
      ls.stretch     := Integer(Read_real_arg('[',']', optional => True));
      ls.dash_length :=         Read_real_arg(         optional => True);
      ls.dash_dot_gap:=         Read_real_arg('[',']', optional => True);
    end Read_dashline_options;

    procedure Read_dottedline_options( ls: in out Line_settings ) is
    begin
      ls.pattern:= dot;
      -- \dottedline[optional dotcharacter]{dotgap in units}(x1,y1)(x2,y2)...(xn,yn)
      Read_arg('[',']', ls.dot_symbol, optional => True);
      Read_real_arg( ls.dot_gap, optional => True);
    end Read_dottedline_options;

    procedure Read_line_vector_parameters(o: in out Obj_type ) is
      o_len: Real;
    begin
      Seek_ch('('); Read_slope(o.line_slope(h));
      Seek_ch(','); Read_slope(o.line_slope(v)); Seek_ch(')');
      Read_real_arg(o_len);
      if  o.line_slope(h) /=0 then
        if  o.line_slope(h) > 0 then
          o.P2.x:= o.P1.x + o_len;
        else
          o.P2.x:= o.P1.x - o_len;
        end if;
        if  o.line_slope(v) /= 0 then
          o.P2.y:= o.P1.y + o_len /
            Real(abs o.line_slope(h) )*Real(o.line_slope(v));
        else
          o.P2.y:= o.P1.y;
        end if;
      else
        o.P2.x:= o.P1.x;
        o.P2.y:= o.P1.y + o_len*Real(o.line_slope(v));
      end if;
    end Read_line_vector_parameters;

    procedure Points_3_to_N(append_to: in out Obj_type) is
      -- Support for epic's chained lines (24-Jan-2007)
      -- \drawline[stretch](x1,y1)(x2,y2)...(xn,yn)
      -- \dashline[stretch]{dash-length}[inter-dot-gap for dash](x1,y1)(x2,y2)...(xn,yn)
      -- \dottedline[optional dotcharacter]{dotgap in units}(x1,y1)(x2,y2)...(xn,yn)
      clone: ptr_Obj_type;
    begin
      Skip_blanks;
      if ch /= '(' or end_of_parsing then
        return;
      end if;
      -- We have a '(' here, then an additional point
      clone:= new Obj_type'(append_to);
      append_to.next:= clone;
      clone.P1:= append_to.P2;
      Read_coords_and_adjust(clone.P2);
      Points_3_to_N(clone.all); -- go further, recursively
    end Points_3_to_N;

    procedure Read_new_object is
    begin
      o:= new Obj_type(ziart);
      o.ls:= ls;
      case  kommando  is
        when cemline1 =>
          o.any_slope:= True;
          Read_real_arg(o.P1.x);
          Read_real_arg(o.P1.y);
          Read_arg('{','}',arg,arg_len, optional => False);
          Read_real_arg(o.P2.x);
          Read_real_arg(o.P2.y);
          Read_arg('{','}',arg,arg_len, optional => False);
          if  macro  then
            o.P1:= o.P1 + pic.opt.P0;
            o.P2:= o.P2 + pic.opt.P0;
          end if;

        when cdrawline | cpath => -- 23-Jan-2007
          o.any_slope:= True;
          Read_drawline_options( o.ls );
          Read_coords_and_adjust(o.P1);
          Read_coords_and_adjust(o.P2);
          Points_3_to_N(o.all);  -- Other points to chain (24-Jan-2007)

        when cdottedline1 | cdottedline2 =>
          o.any_slope:= True;
          Read_dottedline_options( o.ls );
          Read_coords_and_adjust(o.P1);
          Read_coords_and_adjust(o.P2);
          Points_3_to_N(o.all);  -- Other points to chain (24-Jan-2007)
          if kommando = cdottedline2 then
            Skip_until_end_of_emulation;
          end if;

        when cdashline1 | cdashline2 => -- 24-Feb-2004
          o.any_slope:= True;
          Read_dashline_options( o.ls );
          Read_coords_and_adjust(o.P1);
          Read_coords_and_adjust(o.P2);
          Points_3_to_N(o.all);  -- Other points to chain (24-Jan-2007)
          if kommando = cdashline2 then
            Skip_until_end_of_emulation;
          end if;

        when cemline2 | cvector2=>

            o.any_slope:= True;
            if kommando = cvector2 then
              Read_arrows_option( o.ls );
              Read_arg('{','}',dum_str, dumi, optional => True);
              if dum_str(1..dumi) = "dot" then
                Read_dottedline_options( o.ls );
              elsif dum_str(1..dumi) = "dash" then
                Read_dashline_options( o.ls );
              elsif dum_str(1..dumi) = "\line" then
                o.any_slope:= False;
              end if;
            end if;
            if o.any_slope then
              Read_coords_and_adjust(o.P1);
              Read_coords_and_adjust(o.P2);
            else
              Read_word; -- \put
              Read_coords_and_adjust(o.P1);
              Seek_ch('{');
              Read_word; -- \line
              Read_line_vector_parameters(o.all);
              Seek_ch('}');
            end if;
            Skip_until_end_of_emulation;

        when cbezier1  | cbezier2  |
             cqbezier1 | cqbezier2 |
             cbezvec   |
             cqbezvec  =>
            case kommando is
              when cqbezier1 | cqbezier2 => -- 24-Apr-2003
                Read_arg('[',']',dum_str, dumi, optional => True);
                if dumi > 0 then
                  o.num:= Integer'Value(dum_str(1..dumi));
                else
                  o.num:= 0;
                end if;
              when others =>
                -- not a "\qbezier"
                -- 25-Feb-2004: %\qbezvec awaits also {n}
                -- (never written by TC which writes %\bezvec)
                o.num:= Integer( Read_real_arg(optional => True) );
            end case;
            if kommando in cbezvec .. cqbezvec then
              Read_arrows_option( o.ls );
            end if;
            Read_coords_and_adjust(o.P1);
            Read_coords_and_adjust(o.PC);
            Read_coords_and_adjust(o.PE);
            Set_slope_of_bezvec(o.all,pic.ul_in_pt);
            Skip_until_end_of_emulation;

        when ccircle2=>
             Star_to_disc;
             Read_coords_and_adjust(o.P1);
             Read_real_arg(o.rad);
             o.rad:= o.rad*0.5;
             Skip_until_end_of_emulation;

        when cend1   =>
             declare
               mem: constant mode_int:= mode;
             begin
               mode:= 2;
               Read_arg('{','}',arg,arg_len, optional => False);
               stop:= arg(1..arg_len)="picture";
               mode:= mem;
             end;
        when caux    =>
             if com(1..com_len) = "\linethickness" then
               -- \linethickness set inside picture (web001)! 1-Mar-2003
               Read_arg('{','}',pic.opt.linewidth, optional => False);
               o.inhalt:= Null_Unbounded_String;
             else
               o.inhalt:= To_Unbounded_String(com(1..com_len));
               while p <= line_len loop
                 o.inhalt:= o.inhalt & ch;
                 Read_ch;
               end loop;
               o.inhalt:= o.inhalt & ch;
               Read_ch;
             end if;

        when cput   =>
            Read_coords_and_adjust(o.P1);
            Read_arg('{','}',arg,arg_len, optional => False);
            q:= 1;
            ch:= ' ';
            mode:= 1;
            read_com;
            Which_command(com(1..com_len),ziart,False,kommando);
            obj_ptr2:= new Obj_type(ziart);
            obj_ptr2.P1:= o.P1;
            obj_ptr2.picked:= o.picked;
            obj_ptr2.ls:= o.ls;
            Dispose(o);
            o:= obj_ptr2;
            case  kommando  is
              when cmakebox=> --  text
                 if ch='(' then -- 24-Apr-2003
                   Read_coords( Pdum );
                 end if;
                 Read_arg('[',']',o.adjust,o.adjust_len, optional => True);
                 Read_arg('{','}',o.inhalt, optional => False);

              when K_rectangle => -- non-filled box
                 o.solid:= False;
                 case K_rectangle(kommando) is
                   when cframebox =>
                     null;
                   when cdottedbox =>
                     null; -- doesn't happen here (not a \put...)
                   when cdashbox =>
                     o.ls.pattern:= dash;
                     Read_real_arg(o.ls.dash_length);
                 end case;
                 Read_coords( o.size );
                 Read_arg('[',']',o.adjust,o.adjust_len, optional => True);
                 Read_arg('{','}',o.inhalt, optional => True);
                 -- ^optional 24-Apr-2003

             when crule=>
               o.solid:= True;
               o.inhalt:= Null_Unbounded_String;
               Read_arg('[',']',dum_str, dumi, optional => True);
               --  ^ optional, gnuplot1.tcp, 2-Mar-2003
               --  raise-height - specifies how high to raise the rule (optional)
               if dumi > 0 then
                 o.P1.y:= o.P1.y + Convert( dum_str(1..dumi), pt ) / pic.ul_in_pt;
               end if;
               Read_size(o.size.x);
               Read_size(o.size.y);

             when cline | cvector1=>

                o.any_slope:= False;
                if kommando = cvector1 then
                  o.ls.arrows:= head;
                end if;
                Read_line_vector_parameters(o.all);

             when ccircle1=>
                Star_to_disc;
                Read_real_arg(o.rad);
                o.rad:= o.rad*0.5;

             when coval=>
                    Read_coords( o.osize );
                    o.LL:= o.P1 - 0.5 * o.osize;
                    Read_arg('[',']',arg,arg_len, optional => True);
                    begin
                      o.part:= Ovop'Value(arg(1..arg_len));
                    exception
                      when others =>
                        o.part:= entire; -- no or bad option
                    end;
             when caux =>
                    kommando := cputaux;
                    obj_ptr2:= new Obj_type(putaux);
                    obj_ptr2.P1:= o.P1;
                    obj_ptr2.picked:= o.picked;
                    obj_ptr2.ls:= o.ls;
                    Dispose(o);
                    o:= obj_ptr2;
                    o.inhalt:= To_Unbounded_String(arg(1..arg_len));
                    o.adjust_len:= 0;

             when others=> null;
           end case;

           mode := 0;
           if p > 1 then
             ch:=line_buf(p-1);
           end if;

        when cdottedbox => -- 23-Feb-2004
          o.solid:= False;
          o.ls.pattern:= dot;
          Read_coords_and_adjust(o.P1);
          Read_coords( o.size );
          Read_dottedline_options( o.ls );
          o.inhalt:= Null_Unbounded_String;
          o.adjust_len:= 0;
          Read_word;
          if com(Integer'Max(1,com_len-3)..com_len) = "\put" then
            Read_coords_and_adjust(o.P1);
            Seek_ch('{');
            Read_word;
            if com(Integer'Max(1,com_len-7)..com_len) = "\makebox" then
              Read_coords( o.size );
              Read_arg('[',']',o.adjust,o.adjust_len, optional => True);
              Read_arg('{','}',o.inhalt, optional => True);
            end if;
          end if;
          Skip_until_end_of_emulation;

        when others=>
          o.P1:= (0.0,0.0);
          -- ^ should be useless but a case shows up at writing a \put... !
      end case;

      if stop then
        Delete_object_list(o);
      else
        o.picked:= macro; -- we pick all, even the hidden (aux)
        if pic.root = null then
          pic.root:= o;
        else
          cur_obj.next:= o;
        end if;
        cur_obj:= o;
        -- Several segments may have been chained (24-Jan-2007):
        while cur_obj.next /= null loop
          cur_obj:= cur_obj.next;
        end loop;
        pic.total:= pic.total + 1;
        if o.picked then
          pic.picked:= pic.picked + 1;
        end if;
        if hidden( o.art ) then
          pic.totalh:= pic.totalh + 1;
          if o.picked then
            pic.pickedh:= pic.pickedh + 1;
          end if;
        end if;
      end if;
    end Read_new_object;

  begin --  load
    mode := 0;
    Open(tf,In_File,file_name);

    if macro then
      cur_obj:= pic.root;
      if cur_obj /= null then
        -- invar: cur_obj.all exists
        while cur_obj.next /= null loop
          cur_obj:= cur_obj.next;
        end loop;
      end if;
      -- Now: either pic.root is null, or cur_obj.all is the last object.
    else
      Delete_object_list(pic.root);
      pic.opt.pv_insert:= Null_Unbounded_String;
    end if;

    line_n:=0;
    line_len:=0;
    p:= 1;
    q:= 1;
    Refresh_size_dependent_parameters(pic, objects => False);
    -- at least, the default values. (ul_in_pt)

    Read_ch;
    stop:= False;
    while not end_of_parsing loop
      while not end_of_parsing loop
        read_com;
        -- put("com=" & com(1..com_len));
        Which_command(com(1..com_len), TC_option, True,kommando);
        -- put(kom_type'image(kommando));

        if kommando = cunit then
          Read_unitlength;
        elsif kommando = csetlen then
          Read_setlength;
        elsif kommando = caux and com(1..com_len) /= "\begin" then
          -- Sometimes \thinlines, \thicklines appear before \begin!
          if com(1..com_len) = "\thinlines" then
            ls.thickness:= thin;
          elsif com(1..com_len) = "\thicklines" then
            ls.thickness:= thick;
          else
            Read_line;
            Read_ch;
          end if;
        else
          Read_arg('{','}',arg,arg_len, optional => False);
          q:= 1;
          ch:= ' ';
          mode:= 1;
          case  kommando  is
            when cspec   =>
              if  Index(arg(1..arg_len),"em:linewidth")/=0 then
                pic.opt.linewidth:= To_Unbounded_String(arg(14 .. 14 + 19));
              end if;
            when cthickness  =>
              pic.opt.linewidth:= To_Unbounded_String(arg(1..arg_len));
            when cmd_pv_insert => -- Preview insertions (Jan-2007):
              if pic.opt.pv_insert = "" then
                pic.opt.pv_insert:= To_Unbounded_String(arg(1..arg_len));
              else
                pic.opt.pv_insert:=
                  pic.opt.pv_insert & ASCII.CR & ASCII.LF & arg(1..arg_len);
              end if;
            when cqual   =>   Read_real(pic.opt.quality);
            when cgdiff  =>   Read_real(pic.opt.stdiff);
            when czoom   =>   Read_real(pic.opt.zoom_fac);
            when csnapasp=>   pic.opt.snap_asp:= Integer( Read_real );

            when others =>

               if com(1..com_len) /= "\begin" then
                 read_com;
                 case  kommando  is
                   when cgrade => pic.opt.steigung     := Is_com_on;
                   when clines => pic.opt.sty(emlines) := Is_com_on;
                   when cepic  => pic.opt.sty(epic)    := Is_com_on;
                   when cbezmac=> pic.opt.sty(bezier)  := Is_com_on;
                   when creduce=> pic.opt.reduce       := Is_com_on;
                   when csnap  => pic.opt.snapping     := Is_com_on;
                   when others => Read_ch;
                 end case;
               end if;
          end case;

          mode := 0;
          ch:= line_buf(p-1);
        end if;
        exit when com(1..com_len)="\begin";
      end loop;

      -- put("arg=" & arg(1..arg_len));
      exit when arg(1..arg_len)="picture";

    end loop;

    -- Passe: **** \begin{picture} ****

    Read_coords( Pdum );         -- Right-Upper corner (we don't care)
    if ch='(' then
      Read_coords( Pdum ); -- Left-Lower corner (we do care) 1-May-2003
      if not macro then
        pic.opt.P0:= Pdum;
      end if;
    end if;

    while not end_of_parsing loop
      read_com;
      exit when end_of_parsing;
      Which_command(com(1..com_len),ziart, False,kommando);
      if ziart = option then
        case K_context(kommando) is
          when cthinlines  => ls.thickness:= thin;
          when cthicklines => ls.thickness:= thick;
        end case;
      else
        Read_new_object;
      end if;
      exit when stop;
    end loop;

    Close(tf);
    pic.saved:= not macro;
    Refresh_size_dependent_parameters(pic, objects => True);
  end Load;

end TC.Input;
