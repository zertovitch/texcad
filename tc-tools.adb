package body TC.Tools is

  procedure Analyse(o: Obj_type; possible: out Cleanup_action) is
  begin
    case o.art is
       when aux | txt | putaux | box =>
         if o.inhalt = "" then
           possible(empty_text):= True;
         end if;
         if Almost_zero(Norm2(o.size)) then
           possible(zero_sized_object):= True;
         end if;
       when line =>
         if Almost_zero(Norm2(o.P2-o.P1)) then
           possible(zero_sized_object):= True;
         end if;
       when circ | disc =>
         if Almost_zero(o.rad) then
           possible(zero_sized_object):= True;
         end if;
       when oval =>
         if Almost_zero(Norm2(o.osize)) then
           possible(zero_sized_object):= True;
         end if;
       when bezier =>
         if Almost_zero(Norm2(o.PE-o.P1)) then
           possible(zero_sized_object):= True;
         end if;
       when others =>
         null;
    end case;
  end Analyse;

  procedure Detect(pic: Picture; stat: out Detection_stat) is
    o: ptr_Obj_type:= pic.root;
    possible: Cleanup_action;
    obj_pos: Natural:= 0;
  begin
    stat:= (others => (0,1));
    while o /= null loop
      obj_pos:= obj_pos + 1;
      Analyse(o.all, possible);
      for topic in possible'Range loop
        if possible(topic) then
          if stat(topic).number = 0 then
            stat(topic).first_obj_pos:= obj_pos;
          end if;
          stat(topic).number:= stat(topic).number + 1;
        end if;
      end loop;
      o:= o.next;
    end loop;
  end Detect;
  
  procedure Clean(pic: in out Picture; action: Cleanup_action) is
    o: ptr_Obj_type:= pic.root;
    next: ptr_Obj_type;
    possible: Cleanup_action;
    do_it: Boolean;
  begin
    while o /= null loop
      Analyse(o.all, possible);
      do_it:= False;
      for topic in possible'Range loop
        if possible(topic) and action(topic) then
          do_it:= True;
        end if;
      end loop;
      next:= o.next;
      if do_it then
        Dispose(o);
      end if;
      o:= next;
    end loop;
  end Clean;

end TC.Tools;
