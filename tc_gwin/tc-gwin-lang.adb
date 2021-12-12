with TC.GWin.Lang.English,
     TC.GWin.Lang.French;

package body TC.GWin.Lang is

  function Speak (l: Language; m: Message) return GString is
    function Babel return Unbounded_String is
    begin
      case l is
        when l_english => return English.m(m);
        when l_french  => return French.m(m);
      end case;
    end Babel;
  begin
    return
      -- GString can be either a Wide (UNICODE) or an ANSI string type
      To_GString_From_String(
        To_String(Babel)
      );
  end Speak;

  function Msg (m: Message) return GString is
  begin
    return Speak ( startup_language, m );
  end Msg;

  function Filter_amp( s: GString ) return GString is
    r: GString( 1 .. s'Length );
    l: Natural:= 0;
  begin
    for i in s'Range loop
      if s(i) /= '&' then
        l:= l+1;
        r(l):= s(i);
      end if;
    end loop;
    return r(1..l);
  end Filter_amp;

  function Language_rich_image( l: Language ) return GString is
  begin
    case l is
      when l_english => return "English";
      when l_french  => return "Français";
    end case;
  end Language_rich_image;

end TC.GWin.Lang;
