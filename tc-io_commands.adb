package body TC.IO_Commands is

  function Img( k: Kom_type ) return String is
  begin
    case k is
      when cmakebox     => return "\makebox";
      when cframebox    => return "\framebox";
      when cdottedbox   => return "%\dottedbox";
      when cdashbox     => return "\dashbox";
      when crule        => return "\rule";
      when cemline1     => return "\emline";
      when cemline2     => return "%\emline";
      when cline        => return "\line";
      when cdottedline1 => return "\dottedline";
      when cdottedline2 => return "%\dottedline";
      when cdashline1   => return "\dashline";
      when cdashline2   => return "%\dashline";
      when cdrawline    => return "\drawline";
      when cpath        => return "\path";
      when cvector1     => return "\vector";
      when cvector2     => return "%\vector";
      when cbezier1     => return "\bezier";
      when cqbezier1    => return "\qbezier";
      when cbezier2     => return "%\bezier";
      when cqbezier2    => return "%\qbezier";
      when cbezvec      => return "%\bezvec";
      when cqbezvec     => return "%\qbezvec";
      when ccircle1     => return "\circle";
      when ccircle2     => return "%\circle";
      when coval        => return "\oval";
      when cput         => return "\put";
      when cbegin       => return "\begin";
      when cend1        => return "\end";
      when cend2        => return "%\end";
      when caux         => return "";
      when cunit        => return "\unitlength";
      when cspec        => return "\special";
      when con          => return "\on";
      when coff         => return "\off";
      when cgrade       => return "%\grade";
      when clines       => return "%\emlines";
      when cepic        => return "%\epic";
      when cmd_pv_insert=> return "%\pvinsert";
      when cbezmac      => return "%\beziermacro";
      when creduce      => return "%\reduce";
      when csnap        => return "%\snapping";
      when cqual        => return "%\quality";
      when cgdiff       => return "%\graddiff";
      when csnapasp     => return "%\snapasp";
      when czoom        => return "%\zoom";
      when cthickness   => return "\linethickness";
      when cthinlines   => return "\thinlines";
      when cthicklines  => return "\thicklines";
      when csetlen      => return "\setlength";
      when cputaux      => return "";
    end case;
  end Img;

end TC.IO_Commands;

