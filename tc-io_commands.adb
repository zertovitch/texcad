package body TC.IO_Commands is

  function Img( k: Kom_type ) return String is
  begin
    case k is
      when cmakebox     => return "\makebox";
      when cframebox    => return "\framebox";
      when cdottedbox   => return "%\dottedbox";           --  TeXCAD proxy command, as TeX comment
      when cdashbox     => return "\dashbox";
      when crule        => return "\rule";
      when cemline1     => return "\emline";
      when cemline2     => return "%\emline";
      when cline        => return "\line";
      when cdottedline1 => return "\dottedline";
      when cdottedline2 => return "%\dottedline";          --  TeXCAD proxy command, as TeX comment
      when cdashline1   => return "\dashline";
      when cdashline2   => return "%\dashline";            --  TeXCAD proxy command, as TeX comment
      when cdrawline    => return "\drawline";
      when cpath        => return "\path";
      when cvector1     => return "\vector";
      when cvector2     => return "%\vector";
      when cbezier1     => return "\bezier";
      when cqbezier1    => return "\qbezier";
      when cbezier2     => return "%\bezier";              --  TeXCAD proxy command, as TeX comment
      when cqbezier2    => return "%\qbezier";             --  TeXCAD proxy command, as TeX comment
      when cbezvec      => return "%\bezvec";              --  TeXCAD proxy command, as TeX comment
      when cqbezvec     => return "%\qbezvec";             --  TeXCAD proxy command, as TeX comment
      when ccircle1     => return "\circle";
      when ccircle2     => return "%\circle";              --  TeXCAD proxy command, as TeX comment
      when c_paramcurvexy_2 => return "%\paramcurvexy";    --  TeXCAD proxy command, as TeX comment
      when coval        => return "\oval";
      when cput         => return "\put";
      when cbegin       => return "\begin";
      when cend1        => return "\end";
      when cend2        => return "%\end";          --  End of TeXCAD proxy command, as TeX comment
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

