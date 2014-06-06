-- 23-Feb-2004: sperated from TC.
-- Commands as they appear in the TeX files
-- x1, x2: variants with "x" (as is), "%x" (emulated)

package TC.IO_Commands is

  type Kom_type is
           (cmakebox,                   -- text
            cframebox,
            cdottedbox,
            cdashbox,
            crule,                      -- boxes
            cemline1, cemline2,         -- "any slope"-lines
            cline,                      -- \line
            cdottedline1, cdottedline2, -- \dottedline (epic) +20-Jan-2004
            cdashline1, cdashline2,     -- \dashline   (epic) +24-Feb-2004
            cdrawline,                  -- \drawline   (epic) +23-Jan-2007
            cpath,                      -- \path      (eepic) +23-Jan-2007
            cvector1,cvector2,
            cbezier1,cbezier2, cqbezier1, cqbezier2,
            -- \qbezier added 24-Apr-2003
            cbezvec, cqbezvec,
            ccircle1,ccircle2,
            coval,cput,cbegin,cend1,cend2,
            cthinlines, cthicklines,
            caux,
            cunit,csetlen,cspec,con,coff,
            cgrade,cepic,
            cmd_pv_insert, -- Jan-2007
            clines,cbezmac,creduce,csnap,cqual,cgdiff,
            csnapasp,czoom,
            cthickness,
            cputaux);

  -- type Kom_set is array (Kom_type) of Boolean;

  -- specials: constant Kom_set:=
  --   (cunit|csetlen|cthickness|cspec => True, others => False);

  subtype K_rectangle is Kom_type range cframebox .. cdashbox;

  subtype K_context is Kom_type range cthinlines .. cthicklines;

  subtype K_option is Kom_type range cunit .. cthickness;

  kommando_art: constant array(Kom_type) of Obj_art_type :=
    ( cmakebox => txt,
      cframebox .. crule       => box,
      cemline1 .. cvector2     => line,
      cbezier1 .. cqbezvec     => bezier,
      ccircle1 .. ccircle2     => circ,
      coval     => oval,
      cput      => point0,
      cbegin    => beginn,
      cend1     => ende1,
      cend2     => ende2,
      K_context => option,
      caux      => aux,
      K_option  => option,
      cputaux   => putaux
    );

  function Img(k: Kom_type) return String;

end TC.IO_Commands;
