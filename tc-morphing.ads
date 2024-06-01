package TC.Morphing is

  type Morphart is (translation, symmetry, rotation, homothethy);

  procedure Morphit
    (p      : in out Picture;
     m      :        Morphart;
     M1, M2 :        Point;
     keep   :        Boolean;
     iter   :        Positive;
     subcmd :        Positive);

  --  Meaning of subcmd:

  --     Symmetry scheme:
  --        4  3  2
  --         \ | /
  --      1 -- * -- 1
  --         / | \
  --        2  3  4

  --     Rotation scheme:
  --        0
  --      1   3
  --        2

end TC.Morphing;
