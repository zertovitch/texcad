package Gt_Goto is

    type Small_Integer is range -32_000 .. 32_000;

    type Goto_Entry is record
        Nonterm  : Small_Integer;
        Newstate : Small_Integer;
    end record;

  --pragma suppress(index_check);

    subtype Row is Integer range -1 .. Integer'Last;

    type Goto_Parse_Table is array (Row range <>) of Goto_Entry;

    Goto_Matrix : constant Goto_Parse_Table :=
       ((-1,-1)  -- Dummy Entry.
-- State  0
,(-3,1),(-2,3)
-- State  1
,(-4,4)
-- State  4
,(-8,10),(-7,9)
-- State  5
,(-5,11)
-- State  11
,(-6,16)
-- State  13
,(-9,21)
-- State  24
,(-10,27)
-- State  27
,(-11,30)
-- State  29
,(-12,33)
);
--  The offset vector
GOTO_OFFSET : array (0.. 35) of Integer :=
(0,
2,3,3,3,5,6,6,6,6,6,6,7,7,8,8,8,8,
8,8,8,8,8,8,8,9,9,9,10,10,11,11,11,11,11,
 11);

subtype Rule        is Natural;
subtype Nonterminal is Integer;

   Rule_Length : array (Rule range  0 ..  17) of Natural := (2,
2,4,0,2,3,0,2,2,8,0,6,1,1,1,0,2,2);
   Get_LHS_Rule: array (Rule range  0 ..  17) of Nonterminal := (-1,
-2,-3,-5,-5,-6,-4,-4,-4,-7,-10,-8,-9,-9,-9,
-11,-11,-12);
end Gt_Goto;
