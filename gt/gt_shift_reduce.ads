package Gt_Shift_Reduce is

    type Small_Integer is range -32_000 .. 32_000;

    type Shift_Reduce_Entry is record
        T   : Small_Integer;
        Act : Small_Integer;
    end record;
    pragma Pack(Shift_Reduce_Entry);

    subtype Row is Integer range -1 .. Integer'Last;

  --pragma suppress(index_check);

    type Shift_Reduce_Array is array (Row  range <>) of Shift_Reduce_Entry;

    Shift_Reduce_Matrix : constant Shift_Reduce_Array :=
        ( (-1,-1) -- Dummy Entry

-- State  0
,( 11, 2),(-1,-3000)
-- State  1
,(-1,-6)
-- State  2
,( 5, 5),(-1,-3000)
-- State  3
,( 0,-3001),(-1,-3000)
-- State  4
,( 13, 7),( 14, 8),(-1,-1)
-- State  5
,(-1,-3)
-- State  6
,(-1,-3000)
-- State  7
,( 3, 12),(-1,-3000)
-- State  8
,( 3, 13),(-1,-3000)
-- State  9
,(-1,-7)
-- State  10
,(-1,-8)
-- State  11
,( 4, 14),( 6, 15),(-1,-3000)
-- State  12
,( 15, 17),(-1,-3000)
-- State  13
,( 18, 18),( 19, 19),( 20, 20),(-1,-3000)
-- State  14
,( 7, 22),(-1,-3000)
-- State  15
,(-1,-2)
-- State  16
,(-1,-4)
-- State  17
,( 8, 23),(-1,-3000)
-- State  18
,(-1,-12)
-- State  19
,(-1,-13)
-- State  20
,(-1,-14)
-- State  21
,( 3, 24),(-1,-3000)
-- State  22
,( 3, 25),(-1,-3000)
-- State  23
,( 2, 26),(-1,-3000)
-- State  24
,(-1,-10)
-- State  25
,(-1,-5)
-- State  26
,( 10, 28),(-1,-3000)
-- State  27
,( 16, 29),(-1,-15)
-- State  28
,( 2, 31),(-1,-3000)
-- State  29
,( 17, 32),(-1,-3000)
-- State  30
,(-1,-11)
-- State  31
,( 9, 34),(-1,-3000)
-- State  32
,( 2, 35),(-1,-3000)
-- State  33
,(-1,-16)
-- State  34
,(-1,-9)
-- State  35
,(-1,-17)
);
--  The offset vector
SHIFT_REDUCE_OFFSET : array (0.. 35) of Integer :=
( 0,
 2, 3, 5, 7, 10, 11, 12, 14, 16, 17, 18, 21, 23, 27, 29, 30,
 31, 33, 34, 35, 36, 38, 40, 42, 43, 44, 46, 48, 50, 52, 53, 55,
 57, 58, 59);
end Gt_Shift_Reduce;
