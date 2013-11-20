ayacc gt.y
gnatchop -w gt.a

aflex -i -E gt.l

gnatchop -w gt_io.a
gnatchop -w gt_dfa.a
gnatchop -w gt.a

del gt_io.a
del gt_dfa.a
del gt.a

gnatmake -P gt
