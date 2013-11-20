ayacc gt.y Off Off On Off Off .a On
gnatchop -w gt.a

pause

aflex -i -E gt.l

gnatchop -w gt_io.a
gnatchop -w gt_dfa.a
gnatchop -w gt.a

del gt_io.a
del gt_dfa.a
del gt.a

gnatmake -P gt
