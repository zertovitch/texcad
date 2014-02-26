@echo off
echo Build the GT (graphthing) parser from GT.y (parser grammar) and GT.l (lexer).
echo You need GNAT, AYACC and AFLEX visible in the PATH.
echo AYACC needs to be the version with the Prefix_All option (20-Nov-2013 or later).
echo AYACC is available with the Pascal-to-Ada translator @ http://p2ada.sf.net/ .

ayacc gt.y Off Off On Off Off .a On
gnatchop -w gt.a

aflex -i -x -E gt.l

gnatchop -w gt_io.a
gnatchop -w gt_dfa.a
gnatchop -w gt.a

del gt_io.a
del gt_dfa.a
del gt.a

gnatmake -P gt

pause
