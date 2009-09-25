windres texcad.rc texcad.rbj
rem *********************************************************
rem BLOATED EXTRA-SECURE DEBUGGING:
rem *********************************************************

rem -gnatE (dynamic elab checks) is necessary for gnavi/gwindows of ~1.2005 ("1.3") :-(

rem Necessary for GNAT 2005 (no more library access through registry):
call set_lib.cmd

set debug=-g -gnato -gnatwa -gnatVa -gnatecdebug.pra
set debuk=%debug%  -fstack-check

gnatmake %1 %2 -i -C texcad %debuk% -I.. %gnatlibs% -aO..\ACU_Debg -bargs -E -largs texcad.rbj -mwindows

rem 17-Feb-2004: -fstack-check dans le "main" provoque access_violation
rem goto final

del ..\ACU_Debg\texcad.o
gnatmake       -i -C texcad %debug% -I.. %gnatlibs% -aO..\ACU_Debg -bargs -E -largs texcad.rbj -mwindows

:final

ren texcad.exe TeXCAD.exe
copy /b texcad.exe TeXCAD_debug.exe