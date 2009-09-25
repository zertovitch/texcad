windres texcad.rc texcad.rbj
rem *********************************************************
rem OPTIMIZED IN SPACE (excepted explicit inlining) AND TIME:
rem *********************************************************

rem ** Set a good old version of GNAT (small .exe, Win 9x compatible)
rem ** Trick silent if alternative GNAT not installed...
path f:\ada\gnat315p\bin;%path%

rem Necessary for GNAT 2005 (no more library access through registry):
call set_lib.cmd

set mini=-O2 -gnatp -march=pentium

gnatmake %1 %2 -a -i texcad  %mini% -I.. %gnatlibs% -aO..\ACU_Opti -largs texcad.rbj -s -mwindows
rem gnatmake %1 %2 -i uninstall_texcad %mini% -I.. %gnatlibs% -aO..\ACU_Opti -largs texcad.rbj -s -mwindows
ren texcad.exe TeXCAD.exe
rem ren uninstall_texcad.exe Uninstall_TeXCAD.exe
copy /b texcad.exe TeXCAD_optim.exe
rem upx --brute TeXCAD_optim.exe
rem upx --best Uninstall_TeXCAD.exe
