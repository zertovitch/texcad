rem *********************************************************
rem BLOATED EXTRA-SECURE DEBUGGING:
rem *********************************************************

del texcad.exe

gnatmake -Ptexcad_gps_win32 -XBuild_Mode=Debug

ren texcad.exe TeXCAD.exe
copy /b texcad.exe TeXCAD_debug.exe