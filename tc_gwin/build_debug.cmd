rem *********************************************************
rem BLOATED EXTRA-SECURE DEBUGGING:
rem *********************************************************

gnatmake -Ptexcad_gps_win32 -XBuild_Mode=Debug

copy /b ..\acu_debg\texcad.exe TeXCAD_debug.exe