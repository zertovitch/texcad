rem *********************************************************
rem BLOATED EXTRA-SECURE DEBUGGING:
rem *********************************************************

gnatmake -Ptexcad_gwin -XBuild_Mode=Debug

copy /b ..\acu_debg\texcad.exe TeXCAD_debug.exe