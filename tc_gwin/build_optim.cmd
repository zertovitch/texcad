rem *********************************************************
rem OPTIMIZED IN SPACE (excepted explicit inlining) AND TIME:
rem *********************************************************

del texcad.exe

gnatmake -Ptexcad_gps_win32 -XBuild_Mode=Optimize

ren texcad.exe TeXCAD.exe
copy /b texcad.exe TeXCAD_optim.exe

rem upx --ultra-brute TeXCAD_optim.exe
