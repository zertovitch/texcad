rem *********************************************************
rem OPTIMIZED IN SPACE (excepted explicit inlining) AND TIME:
rem *********************************************************

gnatmake -Ptexcad_gwin -XBuild_Mode=Optimize

copy /b ..\acu_opti\texcad.exe TeXCAD_optim.exe

rem upx --ultra-brute TeXCAD_optim.exe
