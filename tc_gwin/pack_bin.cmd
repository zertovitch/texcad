@echo off

if "%tcver%"=="" goto morale
if "%distro%"=="" goto morale
md $_%distro%_pack_dir
cd $_%distro%_pack_dir
copy /B    ..\texcad_%distro%.exe TeXCAD.exe
copy       ..\TC_Todo_GWin.txt
copy    ..\..\TC_Todo.txt
copy       ..\"Uninstall TeXCAD.bat"
copy    ..\..\TeXCAD.txt
copy /B ..\..\TeXCAD.pdf
copy    ..\..\COPYING.txt
copy    ..\..\Changes.txt
copy    ..\..\Changes_also_minor.txt
copy       ..\Readme_%distro%_version.txt

call 7zip a -mx ..\tc_%tcver%_w%distro% *

cd..
goto fin
:morale
echo.
echo You should run pack_distro.cmd !
echo.
pause
:fin