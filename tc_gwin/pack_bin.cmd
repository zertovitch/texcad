@echo off

if "%tcver%"=="" goto morale
if "%distro%"=="" goto morale

if exist ..\doc\TeXCAD.pdf goto doc_ok
cd ..
cd doc
call doc
cd ..
cd tc_gwin
:doc_ok

md $_%distro%_pack_dir
cd $_%distro%_pack_dir
copy /B    ..\texcad_%distro%.exe TeXCAD.exe
copy       ..\TC_Todo_GWin.txt
copy    ..\..\TC_Todo.txt
copy       ..\"Uninstall TeXCAD.bat"
copy    ..\..\TeXCAD.txt
copy /B ..\..\doc\TeXCAD.pdf
copy    ..\..\Copying.txt
copy    ..\..\Changes.txt
copy    ..\..\Changes_also_minor.txt
copy       ..\Readme_%distro%_version.txt

7z a -mx ..\tc_%tcver%_w%distro%.zip *

cd..
goto fin
:morale
echo.
echo You should run distro.cmd !
echo.
pause
:fin