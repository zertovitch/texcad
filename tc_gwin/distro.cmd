rem ***** VERSION: *****
set tcver=451
set tcref=a75

del tc_%tcver%_*.zip

rem Ensure we have latest builds
del TeXCAD_optim.exe
call build_optim.cmd
del TeXCAD_debug.exe
call build_debug.cmd

rem upx --ultra-brute TeXCAD_optim.exe

set distro=optim
call pack_bin
set distro=debug
call pack_bin

ren tc_%tcver%_woptim.zip tc_%tcver%_wo.zip
ren tc_%tcver%_wdebug.zip tc_%tcver%_wd.zip

cd..
call pack_src
cd tc_gwin
copy /B ..\TCA00x_s.zip tc_%tcver%_s.zip

rem goto skip_rezip

call rezip_tc tc_%tcver%_s
call rezip_tc tc_%tcver%_wo
call rezip_tc tc_%tcver%_wd

:skip_rezip

ren tc_%tcver%_s.zip tc_%tcver%_s.%tcref%.zip
ren tc_%tcver%_wo.zip tc_%tcver%_wo.%tcref%.zip
ren tc_%tcver%_wd.zip tc_%tcver%_wd.%tcref%.zip

rem  For our archives
zipada TC_%tcver%_Distro_%tcref%.zip tc_%tcver%_s.%tcref%.zip tc_%tcver%_wo.%tcref%.zip tc_%tcver%_wd.%tcref%.zip

pause