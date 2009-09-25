rem ***** VERSION: *****
set tcver=42

del tc_%tcver%_*.zip

upx --brute TeXCAD_optim.exe

set distro=optim
call pack_bin
set distro=debug
call pack_bin

ren tc_%tcver%_woptim.zip tc_%tcver%_wo.zip
ren tc_%tcver%_wdebug.zip tc_%tcver%_wd.zip

cd..
call pack_src_with_ali
cd tc_gwin
copy /B ..\TCA00x_s.zip tc_%tcver%_s.zip

call rezip tc_%tcver%_s.zip
call rezip tc_%tcver%_wo.zip
call rezip tc_%tcver%_wd.zip

zip -9 TC_%tcver%_Distro_0xx.zip tc_%tcver%_s.zip tc_%tcver%_wo.zip tc_%tcver%_wd.zip

pause