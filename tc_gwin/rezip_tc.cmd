if exist %1.old.zip del %1.old.zip
copy /b %1.zip %1.old.zip

del *.tmp
del $temp$.zip

rezip -defl %1.zip
del %1.zip
move %1.repacked.zip %1.zip

comp_zip %1.old.zip %1.zip