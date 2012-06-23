del b~*.*
cd TC_GWin
del b~*.*
cd..

REM call 7zip a -mx TCA00x_s -i@pack_src.lst
REM ^ On 17-Oct-2003 7z ceased to work for @lists !

rem 7zip_210 a -mx TCA00x_s -i@pack_src.lst

7z a -tzip -mx TCA00x_s.zip -i@pack_src.lst
