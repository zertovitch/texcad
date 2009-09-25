rem --  Due to the disappearance of access to libraries through the registry
rem --  in GNAT 2005 and later, we have to set explicit paths.
rem --  We chose to copy all the stuff from gnatcom, gwindows and win32ada
rem --  into a subdirectory named windows_stuff

rem ****GNAT 2005+
set gnatlibs=-Iwindows_stuff

rem ****GNAT 3.15p
rem set gnatlibs=-aIC:\Ada\gwindows\bindings -aIC:\Ada\gnatcom\bindings

rem -- GNATElim stuff:
set gnatlibs=%gnatlibs% -gnatectc_elim.pra

