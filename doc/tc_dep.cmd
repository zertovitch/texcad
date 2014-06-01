@echo off

rem   Create a DOT ( http://www.graphviz.org/ ) file for displaying
rem   TeXCAD's unit dependency structure as a directed graph.
rem   The tool is called DePlo, http://sites.google.com/site/depplot/

rem Refresh the library files through a fresh build
gnatmake -P ../tc_gwin/texcad_gwin

rem Run DePlo
deplo config=tc_dep.cfg >tc_dep.dot

rem Run DOT, a Graphviz tool
dot -o tc_dep.emf -Temf tc_dep.dot

pause
