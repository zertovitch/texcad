@echo off

rem   Create a DOT ( http://www.graphviz.org/ ) file for displaying
rem   TeXCAD's unit dependency structure as a directed graph.
rem   The tool is called DePlo, http://sites.google.com/site/depplot/

deplo config=tc_dep.cfg >tc_dep.dot

dot -o tc_dep.emf -Temf tc_dep.dot

pause
