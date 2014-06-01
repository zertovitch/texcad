rem Call LaTeX 3 times, because...
rem LaTeX Warning: Label(s) may have changed. Rerun to get cross-references right.
latex texcad 
latex texcad 
latex texcad 
dvipdfm -p a4 TeXCAD
ren TeXCAD.pdf TeXCAD.pdf
