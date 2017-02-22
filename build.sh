#!/bin/bash

/usr/local/bin/pandoc --from=markdown --to=beamer --standalone \
  < presentation.md > presentation.tex

/Library/TeX/texbin/pdflatex presentation.tex
