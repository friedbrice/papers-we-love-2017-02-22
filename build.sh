#!/bin/bash

mkdir -p texbuild
mkdir -p target
cp -r img texbuild/img

pandoc --from=markdown --to=beamer --standalone \
  < presentation.md > texbuild/presentation.tex

cd texbuild
pdflatex -interaction=batchmode -halt-on-error presentation.tex
cd ..
mv texbuild/presentation.pdf presentation.pdf
