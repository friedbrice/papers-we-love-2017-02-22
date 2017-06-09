#!/bin/bash

pandoc --to=html5 --standalone --mathjax --output=summary.html summary.md
pandoc --to=beamer --output=presentation.pdf presentation.md
cd demo
ghc Sampler.hs
cd ..
mv demo/Sampler .
