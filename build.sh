#!/bin/bash

/usr/local/bin/pandoc --from=markdown --to=beamer --standalone < presentation.md > presentation.tex
/Library/TeX/texbin/pdflatex presentation.tex
/usr/bin/osascript << EOF
  set theFile to POSIX file presentation.tex as alias
  tell application "Skim"
  activate
  set theDocs to get documents whose path is (get POSIX path of theFile)
  if (count of theDocs) > 0 then revert theDocs
  open theFile
  end tell
EOF
