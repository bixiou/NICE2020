#!/usr/bin/env bash
# Compiles the versions of the paper in build/ and copies the PDFs here:
#   paper.pdf                          full manuscript, with author details
#   paper_blind.pdf                    anonymised manuscript (no author details)
#   title_page.pdf                     title page alone
#   declaration_competing_interest.pdf declaration of competing interest
# The version is chosen by \version (full | blind | titlepage), set here before
# paper.tex is read; paper.tex itself defaults to full.
set -euo pipefail
cd "$(dirname "$0")"
build() {   # build <jobname> <version>
  latexmk -pdf -interaction=nonstopmode -outdir=build -jobname="$1" \
          -usepretex="\\def\\version{$2}" paper.tex > "build/$1.build.log" 2>&1 \
    || { echo "failed: $1 (see build/$1.build.log)"; exit 1; }
  cp "build/$1.pdf" "$1.pdf"; echo "written: $1.pdf"
}
mkdir -p build
build paper full
build paper_blind blind
build title_page titlepage
latexmk -pdf -interaction=nonstopmode -outdir=build declaration_competing_interest.tex \
  > build/declaration_competing_interest.build.log 2>&1
cp build/declaration_competing_interest.pdf . && echo "written: declaration_competing_interest.pdf"
