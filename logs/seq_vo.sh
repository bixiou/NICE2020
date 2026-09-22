#!/bin/bash
J=~/.julia/juliaup/julia-1.12.3+0.x64.w64.mingw32/bin/julia.exe
cd /c/Users/fabre/Documents/www/NICE2020
export NICE_WORKERS=1 NICE_VARIANTS_ONLY=1 NICE_PROPOSALS=Wolfram,Duflo,EqualRight5,EqualRight
NICE_TARGET=cons $J --heap-size-hint=3G --project=. src/run_solves_sept2026.jl > logs/vo_cons.log 2>&1
NICE_TARGET=ede  $J --heap-size-hint=3G --project=. src/run_solves_sept2026.jl > logs/vo_ede.log 2>&1
echo SEQUENTIAL_VARIANTS_DONE
