#!/bin/bash
# Rebuild every variant (and so every table) from the rho already solved under
# the population-weighted criteria. Needed because `run_variant` reported the
# welfare gain of a country as a per-capita NPV while comparing it with a
# population-weighted target: the solved rho were right, the reported gains and
# the counts of losing members were not.
#
# Cheap next to a solve: four model runs per cell, no search. Resumes through
# its own done-file.
J=~/.julia/juliaup/julia-1.12.3+0.x64.w64.mingw32/bin/julia.exe
cd /c/Users/fabre/Documents/www/NICE2020
export NICE_WORKERS=1 NICE_PROPOSALS=Wolfram,Duflo,EqualRight5,EqualRight
export NICE_RESUME=1 NICE_VARIANTS_ONLY=1 NICE_METHODS=A,B
export NICE_DONE_FILE=logs/tu_done_variants.txt

NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/tu_var_cons.log 2>&1 &
P1=$!
NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/tu_var_ede.log 2>&1 &
P2=$!
wait $P1 $P2
echo VARIANTS_REBUILT
