#!/bin/bash
# Re-run everything under total utilitarianism (population-weighted NPVs).
#
# Ordered by what the paper needs first: the joint solves (main text), then
# Exercise 1 (Figures 1-2 and Table 1), then the isolated solves (appendix).
# Every stage resumes: the proposal driver skips the cells listed in
# logs/tu_done.txt, option A reseeds from its per-country checkpoint, and
# Exercise 1 saves after every grid point. This machine reboots without
# warning, so nothing here may depend on finishing in one go.
J=~/.julia/juliaup/julia-1.12.3+0.x64.w64.mingw32/bin/julia.exe
cd /c/Users/fabre/Documents/www/NICE2020
export NICE_WORKERS=1 NICE_PROPOSALS=Wolfram,Duflo,EqualRight5,EqualRight NICE_RESUME=1

# ── 1. joint solves (option B), one process per criterion ────────────────────
NICE_METHODS=B NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/tu_cons.log 2>&1 &
P1=$!
NICE_METHODS=B NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/tu_ede.log 2>&1 &
P2=$!
wait $P1 $P2
echo JOINT_SOLVES_DONE

# ── 2. Exercise 1: the indifference grid, four countries per process ─────────
NICE_EX1_COUNTRIES=USA,RUS,CHN,TUR $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/tu_ex1_a.log 2>&1 &
P3=$!
NICE_EX1_COUNTRIES=EU27,IND,NGA,COD $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/tu_ex1_b.log 2>&1 &
P4=$!
wait $P3 $P4
$J --heap-size-hint=3G --project=. src/indifference_curves.jl table >> logs/tu_ex1_table.log 2>&1
echo EX1_DONE

# ── 3. isolated solves (option A), re-solved rather than reused: the isolated
#       rho itself depends on the criterion ──────────────────────────────────
NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/tu_cons.log 2>&1 &
P5=$!
NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/tu_ede.log 2>&1 &
P6=$!
wait $P5 $P6
echo ALL_TOTAL_UTIL_RUNS_DONE
