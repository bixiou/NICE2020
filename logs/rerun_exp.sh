#!/bin/bash
# Re-run everything on p* = the exponential 1.8C path of the budget_zoom search.
#
# p* reaches every section: it is Section 4's benchmark and the price of the
# reference run, which sets the recycling shares of every model (Section 5
# included). Ordered as before -- joint solves, Exercise 1, isolated solves --
# and resumable at every stage through its own done-file and per-point saves.
J=~/.julia/juliaup/julia-1.12.3+0.x64.w64.mingw32/bin/julia.exe
cd /c/Users/fabre/Documents/www/NICE2020
export NICE_WORKERS=1 NICE_PROPOSALS=Wolfram,Duflo,EqualRight5,EqualRight NICE_RESUME=1
export NICE_DONE_FILE=logs/exp_done.txt

# p* must be the evaluated budget_zoom vector (it starts in 2020), not a
# stale file: refuse to run otherwise
head -2 cap_and_share/data/output/calibrated_global_exp.csv | grep -q "^2020," || {
    echo "p* file missing or not written by src/_write_exp_path.jl"; exit 1; }

# 1. joint solves
NICE_METHODS=B NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/exp_cons.log 2>&1 &
P1=$!
NICE_METHODS=B NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/exp_ede.log 2>&1 &
P2=$!
wait $P1 $P2
echo JOINT_DONE

# 2. Exercise 1
NICE_EX1_COUNTRIES=USA,RUS,CHN,TUR $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/exp_ex1_a.log 2>&1 &
P3=$!
NICE_EX1_COUNTRIES=EU27,IND,NGA,COD $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/exp_ex1_b.log 2>&1 &
P4=$!
wait $P3 $P4
$J --heap-size-hint=3G --project=. src/indifference_curves.jl table >> logs/exp_ex1_table.log 2>&1
echo EX1_DONE

# 3. isolated solves
NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/exp_cons.log 2>&1 &
P5=$!
NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/exp_ede.log 2>&1 &
P6=$!
wait $P5 $P6
echo ALL_EXP_RUNS_DONE
