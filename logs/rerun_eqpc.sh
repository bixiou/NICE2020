#!/bin/bash
# The equal-per-capita within-country recycling variant (Online Appendix):
# Table 2's analogue (joint solves, both criteria) and Figure 1's analogue
# (the Exercise 1 grid, then the R figures). Everything lands in
# cap_and_share/output/equal_pc/, and the figures carry the "eqpc" tag, so the
# files behind the main text are untouched. The isolated solves are not needed
# for either exhibit and are skipped.
J=~/.julia/juliaup/julia-1.12.3+0.x64.w64.mingw32/bin/julia.exe
cd /c/Users/fabre/Documents/www/NICE2020
export NICE_WORKERS=1 NICE_RECYCLING=equal_pc NICE_RESUME=1
export NICE_PROPOSALS=Wolfram,Duflo,EqualRight5,EqualRight
export NICE_DONE_FILE=logs/eqpc_done.txt

# wait for the Equal Right re-run to release the machine
until grep -q "ER_ALL_DONE" logs/rerun_er.out 2>/dev/null; do sleep 120; done

NICE_METHODS=B NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/eqpc_cons.log 2>&1 &
P1=$!
NICE_METHODS=B NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/eqpc_ede.log 2>&1 &
P2=$!
wait $P1 $P2
echo EQPC_JOINT_DONE

NICE_EX1_COUNTRIES=USA,RUS,CHN,TUR $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/eqpc_ex1_a.log 2>&1 &
P3=$!
NICE_EX1_COUNTRIES=EU27,IND,NGA,COD $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/eqpc_ex1_b.log 2>&1 &
P4=$!
wait $P3 $P4
$J --heap-size-hint=3G --project=. src/indifference_curves.jl table >> logs/eqpc_ex1_table.log 2>&1
echo EQPC_EX1_DONE

"/c/Program Files/R/R-4.4.3/bin/Rscript" cap_and_share/indifference_curves.R >> logs/eqpc_figs.log 2>&1
echo EQPC_ALL_DONE
