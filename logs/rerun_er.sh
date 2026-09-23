#!/bin/bash
# Re-solve the two Equal Right scenarios now that the schedule prices every
# country (ER_COVERAGE=world): joint solves first, then the isolated ones.
# Its own done-file, so the cells finished under the 173-country club are not
# mistaken for these.
J=~/.julia/juliaup/julia-1.12.3+0.x64.w64.mingw32/bin/julia.exe
cd /c/Users/fabre/Documents/www/NICE2020
export NICE_WORKERS=1 NICE_PROPOSALS=EqualRight5,EqualRight NICE_RESUME=1
export NICE_DONE_FILE=logs/er_done.txt

NICE_METHODS=B NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/er_cons.log 2>&1 &
P1=$!
NICE_METHODS=B NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/er_ede.log 2>&1 &
P2=$!
wait $P1 $P2
echo ER_JOINT_DONE

NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/er_cons.log 2>&1 &
P3=$!
NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/er_ede.log 2>&1 &
P4=$!
wait $P3 $P4
echo ER_ALL_DONE
