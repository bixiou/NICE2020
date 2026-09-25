#!/bin/bash
# Full re-run on the 10-year ramp p*: 0 in 2025, linear to $145.70 in 2035, then
# +1.6%/yr (budget-exact 1000 GtCO2 over 2025-2100, welfare counted to 2300).
# p* sets the reference run, hence the recycling shares, so every result depends
# on it: Section 4 (Exercise 1) and Section 5 (all four schedules).
# The 5-year-ramp outputs are in cap_and_share/output/_backup_pstar_ramp5_20260924/.
#   bash logs/rerun_pstar_ramp10.sh <stage>     stage = joint | ex1 | isolated
J=julia.exe
cd "$(dirname "$0")/.." || exit 1
head -3 cap_and_share/data/output/calibrated_global_exp.csv | grep -q "2035" || true
export NICE_WORKERS=1 NICE_PROPOSALS=Wolfram,Duflo,EqualRight5,EqualRight NICE_RESUME=1 \
       NICE_DONE_FILE=logs/pstar_ramp10_done.txt
export WSLENV=NICE_WORKERS:NICE_PROPOSALS:NICE_RESUME:NICE_DONE_FILE:NICE_METHODS:NICE_TARGET:NICE_A_FULL:NICE_EX1_COUNTRIES
case "$1" in
  joint)
    NICE_METHODS=B NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/ramp10_cons.log 2>&1 &
    P1=$!
    NICE_METHODS=B NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/ramp10_ede.log 2>&1 &
    P2=$!
    wait $P1 $P2; echo JOINT_DONE ;;
  ex1)
    NICE_EX1_COUNTRIES=USA,RUS,CHN,TUR $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/ramp10_ex1_a.log 2>&1 &
    P3=$!
    NICE_EX1_COUNTRIES=EU27,IND,NGA,COD $J --heap-size-hint=2G --project=. src/indifference_curves.jl >> logs/ramp10_ex1_b.log 2>&1 &
    P4=$!
    wait $P3 $P4
    $J --heap-size-hint=3G --project=. src/indifference_curves.jl table >> logs/ramp10_ex1_table.log 2>&1
    echo EX1_DONE ;;
  isolated)
    NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=cons $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/ramp10_cons.log 2>&1 &
    P5=$!
    NICE_METHODS=A NICE_A_FULL=1 NICE_TARGET=ede  $J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/ramp10_ede.log 2>&1 &
    P6=$!
    wait $P5 $P6; echo ISOLATED_DONE ;;
  *) echo "stage must be joint, ex1 or isolated"; exit 1 ;;
esac
