#!/bin/bash
# Runs the three stages of the 10-year-ramp re-run in order, then regenerates
# the figures and tables. Exercise 1 caches per-point results in
# cap_and_share/output/indifference/*.csv keyed on nothing but the grid, so they
# are cleared first: p* changed, and stale points would be silently reused.
cd "$(dirname "$0")/.." || exit 1
S=/tmp/claude-1000/-mnt-c-Users-fabre-Documents-www-NICE2020/cbbe87cf-785f-4058-bcb2-563ad16ac819/scratchpad
until grep -q JOINT_DONE $S/stage_joint.out 2>/dev/null; do sleep 60; done
rm -f cap_and_share/output/indifference/*.csv
bash logs/rerun_pstar_ramp10.sh ex1 >> $S/stage_ex1.out 2>&1
bash logs/rerun_pstar_ramp10.sh isolated >> $S/stage_isolated.out 2>&1
export NICE_WORKERS=1 WSLENV=NICE_WORKERS:NICE_TARGET
NICE_TARGET=cons julia.exe --heap-size-hint=3G --project=. src/equivalent_rights_proposals.jl tables >> logs/ramp10_tables.log 2>&1
NICE_TARGET=ede  julia.exe --heap-size-hint=3G --project=. src/equivalent_rights_proposals.jl tables >> logs/ramp10_tables.log 2>&1
Rscript.exe cap_and_share/indifference_curves.R >> logs/ramp10_figures.log 2>&1
echo ALL_RAMP10_DONE
