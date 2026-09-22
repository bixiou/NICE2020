#!/bin/bash
# Re-solve the two Equal Right scenarios (16%/yr and 5%/yr) with their revenue
# recycled equally per capita at the world level (NICE_ER_RECYCLING=world_pc,
# the default since 22 Sept 2026). Wolfram and Banerjee et al. are untouched:
# their results are read back from disk when the tables are written. The
# previous, domestically recycled outputs are in
# cap_and_share/output/_backup_er_domestic_recycling_20260922/.
#
#   bash logs/rerun_er_worldpc.sh cons      (then the same with ede)
#
# Isolated solves first (from scratch: the proposal's welfare targets changed),
# so that the joint solves warm-start from them. Resumable through the done-file.
T=${1:?target: cons or ede}
J=julia.exe
cd "$(dirname "$0")/.." || exit 1

# p* must be the evaluated budget_zoom vector (it starts in 2020)
head -2 cap_and_share/data/output/calibrated_global_exp.csv | grep -q "^2020," || {
    echo "p* file missing or not written by src/_write_exp_path.jl"; exit 1; }

export NICE_TARGET=$T NICE_WORKERS=1 NICE_PROPOSALS=EqualRight5,EqualRight \
       NICE_METHODS=A,B NICE_A_FULL=1 NICE_RESUME=1 NICE_DONE_FILE=logs/er_worldpc_done.txt
export WSLENV=NICE_TARGET:NICE_WORKERS:NICE_PROPOSALS:NICE_METHODS:NICE_A_FULL:NICE_RESUME:NICE_DONE_FILE
$J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/er_worldpc_$T.log 2>&1
echo "ER_WORLDPC_${T}_DONE (exit $?)"
