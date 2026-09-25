#!/bin/bash
# Re-solve the two Equal Right scenarios with the schedule extended to the whole
# world (NICE_ER_COVERAGE=world, the default): the six economies the report does
# not list are priced at their income group's median charge, so the club is all
# 179 countries. Revenue is pooled and paid back per capita (NICE_ER_RECYCLING=world_pc).
# Wolfram and Banerjee et al. are untouched; their results are read from disk.
# The previous outputs, with the six unpriced and outside the club, are in
# cap_and_share/output/_backup_er_listed_club_20260923/ (NICE_ER_COVERAGE=listed
# reproduces that scenario).
#
#   bash logs/rerun_er_world_club.sh cons      (and, in parallel, ede)
#
# Isolated solves first (from scratch), so the joint solves warm-start from them.
# NOTE: this folder is synced by Google Drive/OneDrive, which can restore deleted
# checkpoints; the caller must check that rho_A_equalright*.csv are absent at
# launch, and the log must show "solving N of M club members exactly" rather
# than "option A: resuming".
T=${1:?target: cons or ede}
J=julia.exe
cd "$(dirname "$0")/.." || exit 1

head -2 cap_and_share/data/output/calibrated_global_exp.csv | grep -q "^2020," || {
    echo "p* file missing or not written by src/_write_exp_path.jl"; exit 1; }

export NICE_TARGET=$T NICE_WORKERS=1 NICE_PROPOSALS=EqualRight5,EqualRight \
       NICE_METHODS=A,B NICE_A_FULL=1 NICE_RESUME=1 NICE_DONE_FILE=logs/er_world_club_done.txt
export WSLENV=NICE_TARGET:NICE_WORKERS:NICE_PROPOSALS:NICE_METHODS:NICE_A_FULL:NICE_RESUME:NICE_DONE_FILE
$J --heap-size-hint=2G --project=. src/run_solves_sept2026.jl >> logs/er_world_club_$T.log 2>&1
echo "ER_WORLD_CLUB_${T}_DONE (exit $?)"
