#!/bin/bash
# Chains the re-run on the end of the budget_zoom search, so it does not depend
# on a live Claude session:
#   1. wait for the search to write its result,
#   2. rebuild p* exactly as the search evaluated it (src/_write_exp_path.jl),
#   3. sanity-check it (src/_diag_pstar.jl) and stop if late-century removals
#      offset half or more of the price-weighted world budget -- the unit every
#      rho is measured in would then be ill-conditioned,
#   4. clear the Exercise 1 grid (backed up in _backup_yearly_price_20260921/),
#   5. run logs/rerun_exp.sh.
J=~/.julia/juliaup/julia-1.12.3+0.x64.w64.mingw32/bin/julia.exe
cd /c/Users/fabre/Documents/www/NICE2020

until grep -aqE "Written:.*calibrated_global_exp|ERROR" logs/tu_zoom.log 2>/dev/null; do sleep 60; done
grep -aq "ERROR" logs/tu_zoom.log && { echo "zoom search failed, see logs/tu_zoom.log"; exit 1; }
echo "zoom search done: $(grep -a 'Best path' logs/tu_zoom.log)"

$J --project=. src/_write_exp_path.jl >> logs/exp_prep.log 2>&1 || { echo "writing p* failed"; exit 1; }
NICE_WORKERS=1 $J --heap-size-hint=2G --project=. src/_diag_pstar.jl >> logs/exp_prep.log 2>&1 || { echo "p* check failed"; exit 1; }

off=$(grep -a "removals offset" logs/exp_prep.log | tail -1 | sed 's/.*offset \(-\{0,1\}[0-9.]*\)%.*/\1/')
awk -v o="$off" 'BEGIN { exit !(o < 50) }' || {
    echo "STOPPED: removals offset ${off}% of the price-weighted world budget"; exit 1; }
echo "p* checked: removals offset ${off}% of the price-weighted world budget"

rm -f cap_and_share/output/indifference/uniform_*.csv cap_and_share/output/indifference/autarky_*.csv
echo "Exercise 1 grid cleared; launching the re-run"
bash logs/rerun_exp.sh
