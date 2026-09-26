#!/bin/bash
# 26 Sept 2026: full re-run after p* is re-searched with welfare counted over
# 2025-2100 (it was 2035-2300), in the two variants of the paper. The previous
# results (NPV over 2025-2100, old p*) are in
# cap_and_share/output/_backup_pstar_welf2035_20260926/.
# Run from the repository root, on a 4-core machine.
J="julia --heap-size-hint=2G --project=."
L=logs/pstar2025
O=cap_and_share/output
W=$O/_backup_pstar_welf2035_20260926/indifference     # rest-of-world prices of the old p*: warm starts

# 1. the p* search (launched separately):
#   NICE_USE_BUDGET=1 NICE_BUDGET_EXACT=1 NICE_BUDGET_LIMIT=1000 NICE_BUDGET_START=2025 \
#   NICE_WELFARE_START=2025 NICE_WELFARE_END=2100 NICE_N_ZOOM=3 NICE_B_MIN=0.0 NICE_B_MAX=0.10 \
#   julia --project=. cap_and_share/find_global_exp_carbon_tax_buget_zoom.jl > logs/pstar2025/zoom.log
until grep -aqE "Written:.*calibrated_global_exp|ERROR" $L/zoom.log 2>/dev/null; do sleep 60; done
grep -aq "ERROR" $L/zoom.log && { echo "p* search failed"; exit 1; }
echo "p* search done: $(grep -a 'Best path' $L/zoom.log)"

# 2. clear what a new run would silently reuse (the joint solves' rho are kept as warm starts)
for d in $O $O/equal_pc; do
  rm -f $d/equivalent_rights_variants*.csv $d/country_gains*.csv $d/rho_A_*.csv $d/rho_B2_*.csv
  rm -f $d/indifference/*.csv
done

# 3. build each variant's proposals once, so that the parallel jobs read them from the cache
NICE_RECYCLING=negishi  NICE_TARGET=cons $J src/equivalent_rights_proposals.jl tables > $L/prep_cons.log 2>&1 &
NICE_RECYCLING=equal_pc NICE_TARGET=ede  $J src/equivalent_rights_proposals.jl tables > $L/prep_welf.log 2>&1 &
wait

# 4. every cell, four at a time, longest first
C="NICE_RECYCLING=negishi NICE_TARGET=cons"; E="NICE_RECYCLING=equal_pc NICE_TARGET=ede"
cat > $L/jobs.txt <<EOF
$C NICE_METHODS=B NICE_PROPOSALS=EqualRight5 NICE_DONE_FILE=$L/d1 $J src/run_solves_sept2026.jl > $L/cons_B_er5.log 2>&1
$E NICE_METHODS=B NICE_PROPOSALS=EqualRight5 NICE_DONE_FILE=$L/d2 $J src/run_solves_sept2026.jl > $L/welf_B_er5.log 2>&1
$E NICE_METHODS=B NICE_PROPOSALS=Wolfram NICE_DONE_FILE=$L/d3 $J src/run_solves_sept2026.jl > $L/welf_B_wolfram.log 2>&1
$E NICE_METHODS=B NICE_PROPOSALS=Duflo NICE_DONE_FILE=$L/d4 $J src/run_solves_sept2026.jl > $L/welf_B_duflo.log 2>&1
$C NICE_METHODS=A NICE_A_FULL=1 NICE_PROPOSALS=EqualRight5 NICE_DONE_FILE=$L/d5 $J src/run_solves_sept2026.jl > $L/cons_A_er5.log 2>&1
$E NICE_EX1_WARM=$W $J src/indifference_curves.jl > $L/grid_welf.log 2>&1
$C NICE_METHODS=A NICE_A_FULL=1 NICE_PROPOSALS=Duflo NICE_DONE_FILE=$L/d6 $J src/run_solves_sept2026.jl > $L/cons_A_duflo.log 2>&1
$C NICE_METHODS=B NICE_PROPOSALS=EqualRight NICE_DONE_FILE=$L/d7 $J src/run_solves_sept2026.jl > $L/cons_B_er.log 2>&1
$C NICE_EX1_WARM=$W $J src/indifference_curves.jl > $L/grid_cons.log 2>&1
$C NICE_METHODS=B NICE_PROPOSALS=Duflo NICE_DONE_FILE=$L/d8 $J src/run_solves_sept2026.jl > $L/cons_B_duflo.log 2>&1
$C NICE_METHODS=A NICE_A_FULL=1 NICE_PROPOSALS=Wolfram NICE_DONE_FILE=$L/d9 $J src/run_solves_sept2026.jl > $L/cons_A_wolfram.log 2>&1
$C NICE_METHODS=B NICE_PROPOSALS=Wolfram NICE_DONE_FILE=$L/d10 $J src/run_solves_sept2026.jl > $L/cons_B_wolfram.log 2>&1
EOF
xargs -P 4 -I{} bash -c "export NICE_WORKERS=1; env {}" < $L/jobs.txt

# 5. tables and figures
NICE_RECYCLING=negishi  NICE_TARGET=cons $J src/equivalent_rights_proposals.jl tables     > $L/tables_cons.log 2>&1
NICE_RECYCLING=equal_pc NICE_TARGET=ede  $J src/equivalent_rights_proposals.jl tables     > $L/tables_welf.log 2>&1
NICE_RECYCLING=negishi  NICE_TARGET=cons $J src/equivalent_rights_proposals.jl benchmarks > $L/tables_bench.log 2>&1
NICE_RECYCLING=negishi  Rscript cap_and_share/indifference_curves.R > $L/figs_cons.log 2>&1
NICE_RECYCLING=equal_pc Rscript cap_and_share/indifference_curves.R > $L/figs_welf.log 2>&1
echo PSTAR2025_ALL_DONE
