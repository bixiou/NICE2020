#!/bin/bash
# 25 Sept 2026: every result of the paper recomputed with the NPV over 2025-2100
# (it was 2030-2100), under the two benchmarks of Sections 4-5:
#   cons  NPV of total consumption, revenue recycled in proportion to c^eta (Negishi)
#         -> cap_and_share/output/            (Figure 1, Tables 1-2, Online Appendix A-C)
#   welf  NPV of EDE consumption (welfare), equal per capita dividend within countries
#         -> cap_and_share/output/equal_pc/   (Online Appendix D: figure and table)
# The results of the 2030-2100 runs are in cap_and_share/output/_backup_npv2030_20260925/.
# Run from the repository root, on a 4-core machine: four processes at a time.
J="julia --heap-size-hint=2G --project=."
W=cap_and_share/output/_backup_npv2030_20260925/indifference   # rest-of-world prices already solved
mkdir -p logs/npv2025

# joint solves (option B), several cells per benchmark in parallel: each session
# merges its rows into the shared summary CSVs (merge_with_disk)
NICE_RECYCLING=negishi  NICE_TARGET=cons NICE_METHODS=B NICE_PROPOSALS=Duflo,Wolfram \
  NICE_DONE_FILE=logs/npv2025/done_b1.txt $J src/run_solves_sept2026.jl > logs/npv2025/cons_b_1.log 2>&1 &
sleep 300   # let the first session cache the proposals before the second reads them
NICE_RECYCLING=negishi  NICE_TARGET=cons NICE_METHODS=B NICE_PROPOSALS=EqualRight5,EqualRight \
  NICE_DONE_FILE=logs/npv2025/done_b2.txt $J src/run_solves_sept2026.jl > logs/npv2025/cons_b_2.log 2>&1 &
NICE_RECYCLING=equal_pc NICE_TARGET=ede  NICE_METHODS=B NICE_PROPOSALS=Duflo,Wolfram,EqualRight5 \
  NICE_DONE_FILE=logs/npv2025/done_b3.txt $J src/run_solves_sept2026.jl > logs/npv2025/welf_b.log 2>&1 &
# Figure 1 and its analogue (the grid's runs do not depend on the NPV window, so
# the saved rest-of-world prices are exact warm starts)
( NICE_RECYCLING=negishi  NICE_TARGET=cons NICE_EX1_WARM=$W $J src/indifference_curves.jl > logs/npv2025/grid_cons.log 2>&1
  NICE_RECYCLING=equal_pc NICE_TARGET=ede  NICE_EX1_WARM=$W $J src/indifference_curves.jl > logs/npv2025/grid_welf.log 2>&1 ) &
wait

# isolated solves (option A, Online Appendix B), on the consumption benchmark
NICE_RECYCLING=negishi NICE_TARGET=cons NICE_METHODS=A NICE_A_FULL=1 NICE_WORKERS=3 \
  NICE_PROPOSALS=Wolfram,Duflo,EqualRight5 NICE_DONE_FILE=logs/npv2025/done_a.txt \
  $J src/run_solves_sept2026.jl > logs/npv2025/cons_a.log 2>&1

# tables and figures
NICE_RECYCLING=negishi  NICE_TARGET=cons $J src/equivalent_rights_proposals.jl tables     > logs/npv2025/tables_cons.log 2>&1
NICE_RECYCLING=equal_pc NICE_TARGET=ede  $J src/equivalent_rights_proposals.jl tables     > logs/npv2025/tables_welf.log 2>&1
NICE_RECYCLING=negishi  NICE_TARGET=cons $J src/equivalent_rights_proposals.jl benchmarks > logs/npv2025/tables_bench.log 2>&1
NICE_RECYCLING=negishi  Rscript cap_and_share/indifference_curves.R > logs/npv2025/figs_cons.log 2>&1
NICE_RECYCLING=equal_pc Rscript cap_and_share/indifference_curves.R > logs/npv2025/figs_welf.log 2>&1
echo NPV2025_ALL_DONE
