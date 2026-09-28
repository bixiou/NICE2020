#!/usr/bin/env bash
# Reproduces every result of "International Transfers or Differentiated Carbon
# Prices?" (Fabre & Gorge), from the raw inputs to paper.pdf.
#
#   ./run_all.sh                 everything (about 1-2 days on 4 cores, see README)
#   SKIP_PSTAR=1 ./run_all.sh    keep the shipped benchmark price path p*
#   ONLY=paper ./run_all.sh      one step: setup|pstar|solves|tables|numbers|figures|paper
#   NJOBS=2 ./run_all.sh         number of Julia processes run in parallel (default 4)
#
# Each Julia process needs about 2-3 GB of RAM. Logs go to logs/.
# This is the sequence of logs/rerun_pstar2025.sh in the development repository
# (the run behind the submitted paper), without its warm starts from earlier runs.
set -euo pipefail
cd "$(dirname "$0")"

J="julia --heap-size-hint=2G --project=."
NJOBS=${NJOBS:-4}
ONLY=${ONLY:-all}
L=logs
O=cap_and_share/output
mkdir -p $L $O/equal_pc cap_and_share/paper/figures

# the two variants of the paper (Section 4.1)
CONS="NICE_RECYCLING=negishi NICE_TARGET=cons"   # consumption variant: main text
WELF="NICE_RECYCLING=equal_pc NICE_TARGET=ede"   # welfare variant: Online Appendix B

want() { [[ "$ONLY" == all || "$ONLY" == "$1" ]]; }
say()  { echo "[$(date '+%F %T')] $*"; }

# ── 0. Julia packages, pinned by Manifest-v1.12.toml ────────────────────────
if want setup; then
  say "instantiating the Julia environment"
  julia --project=. -e 'using Pkg; Pkg.instantiate(); Pkg.precompile()'
fi

# ── 1. benchmark price p* (Section 4.2) ─────────────────────────────────────
# Exponential path, ramped up over 2025-2035, spending exactly 1000 GtCO2 over
# 2025-2100 and maximising discounted world welfare over 2025-2300.
# Writes data/uniform_exp_tax_path_params.csv and
# cap_and_share/data/output/calibrated_global_exp.csv.
if want pstar && [[ "${SKIP_PSTAR:-0}" != 1 ]]; then
  say "searching p* (log: $L/pstar.log)"
  NICE_USE_BUDGET=1 NICE_BUDGET_EXACT=1 NICE_BUDGET_LIMIT=1000 NICE_BUDGET_START=2025 \
  NICE_WELFARE_START=2025 NICE_WELFARE_END=2300 NICE_N_ZOOM=3 NICE_B_MIN=-0.02 NICE_B_MAX=0.06 \
    $J cap_and_share/find_global_exp_carbon_tax_buget_zoom.jl > $L/pstar.log 2>&1
  $J src/_write_exp_path.jl >> $L/pstar.log 2>&1
  grep -a "Best path" $L/pstar.log || true
fi

# ── 2. model solves ─────────────────────────────────────────────────────────
if want solves; then
  # Build each variant's proposal scenarios once, so that the parallel jobs
  # below read them from the cache instead of writing it concurrently. With no
  # solve on disk yet, the table step of this call may complain: only the cache
  # matters here.
  say "building the proposal scenarios (logs: $L/prep_*.log)"
  env $CONS $J src/equivalent_rights_proposals.jl tables > $L/prep_cons.log 2>&1 &
  env $WELF $J src/equivalent_rights_proposals.jl tables > $L/prep_welf.log 2>&1 &
  wait || true

  # Every cell: A = isolated solve, B = joint solve (Section 5.2, Online Appendix A);
  # Duflo = Banerjee, Duflo & Greenstone; EqualRight5 = Equal Right at 5%/yr;
  # EqualRight = Equal Right on its own path (Online Appendix C);
  # indifference_curves.jl = the grid of Section 4 (Figure 1, Table 1, Figure A1).
  # Longest first.
  R=src/run_solves_sept2026.jl
  cat > $L/jobs.txt <<EOF
$CONS NICE_METHODS=B NICE_PROPOSALS=EqualRight5 NICE_DONE_FILE=$L/done_1 $J $R > $L/cons_B_er5.log 2>&1
$WELF NICE_METHODS=B NICE_PROPOSALS=EqualRight5 NICE_DONE_FILE=$L/done_2 $J $R > $L/welf_B_er5.log 2>&1
$WELF NICE_METHODS=B NICE_PROPOSALS=Wolfram NICE_DONE_FILE=$L/done_3 $J $R > $L/welf_B_wolfram.log 2>&1
$WELF NICE_METHODS=B NICE_PROPOSALS=Duflo NICE_DONE_FILE=$L/done_4 $J $R > $L/welf_B_duflo.log 2>&1
$CONS NICE_METHODS=A NICE_A_FULL=1 NICE_PROPOSALS=EqualRight5 NICE_DONE_FILE=$L/done_5 $J $R > $L/cons_A_er5.log 2>&1
$WELF $J src/indifference_curves.jl > $L/grid_welf.log 2>&1
$CONS NICE_METHODS=A NICE_A_FULL=1 NICE_PROPOSALS=Duflo NICE_DONE_FILE=$L/done_6 $J $R > $L/cons_A_duflo.log 2>&1
$CONS NICE_METHODS=B NICE_PROPOSALS=EqualRight NICE_DONE_FILE=$L/done_7 $J $R > $L/cons_B_er.log 2>&1
$CONS $J src/indifference_curves.jl > $L/grid_cons.log 2>&1
$CONS NICE_METHODS=B NICE_PROPOSALS=Duflo NICE_DONE_FILE=$L/done_8 $J $R > $L/cons_B_duflo.log 2>&1
$CONS NICE_METHODS=A NICE_A_FULL=1 NICE_PROPOSALS=Wolfram NICE_DONE_FILE=$L/done_9 $J $R > $L/cons_A_wolfram.log 2>&1
$CONS NICE_METHODS=B NICE_PROPOSALS=Wolfram NICE_DONE_FILE=$L/done_10 $J $R > $L/cons_B_wolfram.log 2>&1
EOF
  say "running $(wc -l < $L/jobs.txt) solves, $NJOBS at a time (logs: $L/*.log)"
  xargs -P "$NJOBS" -I{} bash -c "export NICE_WORKERS=1; env {}" < $L/jobs.txt
fi

# ── 3. tables ───────────────────────────────────────────────────────────────
# Table 1: rho1_table.tex (written by indifference_curves.jl in step 2).
# Table 2: equivalent_rights_main.tex; Table A1: equivalent_rights_combined.tex;
# Table A2: equivalent_rights_benchmarks.tex; Table A3: equivalent_rights_equalright_joint.tex.
if want tables; then
  say "writing the tables"
  env $CONS $J src/equivalent_rights_proposals.jl tables     > $L/tables_cons.log 2>&1
  env $WELF $J src/equivalent_rights_proposals.jl tables     > $L/tables_welf.log 2>&1
  env $CONS $J src/equivalent_rights_proposals.jl benchmarks > $L/tables_bench.log 2>&1
fi

# ── 4. numbers quoted in the text only (logs/text_numbers/) ─────────────────
if want numbers; then
  say "computing the in-text numbers (logs: $L/text_numbers/)"
  mkdir -p $L/text_numbers
  env $CONS $J src/_diag_pstar.jl    > $L/text_numbers/pstar_path.log 2>&1   # Section 4.2: p*, emissions, warming
  env $CONS $J src/_diag_growth.jl   > $L/text_numbers/growth.log 2>&1       # footnote of Section 4.2: g, PRTP
  env $CONS $J src/_diag_losers.jl   > $L/text_numbers/losers.log 2>&1       # Section 5.3 and Online Appendix A: Mongolia & co.
  env $CONS $J src/paper_numbers.jl  > $L/text_numbers/section5.log 2>&1     # Section 5.3: implicit transfers, pbar/p*
fi

# ── 5. figures (Figure 1 and Figure A1) ─────────────────────────────────────
if want figures; then
  say "drawing the figures"
  NICE_RECYCLING=negishi  Rscript cap_and_share/indifference_curves.R > $L/figs_cons.log 2>&1
  NICE_RECYCLING=equal_pc Rscript cap_and_share/indifference_curves.R > $L/figs_welf.log 2>&1
fi

# ── 6. the paper ────────────────────────────────────────────────────────────
# Compiled in paper/build/ so that the source folder holds only the PDF.
if want paper; then
  say "compiling the paper"
  ( cd cap_and_share/paper && latexmk -pdf -interaction=nonstopmode -outdir=build paper.tex > build.log 2>&1 \
      && cp build/paper.pdf paper.pdf && mv build.log build/ )
  say "done: cap_and_share/paper/paper.pdf"
fi
