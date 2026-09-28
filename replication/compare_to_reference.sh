#!/usr/bin/env bash
# Compares the tables and in-text CSVs of a run with the published ones in
# reference_output/. Numerical solves stop at a tolerance (0.002% of each
# member's consumption), and joint solves start from a different point in a
# fresh run, so the last printed digit of a rho can differ.
cd "$(dirname "$0")"
status=0
for f in rho1_table.tex equivalent_rights_main.tex equivalent_rights_combined.tex \
         equivalent_rights_benchmarks.tex equivalent_rights_equalright_joint.tex \
         equal_pc/rho1_table.tex implicit_transfers_duflo.csv; do
  new=cap_and_share/output/$f; ref=reference_output/output/$f
  if [[ ! -f $new ]]; then echo "MISSING  $new"; status=1
  elif diff -q -I '^%' "$ref" "$new" > /dev/null; then echo "same     $f"
  else echo "DIFFERS  $f"; diff -I '^%' "$ref" "$new" | head -20; status=1; fi
done
for f in reference_output/figures/*.pdf; do
  [[ -f cap_and_share/paper/figures/$(basename "$f") ]] || { echo "MISSING  figure $(basename "$f")"; status=1; }
done
exit $status
