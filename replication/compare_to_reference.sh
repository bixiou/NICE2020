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
# numbers of the text: printed values of this run against the published run,
# and the numbers that disagree with the paper (in either run)
new=cap_and_share/output/text_numbers.csv; ref=reference_output/output/text_numbers.csv
if [[ -f $new ]]; then
  python3 - "$ref" "$new" <<'PY' || status=1
import csv, sys
ref = {r["id"]: r for r in csv.DictReader(open(sys.argv[1]))}
new = list(csv.DictReader(open(sys.argv[2])))
bad = 0
for r in new:
    o = ref.get(r["id"])
    if o is None or o["printed"] != r["printed"]:
        bad = 1
        print(f"NUMBER   {r['id']}: {o['printed'] if o else '(new)'} published, {r['printed']} now")
off = [r for r in new if r["match"] != "true"]
print(f"text numbers: {len(new)} computed, {sum(1 for r in new if ref.get(r['id'], {}).get('printed') == r['printed'])} as in the published run, {len(off)} differ from the paper:")
for r in off:
    print(f"         {r['id']:24s} {r['printed']:>10s}  paper: {r['paper']}  ({r['quantity']})")
sys.exit(bad)
PY
else
  echo "MISSING  $new"; status=1
fi
exit $status
