#!/usr/bin/env bash
# Compares the results of a run with the shipped ones. The reference is the
# committed version of each file (git HEAD), or, in a copy that is not a git
# repository, an untouched copy of the package: REF=<its root> ./compare_to_reference.sh
# Numerical solves stop at a tolerance (0.002% of each member's consumption),
# and joint solves start from a different point in a fresh run, so the last
# printed digit of a rho can differ.
cd "$(dirname "$0")"
REF=${REF:-}
if [[ -z $REF ]] && ! git rev-parse --git-dir > /dev/null 2>&1; then
  echo "not a git repository: set REF to the root of an untouched copy of the package"; exit 2
fi
ref() { if [[ -n $REF ]]; then cat "$REF/$1"; else git show "HEAD:./$1"; fi; }   # reference version of a file
tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
status=0
for f in rho1_table.tex equivalent_rights_main.tex equivalent_rights_combined.tex \
         equivalent_rights_benchmarks.tex equivalent_rights_equalright_joint.tex \
         implicit_transfers_duflo.csv; do
  new=cap_and_share/output/$f
  ref "$new" > "$tmp/ref" 2> /dev/null || { echo "NO REF   $f"; status=1; continue; }
  if [[ ! -f $new ]]; then echo "MISSING  $new"; status=1
  elif diff -q -I '^%' "$tmp/ref" "$new" > /dev/null; then echo "same     $f"
  else echo "DIFFERS  $f"; diff -I '^%' "$tmp/ref" "$new" | head -20; status=1; fi
done
# figures: all present (they are redrawn by the run, so their bytes can differ)
figs=$(if [[ -n $REF ]]; then (cd "$REF" && find cap_and_share/paper/figures -name '*.pdf'); \
       else git ls-files cap_and_share/paper/figures; fi)
for f in $figs; do [[ -f $f ]] || { echo "MISSING  figure $f"; status=1; }; done
# numbers of the text: printed values of this run against the reference run,
# and the numbers that disagree with the paper (in either run)
new=cap_and_share/output/text_numbers.csv
if [[ -f $new ]] && ref "$new" > "$tmp/numbers.csv" 2> /dev/null; then
  python3 - "$tmp/numbers.csv" "$new" <<'PY' || status=1
import csv, sys
ref = {r["id"]: r for r in csv.DictReader(open(sys.argv[1]))}
new = list(csv.DictReader(open(sys.argv[2])))
bad = 0
for r in new:
    o = ref.get(r["id"])
    if o is None or o["printed"] != r["printed"]:
        bad = 1
        print(f"NUMBER   {r['id']}: {o['printed'] if o else '(new)'} reference, {r['printed']} now")
off = [r for r in new if r["match"] != "true"]
print(f"text numbers: {len(new)} computed, {sum(1 for r in new if ref.get(r['id'], {}).get('printed') == r['printed'])} as in the reference run, {len(off)} differ from the paper:")
for r in off:
    print(f"         {r['id']:24s} {r['printed']:>10s}  paper: {r['paper']}  ({r['quantity']})")
sys.exit(bad)
PY
else
  echo "MISSING  $new (or its reference)"; status=1
fi
# every other file of the run that differs from the reference
if [[ -z $REF ]]; then
  echo "files that differ from the committed ones (git diff --stat):"
  git diff --stat -- cap_and_share/output | tail -1
fi
exit $status
