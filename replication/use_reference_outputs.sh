#!/usr/bin/env bash
# Puts the published results (reference_output/) where the paper expects them,
# so that `ONLY=paper ./run_all.sh` compiles the paper without running the model.
# Overwrites cap_and_share/output/ and cap_and_share/paper/figures/.
set -euo pipefail
cd "$(dirname "$0")"
mkdir -p cap_and_share/output cap_and_share/paper/figures
cp -r reference_output/output/. cap_and_share/output/
cp reference_output/figures/*.pdf cap_and_share/paper/figures/
echo "reference outputs copied; now run: ONLY=paper ./run_all.sh"
