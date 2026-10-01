# Replication package: *International Transfers or Differentiated Carbon Prices?*

Adrien Fabre (CNRS, CIRED) and Constance Gorge (CIRED).

This package reproduces every table, figure and number of the paper, from the model inputs to
`paper.pdf`. It contains only what the paper needs: the NICE2020 integrated assessment model
(Young-Brun et al., 2025) coupled to FaIR v2, its input data, the scripts that run the
paper's experiments, and the LaTeX source of the paper.

The published results are shipped in `reference_output/`, so the paper can be compiled, and a
new run checked, without running the model.

## Quick start

```bash
# 1. compile the paper from the published results (a minute; needs LaTeX only)
./use_reference_outputs.sh
ONLY=paper ./run_all.sh            # -> cap_and_share/paper/paper.pdf

# 2. rerun everything from scratch (Julia, R and LaTeX; see "Computing time")
./run_all.sh
./compare_to_reference.sh          # compares the new results with the published ones
```

`run_all.sh` runs seven steps, which can also be run one at a time with
`ONLY=<step> ./run_all.sh`:

| Step | What it does | Main outputs |
|---|---|---|
| `setup` | installs the Julia packages at the versions pinned in `Manifest-v1.12.toml` | |
| `pstar` | searches the benchmark price path $p^*$ (Section 4.2); skip with `SKIP_PSTAR=1` to keep the shipped one | `data/uniform_exp_tax_path_params.csv`, `cap_and_share/data/output/calibrated_global_exp.csv` |
| `solves` | the 12 model experiments, run `NJOBS` (default 4) at a time | `cap_and_share/output/` (consumption variant), `cap_and_share/output/equal_pc/` (welfare variant) |
| `tables` | writes the LaTeX tables from the solves | `cap_and_share/output/*.tex` |
| `numbers` | recomputes every number quoted in the text and checks it against the paper | `cap_and_share/output/text_numbers.csv`, `logs/ndc_baseline.log` |
| `figures` | draws the heatmaps | `cap_and_share/paper/figures/*.pdf` |
| `paper` | compiles the paper in `cap_and_share/paper/build/` and copies the PDF next to its source | `cap_and_share/paper/paper.pdf` |

## Requirements

Tested on Ubuntu 24.04 (4 cores, 15 GB of RAM) with the versions in brackets.

- **Julia 1.12** [1.12.3]. `Manifest-v1.12.toml` pins every package, including Mimi 1.5.3 and
  MimiFAIRv2 (installed from `https://github.com/FrankErrickson/MimiFAIRv2.jl`, pinned by its
  git tree hash). Julia uses that file only under version 1.12; any other version resolves the
  packages afresh, to possibly different versions. All packages come from Julia's General
  registry.
- **R** [4.3.3] with **ggplot2** [3.4.4], for the figures.
- **LaTeX** with `latexmk`, `pdflatex` and `bibtex` [TeX Live 2023: on Ubuntu, the packages
  `latexmk texlive-latex-extra texlive-fonts-recommended texlive-fonts-extra texlive-bibtex-extra`].
- **bash** and `xargs` (Linux, macOS, or WSL on Windows); `python3` for `compare_to_reference.sh`.
- Internet access during `setup`, to download the Julia packages.

## Computing time

Measured on the test machine (4 cores, 15 GB of RAM), with the main repository's run going
on at the same time on the same machine, so with 2 cores per run (`NJOBS=2`):

| Step | Wall time |
|---|---|
| `setup` | 4 min (download and precompilation of the Julia packages) |
| `pstar` | 1 h (33 growth rates, about 2 min each) |
| `solves` | about 10.5 h (the Equal Right joint solves are the longest cells, 3–4 h each from a cold start) |
| `tables`, `numbers`, `figures`, `paper` | about 20 min together |

With `NJOBS=4` on 4 free cores, the `solves` step should take roughly half as long; we did not
time it. An interrupted run resumes where it stopped: `SKIP_PSTAR=1 ./run_all.sh` skips the
finished solves (the test run was interrupted three times by restarts of the machine, and
resumed this way).

Each Julia process needs about 2 GB of RAM (`--heap-size-hint=2G`; 4 processes used 8 GB in
the test run). `run_all.sh` runs one Julia process per job (`NICE_WORKERS=1`): left to
themselves, the scripts start up to four worker processes each, which exhausted 15 GB.

## Verification

The whole pipeline was run twice from scratch on 30 Sept–1 Oct 2026, in this package (in a
fresh copy, without `reference_output/`) and in the development repository, with its own
environment (which still lists the unused packages, installed from the Mimi registry).

- **The two runs agree exactly.** Both p* searches return the same path (\$191.82/t in 2035,
  1.264% a year); all 215 files the run writes to `cap_and_share/output/` (tables, CSVs,
  `text_numbers.csv`) are byte-identical; the 34 figures are identical pixel for pixel; the two
  compiled papers have the same text (32 pages), the comment-free `paper.tex` of this package
  included.
- **Every number of the text is computed.** `text_numbers.csv` lists 127 of them.
- **`reference_output/` holds the results of this run**, i.e. of the default configuration
  (NICE2020's own baseline emissions, see below). The results with the NDC baselines, which
  the text of the paper still reports, are in the development repository under
  `cap_and_share/output/_backup_ndc_baselines_20260929/`. Against the text of the paper, 81 of
  the 127 numbers differ: the text has not been updated to the new results.

## Where each result comes from

All paths are relative to the root of this package. "cons" is the consumption variant of the
paper (`NICE_RECYCLING=negishi NICE_TARGET=cons`), "welf" the welfare variant
(`NICE_RECYCLING=equal_pc NICE_TARGET=ede`), which writes to `cap_and_share/output/equal_pc/`.

| Exhibit | File | Produced by |
|---|---|---|
| Figure 1 | `cap_and_share/paper/figures/heatmap_{linear_USA,USA,RUS,CHN,EU27,IND,NGA,COD}.pdf` | `src/indifference_curves.jl` (cons), then `cap_and_share/indifference_curves.R` |
| Table 1 | `cap_and_share/output/rho1_table.tex` | `src/indifference_curves.jl` (cons) |
| Table 2 | `cap_and_share/output/equivalent_rights_main.tex` | `src/run_solves_sept2026.jl` (cons, A and B; the B solves also write the implicit transfers of column τ), then `src/equivalent_rights_proposals.jl tables` |
| Table A1 | `cap_and_share/output/equivalent_rights_combined.tex` | same as Table 2 |
| Table A2 | `cap_and_share/output/equivalent_rights_benchmarks.tex` | `src/run_solves_sept2026.jl` (cons and welf, B), then `src/equivalent_rights_proposals.jl benchmarks` |
| Figure A1 | `cap_and_share/paper/figures/heatmap_eqpc_*.pdf` | `src/indifference_curves.jl` (welf), then `cap_and_share/indifference_curves.R` |
| Table A3 | `cap_and_share/output/equivalent_rights_equalright_joint.tex` | `src/run_solves_sept2026.jl` (cons, B, `EqualRight` and `EqualRight5`), then `tables` |
| Numbers in the text | `cap_and_share/output/text_numbers.csv` | `src/text_numbers.jl` |

`text_numbers.csv` has one row per number of the text that comes from the model (about 130):
its identifier, the section(s) where it appears, what it is, the computed value, the value
rounded as the paper prints it, the paper's own figure, and whether they agree. Qualitative
statements ("about half", "most members", "a tenth") are tested as such. Model inputs (tier
prices, the 3% discount rate, the 5% growth of the schedules), survey figures and figures from
other studies are not listed.

## Contents

```
run_all.sh                    the whole pipeline
use_reference_outputs.sh      stages the published results so the paper compiles without a run
compare_to_reference.sh       compares a run with the published results
Project.toml, Manifest-v1.12.toml   Julia environment
src/
  nice2020_module.jl, components/, helper_functions.jl   the NICE2020 model (Mimi)
  equivalent_rights_proposals.jl  proposals, equivalence solves (Section 5), tables
  indifference_curves.jl          indifference grid (Section 4): Figure 1, Table 1
  run_solves_sept2026.jl          driver of the Section 5 solves
  _write_exp_path.jl              writes p* from its two parameters
  text_numbers.jl                 every number quoted in the text
  _diag_ndc_baseline.jl           effect of the EU27 and China baselines (see below)
data/                         NICE2020 calibration (economy, emissions, inequality, damages, FaIR)
cap_and_share/
  find_global_exp_carbon_tax_buget_zoom.jl   p* search (Section 4.2)
  eu_and_china_emissions.jl   EU27 and China emission trajectories (see below)
  indifference_curves.R       heatmaps
  Modeling_co2_emissions/     raw inputs of eu_and_china_emissions.jl
  data/                       Equal Right price schedule, NDC trajectories, p*
  paper/                      paper.tex, price_rights.bib, plainnaturl_clean.bst
  output/                     (created by the run)
reference_output/             the published results: tables, CSVs, figures, paper.pdf
```

The code runs with the package root as working directory (`run_all.sh` sees to it): the model
reads its inputs through paths relative to it.

## Inputs

- `data/nice_inputs.json`, `data/*.csv`, `data/fair_initialize_2020/`: the calibration of
  NICE2020 (Young-Brun et al., 2025): population, GDP, capital, emission intensities,
  consumption deciles, country damage coefficients (Kalkuhl and Wenz), CMIP6 temperature
  patterns, and the state of FaIR in 2020. `data/footprint_over_territorial_2022.csv` converts
  territorial into consumption-based emissions (fixed 2022 ratios, Global Carbon Project).
- `cap_and_share/data/input/ndc_trajectories.csv`: emission trajectories of the EU27 member
  states and China, built by `cap_and_share/eu_and_china_emissions.jl` from territorial CO₂ per
  capita (Global Carbon Project via Our World in Data), World Bank population, the EU's 2030 NDC
  and Effort Sharing Regulation (Regulation (EU) 2023/857), its 2040 target and 2050
  neutrality, and, for China, the CO₂-neutrality scenario of Du et al. (2026), zero from 2071.
  The script is included by `data/parameters.jl` at every model load and rewrites these files
  when they are older than itself; it regenerates them byte for byte.
- `cap_and_share/data/equal_right_prices.csv` and `equal_right_path.csv`: the 2025 charges of
  the Equal Right proposal by country and its escalation path (Equal Right, 2023).
- `cap_and_share/data/output/calibrated_global_exp.csv` and
  `data/uniform_exp_tax_path_params.csv`: $p^*$, as found by the `pstar` step
  (\$191.82/t in 2035, growing at 1.264% a year).

### The baselines of the EU27 and China

`data/parameters.jl` can replace, for every year from 2020 to 2300, the emission intensity σ of
the 27 EU member states and China by σ = E_NDC / GDP_calibrated, where E_NDC is the trajectory
of `ndc_trajectories.csv`. **This is off by default** (NICE2020's own intensities are used);
`NICE_NDC_BASELINES=1` turns it on. In NICE, emissions are E = YGROSS · σ · (1 − μ)
(`src/components/emissions.jl`) and the abatement cost coefficient is proportional to σ
(`src/components/abatement.jl`): with the replacement, the NDC trajectories become the 28
countries' emissions *without any carbon price*, from which every price then abates. The
scenario cache keys include σ, so results computed under one setting are never reused under
the other. `src/_diag_ndc_baseline.jl` (`logs/ndc_baseline.log`) runs the model both ways:

| | NICE σ (default) | NDC σ |
|---|---|---|
| China, emissions 2025–2100 without a carbon price (GtCO₂) | 598 | 229 (zero from 2071) |
| EU27, same (GtCO₂) | 176 | 30 (zero from 2050) |
| World, same (GtCO₂) | 2,844 | 2,330 |
| China's share of world emissions at p*, price-weighted and discounted (the quantity behind $\hat\rho$) | 24.1% | 15.5% |
| EU27's share, same | 7.0% | 1.9% |

(at the default p*, \$191.82/t in 2035.) With the NICE baselines, China's $\rho_1$ is 1.62
instead of 0.95, the EU27's 1.54 instead of 0.37, India's 0.72 instead of 0.91; the coalition
emissions cut of the Banerjee et al. schedule is 3.9% instead of 4.8%
(`cap_and_share/output/text_numbers.csv`).

## Notes on reproducibility

- **Numerical tolerance.** The equivalence solves stop when every member is within 0.002% of its
  target. A fresh run starts the joint solves from the closed-form prediction (or from the
  isolated solve, if that has finished first). Two runs from scratch on the same machine gave
  identical results; across machines, expect at most differences in the last printed digit of
  some $\rho$. `compare_to_reference.sh` lists them.
- **Parallel jobs.** The solves of the two variants write into separate folders; within a
  variant, the jobs share the scenario cache that the first part of the `solves` step builds.
- **Differences with the development repository** (`github.com/bixiou/NICE2020`, same
  commit). Files are copied unchanged, except: `paper.tex`, stripped of its
  comments (the text of the PDF is identical); `Project.toml` and `Manifest-v1.12.toml`, without
  six packages the code never loads (Graphs, JLD2, LaTeXStrings, Measures, MimiDICE2010,
  PrettyTables; MimiDICE2010 is not in the General registry, which made the environment
  impossible to instantiate on a fresh machine), every other version unchanged. New:
  `run_all.sh` (the sequence of `logs/rerun_pstar2025.sh`, which produced the submitted
  results), `use_reference_outputs.sh`, `compare_to_reference.sh` and this README.

## Citation

Fabre, A. and Gorge, C. (2026). International Transfers or Differentiated Carbon Prices?
Working paper.

Please also cite NICE2020: Young-Brun, M. et al. (2025), and FaIR v2: Leach, N. J. et al.
(2021), *Geoscientific Model Development* 14, 3007–3036.
