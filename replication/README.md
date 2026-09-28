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
# 1. compile the paper from the published results (a few seconds; needs LaTeX only)
./use_reference_outputs.sh
ONLY=paper ./run_all.sh            # -> cap_and_share/paper/paper.pdf

# 2. rerun everything from scratch (Julia, R and LaTeX; see "Computing time")
./run_all.sh
./compare_to_reference.sh          # compares the new tables with the published ones
```

`run_all.sh` runs seven steps, which can also be run one at a time with
`ONLY=<step> ./run_all.sh`:

| Step | What it does | Main outputs |
|---|---|---|
| `setup` | installs the Julia packages at the versions pinned in `Manifest-v1.12.toml` | |
| `pstar` | searches the benchmark price path $p^*$ (Section 4.2); skip with `SKIP_PSTAR=1` to keep the shipped one | `data/uniform_exp_tax_path_params.csv`, `cap_and_share/data/output/calibrated_global_exp.csv` |
| `solves` | the 12 model experiments, run `NJOBS` (default 4) at a time | `cap_and_share/output/` (consumption variant), `cap_and_share/output/equal_pc/` (welfare variant) |
| `tables` | writes the LaTeX tables from the solves | `cap_and_share/output/*.tex` |
| `numbers` | computes the numbers quoted only in the text | `logs/text_numbers/*.log`, `cap_and_share/output/implicit_transfers_duflo.csv` |
| `figures` | draws the heatmaps | `cap_and_share/paper/figures/*.pdf` |
| `paper` | compiles the paper in `cap_and_share/paper/build/` and copies the PDF next to its source | `cap_and_share/paper/paper.pdf` |

## Requirements

- **Julia 1.12** (the environment was resolved with 1.12.3). `Manifest-v1.12.toml` pins every
  package, including Mimi 1.5.3 and MimiFAIRv2 (installed from
  `https://github.com/FrankErrickson/MimiFAIRv2.jl`, pinned by its git tree hash). Julia uses
  that file only under version 1.12; any other version resolves the packages afresh, to
  possibly different versions.
- **R** (≥ 4.0) with the package **ggplot2**, for the figures.
- **LaTeX** with `latexmk`, `pdflatex` and `bibtex`, and the packages loaded in
  `cap_and_share/paper/paper.tex` (all in TeX Live).
- **bash** and `xargs` (Linux, macOS, or WSL/Git Bash on Windows).
- Internet access during `setup`, to download the Julia packages.

## Computing time

The unit of cost is one run of NICE2020, about 7–9 s. The figures below are those recorded in
`src/equivalent_rights_proposals.jl` for the September 2026 runs on a 4-core machine; the
authors did not time the full sequence end to end.

- `pstar`: 3 zoom levels × 11 growth rates, each with a root search on the initial price: a few
  hundred model runs.
- `solves`: 25–45 min per isolated solve (A) with 4 worker processes, 20–35 min per joint solve
  (B), plus 15–30 min for the variants of each cell. `run_all.sh`, like the authors' last run,
  gives each job a single worker process, so an isolated solve takes several times longer.
  There are 3 isolated cells, 7 joint cells and 2 indifference grids (8 economies × (20 autarky
  prices, each with a year-by-year solve of the rest of the world's price, + 24 allocations)).
- `tables`, `numbers`, `figures`, `paper`: minutes.

From these figures, expect on the order of a day on 4 cores. Each Julia process needs 2–3 GB of
RAM (`--heap-size-hint=2G`).

## Where each result comes from

All paths are relative to the root of this package. "cons" is the consumption variant of the
paper (`NICE_RECYCLING=negishi NICE_TARGET=cons`), "welf" the welfare variant
(`NICE_RECYCLING=equal_pc NICE_TARGET=ede`), which writes to `cap_and_share/output/equal_pc/`.

### Exhibits

| Exhibit | File | Produced by |
|---|---|---|
| Figure 1 | `cap_and_share/paper/figures/heatmap_{linear_USA,USA,RUS,CHN,EU27,IND,NGA,COD}.pdf` | `src/indifference_curves.jl` (cons), then `cap_and_share/indifference_curves.R` |
| Table 1 | `cap_and_share/output/rho1_table.tex` | `src/indifference_curves.jl` (cons) |
| Table 2 | `cap_and_share/output/equivalent_rights_main.tex` | `src/run_solves_sept2026.jl` (cons, A and B), then `src/equivalent_rights_proposals.jl tables` |
| Table A1 | `cap_and_share/output/equivalent_rights_combined.tex` | same as Table 2 |
| Table A2 | `cap_and_share/output/equivalent_rights_benchmarks.tex` | `src/run_solves_sept2026.jl` (cons and welf, B), then `src/equivalent_rights_proposals.jl benchmarks` |
| Figure A1 | `cap_and_share/paper/figures/heatmap_eqpc_*.pdf` | `src/indifference_curves.jl` (welf), then `cap_and_share/indifference_curves.R` |
| Table A3 | `cap_and_share/output/equivalent_rights_equalright_joint.tex` | `src/run_solves_sept2026.jl` (cons, B, `EqualRight` and `EqualRight5`), then `tables` |

### Numbers quoted in the text

Most numbers in the text are read off the tables above. The others:

| Number (section) | Source |
|---|---|
| $p^*$: \$142/t in 2035, 1.7%/yr, \$179/t in 2050, \$402/t in 2100; world emissions 38, 28, 21 and 2 GtCO₂ in 2025, 2030, 2035, 2100; 1.84 °C in 2100 (4.2) | `logs/text_numbers/pstar_path.log` (`src/_diag_pstar.jl`) and `data/uniform_exp_tax_path_params.csv` |
| 0.3% pure rate of time preference, 1.8% growth (footnote, 4.2) | `logs/text_numbers/growth.log` (`src/_diag_growth.jl`) |
| $\rho_1$ and the zero-price bounds (0.28 for Nigeria, 0.05 for the DRC) (4.3) | `cap_and_share/output/indifference/indifference_curves.csv` and `table_rho1.csv`; welfare variant in `equal_pc/indifference/` |
| Coalition prices in 2030 (\$49, \$56, \$140/t) and their growth (5.2) | `cap_and_share/output/price_paths_B_*.csv`, column `p_ref` (the published set in `reference_output/` includes it for Equal Right only) |
| Implicit transfers: \$40bn (2030) and \$190bn (2050) of gross flows; India +\$91bn, USA −\$93bn in 2050 (abstract, 5.3, conclusion) | `logs/text_numbers/section5.log` and `cap_and_share/output/implicit_transfers_duflo.csv` (`src/paper_numbers.jl`) |
| $\bar p/p^* \approx 0.64 = 0.75 \times 0.85$ under Equal Right (5.3) | `logs/text_numbers/section5.log` (`src/paper_numbers.jl`) |
| Mongolia's loss (−0.02%) and the members that lose from avoided damages (5.3, Online Appendix A) | `logs/text_numbers/losers.log` (`src/_diag_losers.jl`) and `cap_and_share/output/country_gains.csv` |
| Isolated vs joint solves, members losing under each rule (Online Appendix A) | Table A1 and `cap_and_share/output/equivalent_rights_variants.csv` |

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
  _diag_pstar.jl, _diag_growth.jl, _diag_losers.jl, paper_numbers.jl   in-text numbers
data/                         NICE2020 calibration (economy, emissions, inequality, damages, FaIR)
cap_and_share/
  find_global_exp_carbon_tax_buget_zoom.jl   p* search (Section 4.2)
  eu_and_china_emissions.jl   EU27 and China emission trajectories (see "Inputs")
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
- `cap_and_share/data/input/ndc_trajectories.csv`: emission trajectories that replace the
  baseline emission intensities of the EU27 member states and China in `data/parameters.jl`.
  They are built by `cap_and_share/eu_and_china_emissions.jl` from territorial CO₂ per capita
  (Global Carbon Project via Our World in Data), World Bank population, the EU's 2030 NDC and
  Effort Sharing Regulation (Regulation (EU) 2023/857), its 2040 target and 2050 neutrality,
  and, for China, the CO₂-neutrality scenario of Du et al. (2026). The script is included by
  `data/parameters.jl` at every model load and rewrites these files only when they are older
  than itself; the shipped files are its output.
- `cap_and_share/data/equal_right_prices.csv` and `equal_right_path.csv`: the 2025 charges of
  the Equal Right proposal by country and its escalation path (Equal Right, 2023).
- `cap_and_share/data/output/calibrated_global_exp.csv` and
  `data/uniform_exp_tax_path_params.csv`: $p^*$, as found by the `pstar` step
  (initial price 142.10 \$/t in 2035, growth 1.68% a year).

## Notes on reproducibility

- **Numerical tolerance.** The equivalence solves stop when every member is within 0.002% of its
  target. A fresh run starts the joint solves from the closed-form prediction (or from the
  isolated solve, if that has finished first), while the published run was warm-started from
  earlier solves. Expect differences in the last printed digit of some $\rho$;
  `compare_to_reference.sh` shows them.
- **Parallel jobs.** The solves of the two variants write into separate folders; within a
  variant, the jobs share the scenario cache that the first part of the `solves` step builds.
- **Differences with the development repository.** All files are copied unchanged from
  `github.com/bixiou/NICE2020` (commit `e733b29`, 28 Sept 2026), except `run_all.sh`,
  `use_reference_outputs.sh`, `compare_to_reference.sh`, `src/paper_numbers.jl` and this README.
  `run_all.sh` follows `logs/rerun_pstar2025.sh`, the sequence behind the submitted paper.
  `src/paper_numbers.jl` rebuilds two results that were first computed interactively: the
  implicit transfers of Section 5.3 and the decomposition of Equal Right's dividend term.
- **Not scripted.** Two figures of the text come from interactive calculations that are not
  part of this package: the extra rights that would compensate Mongolia (0.03 equal per capita
  shares, 0.45% of its allocation; Section 5.3 footnote) and the flat-weight variant of the
  $\bar p/p^*$ decomposition (a comment in `paper.tex`).

## Citation

Fabre, A. and Gorge, C. (2026). International Transfers or Differentiated Carbon Prices?
Working paper.

Please also cite NICE2020: Young-Brun, M. et al. (2025), and FaIR v2: Leach, N. et al. (2021).
