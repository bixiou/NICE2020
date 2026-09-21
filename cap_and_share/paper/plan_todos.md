# Plan for the TODOs of `paper.tex` (prepared 2026-09-19)

Outlet: JEEM (main text <= 6,000 words, <= 5 tables + figures, abstract <= 250 words).
Every TODO stays in the .tex; a `% DONE (...)` comment right after it explains what was done.

## 0. Inventory of the TODOs (line numbers of the 19 Sept version)

| # | Line | Topic | Type |
|---|------|-------|------|
| T1 | 89 | Is the paper novel? | research |
| T2 | 104 | Improve highlights | text |
| T3 | 136 | Improve sentence on first-order equivalence | text |
| T4 | 153 | Add "robust and original equation" paragraph + roadmap | text |
| T5 | 160 | Main-table formatting (swap rows/cols, BDG name, `welf`, note outside `\input`) | code + text |
| T6 | 164 | Replace "Related literature" by a section on rationales for differentiation | text |
| T7 | 179 | Put back Bauer et al. sovereignty sentence? | text |
| T8 | 189 | Keep only paragraphs 3-4 of related literature | text |
| T9 | 191 | Cite Chateau et al., He et al., Le Moigne et al. | research + text |
| T10 | 228 | Is B_i really a Harberger triangle? | theory |
| T11 | 231 | Remove indices i? | notation |
| T12 | 238 | r^eq -> r^* | notation |
| T13 | 285 | bar = world average, * = uniform price (else ^U) | notation |
| T14 | 287 | \hat\rho not defined | notation |
| T15 | 292 | Hotelling formula for rho in Prop. 3 | theory |
| T16 | 310 | Dissolve the model section? | structure |
| T17 | 312 | Are MAC/damage functions country-specific? | research (code) |
| T18 | 316 | Keep recycling sentence if EDE results cut? | text |
| T19 | 323 | Does p* grow at a constant rate? | research (data) |
| T20 | 329 | Population share at time t? | research (code) |
| T21 | 333 | Heatmap uses welfare or consumption? | research (code) |
| T22 | 344 | Do proposals specify a price path? | research (papers) |
| T23 | 347 | Describe Equal Right's proposal | research + text |
| T24 | 363 | How is the joint variant-1 cap set? | research (code) |
| T25 | 366 | Re-define the solves: criterion x way x surplus type | code + runs |
| T26 | 367 | Update the subsection accordingly | text |
| T27 | 375 | Merge sections: "Equivalent rights by country" / "... of prominent proposals" | structure |
| T28 | 381 | Back up heatmaps, add pi = 0 and 0.25 | code + runs |
| T29 | 382 | Indifference curves without log y-axis: linear? | code + analysis |
| T30 | 385 | Paragraph: fair allocation unattainable with non-negative prices (Le Moigne) | text |
| T31 | 420 | Table 1: drop shares, add emissions p.c. / world average | code + text |
| T32 | 453 | Main vs appendix tables | structure |
| T33 | 455 | Keep the prediction part small | text |
| T34 | 464 | Put back the isolated-vs-joint paragraph? | text (after results) |
| T35 | 469 | Rewrite the dividend paragraphs, focus on Banerjee | text (after results) |
| T36 | 479 | Relegate EDE results to appendix? | decision (after results) |
| T37 | 492 | Correct the figure in the conclusion | text (after results) |

## 1. Research answers (done before coding)

* T17: in NICE2020 the damage function is country-specific (Kalkuhl-Wenz coefficients applied to
  pattern-scaled local temperature). The abatement cost function is not: theta2 = 2.6 and the
  backstop price are common; countries differ only through baseline emissions (sigma x Y).
  The abatement rate at price p, mu = (p/p_back)^(1/(theta2-1)), is the same everywhere, so the
  price elasticity of emissions is common to all countries at a given price.
* T19: p*_t is calibrated year by year to a 1.8C emissions path; growth is 15.8%/yr in 2031,
  6.5% in 2040, 4.8% in 2050, ~3% in 2060-2070, then p* equals the backstop price from 2080
  (slowly declining, -0.1%/yr). Not constant; not Hotelling (beta_t p_t rises until ~2060, then falls).
* T20: rights of i at t = rho x (n_it / N_t) x E*_t: population share of the same year.
* T21: the heatmaps use the NPV of country EDE consumption (welfare).
* T22: Parry et al. (2021) set floors "by 2030"; Wolfram et al. model a static illustrative year
  and suggest "sunset provisions that gradually raise lower-tier prices so countries graduate
  into a single uniform price"; Banerjee et al. ramp tiers up only as countries move to a higher
  income group. None specifies an escalation path.
* T23: Equal Right (2023, 2025): cap on fossil extraction, lowered 10%/yr (350 GtCO2 budget,
  1.6C); national allowances per capita; price floor per licence graduated by GNI group
  ($240/120/60/30) with 0-60% discounts by climate vulnerability ($12-240/t, mean $144), rising
  ~16%/yr up to $4,000; revenues pooled in a Global Commons Fund paying $1.3tn/yr climate grants
  and a universal dividend. We only use its schedule of price floors, with revenue retained
  domestically, as an example of a highly dispersed schedule. This must be stated.
* T24: the joint cap is fixed at the isolated solve's level (`fix_level = true`, L = mean of
  option A's rho); only the spread is solved; the residual mean gap (+0.034%, +0.048%) is
  left as surplus. So the joint variant 1 is currently NOT "every member exactly indifferent".
* T10: B_i is the area between the MAC curve and the horizontal line at p* between e*_i and
  e^A_i: a curvilinear triangle with vertices (e*_i, p*), (e^A_i, p*), (e^A_i, p_i). It is the
  deadweight loss (Harberger triangle) of the price wedge; formally a Bregman divergence.
* T1 (novelty): known results: with lump-sum transfers efficiency and equity separate (Sandmo;
  Chichilnisky-Heal 1994; Shiell 2003; d'Autume-Schubert-Withagen 2016); without transfers
  optimal prices are differentiated; emissions trading is Pareto-improving given suitable
  allocation (Montgomery 1972). Not found: (i) the closed-form map from a given price schedule
  to the equivalent allocation (first order = schedule-induced emissions; second-order Bregman
  term; dynamic price weighting; Hotelling = budget); (ii) the non-invertibility result
  (non-negative prices cannot replicate allocations above the zero-price emissions); (iii)
  quantification for actual proposals in a 179-country IAM. Novelty is real but modest on
  theory; the paper should frame the formulas as simple and useful rather than deep.
* T9: He et al. (2024): the IMF floors are non-binding for HICs relative to their NDCs and bind
  only on China and India. Chateau et al. (2024): the differentiated floor costs 0.3% of world
  GDP more than a uniform price, is progressive, and makes a border adjustment unnecessary.
  Le Moigne et al. (2026): heterogeneous prices do not necessarily yield fairer outcomes; small
  transfers (USD 107-169bn) suffice to equalise the costs of a uniform tax.

## 2. Code and runs (order)

1. **Back up** current heatmaps (`paper/figures/backup_20260919/`, `output/*/backup_20260919/`).
2. **Exercise 1 rewritten** (`src/indifference_curves.jl`, reusing the definitions of
   `equivalent_rights_proposals.jl`). Bug found: the old autarky runs used the neutral refund
   (`switch_recycle = 0`) and a zero income-elasticity slope, while the uniform runs used the
   c^eta recycling and the default slope, so at pi = 1 (where the two regimes coincide exactly)
   the EDE comparison picked up a within-country difference. The new runs use the same
   configuration as Exercise 2 in both regimes. Grid: pi in {0, 0.25, ..., 3}, rho on a dense
   linear grid; welfare (EDE) and consumption both stored. Indifference curve by interpolation
   in rho (linear). Outputs: heatmaps with log and linear y-axes, the curves on one linear
   panel, and Table 1 (`tab:summary`) as a generated .tex with the new columns.
3. **Exercise 2 solves redefined** (`src/equivalent_rights_proposals.jl`):
   * criterion: consumption or welfare (EDE) [unchanged, `NICE_TARGET`];
   * isolated: rho_i^iso as now; variant 1 = at face value; variant 2 = scaled up uniformly to
     the proposal's emissions;
   * joint: variant 1 = all rho solved with the level free, so that every member is exactly at
     its proposal level (cap = sum of rights, price recalibrated); variant 2 = total rights =
     proposal emissions each year, gains equalised across members (the maximin solution),
     solved by a per-country secant rather than the annealed heuristic;
   * appendix: alternative sharing of the variant-2 surplus: marginal utility (v3), uniform
     scaling (v4) of the joint variant-1 allocation;
   * first-order prediction \hat rho computed per capita (as in the paper's formula) instead of
     population-weighted; both kept for comparison.
4. **Tables**: main table = Wolfram / Banerjee-Duflo-Greenstone / Equal Right 5%, joint solve,
   rho^welf and rho^cons (T5 formatting, tabular only, note written in paper.tex). Appendix:
   isolated vs joint for all proposals; members losing; Equal Right own path.
5. **Runs**: two processes in parallel (consumption and welfare targets write to different
   files), each sequential over the four proposals. First a test on Wolfram.

## 3. Notation (T11-T15)

* `*` = uniform-price regime: p* (was \bar p and p^ref), e*_i (was \bar e_i), r*_i the
  equivalent rights (was r^eq), rho*_i = r*_i / \bar e the equivalent ratio.
* `A` = differentiated (autarky) regime: p_i, e^A_i. pi_i = p_i / p*.
* bar = world (club) average per capita: \bar e = E/N; dynamic \bar e_t = E^A_t / N_t.
* E = common global emissions (was E* / E^A in the static section).
* \hat rho_i = first-order approximation of rho*_i (defined explicitly).
* Hotelling: \hat rho_i = sum_t e^A_it / sum_t \bar e_t.
* Indices i kept: the propositions sum over countries and compare i with averages, so dropping
  them would lose information (T11).
* Tables: rho^welf, rho^cons, \hat rho, p (no i subscripts).

## 3b. What the runs changed (20 Sept 2026)

* **Exercise 1 was inconsistent.** The old autarky runs refunded each decile its own carbon
  burden and used a zero income-elasticity slope, while the uniform runs used the c^eta rule.
  At pi = 1 the two regimes coincide by construction, so the comparison was picking up a
  within-country difference. With both regimes on the same configuration, the simulated rho_1
  now matches the closed-form prediction within 1% for six of eight entities (China: 0.92
  against 0.93 predicted; it was 0.67 against 0.95 before), within 3% for India and 6% for the
  EU27. Table 1 and Figure 1 are rebuilt from these runs.
* **The rest-of-world price solver could not move where p* is the backstop.** It inferred the
  rest of the world's baseline emissions as E/(1-mu), which is 0/0 in the years when the
  benchmark price drives emissions to zero, so pi = 0 missed the global path by 1% (Nigeria)
  to 19% (India). The baseline is now read from the model (YGROSS x sigma). Every grid point
  is within 0.01% of the target path.
* **The joint "reduced emissions" solve needed damages held fixed.** With damages endogenous,
  the solve keeps tightening the cap as long as the club's avoided damages exceed its
  abatement costs, which for these schedules (priced below the club's own marginal damage)
  runs to cuts above 20% and measures the schedules' lack of ambition, not the efficiency of
  uniform pricing. Propositions 1 and 2 compare regimes at equal damages, so the solve now
  pins each country's temperature at its path under the schedule. Reported outcomes
  (temperature, world gains) still use endogenous damages.
* **The joint solve is now nested.** An outer secant on the level of the allocation, whose
  inner problem is the fixed-level equal-gains solve. Solving level and spread in one loop
  overshot, because the common gain responds weakly to the level.
* **Final numbers (joint solve).** Emissions cut, consumption criterion: 6.9% (Wolfram et al.),
  6.0% (Banerjee, Duflo and Greenstone), 16.5% (Equal Right at 5%/yr), 19.3% (Equal Right on
  its own path); on the welfare criterion, 6.1%, 5.2% and 14.0%. Taking the surplus as
  consumption instead, every member gains 0.049%, 0.064% and 0.198% of the NPV of its
  consumption. 2100 warming falls by 0.014, 0.024 and 0.040 C.
* **The 2025-2029 window, quantified** (src/_diag_window.jl, Wolfram et al.): on the window
  solved (2030-2100) every member is within 0.0005% of its level under the schedule; read over
  2025-2100 the mean member is 0.037% *above* it, with a spread of -0.16% to +0.16%. The five
  uncounted years therefore do not flatter the aggregate dividend (if anything the reported cut
  is conservative), but they do shift the allocation across members. Stated in Online Appendix B.

## 4b. Second round (20 Sept 2026, after review)

* **Reduced emissions stays the headline**, on the author's decision: the emissions figure is
  more telling than the consumption one. The fixed-damages device is therefore stated plainly
  wherever it carries a number (Section 5 design, the main table's note, Online Appendix B):
  it is an accounting device, not a scenario, and it is the numerical counterpart of
  Proposition 2's "the cap can be tightened by sum_i n_i B_i / p*", which likewise changes
  emissions while treating damages as given. The endogenous-damage variant is *not* computed:
  it converges on the club's own optimal cap, i.e. it changes the ambition level.
* **Figure 1 restored to the layout of the figures it replaces**: same cells (pi in steps of
  0.25 up to 4.75, rho on the ladder 0.02-10), the same axes in every panel, black annotations,
  the key inside the panel, the colour bar title reading bottom to top, and the same colour rule
  (95th percentile of |gain|, now +/-11.2). The only differences are pi = 0 and 0.25, and the
  revised computations.
* **The curve is traced as pi*(rho)** on a dense rho ladder rather than as rho*(pi), so it has a
  point for every y value of the panel, as the old contour did, and does not break where the
  equivalent allocation leaves the plotted range.
* **Grid extended incrementally**: `todo_values` in src/indifference_curves.jl computes only the
  missing pi and rho, so the 7 new pi values and rho = 0.02 cost ~6 min per country instead of a
  full re-run. Table 1 is unchanged by the denser grid.
* **Reproducibility checked**: re-running the DR Congo grid reproduces the stored CSV byte for
  byte; re-running both table entry points reproduces Table 1 and the proposal tables.
* **Note on running the code**: `src/equivalent_rights_proposals.jl` spawns 4 worker processes
  unless `NICE_WORKERS=1` is set. That is what asks for a firewall authorisation (Distributed
  opens a listening socket) and, on this machine, what exhausts memory. Always set it.

## 4bis. Total utilitarianism (20 Sept 2026)

The last TODO of the dynamic proposition asked for a factor n_it in the indifference condition.
Checked in the code: the NPVs did **not** account for population changes -- every criterion
discounted a per-capita series, which is average utilitarianism within a country and over time.

* **Code**: `npv_pop(series, pop)` added in `src/equivalent_rights_proposals.jl`; used by
  `country_cons_npv` (replaces `country_mean_cons`), `world_cons_npv` (replaces the NPV of
  `mean_consumption`), `entity_welfare_npv`, `world_welfare_npv` and `objective_by_country`.
  `src/indifference_curves.jl`: `entity_cons_npv` is now a population-weighted total, and
  `predicted_rho1` returns the totals version of (eq:rhohat_dyn) by default, the per-capita
  variant being kept as the secondary column `rho_hat_pc`. `write_main_table` predicts with
  `predicted_rho_priced` (`pred_kind = :formula`), the population-weighted formula.
* **Paper**: the total-utilitarian objective is stated in the static setting (3.1) and at the
  start of the dynamic one (3.2); n_it appears in (eq:dyn_equiv) and (eq:rhohat_dyn); the
  Hotelling case (eq:rhohat_hotelling) now reads as the country's share of the cumulative world
  carbon budget over its share of world population, so that an equal per capita division of the
  budget is rho = 1. The proof spells out where n_it enters. The criteria are described as
  population-weighted NPVs of EDE consumption and of total consumption throughout Sections 4-5
  and in the table notes.
* **Runs**: all proposal solves (both criteria) and the whole Exercise 1 grid were recomputed on
  the new criterion (`logs/tu_*.log`); every number in the abstract, introduction, Sections 4-5,
  conclusion, Table 1, Table 2 and the appendix tables was refreshed from those runs.

## 5bis. Two criterion-mixing bugs found while re-running (21 Sept 2026)

Switching the criteria to population-weighted NPVs left two places reading the old ones:

* **Cached proposals.** `build_proposal` serialises a `Proposal`, which carries the *target*
  values the solvers aim at (`welf`, `consd` and the world aggregates). Its cache key was
  `hash(name, tax)`, so a stale cache fed the solver per-capita targets against
  population-weighted values: the first joint solve reported `max gap = 155384536%`. The key now
  includes `CRITERION_VERSION`; bump that string whenever a criterion changes. The stale option-A
  checkpoints were moved to `cap_and_share/output/_pre_popw/`, since `seed_solved!` checks the
  target name but not the criterion.
* **Reported welfare gains.** `run_variant` computed a country's welfare gain as a per-capita NPV
  and compared it with the population-weighted `P.welfare[e]`, giving -100% for every member.
  The solved rho were unaffected (both solvers drive on `objective_by_country` against `target`,
  consistently weighted), but the "members losing on welfare" counts and the `ede_gain_pct`
  column were wrong. Fixed with `npv_pop`, and every variant was rebuilt from the solved rho
  (`logs/rerun_variants.sh`, four model runs per cell).

Reboot resilience, added the same night (the machine lost power five times): the proposal driver
records each finished cell in `logs/tu_done.txt` (file settable through `NICE_DONE_FILE`) and
skips it under `NICE_RESUME=1`; Exercise 1 saves after every grid point rather than at the end of
a country's grid; `logs/rerun_total.bat` restarts the whole pipeline where it stopped.

## 5. Status (21 Sept 2026): complete

All 38 TODOs are addressed; each is kept verbatim in paper.tex and followed by a `% DONE` note.
Everything is recomputed under total utilitarianism: 8 joint cells, 8 isolated cells, the full
Exercise 1 grid, the figures, and the window diagnostic (now population-weighted too). The paper
compiles in `build/` (30 pages, no undefined references), the PDF is copied next to the .tex, and
the main text is 5,907 words (JEEM limit 6,000; 6,092 counting the back-matter declarations),
with the abstract at 196 words and four exhibits.

Headline numbers under the new criterion: the equivalent-rights cut is 5.1% of the coalition's
emissions for Wolfram et al., 4.7% for Banerjee, Duflo and Greenstone and 15.2% for Equal Right
at 5%/yr (18.8% on its own path); at unchanged emissions, every member gains 0.019%, 0.052% and
0.180% of the NPV of its consumption. The first-order prediction is now within 1% of the
simulated rho_1 for five of the eight countries of Exercise 1.


All 37 TODOs are addressed; each is kept verbatim in paper.tex and followed by a `% DONE` note.
The paper compiles in `build/` (30 pages, no undefined references), the PDF is copied next to
the .tex, the main text is ~5,850 words (JEEM limit 6,000) with four exhibits (limit five), and
the abstract is 196 words (limit 250).

New or rewritten code: `src/indifference_curves.jl` (Exercise 1), `cap_and_share/indifference_curves.R`
(its figures), `src/run_solves_sept2026.jl` (driver), `src/_diag_window.jl` (diagnostic), and the
solver changes in `src/equivalent_rights_proposals.jl` (nested joint solve, fixed damages,
per-capita prediction, `write_main_table`).

## 4. Rewriting order

1. Theory section (notation, Harberger, \hat rho, Hotelling) - independent of runs.
2. Related literature -> "Why differentiate?" section (T6-T9), plus the novelty framing (T1, T4).
3. Model description dissolved (T16, T17, T19, T20) into the two quantitative sections.
4. Section "Equivalent rights by country" (3.2 + 4.1): after Exercise 1 reruns (T28-T31).
5. Section "Equivalent rights of prominent proposals" (3.1 + 3.3 + 4.2): after Exercise 2
   runs (T22-T26, T32-T36), Equal Right description (T23).
6. Conclusion figure (T37), then abstract, intro numbers (T3, T4), highlights (T2) last,
   since they quote results.
7. Word count and exhibit count check; compile in `build/`.
