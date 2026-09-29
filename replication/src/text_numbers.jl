################################################################################
# Every number the text of the paper quotes from the model, recomputed from the
# results of the run (and, for Mongolia's compensation and the effect of avoided
# damages, from a few extra model runs).
#
# Writes cap_and_share/output/text_numbers.csv: one row per number, with the
# section where it appears, the computed value, the value formatted as the paper
# prints it, the paper's own figure and whether the two agree. Numbers that are
# inputs of the model (tier prices, the 3% discount rate, the 1000 GtCO2 budget
# as a target, the 5% growth of the schedules) or come from other studies are
# not listed.
#
#   julia --project=. src/text_numbers.jl
# Run after the solves, the tables and the indifference grids of both variants.
################################################################################
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
ENV["NICE_RECYCLING"] = "negishi"; ENV["NICE_TARGET"] = "cons"   # the main variant
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

const OUT_CONS = joinpath(ROOT, "cap_and_share", "output")
const OUT_WELF = joinpath(OUT_CONS, "equal_pc")

# ── the table of numbers ──────────────────────────────────────────────────────
const ROWS = NamedTuple[]
fmt(x, d) = (s = @sprintf("%.*f", d, x); s == "-" * @sprintf("%.*f", d, 0.0) ? s[2:end] : s)

"""
    num!(id, section, quantity, value, digits, paper; ok)

Record a number. `paper` is the figure as printed in the paper (without sign
decoration); by default the check is that `value` rounded to `digits` prints as
`paper`. Qualitative statements ("about half", "most members") pass `ok`, the
test that the statement holds for the computed value.
"""
function num!(id, section, quantity, value, digits, paper; ok = nothing)
    printed = fmt(value, digits)
    good    = ok === nothing ? printed == paper : ok
    push!(ROWS, (id = id, section = section, quantity = quantity, value = value,
                 printed = printed, paper = paper, match = good))
    @printf("%-4s %-34s %-12s paper %-12s %s\n", good ? "ok" : "DIFF", id, printed, paper,
            quantity)
end

ci(e)       = only(findall(==(Symbol(e)), COUNTRIES))
yr(y)       = YEAR_IDX[y]
members(P)  = P.members

# ── scenarios and results ─────────────────────────────────────────────────────
W   = build_proposal("Wolfram",     proposal_tax_matrix(wolfram_rate))
D   = build_proposal("Duflo",       proposal_tax_matrix(duflo_rate))
ER  = build_proposal("EqualRight",  equal_right_tax_matrix(escalation = :own))
ER5 = build_proposal("EqualRight5", equal_right_tax_matrix(escalation = :common))
props = [W, D, ER, ER5]
RC = read_results_for("",     props; base = OUT_CONS)      # consumption variant
RW = read_results_for("_ede", props; base = OUT_WELF)      # welfare variant
for k in (("A", "Wolfram"), ("A", "Duflo"), ("B", "Wolfram"), ("B", "Duflo"),
          ("B", "EqualRight"), ("B", "EqualRight5"))
    haskey(RC, k) || error("missing consumption-variant results for $k: run the solves first")
end

cut(R, m, n)     = -R[(m, n)].v1.emissions_change_pct                     # % cut of coalition emissions
mingain(R, n, P) = minimum(R[("B", n)].v2.cons_gain[c] for c in P.members)  # maximin gain (Table 2)
losers(v, field, P) = [string(COUNTRIES[c]) for c in P.members if getfield(v, field)[c] < -LOSS_TOL]

# ══ Section 4: the benchmark p* and the indifference curves ══════════════════
let m = REFERENCE_RUN
    num!("pstar_2035", "4.2", "p* in 2035 (USD/tCO2)", P_STAR[yr(2035)], 0, "142")
    num!("pstar_growth", "4.2", "growth of p* after 2035 (%/yr)", (P_STAR[yr(2041)] / P_STAR[yr(2040)] - 1) * 100, 1, "1.7")
    num!("pstar_2050", "4.2", "p* in 2050 (USD/tCO2)", P_STAR[yr(2050)], 0, "179")
    num!("pstar_2100", "4.2", "p* in 2100 (USD/tCO2)", P_STAR[yr(2100)], 0, "402")
    E = world_emissions(m)
    # the budget is spent exactly in the configuration of the p* search (territorial
    # emissions, no recycling: cap_and_share/find_global_exp_carbon_tax_buget_zoom.jl) ...
    let ms = MimiNICE2020.create_nice2020()
        update_param!(ms, :switch_recycle, 0)
        update_param!(ms, :abatement, :control_regime, 1)
        update_param!(ms, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
        update_param!(ms, :abatement, :global_carbon_tax, P_STAR)
        run(ms)
        num!("pstar_budget_search", "4.2", "world emissions 2025-2100 at p*, p* search configuration (GtCO2)",
             sum(world_emissions(ms)[yr(2025):yr(2100)]), 0, "1000")
    end
    # ... not quite in the configuration of the results (consumption-based emissions, recycling)
    num!("pstar_budget_results", "4.2", "world emissions 2025-2100 at p*, configuration of the results (GtCO2)",
         sum(E[yr(2025):yr(2100)]), 0, "1000")
    for (y, p) in ((2025, "38"), (2030, "28"), (2035, "21"), (2100, "2"))
        num!("E_world_$y", "4.2", "world emissions in $y at p* (GtCO2)", E[yr(y)], 0, p)
    end
    num!("T_2100_pstar", "4.2", "warming in 2100 at p* (C)", temperature(m)[yr(2100)], 2, "1.84")
    # footnote: Ramsey, 1 + r = (1 + prtp)(1 + g)^eta, with g the growth of world
    # mean consumption per capita over the NPV window
    tot  = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    pop  = population(m)
    c_pc = vec(sum(tot .* pop, dims = 2)) ./ vec(sum(pop, dims = 2))
    g    = (c_pc[yr(2100)] / c_pc[yr(2025)])^(1 / 75) - 1
    num!("growth_cpc", "4.2 fn", "growth of consumption per capita 2025-2100 (%/yr)", g * 100, 1, "1.8")
    num!("prtp", "4.2 fn", "pure rate of time preference implied (%)", ((1 + DISCOUNT_RATE) / (1 + g)^ETA - 1) * 100, 1, "0.3")
end

let t1 = CSV.read(joinpath(OUT_CONS, "indifference", "table_rho1.csv"), DataFrame),
    cv = CSV.read(joinpath(OUT_CONS, "indifference", "indifference_curves.csv"), DataFrame)
    row(e) = only(eachrow(t1[String.(t1.country) .== e, :]))
    at(e, p, col) = only(cv[(String.(cv.country) .== e) .& (cv.pi .== p), col])
    num!("rel_2025_CHN", "4.3", "China's 2025 emissions p.c. over world average", row("CHN").emissions_pc_rel_2025, 1, "1.7")
    num!("rho1_CHN", "4.3", "China's rho_1", row("CHN").rho1_cons, 2, "0.95")
    num!("rel_2025_IND", "4.3", "India's 2025 emissions p.c. over world average", row("IND").emissions_pc_rel_2025, 1, "0.4")
    num!("rho1_IND", "4.3", "India's rho_1", row("IND").rho1_cons, 2, "0.91")
    num!("rel_2025_EU27", "4.3", "EU27's 2025 emissions p.c. over world average", row("EU27").emissions_pc_rel_2025, 1, "1.4")
    num!("rho1_EU27", "4.3", "EU27's rho_1", row("EU27").rho1_cons, 2, "0.37")
    num!("rho0_NGA", "Intro, 4.3", "Nigeria's equivalent rights at a zero autarky price", at("NGA", 0.0, :rho_cons), 2, "0.28")
    num!("rho0_NGA_pct", "Conclusion", "Nigeria's zero-price bound below 30% of an equal share", at("NGA", 0.0, :rho_cons), 2, "< 0.30";
         ok = at("NGA", 0.0, :rho_cons) < 0.30)
    num!("rho0_COD", "4.3", "DRC's equivalent rights at a zero autarky price", at("COD", 0.0, :rho_cons), 2, "0.05")
    num!("rho0_COD_pct", "Conclusion", "DRC's zero-price bound below 6% of an equal share", at("COD", 0.0, :rho_cons), 2, "< 0.06";
         ok = at("COD", 0.0, :rho_cons) < 0.06)
end

# ══ Section 5.1: the proposals ════════════════════════════════════════════════
num!("n_countries", "Abstract, 4.1", "countries in NICE", NB_COUNTRY, 0, "179")
num!("W_members", "5.1", "members of the Wolfram et al. coalition", length(W.members), 0, "47")
num!("W_share_2030", "5.1", "Wolfram et al. members' share of world CO2 in 2030, under the schedule (%)",
     W.club_emissions[yr(2030)] / W.world_emissions[yr(2030)] * 100, 0, "48")
let listed = CSV.read(joinpath(ROOT, "cap_and_share", "data", "equal_right_prices.csv"), DataFrame)
    num!("ER_min_2025", "5.1", "lowest Equal Right charge in 2025 (USD/t)", minimum(listed.price_2025), 0, "12")
    num!("ER_max_2025", "5.1", "highest Equal Right charge in 2025 (USD/t)", maximum(listed.price_2025), 0, "240")
    t = yr(2025)
    p = [ER.tax[t, c] for c in ER.members]
    e = [ER.emissions[t, c] for c in ER.members]
    n = [ER.pop[t, c] for c in ER.members]
    num!("ER_mean_2025_emw", "5.1", "Equal Right charge in 2025, emission-weighted mean (USD/t)", sum(p .* e) / sum(e), 0, "144")
    num!("ER_mean_2025_unw", "5.1", "Equal Right charge in 2025, unweighted mean over the listed economies (USD/t)",
         sum(listed.price_2025) / nrow(listed), 0, "144")
    num!("ER_mean_2025_popw", "5.1", "Equal Right charge in 2025, population-weighted mean (USD/t)", sum(p .* n) / sum(n), 0, "144")
end
num!("ER_growth", "5.1, App. C", "growth of the Equal Right charges (%/yr)", (EQUAL_RIGHT_FACTOR[2026] - 1) * 100, 0, "16")
let t = yr(2050), mem = ER.members
    sh = count(c -> ER.tax[t, c] >= PBACKTIME[t] - 1e-9, mem) / length(mem)
    num!("ER_backstop_2050", "5.1, App. C", "share of members at the backstop by 2050 on Equal Right's own path",
         sh * 100, 0, "most"; ok = sh > 0.5)
end
num!("T2100_W", "5.1", "2100 warming, Wolfram et al. (C)", W.temp_2100, 2, "2.16")
num!("T2100_D", "5.1", "2100 warming, Banerjee et al. (C)", D.temp_2100, 2, "1.80")
num!("T2100_ER5", "5.1", "2100 warming, Equal Right at 5%/yr (C)", ER5.temp_2100, 2, "1.65")
num!("T2100_ER", "App. C", "2100 warming, Equal Right on its own path (C)", ER.temp_2100, 2, "1.48")

# ══ Section 5.2: the coalition prices ═════════════════════════════════════════
for (P, s) in ((W, "49"), (D, "56"), (ER5, "140"))
    num!("pref_2030_$(P.name)", "5.2, 5.3", "coalition price in 2030, $(P.name) (USD/t)", P.p_ref[yr(2030)], 0, s)
end
for P in (W, D)
    g = ((P.p_ref[yr(2050)] / P.p_ref[yr(2030)])^(1 / 20) - 1) * 100
    num!("pref_growth_$(P.name)", "5.2", "growth of the coalition price 2030-2050, $(P.name) (%/yr)", g, 1, "4-5";
         ok = 3.5 <= g < 5.5)
end

# ══ Section 5.3, abstract, introduction, conclusion ═══════════════════════════
num!("cut_D", "Abstract, Intro, 5.3", "coalition emissions cut, Banerjee et al. (%)", cut(RC, "B", "Duflo"), 1, "4.8")
num!("cut_W", "Abstract, Intro, 5.3", "coalition emissions cut, Wolfram et al. (%)", cut(RC, "B", "Wolfram"), 1, "4.8")
num!("cut_ER5", "Intro, 5.3", "coalition emissions cut, Equal Right (%)", cut(RC, "B", "EqualRight5"), 1, "13.4")
num!("cut_ER5_conc", "Conclusion", "coalition emissions cut, Equal Right, rounded (%)", cut(RC, "B", "EqualRight5"), 0, "13")
num!("cut_about5", "Abstract, Conclusion", "Wolfram and Banerjee cuts are about 5%",
     cut(RC, "B", "Duflo"), 1, "about 5"; ok = all(round(cut(RC, "B", n)) == 5 for n in ("Duflo", "Wolfram")))
num!("dT_D", "5.3", "change in 2100 warming, Banerjee et al., reduced emissions (C)",
     RC[("B", "Duflo")].v1.temp_2100 - D.temp_2100, 2, "-0.02")
num!("gain_D", "5.3", "equal consumption gain, Banerjee et al. (%)", mingain(RC, "Duflo", D), 3, "0.049")
num!("gain_D_2", "Intro, Conclusion", "equal consumption gain, Banerjee et al., rounded (%)", mingain(RC, "Duflo", D), 2, "0.05")
num!("gain_W_2", "Intro, Conclusion", "equal consumption gain, Wolfram et al., rounded (%)", mingain(RC, "Wolfram", W), 2, "0.04")
num!("gain_ER5_2", "Intro, Conclusion", "equal consumption gain, Equal Right (%)", mingain(RC, "EqualRight5", ER5), 2, "0.17")
num!("cut_D_welf", "5.3, App. B", "coalition emissions cut, Banerjee et al., welfare variant (%)", cut(RW, "B", "Duflo"), 1, "27.2")
num!("cut_W_welf", "App. B", "coalition emissions cut, Wolfram et al., welfare variant (%)", cut(RW, "B", "Wolfram"), 1, "34.5")
num!("cut_ER5_welf", "App. B", "coalition emissions cut, Equal Right, welfare variant (%)", cut(RW, "B", "EqualRight5"), 1, "29.6")

# the equivalent rights of the Banerjee et al. schedule
let rho = RC[("B", "Duflo")].rho
    for (e, s) in (("USA", "2.99"), ("RUS", "1.90"), ("CHN", "1.00"), ("IND", "1.06"), ("NGA", "0.22"), ("COD", "0.06"))
        num!("rho_D_$e", "5.3", "equivalent rights of $e, Banerjee et al.", rho[ci(e)], 2, s)
    end
    for (e, s) in (("IND", "1.16"), ("COD", "0.09"))
        num!("rhohat_D_$e", "5.3", "first-order prediction for $e, Banerjee et al.", predicted_rho_priced(e, D), 2, s)
    end
    # "Nigeria, at the same price [as India] but emitting a tenth as much per capita"
    t = yr(2030)
    pc(e) = D.emissions[t, ci(e)] / D.pop[t, ci(e)]
    r = pc("NGA") / pc("IND")
    num!("NGA_over_IND_pc", "5.3", "Nigeria's emissions p.c. over India's, 2030, under the schedule", r, 2, "a tenth";
         ok = 0.08 <= r <= 0.12)
    t1 = CSV.read(joinpath(OUT_CONS, "indifference", "table_rho1.csv"), DataFrame)
    rw = only(t1[String.(t1.country) .== "NGA", :emissions_pc_rel_2025])
    num!("NGA_over_world_pc", "5.3", "Nigeria's emissions p.c. over the world average, 2025 (Table 1)", rw, 2, "a tenth";
         ok = 0.08 <= rw <= 0.15)
end

# the implicit transfers of the Banerjee et al. schedule (write_transfers)
let df = CSV.read(joinpath(OUT_CONS, "implicit_transfers_duflo.csv"), DataFrame)
    df  = df[df.member, :]
    row(e) = only(eachrow(df[String.(df.country) .== e, :]))
    for (e, s) in (("USA", "-0.42"), ("CHN", "0.02"), ("IND", "0.45"))
        num!("tau_$e", "5.3", "implicit transfer of $e, NPV, % of its consumption", 100 * row(e).npv_transfer / row(e).npv_consumption, 2, s)
    end
    gross = 100 * sum(max.(df.npv_transfer, 0.0)) / sum(df.npv_consumption)
    num!("gross_tau", "Abstract, Intro, 5.3, Conclusion", "gross implicit transfers, NPV, % of coalition consumption", gross, 2, "0.10")
    ratio = mingain(RC, "Duflo", D) / gross
    num!("gain_over_tau", "Abstract, Intro, 5.3, Conclusion", "efficiency gain over gross implicit transfers", ratio, 2, "about half";
         ok = 0.4 <= ratio <= 0.6)
    for (y, s) in ((2030, "0.03"), (2050, "0.10"))
        v = 100 * sum(max.(df[!, "transfer_$y"], 0.0)) / sum(df[!, "consumption_$y"])
        num!("gross_tau_$y", "5.3", "gross implicit transfers in $y, % of coalition consumption", v, 2, s)
    end
    num!("tau_2050_IND", "5.3", "India's implicit transfer in 2050, % of its consumption",
         100 * row("IND").transfer_2050 / row("IND").consumption_2050, 2, "0.44")
    num!("tau_2050_USA", "5.3", "United States' implicit transfer in 2050, % of its consumption",
         100 * row("USA").transfer_2050 / row("USA").consumption_2050, 2, "-0.43")
end

# Equal Right: the dividend term and its decomposition, with each year weighted
# as in the dividend term of the first-order formula for the DRC,
# beta_t (N_DRC,t / N_t) E_t
let pp = CSV.read(joinpath(OUT_CONS, "price_paths_B_equalright5.csv"), DataFrame)
    col(s) = Dict(Int(r.time) => Float64(r[s]) for r in eachrow(pp))
    pref, pbar, p1 = col(:p_ref), col(:pbar), col(:p_v1)
    cod = ci("COD")
    w   = [NPV_DISC[k] * ER5.pop[t, cod] / ER5.club_pop[t] * ER5.club_emissions[t] for (k, t) in enumerate(NPV_IDX)]
    ratio(a, b) = sum(w[k] * a[YEARS[t]] for (k, t) in enumerate(NPV_IDX)) /
                  sum(w[k] * b[YEARS[t]] for (k, t) in enumerate(NPV_IDX))
    num!("ER5_pbar_p1", "5.3", "Equal Right: pbar / p* (dividend worth, in equal shares)", ratio(pbar, p1), 2, "0.64")
    num!("ER5_pref_p1", "5.3", "Equal Right: p_ref / p* (tighter cap)", ratio(pref, p1), 2, "0.85")
    num!("ER5_pbar_pref", "5.3", "Equal Right: pbar / p_ref (composition effect)", ratio(pbar, pref), 2, "0.75")
    # "the tonnes that survive sit in the $15-42/t tiers": share of the schedule's
    # emissions from members charged at most $42/t in 2025, by year
    low = [c for c in ER5.members if ER5.tax[yr(2025), c] <= 42 + 1e-9]
    for (y, ok_) in ((2030, nothing), (2050, true), (2060, true))
        s = sum(ER5.emissions[yr(y), c] for c in low) / ER5.club_emissions[yr(y)]
        num!("ER5_low_tiers_$y", "5.3", "Equal Right: share of the schedule's emissions from members at <= \$42/t, $y (%)",
             100 * s, 0, ok_ === nothing ? "(not claimed)" : "most"; ok = ok_ === nothing ? true : s > 0.5)
    end
end
let r1 = RC[("B", "EqualRight5")].rho, r2 = RC[("B", "EqualRight5")].v2.rho_eff
    num!("rho_ER5_COD", "5.3", "DRC's equivalent rights, Equal Right, reduced emissions", r1[ci("COD")], 2, "0.68")
    for (e, s) in (("COD", "0.85"), ("NGA", "0.98"), ("IND", "1.44"), ("CHN", "1.19"), ("IDN", "1.12"))
        num!("rho2_ER5_$e", "5.3", "$e's rights, Equal Right, increased consumption", r2[ci(e)], 2, s)
    end
    num!("ER5_negative", "5.3", "members with a negative equivalent ratio, Equal Right", count(c -> r1[c] < 0, ER5.members), 0, "11")
    num!("rho_ER5_USA", "5.3", "United States' equivalent rights, Equal Right", r1[ci("USA")], 2, "-0.34")
end

# ══ Online Appendix A: solves, sharing rules, losers ══════════════════════════
num!("cut_W_A", "App. A", "coalition emissions cut, Wolfram et al., isolated solve (%)", cut(RC, "A", "Wolfram"), 1, "4.9")
num!("cut_D_A", "App. A", "coalition emissions cut, Banerjee et al., isolated solve (%)", cut(RC, "A", "Duflo"), 1, "5.0")
num!("lose_A_W", "App. A", "Wolfram et al. members losing consumption, isolated solve", length(losers(RC[("A", "Wolfram")].v1, :cons_gain, W)), 0, "26")
num!("lose_A_D", "App. A", "Banerjee et al. members losing consumption, isolated solve", length(losers(RC[("A", "Duflo")].v1, :cons_gain, D)), 0, "44")
num!("welf_B3_D", "App. A", "world welfare gain, marginal-utility sharing, Banerjee et al. (%)", RC[("B", "Duflo")].v3.welfare_gain_pct, 3, "0.232")
num!("welf_B2_D", "App. A", "world welfare gain, maximin sharing, Banerjee et al. (%)", RC[("B", "Duflo")].v2.welfare_gain_pct, 3, "0.053")
num!("lose_B3_D", "App. A", "Banerjee et al. members losing consumption, marginal-utility sharing", length(losers(RC[("B", "Duflo")].v3, :cons_gain, D)), 0, "11")
num!("welf_B4_D", "App. A", "world welfare gain, uniform scaling, Banerjee et al. (%)", RC[("B", "Duflo")].v4.welfare_gain_pct, 3, "0.040")
num!("lose_B4_cons", "App. A", "members losing consumption under uniform scaling (both schedules)",
     length(losers(RC[("B", "Duflo")].v4, :cons_gain, D)) + length(losers(RC[("B", "Wolfram")].v4, :cons_gain, W)), 0, "0")
num!("lose_B4_W_welf", "App. A", "Wolfram et al. members losing welfare, uniform scaling", length(losers(RC[("B", "Wolfram")].v4, :ede_gain, W)), 0, "9")
num!("lose_B4_D_welf", "App. A", "Banerjee et al. members losing welfare, uniform scaling", length(losers(RC[("B", "Duflo")].v4, :ede_gain, D)), 0, "64")
for (n, P, s) in (("Duflo", D, "MNG"), ("EqualRight5", ER5, "FIN ISL MNG RUS"), ("Wolfram", W, ""))
    l = join(sort(losers(RC[("B", n)].v1, :cons_gain, P)), " ")
    num!("lose_B1_$n", "5.3, App. A", "members below the schedule, reduced emissions, $n", length(split(l)), 0, s; ok = l == s)
end

# damages held at the schedule's temperatures (as solved) or endogenous (as
# reported), for the joint reduced-emissions allocation of Banerjee et al.
function gains_of(P, rho, fixed_temp)
    rights = rights_from_rho(rho, P)
    m = make_uniform_model(RECYCLE_SHARE, P.members; fixed_temp = fixed_temp)
    p, _ = calibrate_price_to_cap(m, rights, vec(sum(rights, dims = 2)); p_init = P.p_ref, tol = 1e-4,
                                  label = "text numbers/$(P.name)")
    run_uniform!(m, rights, p)
    tot = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    pop = population(m)
    return Dict(c => (npv_pop(@view(tot[:, c]), @view(pop[:, c])) - P.cons[string(COUNTRIES[c])]) /
                     abs(P.cons[string(COUNTRIES[c])]) * 100 for c in P.members)
end
let rho = RC[("B", "Duflo")].rho
    gp = gains_of(D, rho, proposal_local_temp(D))
    ge = gains_of(D, rho, nothing)
    num!("max_gap_pinned", "App. A", "largest |gap| to indifference with damages held (%)", maximum(abs, values(gp)), 3, "<= 0.001";
         ok = maximum(abs, values(gp)) <= 0.0015)
    num!("loss_MNG", "5.3, App. A", "Mongolia's consumption change, damages endogenous (%)", ge[ci("MNG")], 2, "-0.02")
    num!("avoided_NGA", "App. A", "Nigeria's gain from avoided warming (%)", ge[ci("NGA")] - gp[ci("NGA")], 2, "0.10")
    num!("avoided_IND", "App. A", "India's gain from avoided warming (%)", ge[ci("IND")] - gp[ci("IND")], 2, "0.07")
    # the extra rights that bring Mongolia back to indifference: secant on its
    # ratio, damages endogenous
    mng = ci("MNG")
    g(d) = (r = copy(rho); r[mng] += d; gains_of(D, r, nothing)[mng])
    d0, g0 = 0.0, ge[mng]
    d1 = 0.05; g1 = g(d1)
    dstar = d0 - g0 * (d1 - d0) / (g1 - g0)
    @printf("Mongolia: extra ratio %.4f, gain there %.5f%%\n", dstar, g(dstar))
    num!("comp_MNG_shares", "5.3, App. A", "extra equal per capita shares compensating Mongolia", dstar, 2, "0.03")
    num!("comp_MNG_pct", "5.3, App. A", "the same, % of Mongolia's allocation", 100 * dstar / rho[mng], 2, "0.45")
    extra = sum(dstar * D.pop[t, mng] * D.ebar[t] for t in NPV_IDX)
    num!("comp_MNG_world", "5.3, App. A", "the same, % of global emissions 2025-2100", 100 * extra / sum(D.world_emissions[NPV_IDX]), 3, "0.001")
end
let coef = CSV.read(joinpath(ROOT, "data", "country_damage_coefficients.csv"), DataFrame),
    T = proposal_local_temp(D)
    b1 = Dict(String(r.countrycode) => Float64(r.beta1_KW) for r in eachrow(coef))
    b2 = Dict(String(r.countrycode) => Float64(r.beta2_KW) for r in eachrow(coef))
    neg(t) = count(c -> (e = string(COUNTRIES[c]); b1[e] < 0 && T[t, c] < -b1[e] / (2 * b2[e])), 1:NB_COUNTRY)
    num!("beta1_negative", "App. A", "countries with beta1 < 0 (Kalkuhl-Wenz)", count(c -> b1[string(c)] < 0, COUNTRIES), 0, "13")
    num!("neg_marginal_damage", "App. A", "countries with beta1 < 0 and local anomaly below -beta1/(2 beta2), 2100, Banerjee et al.",
         neg(yr(2100)), 0, "13")
end

# ══ Online Appendix B: the welfare variant ════════════════════════════════════
let t1w = CSV.read(joinpath(OUT_WELF, "indifference", "table_rho1.csv"), DataFrame),
    t1c = CSV.read(joinpath(OUT_CONS, "indifference", "table_rho1.csv"), DataFrame),
    cw  = CSV.read(joinpath(OUT_WELF, "indifference", "indifference_curves.csv"), DataFrame)
    rw(e) = only(t1w[String.(t1w.country) .== e, :rho1_welfare])
    rc(e) = only(t1c[String.(t1c.country) .== e, :rho1_cons])
    for (e, s) in (("USA", "4.26"), ("CHN", "0.96"), ("EU27", "0.37"), ("IND", "0.94"), ("NGA", "0.18"))
        num!("rho1w_$e", "App. B", "rho_1 of $e, welfare variant", rw(e), 2, s)
    end
    d = maximum(abs(rw(e) - rc(e)) for e in String.(t1w.country))
    num!("rho1_w_minus_c", "App. B", "largest |rho_1 welfare - rho_1 consumption| over the 8 economies", d, 2, "<= 0.03"; ok = d <= 0.03)
    at(e, p) = only(cw[(String.(cw.country) .== e) .& (cw.pi .== p), :rho_welfare])
    mx(e)    = maximum(cw[String.(cw.country) .== e, :rho_welfare])
    num!("rhow_USA_pi05", "App. B", "US equivalent rights at pi = 0.5, welfare variant", at("USA", 0.5), 2, "4.53")
    num!("rhow_USA_pi0", "App. B", "US equivalent rights at pi = 0, welfare variant", at("USA", 0.0), 2, "3.58")
    num!("rhow_NGA_max", "App. B", "Nigeria's largest equivalent rights, welfare variant", mx("NGA"), 2, "0.19")
    num!("rhow_NGA_pi0", "App. B", "Nigeria's equivalent rights at pi = 0, welfare variant", at("NGA", 0.0), 2, "0.14")
    num!("rhow_COD_max", "App. B", "DRC's largest equivalent rights, welfare variant", mx("COD"), 2, "0.04")
    usa = cw[String.(cw.country) .== "USA", :]
    pmax = usa.pi[argmax(usa.rho_welfare)]
    num!("rhow_USA_argmax", "App. B", "autarky price factor at which US rights peak, welfare variant", pmax, 2, "0.5-1";
         ok = 0.5 <= pmax <= 1.0)
    # colour limit of Figure A1 (cap_and_share/indifference_curves.R): 95th
    # percentile of |gain| over the cells shown, all eight economies
    PI_SHOW  = collect(0.0:0.25:4.75)
    RHO_SHOW = [0.02, 0.05, 0.1, 0.2, 0.5, 1, 2, 5, 10]
    gains = Float64[]
    for e in unique(String.(cw.country))
        u = CSV.read(joinpath(OUT_WELF, "indifference", "uniform_$e.csv"), DataFrame)
        a = CSV.read(joinpath(OUT_WELF, "indifference", "autarky_$e.csv"), DataFrame)
        u = u[in.(u.rho, Ref(RHO_SHOW)), :]; a = a[in.(a.pi, Ref(PI_SHOW)), :]
        for wu in u.welfare, wa in a.welfare
            push!(gains, (wu - wa) / abs(wa) * 100)
        end
    end
    num!("clip_eqpc", "App. B", "colour limit of Figure A1 (95th percentile of |gain|, %)", quantile(abs.(filter(isfinite, gains)), 0.95), 0, "35")
end
let rc = RC[("B", "Duflo")].rho, rw = RW[("B", "Duflo")].rho
    for (e, sw, sc) in (("IND", "0.62", "1.06"), ("CHN", "0.82", "1.00"), ("USA", "2.83", "2.99"))
        num!("rhow_D_$e", "App. B", "equivalent rights of $e, Banerjee et al., welfare variant", rw[ci(e)], 2, sw)
    end
end

# ══ Online Appendix C: Equal Right on its own path ════════════════════════════
num!("cut_ER", "App. C", "coalition emissions cut, Equal Right on its own path (%)", cut(RC, "B", "EqualRight"), 1, "14.4")
num!("dT_ER", "App. C", "change in 2100 warming, Equal Right on its own path (C)", RC[("B", "EqualRight")].v1.temp_2100 - ER.temp_2100, 3, "-0.010")
num!("dT_ER5", "App. C", "change in 2100 warming, Equal Right at 5%/yr (C)", RC[("B", "EqualRight5")].v1.temp_2100 - ER5.temp_2100, 3, "-0.033")

# ── write ─────────────────────────────────────────────────────────────────────
out = joinpath(OUT_CONS, "text_numbers.csv")
CSV.write(out, DataFrame(ROWS))
n_bad = count(r -> !r.match, ROWS)
@printf("\n%d numbers, %d differ from the paper; written to %s\n", length(ROWS), n_bad, out)
