################################################################################
# Exercise 1 of the paper: single-country indifference curves between
#   AUTARKY  country i prices at pi * p*_t and keeps its revenue; the rest of the
#            world applies one price p_{-i,t}, solved year by year so that world
#            emissions follow the uniform-price path E*_t;
#   UNIFORM  every country prices at p*_t; i holds rights
#            rho * (n_it / N_t) * E*_t, the rest of the world the remainder, pro
#            rata to its own emissions (so its members trade roughly nothing).
# Both regimes emit E*_t, so damages are the same and the comparison is between
# i's welfare in the two. A grid over (pi, rho) gives the heatmaps of Figure 1,
# the indifference curves, and Table 1 (rho at pi = 1).
#
# Replaces the Exercise 1 code of src/deprecated/equivalence_price_rights.jl.
# Differences, all in the direction of consistency (Sept 2026):
#   * both regimes use the model configuration of Exercise 2 (make_autarky_model
#     / make_uniform_model of equivalent_rights_proposals.jl): revenue recycled
#     to deciles in proportion to c^eta, default income elasticity of emissions.
#     The deprecated autarky runs refunded each decile its own burden
#     (switch_recycle = 0) with a zero elasticity slope, while its uniform runs
#     used the c^eta rule: at pi = 1, where the two regimes coincide by
#     construction, the EDE comparison then picked up a within-country
#     difference unrelated to the question.
#   * p* is the same path as in Exercise 2 (P_STAR, priced from 2025).
#   * pi grid includes 0 and 0.25; rho grid is dense and includes 0, so that the
#     curves can be read on a linear scale; the indifference rho is interpolated
#     linearly in rho (welfare is close to linear in rho: rights are sold at p*).
#   * welfare (NPV of EDE consumption) and mean consumption are both stored.
#
#   NICE_EX1_COUNTRIES=USA,CHN julia --project=. src/indifference_curves.jl   # grid
#   julia --project=. src/indifference_curves.jl table                        # Table 1
# Outputs: cap_and_share/output/indifference/ (CSV), then plotted by
# cap_and_share/indifference_curves.R.
################################################################################

ENV["NICE_AUTORUN"] = "0"
haskey(ENV, "NICE_WORKERS") || (ENV["NICE_WORKERS"] = "1")
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

const EX1_DIR       = joinpath(OUTPUT_BASE, "indifference")
const EX1_COUNTRIES = String.(split(get(ENV, "NICE_EX1_COUNTRIES", "USA,RUS,CHN,TUR,EU27,IND,NGA,COD"), ","))
const PI_GRID  = [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0]
const RHO_GRID = [0.0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.8, 1.0, 1.25, 1.5, 2.0, 2.5,
                  3.0, 3.5, 4.0, 4.5, 5.0, 6.0, 8.0, 10.0]
const ALL      = collect(1:NB_COUNTRY)
const THETA2   = 2.6                       # abatement cost exponent (nice2020_module.jl)
mkpath(EX1_DIR)

"NPV of per-capita mean consumption of an entity (bloc: population-weighted)."
function entity_cons_npv(m, idx::Vector{Int}, pop::Matrix{Float64})
    tot = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    s   = [sum(tot[t, idx] .* pop[t, idx]) / sum(pop[t, idx]) for t in 1:NB_STEPS]
    return npv(s)
end

# ── the uniform-price reference: everyone at p*, no net transfers ────────────
const BASE = let
    m = make_uniform_model(RECYCLE_SHARE, ALL)
    # zero-transfer rights: each country's own emissions at p*, from the plain run
    run_uniform!(m, copy(country_emissions(REFERENCE_RUN)), P_STAR)
    ems = country_emissions(m)
    (ems = ems, pop = population(m), E = vec(sum(ems, dims = 2)),
     N = vec(sum(population(m), dims = 2)))
end

# ── autarky: rest-of-world price keeping world emissions on E*_t ─────────────
# Largest abatement rate asked of the rest of the world: removal of up to half
# its baseline emissions. Beyond ~1.9 the extrapolated cost function exceeds
# output in carbon-intensive countries and the model returns NaN; a point that
# needs more than MU_ROW_MAX cannot keep the global path and is flagged.
const MU_ROW_MAX = 1.5

"""
    solve_row_price(m, idx, pi)

Newton in abatement-rate space, as in the deprecated script: with mu the ROW's
abatement rate, E_row = B_row (1 - mu), so the target rate is 1 - E_row*/B_row
and the price p = p_back mu^(theta2 - 1) (sign-preserving: a negative mu is a
subsidy, a mu above 1 is removal beyond the backstop -- regime 4 allows both).
B_row moves a little with the price through output, hence the iterations.
"""
function solve_row_price(m, idx::Vector{Int}, pi::Float64; tol = 1e-4, max_iter = 25)
    others = setdiff(ALL, idx)
    scale  = maximum(BASE.E)
    tax    = zeros(Float64, NB_STEPS, NB_COUNTRY)
    mu_of(p, t) = (r = p / PBACKTIME[t]; sign(r) * abs(r)^(1 / (THETA2 - 1)))
    p_of(mu, t) = PBACKTIME[t] * sign(mu) * abs(mu)^(THETA2 - 1)
    function evaluate(prow)
        for t in 1:NB_STEPS
            tax[t, idx]    .= pi * P_STAR[t]
            tax[t, others] .= prow[t]
        end
        run_autarky!(m, tax)
        e = country_emissions(m)
        return vec(sum(@view(e[:, others]), dims = 2)), vec(sum(@view(e[:, idx]), dims = 2))
    end
    # the rest of the world's unabated emissions, read off the model: inferring
    # them as E/(1 - mu) fails exactly where it matters, in the years where p* is
    # the backstop and mu = 1, since emissions are then 0/0
    function baseline_row()
        Y = f64(m[:grosseconomy, :YGROSS]); sig = f64(m[:emissions, :emissionsrate_footprint])
        return vec(sum(@view(Y[:, others]) .* @view(sig[:, others]), dims = 2))
    end
    prow = copy(P_STAR)
    erow, ei = evaluate(prow)
    Brow = baseline_row()
    err  = Inf
    for it in 1:max_iter
        err = maximum(abs(ei[t] + erow[t] - BASE.E[t]) for t in CALIB_IDX) / scale
        (isfinite(err) && err < tol) && break
        # Newton step in abatement-rate space, damped and capped
        pnew = copy(prow)
        for t in CALIB_IDX
            P_STAR[t] <= 0 && continue
            mu = mu_of(prow[t], t)
            B  = Brow[t]
            (B <= 0 || !isfinite(B)) && continue
            mun = clamp(1 - (BASE.E[t] - ei[t]) / B, -1.0, MU_ROW_MAX)
            pnew[t] = p_of(mu + 0.9 * (mun - mu), t)
        end
        # back off towards the last good point if the model breaks down
        for _ in 1:6
            er, eii = evaluate(pnew)
            if all(isfinite, er[CALIB_IDX]) && all(isfinite, eii[CALIB_IDX])
                erow, ei, prow = er, eii, pnew
                Brow = baseline_row()
                break
            end
            pnew = [p_of(0.5 * (mu_of(prow[t], t) + mu_of(pnew[t], t)), t) for t in 1:NB_STEPS]
        end
        prow === pnew || (erow, ei = evaluate(prow))   # restore the model state
    end
    return prow, err, tax
end

function run_autarky_point(m, entity::String, pi::Float64)
    idx = entity_indices(entity)
    prow, err, tax = solve_row_price(m, idx, pi)
    pop = population(m)
    e   = country_emissions(m)
    win = [YEAR_IDX[y] for y in YEARS_NPV]
    return (pi = pi, welfare = entity_welfare_npv(m, idx, pop), cons = entity_cons_npv(m, idx, pop),
            calib_err = err, path_missed = !(err < 1e-3),
            row_price_negative = any(prow[t] < 0 for t in win),
            row_price_above_backstop = any(prow[t] > PBACKTIME[t] for t in win),
            emissions = vec(sum(@view(e[:, idx]), dims = 2)), prow = prow)
end

# ── uniform: i holds rho times its population share of E*_t ─────────────────
function uniform_rights(idx::Vector{Int}, rho::Float64)
    others = setdiff(ALL, idx)
    r = zeros(Float64, NB_STEPS, NB_COUNTRY)
    for t in 1:NB_STEPS
        pi_ = sum(BASE.pop[t, idx])
        ri  = rho * pi_ / BASE.N[t] * BASE.E[t]
        for c in idx
            r[t, c] = BASE.pop[t, c] / pi_ * ri
        end
        rem = BASE.E[t] - ri             # negative: the rest of the world pays
        oth = sum(BASE.ems[t, others])
        for c in others
            r[t, c] = oth != 0 ? BASE.ems[t, c] / oth * rem : rem / length(others)
        end
    end
    return r
end

function run_uniform_point(m, entity::String, rho::Float64)
    idx = entity_indices(entity)
    run_uniform!(m, uniform_rights(idx, rho), P_STAR)
    pop = population(m)
    win = [YEAR_IDX[y] for y in YEARS_NPV]
    bad = any(rho * sum(BASE.pop[t, idx]) / BASE.N[t] > 1 for t in win)
    return (rho = rho, welfare = entity_welfare_npv(m, idx, pop), cons = entity_cons_npv(m, idx, pop),
            row_rights_negative = bad)
end

function run_grid(entity::String)
    t0 = time()
    ma = make_autarky_model(RECYCLE_SHARE)
    mu = make_uniform_model(RECYCLE_SHARE, ALL)
    ua = NamedTuple[]
    fu = joinpath(EX1_DIR, "uniform_$(entity).csv")
    # resume: the uniform runs do not depend on pi, keep them if already done
    if isfile(fu) && nrow(CSV.read(fu, DataFrame)) == length(RHO_GRID)
        @info "uniform runs already on disk" entity
        RHO_GRID_TODO = Float64[]
    else
        RHO_GRID_TODO = RHO_GRID
    end
    for rho in RHO_GRID_TODO
        push!(ua, run_uniform_point(mu, entity, rho))
        @printf("  [%s] uniform rho = %5.2f  welfare %.4f  cons %.4f\n", entity, rho, ua[end].welfare, ua[end].cons)
        flush(stdout)
    end
    isempty(ua) || CSV.write(fu, DataFrame(ua))
    aa = NamedTuple[]; ems = DataFrame(time = YEARS); prs = DataFrame(time = YEARS)
    for pi in PI_GRID
        r = run_autarky_point(ma, entity, pi)
        push!(aa, (pi = r.pi, welfare = r.welfare, cons = r.cons, calib_err = r.calib_err,
                   path_missed = r.path_missed, row_price_negative = r.row_price_negative,
                   row_price_above_backstop = r.row_price_above_backstop))
        ems[!, "pi_$(pi)"] = r.emissions
        prs[!, "pi_$(pi)"] = r.prow
        @printf("  [%s] autarky pi = %4.2f  welfare %.4f  cons %.4f  (calib err %.4f%%)\n",
                entity, pi, r.welfare, r.cons, r.calib_err * 100)
        flush(stdout)
    end
    CSV.write(joinpath(EX1_DIR, "autarky_$(entity).csv"), DataFrame(aa))
    CSV.write(joinpath(EX1_DIR, "autarky_emissions_$(entity).csv"), ems)
    CSV.write(joinpath(EX1_DIR, "autarky_row_price_$(entity).csv"), prs)
    @printf("### %s done in %.1f min\n", entity, (time() - t0) / 60)
end

# ── Table 1 and the predicted curve ─────────────────────────────────────────
"Linear interpolation of the rho at which uniform welfare equals `target`."
function indifference_rho(rhos, vals, target)
    for k in 1:length(rhos)-1
        a, b = vals[k] - target, vals[k+1] - target
        if a == 0
            return rhos[k]
        elseif a * b < 0
            return rhos[k] + (rhos[k+1] - rhos[k]) * a / (a - b)
        end
    end
    # Outside the grid: welfare is linear in rho (rights are sold at the fixed
    # p*; the uniform runs confirm it to the fourth digit), so extrapolate from
    # the two nearest grid points. A negative rho means the country would pay a
    # net fee for its rights and still prefer the uniform price.
    k = vals[1] > target ? 1 : length(rhos) - 1
    slope = (vals[k+1] - vals[k]) / (rhos[k+1] - rhos[k])
    return rhos[k] + (target - vals[k]) / slope
end

"First-order prediction (equation rhohat_dyn): per capita, weighted by beta_t p*_t."
function predicted_rho1(entity::String, emissions::Vector{Float64}; pc = true)
    idx = entity_indices(entity)
    w   = NPV_DISC .* P_STAR[NPV_IDX]
    n   = vec(sum(BASE.pop[:, idx], dims = 2))[NPV_IDX]
    e   = emissions[NPV_IDX]
    eb  = (BASE.E ./ BASE.N)[NPV_IDX]
    return pc ? sum(w .* e ./ n) / sum(w .* eb) : sum(w .* e) / sum(w .* eb .* n)
end

function write_curves_and_table()
    rows = NamedTuple[]; curves = NamedTuple[]
    t30  = YEAR_IDX[2030]
    for entity in EX1_COUNTRIES
        fu = joinpath(EX1_DIR, "uniform_$(entity).csv"); fa = joinpath(EX1_DIR, "autarky_$(entity).csv")
        (isfile(fu) && isfile(fa)) || continue
        u  = CSV.read(fu, DataFrame); a = CSV.read(fa, DataFrame)
        em = CSV.read(joinpath(EX1_DIR, "autarky_emissions_$(entity).csv"), DataFrame)
        for r in eachrow(a)
            e = Float64.(em[!, "pi_$(r.pi)"])
            push!(curves, (country = entity, pi = r.pi,
                           rho_welfare = indifference_rho(u.rho, u.welfare, r.welfare),
                           rho_cons = indifference_rho(u.rho, u.cons, r.cons),
                           rho_hat = predicted_rho1(entity, e),
                           row_price_negative = r.row_price_negative,
                           path_missed = hasproperty(r, :path_missed) ? r.path_missed : false))
            c = curves[end]
            curves[end] = merge(c, (extrapolated = !(first(RHO_GRID) <= c.rho_welfare <= last(RHO_GRID)),))
        end
        idx = entity_indices(entity)
        e1  = Float64.(em[!, "pi_1.0"])
        epc = sum(BASE.ems[t30, idx]) / sum(BASE.pop[t30, idx]) * 1e6     # GtCO2 / thousand -> t
        rel = (sum(BASE.ems[t30, idx]) / sum(BASE.pop[t30, idx])) / (BASE.E[t30] / BASE.N[t30])
        c1  = only(filter(r -> r.pi == 1.0, curves[findall(c -> c.country == entity, curves)]))
        push!(rows, (country = entity, emissions_pc_2030 = epc, emissions_pc_rel_2030 = rel,
                     rho_hat = predicted_rho1(entity, e1), rho_hat_pop = predicted_rho1(entity, e1; pc = false),
                     rho1_welfare = c1.rho_welfare, rho1_cons = c1.rho_cons))
    end
    CSV.write(joinpath(EX1_DIR, "indifference_curves.csv"), DataFrame(curves))
    df = sort(DataFrame(rows), :emissions_pc_2030, rev = true)
    CSV.write(joinpath(EX1_DIR, "table_rho1.csv"), df)
    name(e) = e == "EU27" ? "EU27" : entity_name(e)
    f2(x) = isfinite(x) ? @sprintf("%.2f", x) : "--"
    open(joinpath(OUTPUT_BASE, "rho1_table.tex"), "w") do io
        println(io, "% Generated by src/indifference_curves.jl -- do not edit by hand.")
        println(io, "\\begin{tabular}{lrrrr}")
        println(io, "  \\toprule")
        println(io, "  & Emissions p.c. & Emissions p.c. & Predicted & Simulated \\\\")
        println(io, "  & 2030 (tCO\$_2\$) & over world average & \$\\hat\\rho_1\$ & \$\\rho_1\$ \\\\")
        println(io, "  \\midrule")
        for r in eachrow(df)
            println(io, "  ", name(r.country), " & ", f2(r.emissions_pc_2030), " & ", f2(r.emissions_pc_rel_2030),
                        " & ", f2(r.rho_hat), " & ", f2(r.rho1_welfare), " \\\\")
        end
        println(io, "  \\bottomrule")
        println(io, "\\end{tabular}")
    end
    println(df)
    write_appendix_examples()
    @info "wrote Table 1 and the curves" dir = EX1_DIR
end

"Online Appendix Tables A1 (US autarky at pi = 2) and A2 (DRC uniform at rho = 0.5)."
function write_appendix_examples(; years = 2030:10:2100)
    ti  = [YEAR_IDX[y] for y in years]
    f1(x) = (x < 0 ? "\$-\$" : "") * @sprintf("%.1f", abs(x))
    f2(x) = (x < 0 ? "\$-\$" : "") * @sprintf("%.2f", abs(x))
    f3(x) = (x < 0 ? "\$-\$" : "") * @sprintf("%.3f", abs(x))
    head(io) = (println(io, "\\begin{tabular}{l", "r"^length(years), "}"); println(io, "  \\toprule");
                println(io, "  & ", join(string.(years), " & "), " \\\\"); println(io, "  \\midrule"))
    fa = joinpath(EX1_DIR, "autarky_emissions_USA.csv")
    if isfile(fa)
        em = CSV.read(fa, DataFrame)
        pr = CSV.read(joinpath(EX1_DIR, "autarky_row_price_USA.csv"), DataFrame)
        open(joinpath(OUTPUT_BASE, "appendix_autarky_usa.tex"), "w") do io
            println(io, "% Generated by src/indifference_curves.jl -- do not edit by hand.")
            head(io)
            println(io, "  \$p^*\$ (USD/tCO\$_2\$) & ", join(f1.(P_STAR[ti]), " & "), " \\\\")
            println(io, "  \$p_i\$ (USD/tCO\$_2\$) & ", join(f1.(2 .* P_STAR[ti]), " & "), " \\\\")
            println(io, "  \$p_{-i}\$ (USD/tCO\$_2\$) & ", join(f1.(Float64.(pr[ti, "pi_2.0"])), " & "), " \\\\")
            println(io, "  \$E_i\$ (GtCO\$_2\$) & ", join(f2.(Float64.(em[ti, "pi_2.0"])), " & "), " \\\\")
            println(io, "  \\bottomrule\n\\end{tabular}")
        end
    end
    idx = entity_indices("COD")
    R   = [0.5 * sum(BASE.pop[t, idx]) / BASE.N[t] * BASE.E[t] for t in 1:NB_STEPS]
    E   = vec(sum(BASE.ems[:, idx], dims = 2))
    open(joinpath(OUTPUT_BASE, "appendix_uniform_cod.tex"), "w") do io
        println(io, "% Generated by src/indifference_curves.jl -- do not edit by hand.")
        head(io)
        println(io, "  \$p^*\$ (USD/tCO\$_2\$) & ", join(f1.(P_STAR[ti]), " & "), " \\\\")
        println(io, "  \$R_i\$ (GtCO\$_2\$) & ", join(f3.(R[ti]), " & "), " \\\\")
        println(io, "  \$E_i\$ (GtCO\$_2\$) & ", join(f3.(E[ti]), " & "), " \\\\")
        # GtCO2 x USD/tCO2 = billion USD
        println(io, "  Transfer (bn USD) & ", join(f1.((R[ti] .- E[ti]) .* P_STAR[ti]), " & "), " \\\\")
        println(io, "  \\bottomrule\n\\end{tabular}")
    end
end

if abspath(PROGRAM_FILE) == abspath(@__FILE__)
    if length(ARGS) >= 1 && ARGS[1] == "table"
        write_curves_and_table()
    else
        for e in EX1_COUNTRIES
            run_grid(e)
        end
        write_curves_and_table()
    end
end
