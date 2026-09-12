################################################################################
# Equivalent emission rights for the Wolfram and Duflo price proposals.
#
# For a proposal S (a schedule of differentiated national carbon prices, run in
# autarky: no cross-border revenue sharing), we look for the allocation of
# emission rights that, combined with a *uniform* world carbon price, leaves
# every country exactly as well off as under S.
#
# Country i's allocation is written
#       rights_i(t) = rho_i * pop_i(t) * Ebar_S(t),      Ebar_S(t) = E_S(t)/pop_world(t)
# so rho_i = 1 means "equal per capita share of the proposal's world emissions".
#
#   Option A  rho_i solved country by country, holding every other country at the
#             pro-rata (zero-transfer) split. Partial equilibrium; this is the
#             method already used in equivalence_price_rights.jl.
#   Option B  all rho_i solved jointly: one rights matrix, one model run, one
#             secant step per country per iteration. General equilibrium.
#
# Each option yields two variants:
#   variant 1 (unscaled)  the cap is whatever the equivalent rights add up to,
#                         C1(t) = sum_i rho_i pop_i(t) Ebar_S(t) <= E_S(t).
#                         Reported: gain in rights, world temperature in 2100.
#   variant 2 (rescaled)  rights scaled uniformly so they add up to E_S(t), so
#                         emissions (hence temperature) match the proposal and
#                         the comparison is pure allocative efficiency.
#                         Reported: world welfare gain.
#
# NOTE ON THE MODEL. revenue_recycle.jl computes
#       rights_actual[t,c] = E_gtco2_club[t] * rights_proposed[t,c] / sum(rights_proposed[t,:])
# i.e. only the *shares* of rights_proposed matter; the level is renormalised
# away, and world emissions are set by the carbon price, not by the rights.
# Scaling a rights matrix by a per-year scalar therefore changes nothing at all.
# For the rights to act as a cap -- which is what variants 1 and 2 require --
# the uniform price path must be recalibrated to deliver exactly the emissions
# the allocation adds up to. That is what calibrate_price_to_cap! does below.
################################################################################

const ROOT = normpath(joinpath(@__DIR__, ".."))

# data/parameters.jl reads its inputs through paths relative to the project root
# ("data/nice_inputs.json"), so the working directory has to be the root.
cd(ROOT)

using Pkg
Pkg.activate(ROOT)

using Mimi, MimiFAIRv2, DataFrames, CSV, CSVFiles, Statistics, Printf, Dates

include(joinpath(ROOT, "src", "nice2020_module.jl"))
include(joinpath(ROOT, "src", "helper_functions.jl"))
include(joinpath(ROOT, "data", "parameters.jl"))

# ──────────────────────────────────────────────────────────────────────────────
# CONFIGURATION
# ──────────────────────────────────────────────────────────────────────────────

const OUTPUT_BASE   = joinpath(ROOT, "cap_and_share", "output")
const YEARS_NPV     = 2030:2100          # welfare NPV window (= paper convention)
const DISCOUNT_RATE = 0.03
const ETA           = 1.5                # elasticity of marginal utility (welfare.jl default)

# Both proposals start pricing in 2025, so the uniform regime has to price from
# 2025 too: otherwise it would get five free years of unpriced emissions and the
# "same emissions as the proposal" of variant 2 would not hold. The repository's
# calibrated p* path starts in 2030, so it is extended back to 2025 at its own
# 2030->2031 growth rate (Hotelling-consistent, see P_STAR below).
const CALIB_YEARS = 2025:2100

# Neither report specifies how its price floors evolve after the initial period,
# so both are grown at a constant rate from 2031 on (per instructions).
const PRICE_GROWTH_RATE = 0.05
const PRICE_START_YEAR  = 2025
const PRICE_LEVEL_UNTIL = 2030           # flat at the announced floor through 2030

# Countries reported individually in the tables (EU27 aggregated as one bloc).
const REPORT_COUNTRIES = ["USA", "EU27", "CHN", "IND", "RUS", "TUR", "NGA", "COD", "BRA", "IDN"]

# Share of discounted world emissions whose rho is solved country by country in
# option A; the remaining small emitters take the closed-form prediction.
const A_COVERAGE = parse(Float64, get(ENV, "NICE_A_COVERAGE", "0.98"))

mkpath(OUTPUT_BASE)

# ──────────────────────────────────────────────────────────────────────────────
# SETUP
# ──────────────────────────────────────────────────────────────────────────────

const TEMPLATE    = MimiNICE2020.create_nice2020()
const YEARS       = collect(dim_keys(TEMPLATE, :time))
const COUNTRIES   = collect(dim_keys(TEMPLATE, :country))
const NB_STEPS    = length(YEARS)
const NB_COUNTRY  = length(COUNTRIES)
const NB_QUANTILE = length(dim_keys(TEMPLATE, :quantile))
const YEAR_IDX    = Dict(y => i for (i, y) in enumerate(YEARS))
const POLICY_SC   = MimiNICE2020.scenario_index[:All_World]

const EU27_SYMS = Symbol.(eu27_countries)
const EU27_IDX  = findall(c -> c in EU27_SYMS, COUNTRIES)

# income tiers (World Bank classification, from data/parameters.jl)
const LIC_S      = Symbol.(LIC)
const LMIC_S     = Symbol.(LMIC)
const UMIC_S     = Symbol.(UMIC)
const HIC_S      = Symbol.(HIC)
const LIC_LMIC_S = Symbol.(LIC_LMIC)

"Indices of the countries making up a reporting entity (\"EU27\" is the bloc)."
function entity_indices(name::AbstractString)
    name == "EU27" && return EU27_IDX
    return findall(==(Symbol(name)), COUNTRIES)
end

# ── reference uniform price p*(t): calibrated 1.8C cap-and-share path ─────────
const P_STAR = let p = zeros(Float64, NB_STEPS)
    df = CSV.read(joinpath(ROOT, "cap_and_share", "data", "output", "calibrated_global_cs.csv"), DataFrame)
    d = Dict(Int(r.time) => Float64(r.global_tax) for r in eachrow(df))
    first_y, last_y = minimum(keys(d)), maximum(keys(d))
    # back-extrapolate to PRICE_START_YEAR at the path's own initial growth rate
    g = d[first_y + 1] / d[first_y]
    for (i, y) in enumerate(YEARS)
        p[i] = y < PRICE_START_YEAR ? 0.0 :
               y < first_y          ? d[first_y] / g^(first_y - y) :
               y <= last_y          ? get(d, y, 0.0) : d[last_y]
    end
    p
end

# ──────────────────────────────────────────────────────────────────────────────
# MODEL RUNNERS
#
# A Mimi model costs ~17 s to build but ~9 s to run, so every solver below
# reuses a single instance and only touches the parameters that vary.
# ──────────────────────────────────────────────────────────────────────────────

"Model configured for the *uniform price + differentiated rights* regime."
function make_uniform_model(recycle_share)
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :switch_custom_transfers,                    1)
    update_param!(m, :switch_recycle,                             1)
    update_param!(m, :switch_global_recycling,                    1)
    update_param!(m, :revenue_recycle, :global_recycle_share,     ones(NB_COUNTRY))
    update_param!(m, :revenue_recycle, :switch_global_pc_recycle, 1)
    update_param!(m, :switch_footprint,                           1)
    update_param!(m, :switch_transfers_affect_growth,             1)
    update_param!(m, :abatement, :control_regime,                 1)   # global_carbon_tax
    update_param!(m, :quantile_recycle, :recycle_share,           recycle_share)
    update_param!(m, :policy_scenario,                            POLICY_SC)
    return m
end

"Model configured for a proposal: country-specific prices, revenue kept at home."
function make_autarky_model(recycle_share)
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :switch_custom_transfers,                    0)
    update_param!(m, :switch_recycle,                             1)
    update_param!(m, :switch_global_recycling,                    0)
    update_param!(m, :revenue_recycle, :global_recycle_share,     zeros(NB_COUNTRY))
    update_param!(m, :revenue_recycle, :switch_global_pc_recycle, 0)
    update_param!(m, :switch_footprint,                           1)
    update_param!(m, :switch_transfers_affect_growth,             1)
    update_param!(m, :abatement, :control_regime,                 4)   # direct_country_tax
    update_param!(m, :quantile_recycle, :recycle_share,           recycle_share)
    update_param!(m, :policy_scenario,                            POLICY_SC)
    return m
end

function run_uniform!(m, rights_mat, price_path)
    update_param!(m, :revenue_recycle, :rights_proposed, rights_mat)
    update_param!(m, :abatement, :global_carbon_tax,    price_path)
    run(m)
    return m
end

function run_autarky!(m, tax_mat)
    update_param!(m, :abatement, :direct_country_tax, tax_mat)
    run(m)
    return m
end

# ── fast extractors (matrix access, no DataFrame round-trip) ─────────────────
# Mimi hands variables back as Union{Missing,Number} arrays; coerce once here so
# every downstream routine can stay on concrete Float64.
f64(x::AbstractMatrix) = Float64[ismissing(v) ? 0.0 : Float64(v) for v in x]
f64(x::AbstractVector) = Float64[ismissing(v) ? 0.0 : Float64(v) for v in x]

country_emissions(m) = f64(m[:emissions, :E_gtco2])
population(m)        = f64(m[:grosseconomy, :l])
temperature(m)       = f64(m[:temperature, :T])
world_emissions(m)   = vec(sum(country_emissions(m), dims = 2))

const NPV_IDX = [YEAR_IDX[y] for y in YEARS_NPV]
const NPV_DISC = [1 / (1 + DISCOUNT_RATE)^(y - first(YEARS_NPV)) for y in YEARS_NPV]

npv(series::AbstractVector) = sum(series[NPV_IDX] .* NPV_DISC)

"""
    entity_welfare_npv(m, idx, pop)

NPV of equally-distributed-equivalent consumption for an entity made of the
country indices `idx`. A multi-country entity (the EU27) is aggregated with the
model's own population-weighted EDE, so the bloc figure is comparable to a
single country's.
"""
function entity_welfare_npv(m, idx::Vector{Int}, pop::Matrix{Float64})
    ede = f64(m[:welfare, :cons_EDE_country])
    if length(idx) == 1
        return npv(@view ede[:, idx[1]])
    end
    agg = [MimiNICE2020.EDE_aggregated(ede[t, idx], pop[t, idx], ETA) for t in 1:NB_STEPS]
    return npv(agg)
end

world_welfare_npv(m) = npv(f64(m[:welfare, :cons_EDE_global]))

# ──────────────────────────────────────────────────────────────────────────────
# PROPOSAL PRICE SCHEDULES
#
# Wolfram et al. 2025, "Building a Climate Coalition", Graduated Price scenario
#   (Table 2 p.10): HIC $75/t, UMIC $50/t, LMIC/LIC $25/t, applied by the 22
#   listed coalition members; non-members price nothing. The report covers only
#   steel, aluminium, cement and fertilisers; NICE2020 has no sectors, so the
#   floor is applied economy-wide -- this makes the modelled scenario markedly
#   more stringent than the report's, and is stated as such in the paper.
#
# Banerjee, Duflo & Greenstone 2025, "Getting serious about Loss and Damages"
#   (p.6 and p.14): economy-wide price of $10/t for low-income, $30/t for
#   lower-middle-income and $50/t for upper-middle-income countries, in exchange
#   for damage compensation. High-income countries fund the compensation and are
#   not required to price, so they get no tax here.
# ──────────────────────────────────────────────────────────────────────────────

# Table 2, p.10 of Wolfram et al. (2025): 22 members, the EU counting as one.
const WOLFRAM_MEMBERS = ["DZA", "AUS", "BRA", "CMR", "CAN", "CHN", "EGY", "EU27", "GHA",
                         "ISL", "IND", "IDN", "KEN", "LIE", "MOZ", "NOR", "CHE", "THA",
                         "TGO", "GBR", "UGA", "ZMB"]

const WOLFRAM_MEMBER_SYMS = let s = Symbol[]
    for c in WOLFRAM_MEMBERS
        c == "EU27" ? append!(s, EU27_SYMS) : push!(s, Symbol(c))
    end
    unique(s)
end

"""
    proposal_tax_matrix(rate_of)

Builds the [time, country] tax schedule: the announced floor from
`PRICE_START_YEAR` through `PRICE_LEVEL_UNTIL`, then `PRICE_GROWTH_RATE` a year.
`rate_of(country_symbol)` returns the announced floor in \$/tCO2.

The schedule is capped at the backstop price. This matters: the proposals are
fed to the model through `control_regime = 4`, which -- alone among the control
regimes -- neither caps the price at `pbacktime` nor clamps the abatement rate
to 1 (abatement.jl:95-99, against the `min(pbacktime, ...)` and `min(max(.,0),1)`
of regimes 1, 2 and 5). Left uncapped, a 5%/year schedule passes the backstop
around 2068 and the proposal scenario then books unbounded negative emissions,
while the uniform-price regime it is compared against is held to 100% abatement.
Capping here restores the comparison without touching the shared component.
"""
function proposal_tax_matrix(rate_of)
    tax = zeros(Float64, NB_STEPS, NB_COUNTRY)
    for (ci, c) in enumerate(COUNTRIES)
        r = rate_of(c)
        r == 0.0 && continue
        for (t, y) in enumerate(YEARS)
            y < PRICE_START_YEAR && continue
            announced = y <= PRICE_LEVEL_UNTIL ? r :
                        r * (1 + PRICE_GROWTH_RATE)^(y - PRICE_LEVEL_UNTIL)
            tax[t, ci] = min(announced, PBACKTIME[t])
        end
    end
    return tax
end

wolfram_rate(c) = !(c in WOLFRAM_MEMBER_SYMS) ? 0.0 :
                  c in LIC_LMIC_S ? 25.0 :
                  c in UMIC_S     ? 50.0 :
                  c in HIC_S      ? 75.0 : 0.0

duflo_rate(c) = c in LIC_S  ? 10.0 :
                c in LMIC_S ? 30.0 :
                c in UMIC_S ? 50.0 : 0.0   # HIC: no domestic price required

# ──────────────────────────────────────────────────────────────────────────────
# WITHIN-COUNTRY RECYCLING
#
# Revenue is returned to deciles in proportion to c^eta (inverse marginal
# utility), which leaves the within-country distribution untouched and isolates
# the inter-country efficiency channel. It must be applied to the *proposal*
# runs too, otherwise the welfare target and the uniform runs would differ by a
# within-country redistribution that has nothing to do with the question.
# ──────────────────────────────────────────────────────────────────────────────

function negishi_recycle_shares(m; ref_years = 2025:2035)
    conso_raw = m[:quantile_recycle, :conso_pc_post_damage_abatement]   # [time, country, quantile]
    conso = Float64[ismissing(v) ? NaN : Float64(v) for v in conso_raw]
    idx   = [YEAR_IDX[y] for y in ref_years if haskey(YEAR_IDX, y)]
    shares = zeros(Float64, NB_COUNTRY, NB_QUANTILE)
    for c in 1:NB_COUNTRY
        w = zeros(NB_QUANTILE)
        for q in 1:NB_QUANTILE
            vals = filter(!isnan, [conso[t, c, q] for t in idx])
            avg  = isempty(vals) ? 0.0 : mean(vals)
            w[q] = avg > 0 ? avg^ETA : 0.0
        end
        s = sum(w)
        shares[c, :] = s > 0 ? w ./ s : fill(1 / NB_QUANTILE, NB_QUANTILE)
    end
    return shares
end

# Derived once, from the plain equal-per-capita cap-and-share run at p*, and
# then used for every run in this script (proposals included) so that the
# welfare targets and the uniform runs differ only in what we are studying.
const REFERENCE_RUN = let
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :switch_custom_transfers,                    0)
    update_param!(m, :switch_recycle,                             1)
    update_param!(m, :switch_global_recycling,                    1)
    update_param!(m, :revenue_recycle, :global_recycle_share,     ones(NB_COUNTRY))
    update_param!(m, :revenue_recycle, :switch_global_pc_recycle, 1)
    update_param!(m, :switch_footprint,                           1)
    update_param!(m, :switch_transfers_affect_growth,             1)
    update_param!(m, :abatement, :control_regime,                 1)
    update_param!(m, :abatement, :global_carbon_tax,              P_STAR)
    update_param!(m, :policy_scenario,                            POLICY_SC)
    run(m)
    m
end

const RECYCLE_SHARE = negishi_recycle_shares(REFERENCE_RUN)

# Backstop price: the price at which abatement reaches 100%.
const PBACKTIME = f64(REFERENCE_RUN[:abatement, :pbacktime])

# ──────────────────────────────────────────────────────────────────────────────
# SCENARIO DATA
# ──────────────────────────────────────────────────────────────────────────────

struct Proposal
    name::String
    tax::Matrix{Float64}            # [time, country] announced price schedule
    pop::Matrix{Float64}            # [time, country] population (thousands)
    emissions::Matrix{Float64}      # [time, country] CO2 (GtCO2)
    world_emissions::Vector{Float64}
    world_pop::Vector{Float64}
    ebar::Vector{Float64}           # world emissions per capita (GtCO2 per thousand people)
    welfare::Dict{String,Float64}   # entity -> NPV welfare under the proposal
    world_welfare::Float64
    temp_2100::Float64
    p_ref::Vector{Float64}          # uniform price delivering this proposal's own emissions
end

"""
    build_proposal(name, tax)

Runs the proposal, then calibrates the uniform price `p_ref` that reproduces its
world emissions path under an equal-per-capita allocation.

`p_ref`, not the repository's 1.8C path `P_STAR`, is the price against which the
equivalent rights are solved. The equivalence of Appendix A holds *at a given
global cap*: it asks what allocation makes a country indifferent between facing
a differentiated price and facing the uniform price, with world emissions the
same either way. Solving against `P_STAR` instead compares the proposal with a
far more ambitious regime (1.8C against the proposals' 2.0-2.2C), so the
resulting rho absorbs the difference in ambition on top of the difference in
price structure -- which is what it is meant to isolate. Doing that turns the
efficiency dividend negative: every country can still be made indifferent, but
only by handing out 13% *more* rights than the proposal's own emissions, and
the world welfare gain collapses to zero.
"""
function build_proposal(name, tax)
    @info "Running proposal scenario" name
    m = run_autarky!(make_autarky_model(RECYCLE_SHARE), tax)
    pop = population(m)
    ems = country_emissions(m)
    we  = vec(sum(ems, dims = 2))
    wp  = vec(sum(pop, dims = 2))
    entities = unique(vcat(REPORT_COUNTRIES, [string(c) for c in COUNTRIES]))
    welf = Dict{String,Float64}()
    for e in entities
        idx = entity_indices(e)
        isempty(idx) && continue
        welf[e] = entity_welfare_npv(m, idx, pop)
    end

    # equal-per-capita rights, then find the price reproducing the proposal's cap
    equal_rights = similar(pop)
    @inbounds for t in 1:NB_STEPS, c in 1:NB_COUNTRY
        equal_rights[t, c] = pop[t, c] * (we[t] / wp[t])
    end
    guess = let p = zeros(Float64, NB_STEPS)
        for t in 1:NB_STEPS
            s = sum(@view ems[t, :])
            p[t] = s > 0 ? sum(tax[t, :] .* ems[t, :]) / s : 0.0
        end
        p
    end
    mu = make_uniform_model(RECYCLE_SHARE)
    p_ref, err = calibrate_price_to_cap(mu, equal_rights, we; p_init = guess,
                                        label = "p_ref/$name")
    @printf("  p_ref(%s): 2030 = %.1f, 2050 = %.1f, 2100 = %.1f \$/t (calib error %.4f%%)\n",
            name, p_ref[YEAR_IDX[2030]], p_ref[YEAR_IDX[2050]], p_ref[YEAR_IDX[2100]], err * 100)

    return Proposal(name, tax, pop, ems, we, wp, we ./ wp, welf,
                    world_welfare_npv(m), temperature(m)[YEAR_IDX[2100]], p_ref)
end

# ──────────────────────────────────────────────────────────────────────────────
# RIGHTS MATRICES
# ──────────────────────────────────────────────────────────────────────────────

"""
    rights_from_rho(rho, P)

`rights_i(t) = rho_i * pop_i(t) * Ebar_S(t)` for every model country. This is
variant 1 (unscaled): the world total is whatever the equivalent ratios add up
to, and is the cap the uniform price then has to deliver.
"""
function rights_from_rho(rho::Vector{Float64}, P::Proposal)
    r = similar(P.pop)
    @inbounds for t in 1:NB_STEPS, c in 1:NB_COUNTRY
        r[t, c] = rho[c] * P.pop[t, c] * P.ebar[t]
    end
    return r
end

"Variant 2: same allocation rescaled year by year to the proposal's emissions."
function rescale_to(rights::Matrix{Float64}, target::Vector{Float64})
    out = copy(rights)
    for t in 1:NB_STEPS
        s = sum(@view rights[t, :])
        out[t, :] .= s != 0 ? rights[t, :] .* (target[t] / s) : rights[t, :]
    end
    return out
end

"""
Rights used while solving country `i` alone (option A): `i` gets
`rho * pop_share_i * E_S`, everyone else is grandfathered on their own
proposal-scenario emissions, i.e. left with a zero net transfer.
"""
function rights_single_deviation(idx::Vector{Int}, rho::Float64, P::Proposal)
    r = zeros(Float64, NB_STEPS, NB_COUNTRY)
    others = setdiff(1:NB_COUNTRY, idx)
    for t in 1:NB_STEPS
        pop_i = sum(@view P.pop[t, idx])
        cap_t = P.world_emissions[t]
        rights_i = pop_i > 0 ? rho * (pop_i / P.world_pop[t]) * cap_t : 0.0
        if pop_i > 0
            for c in idx
                r[t, c] = P.pop[t, c] / pop_i * rights_i
            end
        end
        rem = cap_t >= 0 ? max(0.0, cap_t - rights_i) : cap_t - rights_i
        oth = sum(@view P.emissions[t, others])
        if oth > 0
            for c in others
                r[t, c] = P.emissions[t, c] / oth * rem
            end
        else
            for c in others
                r[t, c] = rem / length(others)
            end
        end
    end
    return r
end

# ──────────────────────────────────────────────────────────────────────────────
# PRICE CALIBRATION: find the uniform price path delivering a given cap
#
# One model run gives the whole emissions path, so we update every year at once:
# a multiplicative step on the first pass, then a per-year secant using the two
# most recent (price, emissions) pairs. ~8-12 runs to hit 0.2%.
# ──────────────────────────────────────────────────────────────────────────────

function calibrate_price_to_cap(m, rights_mat, cap::Vector{Float64};
                                p_init = P_STAR, tol = 2e-3, max_iter = 25, label = "")
    # Every calibrated year counts, including the late-century ones where a
    # proposal's cap turns negative (Duflo's does from 2093 on, its price having
    # passed the backstop): dropping them would leave exactly the years that
    # matter most for the 2100 temperature unmatched.
    idx = [YEAR_IDX[y] for y in CALIB_YEARS]
    # Gaps are scored against the largest cap in the window, so a near-zero cap
    # cannot make a rounding-size absolute miss look like a huge relative one.
    scale = max(maximum(abs, cap[idx]), 1e-6)
    p     = copy(p_init)
    pprev = similar(p); eprev = fill(NaN, NB_STEPS)
    local err = Inf
    for it in 1:max_iter
        run_uniform!(m, rights_mat, p)
        e = world_emissions(m)
        worst = idx[argmax([abs(e[t] - cap[t]) for t in idx])]
        err   = abs(e[worst] - cap[worst]) / scale
        @printf("    [calib %s] iter %2d  max gap = %.4f%% of peak cap  in %d (e=%.2f cap=%.2f p=%.1f)  p2050=%.1f\n",
                label, it, err * 100, YEARS[worst], e[worst], cap[worst], p[worst], p[YEAR_IDX[2050]])
        flush(stdout)
        err < tol && break
        it == max_iter && break
        pnew = copy(p)
        for t in idx
            if !isnan(eprev[t]) && abs(e[t] - eprev[t]) > 1e-12 && abs(p[t] - pprev[t]) > 1e-9
                slope = (e[t] - eprev[t]) / (p[t] - pprev[t])          # negative
                step  = -(e[t] - cap[t]) / slope
                step  = clamp(step, -0.5 * p[t], max(p[t], 1.0))       # damp wild steps
                pnew[t] = max(1e-3, p[t] + step)
            elseif cap[t] > 0 && e[t] > 0
                # emissions fall with price: raise p when emissions overshoot
                pnew[t] = max(1e-3, p[t] * (e[t] / cap[t])^1.5)
            else
                # cap at or below zero: no usable ratio, step in the right direction
                pnew[t] = e[t] > cap[t] ? max(1e-3, p[t] * 1.3 + 1.0) : max(1e-3, p[t] * 0.8)
            end
        end
        pprev .= p; eprev .= e
        p = pnew
    end
    err > tol && @warn "price calibration did not fully converge" label err
    return p, err
end

# ──────────────────────────────────────────────────────────────────────────────
# OPTION A — rho solved country by country (partial equilibrium)
#
# Illinois (modified regula falsi) on welfare(rho) - target, bracketed around
# the static prediction rho_hat = (per capita emissions of i) / (world average),
# which is the closed-form answer of the paper's Appendix A.
# ──────────────────────────────────────────────────────────────────────────────

"""
Static prediction for rho (paper, Appendix A): the entity's emissions over what
an equal-per-capita allocation of the same world total would hand it, both
discounted over the NPV window.
"""
function predicted_rho(idx::Vector{Int}, P::Proposal)
    own   = npv(vec(sum(P.emissions[:, idx], dims = 2)))
    equal = npv(vec(sum(P.pop[:, idx], dims = 2)) ./ P.world_pop .* P.world_emissions)
    return equal > 0 ? own / equal : 1.0
end

function solve_rho_single(m, entity::String, P::Proposal;
                          tol_rho = 1e-3, max_iter = 14, verbose = true)
    idx    = entity_indices(entity)
    isempty(idx) && return (rho = NaN, flag = :missing, evals = 0)
    target = P.welfare[entity]
    evals  = Ref(0)

    function f(x)
        evals[] += 1
        run_uniform!(m, rights_single_deviation(idx, x, P), P.p_ref)
        return entity_welfare_npv(m, idx, P.pop) - target
    end

    # bracket around the static prediction, widening until the sign flips
    # Bracket around the static prediction. rho is allowed to go negative: a
    # country that is better off under the uniform price even with no allocation
    # at all is one that would pay a net fee to join, and rights_proposed < 0 is
    # handled consistently by revenue_recycle.jl (it just flips the transfer).
    rhat = clamp(predicted_rho(idx, P), 0.05, 8.0)
    lo, hi = 0.0, max(2.0 * rhat, 1.0)
    flo, fhi = f(lo), f(hi)
    tries = 0
    while flo > 0 && tries < 4            # zero rights already beats the proposal
        hi, fhi = lo, flo
        lo = lo == 0.0 ? -1.0 : lo * 2.5
        flo = f(lo)
        tries += 1
    end
    flo > 0 && return (rho = lo, flag = :below_range, evals = evals[])
    tries = 0
    while fhi < 0 && tries < 5            # even a generous allocation falls short
        lo, flo = hi, fhi
        hi *= 2.5
        fhi = f(hi)
        tries += 1
    end
    fhi < 0 && return (rho = hi, flag = :above_range, evals = evals[])

    side = 0; mid = lo
    for _ in 1:max_iter
        mid = (fhi * lo - flo * hi) / (fhi - flo)
        abs(hi - lo) < tol_rho && break
        fmid = f(mid)
        if fmid * fhi > 0
            hi, fhi = mid, fmid
            side == -1 && (flo /= 2)
            side = -1
        else
            lo, flo = mid, fmid
            side == 1 && (fhi /= 2)
            side = 1
        end
    end
    verbose && @printf("    %-6s rho = %6.3f  (predicted %6.3f, %d runs)\n", entity, mid, rhat, evals[])
    return (rho = mid, flag = :converged, evals = evals[])
end

"""
    solve_A_order(P; coverage)

Countries to solve exactly, largest emitter first, until they cover `coverage`
of discounted world emissions; every member of a reporting entity is always
included. Each solve costs ~5 model runs, so solving all 179 would take hours
while the last hundred carry a per-mille weight in the world aggregates --- the
tail falls back to the closed-form prediction, which the paper shows tracks the
solved values closely.
"""
function solve_A_order(P::Proposal; coverage = 0.98)
    e_npv = [npv(@view P.emissions[:, c]) for c in 1:NB_COUNTRY]
    order = sortperm(e_npv, rev = true)
    total = sum(max.(e_npv, 0.0))
    must  = Set(vcat([entity_indices(e) for e in REPORT_COUNTRIES]...))
    exact = Int[]; acc = 0.0
    for c in order
        if acc < coverage * total || c in must
            push!(exact, c)
            acc += max(e_npv[c], 0.0)
        end
    end
    return exact, acc / total
end

"""
    solve_option_A(P; checkpoint, coverage)

Independent per-country solves (partial equilibrium). Returns a rho for every
model country: solved for the ones that matter, predicted for the tail.
Checkpoints after every country so a long run can be resumed or read early.
"""
function solve_option_A(P::Proposal; checkpoint = nothing, coverage = 0.98, resume = true)
    exact, cov = solve_A_order(P; coverage)
    @printf("  [A/%s] solving %d countries exactly (%.1f%% of discounted world emissions), %d predicted\n",
            P.name, length(exact), cov * 100, NB_COUNTRY - length(exact))

    rho   = Dict{String,Float64}()
    flags = Dict{String,Symbol}()
    if resume && checkpoint !== nothing && isfile(checkpoint)
        df = CSV.read(checkpoint, DataFrame)
        for r in eachrow(df)
            rho[String(r.country)] = Float64(r.rho); flags[String(r.country)] = Symbol(r.flag)
        end
        @info "resuming from checkpoint" n = length(rho)
    end

    m  = make_uniform_model(RECYCLE_SHARE)
    t0 = time(); done = 0
    todo = [c for c in exact if !haskey(rho, string(COUNTRIES[c]))]
    for (k, c) in enumerate(todo)
        e = string(COUNTRIES[c])
        res = solve_rho_single(m, e, P)
        rho[e] = res.rho; flags[e] = res.flag
        done += 1
        el = time() - t0
        @printf("  [A/%s] %d/%d exact, %.1f min elapsed, ~%.1f min left\n",
                P.name, k, length(todo), el / 60, el / 60 * (length(todo) - k) / k)
        flush(stdout)
        checkpoint !== nothing && write_rho_csv(checkpoint, rho, flags)
    end

    # tail: closed-form prediction
    for c in 1:NB_COUNTRY
        e = string(COUNTRIES[c])
        haskey(rho, e) && continue
        rho[e]   = clamp(predicted_rho([c], P), 0.0, 30.0)
        flags[e] = :predicted
    end
    checkpoint !== nothing && write_rho_csv(checkpoint, rho, flags)
    return rho, flags
end

function write_rho_csv(path, rho::Dict{String,Float64}, flags::Dict{String,Symbol})
    ks = sort(collect(keys(rho)))
    CSV.write(path, DataFrame(country = ks, rho = [rho[k] for k in ks],
                              flag = [string(flags[k]) for k in ks]))
end

chunk_path(name, k, n) = joinpath(OUTPUT_BASE, "rho_A_$(lowercase(name))_chunk$(k)of$(n).csv")

"Entities handled by chunk `k` of `n` (round-robin, so the chunks take
comparable time even though solve cost varies by country)."
chunk_entities(k, n) = [string(c) for (i, c) in enumerate(COUNTRIES) if mod1(i, n) == k]

"""
    run_chunk(scenario, k, n)

Worker entry point: solve option-A rho for chunk `k` of `n` and write it to
disk. Invoked as
    julia src/equivalent_rights_proposals.jl chunk <wolfram|duflo> <k> <n>
so the 179 country solves can be spread over several processes.
"""
function run_chunk(scenario::String, k::Int, n::Int)
    P = scenario == "wolfram" ? build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate)) :
        scenario == "duflo"   ? build_proposal("Duflo",   proposal_tax_matrix(duflo_rate)) :
        error("unknown scenario $scenario")
    ents = chunk_entities(k, n)
    @info "chunk start" scenario k n n_entities = length(ents)
    rho, flags = solve_option_A(P; entities = ents, checkpoint = chunk_path(P.name, k, n))
    write_rho_csv(chunk_path(P.name, k, n), rho, flags)
    @info "chunk done" scenario k n
end

"Collect the per-chunk CSVs written by `run_chunk` into one rho vector."
function gather_chunks(name::String, n::Int)
    rho = Dict{String,Float64}(); flags = Dict{String,Symbol}()
    for k in 1:n
        p = chunk_path(name, k, n)
        isfile(p) || error("missing chunk file $p — run the chunk jobs first")
        df = CSV.read(p, DataFrame)
        for r in eachrow(df)
            rho[String(r.country)] = Float64(r.rho)
            flags[String(r.country)] = Symbol(r.flag)
        end
    end
    missing_c = [string(c) for c in COUNTRIES if !haskey(rho, string(c))]
    isempty(missing_c) || error("chunks incomplete, missing $(length(missing_c)) countries: $(first(missing_c, 5))")
    return rho, flags
end

# ──────────────────────────────────────────────────────────────────────────────
# OPTION B — all rho solved jointly (general equilibrium)
#
# One rights matrix from the current vector, every country's welfare gap read
# off a single model run, then a damped secant step per country.
#
# The price is recalibrated to the cap the current rights imply, inside the
# loop. That is not an optimisation: it is what makes the problem well posed.
# At a FIXED price, revenue_recycle.jl renormalises rights_proposed to actual
# emissions, so multiplying the whole rho vector by any constant leaves every
# country's welfare untouched -- the welfare targets would pin the rho only up
# to scale, and the solver drifts along that flat direction until the whole
# vector collapses towards zero (which is exactly what it did: median rho of
# 0.002 and an absurd "99% gain in rights"). Letting the rights set the cap, and
# the cap set the price, removes the degeneracy: a uniform increase in rho
# loosens the cap, lowers the price and changes welfare.
# ──────────────────────────────────────────────────────────────────────────────

function solve_option_B(P::Proposal, rho_init::Vector{Float64};
                        max_iter = 60, tol = 5e-3, rho_min = -5.0, rho_max = 30.0,
                        fix_level = true, calib_tol = 5e-3)
    m = make_uniform_model(RECYCLE_SHARE)

    # population weights over the reporting window, used to split "level" from
    # "distribution" below
    wts = [sum(@view P.pop[CALIB_IDX, c]) for c in 1:NB_COUNTRY]
    wts ./= sum(wts)

    # L is the population-weighted mean of rho, i.e. the cap as a fraction of
    # the proposal's emissions; sigma is the distribution, normalised so that
    # its own population-weighted mean is 1.
    L     = max(sum(rho_init .* wts), 1e-3)
    sigma = rho_init ./ L

    target = [P.welfare[string(c)] for c in COUNTRIES]
    scale  = abs.(target); scale[scale .== 0] .= 1.0
    price  = copy(P.p_ref)

    L_prev, mbar_prev = NaN, NaN
    L_ok = L
    sigma_prev = copy(sigma); dev_prev = fill(NaN, NB_COUNTRY)
    best_rho, best_rel = L .* sigma, Inf

    for it in 1:max_iter
        rho    = clamp.(L .* sigma, rho_min, rho_max)
        rights = rights_from_rho(rho, P)
        cap    = vec(sum(rights, dims = 2))
        if any(cap[CALIB_IDX] .<= 0)
            @warn "option B: implied cap non-positive, backing off" P.name it
            L = 0.5 * (L + (isnan(L_prev) ? L * 1.5 : L_prev))
            continue
        end
        price, cerr = calibrate_price_to_cap(m, rights, cap; p_init = price,
                                             max_iter = (it == 1 ? 20 : 8),
                                             label = "B/$(P.name) it$it")
        if cerr > calib_tol
            # The uniform regime bottoms out at 100% abatement, so a cap below
            # what the backstop delivers simply cannot be hit; welfare then stops
            # responding and any level search walks off into meaningless caps.
            @warn "option B: cap not attainable, raising level" P.name it cerr
            L = min(1.5 * L, L_ok)
            continue
        end
        L_ok = max(L_ok, L)
        run_uniform!(m, rights, price)
        ede = f64(m[:welfare, :cons_EDE_country])
        rel = [(npv(@view ede[:, c]) - target[c]) / scale[c] for c in 1:NB_COUNTRY]

        mbar    = sum(rel .* wts)      # common component: moved by the cap level
        dev     = rel .- mbar          # distributional component: moved by the shares
        max_rel = maximum(abs, rel)
        @printf("  [B/%s] iter %2d  max gap = %.4f%%  mean = %+.4f%%  cap = %.1f%% of proposal\n",
                P.name, it, max_rel * 100, mbar * 100,
                sum(cap[CALIB_IDX]) / sum(P.world_emissions[CALIB_IDX]) * 100)
        flush(stdout)

        if max_rel < best_rel
            best_rel = max_rel; best_rho = copy(rho)
        end
        max_rel < tol && break

        # ── level: 1-D secant on the population-weighted mean gap ────────────
        # A looser cap means a lower price, less abatement and higher welfare,
        # so mbar rises with L: a usable slope is positive.
        L_new = if fix_level
            # The aggregate stringency is only weakly identified: over the
            # attainable range the population-weighted mean welfare gap moves by
            # less than 0.05pp while the cap moves by several points, so the
            # indifference conditions do not pin it down. We therefore hold the
            # level at option A's value and solve the distribution, reporting the
            # residual mean gap as the aggregate surplus of uniform pricing.
            L
        elseif it == 1
            L * 0.95                              # seed the secant
        elseif !isnan(mbar_prev) && abs(L - L_prev) > 1e-9 &&
               (mbar - mbar_prev) / (L - L_prev) > 1e-9
            L - mbar / ((mbar - mbar_prev) / (L - L_prev))
        else
            L * (1 - clamp(2 * mbar, -0.2, 0.2))
        end
        L_new = clamp(L_new, 0.2 * L, 1.8 * L)    # trust region on the level

        # ── shares: per-country secant on the distributional gap ─────────────
        sigma_new = copy(sigma)
        for c in 1:NB_COUNTRY
            s = if it == 1 || isnan(dev_prev[c]) || abs(sigma[c] - sigma_prev[c]) < 1e-9 ||
                   (dev[c] - dev_prev[c]) / (sigma[c] - sigma_prev[c]) <= 1e-10
                -sign(dev[c]) * 0.08 * max(abs(sigma[c]), 0.2)
            else
                -dev[c] / ((dev[c] - dev_prev[c]) / (sigma[c] - sigma_prev[c]))
            end
            lim = 0.30 * max(abs(sigma[c]), 0.2)
            sigma_new[c] = sigma[c] + clamp(s, -lim, lim)
        end
        # renormalise so the shares carry no level information
        mean_sigma = sum(sigma_new .* wts)
        abs(mean_sigma) > 1e-6 && (sigma_new ./= mean_sigma)

        sigma_prev .= sigma; dev_prev .= dev
        L_prev, mbar_prev = L, mbar
        sigma = sigma_new
        L     = L_new
    end
    @printf("  [B/%s] best max rel. welfare gap = %.4f%%\n", P.name, best_rel * 100)
    return best_rho, best_rel, price
end

# ──────────────────────────────────────────────────────────────────────────────
# VARIANTS
# ──────────────────────────────────────────────────────────────────────────────

struct VariantResult
    label::String
    total_rights::Float64      # NPV-window cumulated world rights (GtCO2)
    total_proposal::Float64    # same for the proposal's emissions
    rights_gain_pct::Float64   # 1 - rights/proposal, in %
    temp_2100::Float64
    world_welfare::Float64
    welfare_gain_pct::Float64
    price_path::Vector{Float64}
    calib_error::Float64
end

const CALIB_IDX = [YEAR_IDX[y] for y in CALIB_YEARS]

"Emission-weighted mean of the proposal's national prices — the opening guess
for the uniform price that would deliver comparable world emissions."
function proposal_price_guess(P::Proposal)
    p = zeros(Float64, NB_STEPS)
    for t in 1:NB_STEPS
        e = @view P.emissions[t, :]
        s = sum(e)
        p[t] = s > 0 ? sum(P.tax[t, :] .* e) / s : 0.0
    end
    return p
end

function run_variant(P::Proposal, rho::Vector{Float64}, variant::Int, label::String;
                     p_init = nothing, model = nothing)
    rights = rights_from_rho(rho, P)
    cap    = vec(sum(rights, dims = 2))
    if variant == 2
        rights = rescale_to(rights, P.world_emissions)
        cap    = copy(P.world_emissions)
    end
    m = model === nothing ? make_uniform_model(RECYCLE_SHARE) : model
    # Warm start: the proposal's own emission-weighted price is a far better
    # opening guess than p*, which was calibrated for a much tighter cap.
    p0 = p_init === nothing ? P.p_ref : p_init
    p, err = calibrate_price_to_cap(m, rights, cap; p_init = p0, label = label)
    run_uniform!(m, rights, p)

    tot_r = sum(cap[CALIB_IDX])
    tot_p = sum(P.world_emissions[CALIB_IDX])
    ww    = world_welfare_npv(m)
    return VariantResult(label, tot_r, tot_p, (1 - tot_r / tot_p) * 100,
                         temperature(m)[YEAR_IDX[2100]], ww,
                         (ww - P.world_welfare) / abs(P.world_welfare) * 100, p, err)
end

# ──────────────────────────────────────────────────────────────────────────────
# REPORTING
# ──────────────────────────────────────────────────────────────────────────────

"""
    entity_rho(rho, entity, P)

Aggregate rho for a reporting entity: the bloc's total rights divided by what an
equal-per-capita allocation would give it, i.e. the population-weighted mean of
its members' rho. For a single country this is just its own rho.
"""
function entity_rho(rho::Vector{Float64}, entity::String, P::Proposal)
    idx = entity_indices(entity)
    isempty(idx) && return NaN
    num = 0.0; den = 0.0
    for t in CALIB_IDX, c in idx
        w = P.pop[t, c] * P.ebar[t]
        num += rho[c] * w
        den += w
    end
    return den > 0 ? num / den : NaN
end

fmt(x; d = 3) = isnan(x) ? "--" : replace(string(round(x, digits = d)), "-" => "\$-\$")

function write_table(path::String, method::String, props, rhos, v1, v2)
    open(path, "w") do io
        println(io, "\\begin{table}[htbp]")
        println(io, "\\centering")
        println(io, "\\small")
        println(io, "\\caption{Uniform-price rights ratios \$\\rho_i\$ equivalent to the Wolfram and ",
                    "Duflo proposals --- ", method, "}")
        println(io, "\\renewcommand{\\arraystretch}{1.15}")
        println(io, "\\begin{tabular}{lcccc}")
        println(io, "  \\toprule")
        println(io, "  & \\multicolumn{2}{c}{\\textbf{Wolfram}} & \\multicolumn{2}{c}{\\textbf{Duflo}} \\\\")
        println(io, "  \\cmidrule(lr){2-3} \\cmidrule(lr){4-5}")
        println(io, "  \\textbf{Country} & \$\\Delta p_i\$ \\textbf{2030 (\\\$/t)} & \$\\rho_i\$ & ",
                    "\$\\Delta p_i\$ \\textbf{2030 (\\\$/t)} & \$\\rho_i\$ \\\\")
        println(io, "  \\midrule")
        for e in REPORT_COUNTRIES
            cells = String[]
            for (P, rho) in zip(props, rhos)
                idx = entity_indices(e)
                dp  = isempty(idx) ? NaN :
                      mean(P.tax[YEAR_IDX[2030], idx]) - P.p_ref[YEAR_IDX[2030]]
                push!(cells, fmt(dp; d = 1), fmt(entity_rho(rho, e, P)))
            end
            println(io, "  ", e, " & ", join(cells, " & "), " \\\\")
        end
        println(io, "  \\midrule")
        println(io, "  \\multicolumn{5}{l}{\\textit{Variant 1 --- unscaled rights (the allocation sets the cap)}} \\\\")
        @printf(io, "  \\quad Gain in rights (\\%% of proposal's) & \\multicolumn{2}{c}{%.1f\\%%} & \\multicolumn{2}{c}{%.1f\\%%} \\\\\n",
                v1[1].rights_gain_pct, v1[2].rights_gain_pct)
        @printf(io, "  \\quad World temperature 2100 (\$^\\circ\$C) & \\multicolumn{2}{c}{%.2f \$\\to\$ %.2f} & \\multicolumn{2}{c}{%.2f \$\\to\$ %.2f} \\\\\n",
                props[1].temp_2100, v1[1].temp_2100, props[2].temp_2100, v1[2].temp_2100)
        println(io, "  \\midrule")
        println(io, "  \\multicolumn{5}{l}{\\textit{Variant 2 --- rights rescaled to the proposal's emissions}} \\\\")
        @printf(io, "  \\quad World welfare gain (\\%%) & \\multicolumn{2}{c}{%.2f\\%%} & \\multicolumn{2}{c}{%.2f\\%%} \\\\\n",
                v2[1].welfare_gain_pct, v2[2].welfare_gain_pct)
        println(io, "  \\bottomrule")
        println(io, "\\end{tabular}")
        println(io, "\\label{tab:equivalent_rights_", lowercase(replace(method, " " => "_")), "}")
        println(io, "\\\\[4pt]")
        println(io, "{\\footnotesize Note: \$\\rho_i\$ is the country's allocation as a multiple of an ",
                    "equal-per-capita share of the proposal's world emissions; the EU27 figure aggregates ",
                    "its members' rights. \$\\Delta p_i\$ is the proposal's national price minus the uniform ",
                    "price in 2030 (negative: the uniform regime prices the country more heavily). ",
                    "\$\\rho_i < 0\$ means the country prefers the uniform regime even while paying a net ",
                    "fee for its allocation. Variant 1 lets the equivalent allocation set the cap, so the ",
                    "uniform price is recalibrated to deliver exactly those rights; variant 2 rescales the ",
                    "same allocation to the proposal's own emissions, so temperature is held fixed and only ",
                    "allocative efficiency remains. Both proposals are priced from 2025 and capped at the ",
                    "backstop price.}")
        println(io, "\\end{table}")
    end
    @info "wrote table" path
end

# ──────────────────────────────────────────────────────────────────────────────
# MAIN
# ──────────────────────────────────────────────────────────────────────────────

function main(; n_chunks::Int = 0)
    @info "Building proposal scenarios"
    wolfram = build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate))
    duflo   = build_proposal("Duflo",   proposal_tax_matrix(duflo_rate))
    props   = (wolfram, duflo)

    for P in props
        @printf("  %s: world emissions 2030 = %.2f GtCO2, T(2100) = %.3f C, welfare NPV = %.2f\n",
                P.name, P.world_emissions[YEAR_IDX[2030]], P.temp_2100, P.world_welfare)
    end

    results = Dict{String,Any}()

    # ── option A ──────────────────────────────────────────────────────────────
    @info "OPTION A: per-country solves"
    rho_A = Vector{Float64}[]
    for P in props
        d, flags = if n_chunks > 0
            gather_chunks(P.name, n_chunks)      # produced by the `chunk` jobs
        else
            solve_option_A(P; checkpoint = joinpath(OUTPUT_BASE,
                           "rho_A_$(lowercase(P.name))_checkpoint.csv"),
                           coverage = A_COVERAGE)
        end
        v = [get(d, string(c), 1.0) for c in COUNTRIES]
        push!(rho_A, v)
        write_rho_csv(joinpath(OUTPUT_BASE, "rho_A_$(lowercase(P.name)).csv"), d, flags)
    end
    v2_A = [run_variant(P, r, 2, "A2/$(P.name)") for (P, r) in zip(props, rho_A)]
    v1_A = [run_variant(P, r, 1, "A1/$(P.name)"; p_init = v.price_path)
            for (P, r, v) in zip(props, rho_A, v2_A)]
    write_table(joinpath(OUTPUT_BASE, "equivalent_rights_option_A.tex"),
                "option A (per-country solve)", props, rho_A, v1_A, v2_A)

    # ── option B ──────────────────────────────────────────────────────────────
    @info "OPTION B: simultaneous solve"
    rho_B = Vector{Float64}[]
    price_B = Vector{Float64}[]
    for (P, warm) in zip(props, rho_A)
        v, gaps, pB = solve_option_B(P, warm)
        push!(rho_B, v); push!(price_B, pB)
        CSV.write(joinpath(OUTPUT_BASE, "rho_B_$(lowercase(P.name)).csv"),
                  DataFrame(country = string.(COUNTRIES), rho = v,
                            max_rel_welfare_gap = fill(gaps, NB_COUNTRY)))
    end
    v1_B = [run_variant(P, r, 1, "B1/$(P.name)"; p_init = pB)
            for (P, r, pB) in zip(props, rho_B, price_B)]
    v2_B = [run_variant(P, r, 2, "B2/$(P.name)") for (P, r) in zip(props, rho_B)]
    write_table(joinpath(OUTPUT_BASE, "equivalent_rights_option_B.tex"),
                "option B (simultaneous solve)", props, rho_B, v1_B, v2_B)

    # ── machine-readable summary ──────────────────────────────────────────────
    summary = DataFrame(
        method   = repeat(["A", "B"], inner = 4),
        scenario = repeat(["Wolfram", "Wolfram", "Duflo", "Duflo"], outer = 2),
        variant  = repeat([1, 2], outer = 4),
        rights_gain_pct  = [v1_A[1].rights_gain_pct, v2_A[1].rights_gain_pct,
                            v1_A[2].rights_gain_pct, v2_A[2].rights_gain_pct,
                            v1_B[1].rights_gain_pct, v2_B[1].rights_gain_pct,
                            v1_B[2].rights_gain_pct, v2_B[2].rights_gain_pct],
        temp_2100        = [v1_A[1].temp_2100, v2_A[1].temp_2100, v1_A[2].temp_2100, v2_A[2].temp_2100,
                            v1_B[1].temp_2100, v2_B[1].temp_2100, v1_B[2].temp_2100, v2_B[2].temp_2100],
        temp_2100_proposal = repeat([wolfram.temp_2100, wolfram.temp_2100,
                                     duflo.temp_2100, duflo.temp_2100], outer = 2),
        welfare_gain_pct = [v1_A[1].welfare_gain_pct, v2_A[1].welfare_gain_pct,
                            v1_A[2].welfare_gain_pct, v2_A[2].welfare_gain_pct,
                            v1_B[1].welfare_gain_pct, v2_B[1].welfare_gain_pct,
                            v1_B[2].welfare_gain_pct, v2_B[2].welfare_gain_pct],
    )
    CSV.write(joinpath(OUTPUT_BASE, "equivalent_rights_variants.csv"), summary)
    println(summary)
    return (; props, rho_A, rho_B, v1_A, v2_A, v1_B, v2_B, summary)
end

if abspath(PROGRAM_FILE) == @__FILE__
    if length(ARGS) >= 1 && ARGS[1] == "chunk"
        run_chunk(ARGS[2], parse(Int, ARGS[3]), parse(Int, ARGS[4]))
    else
        main(; n_chunks = length(ARGS) >= 2 && ARGS[1] == "gather" ? parse(Int, ARGS[2]) : 0)
    end
end
