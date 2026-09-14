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
#             secant step per country per iteration. General equilibrium. It
#             runs on its own (starting from the closed-form prediction) or
#             warm-started from option A.
#
# THE CLUB. "Uniform price" is a misnomer, and the code no longer pretends
# otherwise: the comparison regime prices the countries the proposal itself
# prices -- one common price among them -- and leaves everyone else at zero,
# exactly as the proposal does. Members hold the rights and receive the
# transfers; non-members are outside both regimes and have no equivalent rho.
# Wolfram's club is its 22 signatories, the Duflo scenario's is the whole world,
# the legacy Duflo's is everything but the high-income countries. Everything
# below -- the cap, Ebar, the price calibration, rho = 1 -- is therefore on a
# club basis, not a world basis.
#
# Each option yields two variants, both of which set the club price so that the
# members' emissions match a cap:
#   variant 1 (unscaled)  the cap is whatever the equivalent rights add up to,
#                         C1(t) = sum_{i in club} rho_i pop_i(t) Ebar_S(t).
#                         Reported: gain in rights, world temperature in 2100.
#   variant 2 (rescaled)  rights scaled uniformly so they add up to the club's
#                         own emissions under the proposal, so club emissions
#                         match and the comparison is pure allocative
#                         efficiency. Reported: the world welfare gain on the
#                         inequality-averse global EDE *and* on world mean
#                         consumption per capita, which is the utilitarian
#                         efficiency question the Step 3 result speaks to.
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

using Distributed

using Mimi, MimiFAIRv2, DataFrames, CSV, CSVFiles, Statistics, Printf, Dates

include(joinpath(ROOT, "src", "nice2020_module.jl"))
include(joinpath(ROOT, "src", "helper_functions.jl"))
include(joinpath(ROOT, "data", "parameters.jl"))

# ── worker processes ────────────────────────────────────────────────────────
# The per-country solves of option A are embarrassingly parallel, so the script
# spawns its own workers instead of relying on the shell to start several Julia
# processes. Workers are handed this very file; the block is guarded on
# `myid() == 1`, so a worker including it does not spawn workers of its own, and
# the autorun block at the bottom does not fire on them either.
#
# Re-running in an *open* REPL re-sends the file to the workers, which is what
# you want after an edit -- but Julia 1.11 cannot redefine a struct, so restart
# the REPL between runs (or drive the file with Revise).
# @__FILE__ is empty when the code is pasted into the REPL rather than included,
# and the workers need a real path, so fall back to the canonical location.
const SELF = let f = @__FILE__
    isfile(f) ? abspath(f) : joinpath(ROOT, "src", "equivalent_rights_proposals.jl")
end
const NWORKERS = parse(Int, get(ENV, "NICE_WORKERS",
                                string(clamp(Sys.CPU_THREADS ÷ 2, 1, 4))))

if myid() == 1 && NWORKERS > 1
    nprocs() == 1 && addprocs(NWORKERS; exeflags = "--project=$(ROOT)")
    @info "worker processes" n = nworkers()
    @everywhere workers() include($SELF)
end


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

# NICE_FRESH=1 ignores everything a previous run left behind -- option-A
# checkpoints and chunk files, the p_ref cache, saved rho vectors and the
# variant results in the summary CSV -- and recomputes the lot from the model.
const FRESH = get(ENV, "NICE_FRESH", "0") in ("1", "true")
FRESH && @warn "NICE_FRESH: ignoring all stored results; everything is recomputed from scratch"

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

# ── the club ────────────────────────────────────────────────────────────────
# "Uniform price" is a misnomer: the price is uniform *among the countries the
# proposal prices* and zero everywhere else, exactly as in the proposal itself.
# One shared parameter delivers all of that. `club_country[scenario, country]`
# is read by three components:
#   abatement       regime 1 multiplies the price by the mask -> members pay the
#                   club price, non-members pay nothing (abatement.jl:50);
#   emissions       E_gtco2_club = sum of members' emissions (emissions.jl:39);
#   revenue_recycle rights are renormalised to E_gtco2_club over club members
#                   only, and a non-member's transfer is zero whatever its
#                   rights, because its carbon price is zero.
# Scenario row 6 ("personalized") is the one row no entry of `scenario_index`
# points to, so it is free to hold the proposal's membership.
const CLUB_SC = 6

"`club_country` matrix with row `CLUB_SC` set to this proposal's membership."
function club_mask(members::Vector{Int})
    mat = Float64.(Matrix(club_country))
    mat[CLUB_SC, :] .= 0.0
    mat[CLUB_SC, members] .= 1.0
    return mat
end

"""
Countries the proposal actually prices. These are the club: the ones that face
the uniform price in the comparison regime, receive rights, and for which an
equivalent rho is defined. Wolfram prices its 22 signatories, the Duflo scenario
prices every country, the legacy Duflo everything but the HICs.
"""
proposal_members(tax::Matrix{Float64}) = [c for c in 1:NB_COUNTRY if any(>(0.0), @view tax[:, c])]

"Model configured for the *club price + differentiated rights* regime."
function make_uniform_model(recycle_share, members::Vector{Int})
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
    update_param!(m, :club_country,                               club_mask(members))
    update_param!(m, :policy_scenario,                            CLUB_SC)
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

"Emissions of the club the model is currently configured with (GtCO2/year)."
club_emissions(m)    = f64(m[:emissions, :E_gtco2_club])

"Emissions of `members` in a run whose club setting may be something else."
club_emissions(ems::Matrix{Float64}, members::Vector{Int}) =
    vec(sum(@view(ems[:, members]), dims = 2))

"NPV of each country's mean consumption per capita (thousand USD2017/person)."
function country_mean_cons(m)
    tot = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    return [npv(@view tot[:, c]) for c in 1:NB_COUNTRY]
end

"""
    mean_consumption(m, pop)

Population-weighted world mean consumption per capita (thousand USD2017/person),
the utilitarian counterpart of `cons_EDE_global`: same consumption concept (post
damage, abatement and recycling), aggregated with no inequality aversion. The
EDE answers "is the world better off once distribution is priced in", this one
answers "is there more consumption in total" -- the pure efficiency question the
Step 3 dominance result speaks to.
"""
function mean_consumption(m, pop)
    tot = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    return vec(sum(tot .* pop, dims = 2)) ./ vec(sum(pop, dims = 2))
end

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
#   for damage compensation. The report sets no tier for high-income countries:
#   they fund the compensation, and are expected to price carbon at least as
#   heavily ("rich countries want them to implement high carbon taxes"), but no
#   number is given. Leaving them at zero would make the proposal a *cut* in
#   world pricing ambition rather than a differentiated-price schedule, so the
#   main Duflo scenario prices HICs at $75/t -- the HIC tier of Wolfram et al.,
#   so that the two proposals' high-income tiers are comparable. The original
#   reading, with HICs left unpriced, is kept as the legacy scenario
#   `duflo_legacy_rate` (scenario name "Duflo_legacy"); run it with
#   NICE_DUFLO_LEGACY=1 (or `main(; duflo_legacy = true)`).
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

# HIC tier assumed for Duflo (not in the report); matches Wolfram's HIC floor.
const DUFLO_HIC_PRICE = 75.0

duflo_rate(c) = c in LIC_S  ? 10.0 :
                c in LMIC_S ? 30.0 :
                c in UMIC_S ? 50.0 :
                c in HIC_S  ? DUFLO_HIC_PRICE : 0.0

# Legacy reading: high-income countries pay no domestic carbon price at all.
duflo_legacy_rate(c) = c in LIC_S  ? 10.0 :
                       c in LMIC_S ? 30.0 :
                       c in UMIC_S ? 50.0 : 0.0   # HIC: no domestic price

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
    members::Vector{Int}            # countries the proposal prices = the club
    pop::Matrix{Float64}            # [time, country] population (thousands)
    emissions::Matrix{Float64}      # [time, country] CO2 (GtCO2)
    world_emissions::Vector{Float64}
    club_emissions::Vector{Float64} # members' emissions under the proposal
    world_pop::Vector{Float64}
    club_pop::Vector{Float64}
    ebar::Vector{Float64}           # club emissions per club capita (GtCO2 per thousand people)
    welfare::Dict{String,Float64}   # entity -> NPV of EDE consumption under the proposal
    cons::Dict{String,Float64}      # country -> NPV of mean consumption per capita
    world_welfare::Float64          # NPV of global EDE consumption
    world_cons::Float64             # NPV of world mean consumption per capita
    temp_2100::Float64
    p_ref::Vector{Float64}          # club price delivering the proposal's own club emissions
end

"True when the proposal prices every country, so the club is the whole world."
is_global(P::Proposal) = length(P.members) == NB_COUNTRY

const CACHE_DIR = joinpath(OUTPUT_BASE, "cache")

"""
    cached_p_ref(f, name, tax, members, club_emissions)

`p_ref` is a property of the scenario, not of the solve, but it costs a secant
loop of 8-25 model runs. Cache it on disk, keyed on everything it depends on:
the announced schedule, the club, and the emissions path it has to reproduce.
Any change upstream changes the key, so a stale cache cannot be picked up
silently. Set NICE_NO_CACHE=1 to recompute regardless.
"""
function cached_p_ref(f, name, tax, members, ce)
    key  = string(hash((round.(tax, digits = 6), members, round.(ce, digits = 8),
                        collect(CALIB_YEARS))), base = 16)
    path = joinpath(CACHE_DIR, "p_ref_$(lowercase(name))_$key.csv")
    if !FRESH && get(ENV, "NICE_NO_CACHE", "0") == "0" && isfile(path)
        df = CSV.read(path, DataFrame)
        if nrow(df) == NB_STEPS
            @info "p_ref from cache" name file = basename(path)
            return Float64.(df.p_ref), 0.0
        end
    end
    p_ref, err = f()
    mkpath(CACHE_DIR)
    CSV.write(path, DataFrame(time = YEARS, p_ref = p_ref))
    return p_ref, err
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
    members = proposal_members(tax)
    m = run_autarky!(make_autarky_model(RECYCLE_SHARE), tax)
    pop = population(m)
    ems = country_emissions(m)
    we  = vec(sum(ems, dims = 2))
    wp  = vec(sum(pop, dims = 2))
    ce  = club_emissions(ems, members)
    cp  = vec(sum(@view(pop[:, members]), dims = 2))
    @printf("  %s: club of %d countries, %.1f%% of world emissions in 2030\n",
            name, length(members), ce[YEAR_IDX[2030]] / we[YEAR_IDX[2030]] * 100)
    entities = unique(vcat(REPORT_COUNTRIES, [string(c) for c in COUNTRIES]))
    welf = Dict{String,Float64}()
    for e in entities
        idx = entity_indices(e)
        isempty(idx) && continue
        welf[e] = entity_welfare_npv(m, idx, pop)
    end

    # equal-per-capita rights *within the club*, then find the club price that
    # reproduces the members' own emissions under the proposal
    equal_rights = zeros(Float64, NB_STEPS, NB_COUNTRY)
    @inbounds for t in 1:NB_STEPS, c in members
        equal_rights[t, c] = pop[t, c] * (ce[t] / cp[t])
    end
    guess = let p = zeros(Float64, NB_STEPS)
        for t in 1:NB_STEPS
            e = @view ems[t, members]
            s = sum(e)
            p[t] = s > 0 ? sum(@view(tax[t, members]) .* e) / s : 0.0
        end
        p
    end
    p_ref, err = cached_p_ref(name, tax, members, ce) do
        mu = make_uniform_model(RECYCLE_SHARE, members)
        calibrate_price_to_cap(mu, equal_rights, ce; p_init = guess, label = "p_ref/$name")
    end
    @printf("  p_ref(%s): 2030 = %.1f, 2050 = %.1f, 2100 = %.1f \$/t (calib error %.4f%%)\n",
            name, p_ref[YEAR_IDX[2030]], p_ref[YEAR_IDX[2050]], p_ref[YEAR_IDX[2100]], err * 100)

    cmc  = country_mean_cons(m)
    consd = Dict(string(COUNTRIES[c]) => cmc[c] for c in 1:NB_COUNTRY)

    return Proposal(name, tax, members, pop, ems, we, ce, wp, cp, ce ./ cp, welf, consd,
                    world_welfare_npv(m), npv(mean_consumption(m, pop)),
                    temperature(m)[YEAR_IDX[2100]], p_ref)
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
    r = zeros(Float64, NB_STEPS, NB_COUNTRY)
    @inbounds for t in 1:NB_STEPS, c in P.members
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
`rho * club_pop_share_i * E_S(club)`, every *other member* is grandfathered on
its own proposal-scenario emissions, i.e. left with a zero net transfer.
Non-members hold no rights: they are outside the club in both regimes.
"""
function rights_single_deviation(idx::Vector{Int}, rho::Float64, P::Proposal)
    r = zeros(Float64, NB_STEPS, NB_COUNTRY)
    others = setdiff(P.members, idx)
    for t in 1:NB_STEPS
        pop_i = sum(@view P.pop[t, idx])
        cap_t = P.club_emissions[t]
        rights_i = pop_i > 0 ? rho * (pop_i / P.club_pop[t]) * cap_t : 0.0
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
# PRICE CALIBRATION: find the club price path delivering a given cap
#
# The cap is the club's, not the world's: non-members are unpriced in both
# regimes, so nothing the solver does moves their emissions, and including them
# in the target would make it unreachable.
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
    # A cap of zero can only be delivered by the backstop price (at p = pbacktime
    # the abatement rate is 1 and emissions are exactly 0). Start those years
    # there instead of letting the fallback branch crawl up 30% at a time: the
    # late-century years of a 5%/year schedule are all of this kind, and ramping
    # from ~1 $/t to a backstop in the hundreds costs ~20 model runs.
    for t in idx
        cap[t] <= 1e-4 * scale && (p[t] = max(p[t], PBACKTIME[t]))
    end
    pprev = similar(p); eprev = fill(NaN, NB_STEPS)
    local err = Inf
    for it in 1:max_iter
        run_uniform!(m, rights_mat, p)
        # E_gtco2_club is the emissions of the club `m` is configured with, i.e.
        # of the countries the club price actually reaches
        e = club_emissions(m)
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
                # cap at or below zero: no usable ratio. Emissions too high can
                # only be fixed by the backstop, which is also the most the model
                # will apply (regime 1 caps the price at pbacktime), so go there
                # directly rather than creeping towards it.
                pnew[t] = e[t] > cap[t] ? max(p[t], PBACKTIME[t]) : max(1e-3, p[t] * 0.8)
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
an equal-per-capita allocation of the same *club* total would hand it, both
discounted over the NPV window.
"""
function predicted_rho(idx::Vector{Int}, P::Proposal)
    own   = npv(vec(sum(P.emissions[:, idx], dims = 2)))
    equal = npv(vec(sum(P.pop[:, idx], dims = 2)) ./ P.club_pop .* P.club_emissions)
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

# ── worker-side state ───────────────────────────────────────────────────────
# A worker keeps the proposal it is solving and one uniform model per club, so
# a country solve costs a bare remote call: no Proposal serialised per task, no
# model rebuilt per country.
const WORKER_P     = Ref{Any}(nothing)
const WORKER_MODEL = Ref{Any}(nothing)
const WORKER_CLUB  = Ref{Vector{Int}}(Int[])

set_worker_proposal!(P) = (WORKER_P[] = P; nothing)

function worker_uniform_model(members::Vector{Int})
    if WORKER_MODEL[] === nothing || WORKER_CLUB[] != members
        WORKER_MODEL[] = make_uniform_model(RECYCLE_SHARE, members)
        WORKER_CLUB[]  = copy(members)
    end
    return WORKER_MODEL[]
end

"Solve one country on whichever process this runs on. Silent: the master prints."
function solve_one_country(c::Int)
    P = WORKER_P[]
    P === nothing && error("no proposal set on process $(myid())")
    res = solve_rho_single(worker_uniform_model(P.members), string(COUNTRIES[c]), P;
                           verbose = false)
    return (country = string(COUNTRIES[c]), rho = res.rho, flag = res.flag, evals = res.evals)
end

"""
    solve_A_order(P; coverage)

Club members to solve exactly, largest emitter first, until they cover
`coverage` of the club's discounted emissions; every member of a reporting
entity is always included. Non-members hold no rights, so they have no rho. Each solve costs ~5 model runs, so solving all 179 would take hours
while the last hundred carry a per-mille weight in the world aggregates --- the
tail falls back to the closed-form prediction, which the paper shows tracks the
solved values closely.
"""
function solve_A_order(P::Proposal; coverage = 0.98)
    e_npv = Dict(c => npv(@view P.emissions[:, c]) for c in P.members)
    order = sort(P.members, by = c -> -e_npv[c])
    total = sum(max(e_npv[c], 0.0) for c in P.members)
    # reporting entities are always solved, but only where they are members
    must  = Set(intersect(vcat([entity_indices(e) for e in REPORT_COUNTRIES]...), P.members))
    exact = Int[]; acc = 0.0
    for c in order
        if acc < coverage * total || c in must
            push!(exact, c)
            acc += max(e_npv[c], 0.0)
        end
    end
    return exact, total > 0 ? acc / total : 1.0
end

"""
    solve_option_A(P; checkpoint, coverage)

Independent per-country solves (partial equilibrium). Returns a rho for every
model country: solved for the ones that matter, predicted for the tail.
Checkpoints after every country so a long run can be resumed or read early.
"""
function solve_option_A(P::Proposal; checkpoint = nothing, coverage = A_COVERAGE,
                        resume = !FRESH, entities = nothing, seed_chunks = true)
    exact, cov = solve_A_order(P; coverage)
    # `entities` restricts the solve to one chunk's countries (see run_chunk), so
    # that several machines can split the club between them; the resulting chunk
    # files are picked up again by `seed_solved!`.
    mine = entities === nothing ? nothing : Set(String.(entities))
    mine === nothing || (exact = [c for c in exact if string(COUNTRIES[c]) in mine])
    @printf("  [A/%s] solving %d of %d club members exactly (%.1f%% of the club's discounted emissions), %d predicted\n",
            P.name, length(exact), length(P.members), cov * 100,
            (mine === nothing ? length(P.members) : length(mine)) - length(exact))

    rho   = Dict{String,Float64}()
    flags = Dict{String,Symbol}()
    resume && seed_solved!(rho, flags, P, checkpoint; chunks = seed_chunks)

    todo = [c for c in exact if !haskey(rho, string(COUNTRIES[c]))]
    if isempty(todo)
        @info "option A: everything already solved, nothing to do" P.name n = length(rho)
    else
        # one dispatcher, whether or not there are worker processes
        solve = if nworkers() > 1
            for w in workers()
                remotecall_wait(set_worker_proposal!, w, P)
            end
            pool = WorkerPool(collect(workers()))
            c -> remotecall_fetch(solve_one_country, pool, c)
        else
            set_worker_proposal!(P)
            c -> solve_one_country(c)
        end

        lk = ReentrantLock(); t0 = time(); done = 0
        asyncmap(todo; ntasks = max(1, nworkers())) do c
            res = solve(c)
            lock(lk) do
                rho[res.country] = res.rho; flags[res.country] = res.flag
                done += 1
                el = time() - t0
                @printf("    %-6s rho = %7.3f  (predicted %6.3f, %d runs)   [A/%s %d/%d, %.1f min elapsed, ~%.1f min left]\n",
                        res.country, res.rho, predicted_rho([c], P), res.evals,
                        P.name, done, length(todo), el / 60, el / 60 * (length(todo) - done) / done)
                flush(stdout)
                checkpoint !== nothing && write_rho_csv(checkpoint, rho, flags)
            end
        end
    end

    # tail: closed-form prediction (club members only)
    for c in P.members
        e = string(COUNTRIES[c])
        mine === nothing || e in mine || continue
        haskey(rho, e) && continue
        rho[e]   = clamp(predicted_rho([c], P), 0.0, 30.0)
        flags[e] = :predicted
    end
    checkpoint !== nothing && write_rho_csv(checkpoint, rho, flags)
    return rho, flags
end

"""
    seed_solved!(rho, flags, P, checkpoint)

Pick up work already done: the checkpoint of an interrupted run first, then any
`chunk` files lying around for this scenario. Only `:converged` and the bound
flags are kept -- a `:predicted` entry is not work, and re-deriving it is free.
"""
function seed_solved!(rho, flags, P::Proposal, checkpoint; chunks = true)
    files = String[]
    checkpoint !== nothing && isfile(checkpoint) && push!(files, checkpoint)
    chunks && append!(files,
                      filter(f -> occursin(Regex("^rho_A_$(lowercase(P.name))_chunk\\d+of\\d+\\.csv\$"),
                                           basename(f)),
                             readdir(OUTPUT_BASE; join = true)))
    for f in files, r in eachrow(CSV.read(f, DataFrame))
        flag = Symbol(r.flag)
        flag === :predicted && continue
        rho[String(r.country)]   = Float64(r.rho)
        flags[String(r.country)] = flag
    end
    isempty(rho) || @info "option A: resuming" P.name solved = length(rho) from = basename.(files)
    return rho
end

function write_rho_csv(path, rho::Dict{String,Float64}, flags::Dict{String,Symbol})
    ks = sort(collect(keys(rho)))
    CSV.write(path, DataFrame(country = ks, rho = [rho[k] for k in ks],
                              flag = [string(flags[k]) for k in ks]))
end

chunk_path(name, k, n) = joinpath(OUTPUT_BASE, "rho_A_$(lowercase(name))_chunk$(k)of$(n).csv")

"Entities handled by chunk `k` of `n` (round-robin, so the chunks take
comparable time even though solve cost varies by country)."
chunk_entities(P::Proposal, k, n) = [string(COUNTRIES[c]) for (i, c) in enumerate(P.members) if mod1(i, n) == k]

"""
    run_chunk(scenario, k, n)

Worker entry point: solve option-A rho for chunk `k` of `n` and write it to
disk. Invoked as
    julia src/equivalent_rights_proposals.jl chunk <wolfram|duflo|duflo_legacy> <k> <n>
so the 179 country solves can be spread over several processes.
"""
function run_chunk(scenario::String, k::Int, n::Int)
    P = scenario == "wolfram"      ? build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate)) :
        scenario == "duflo"        ? build_proposal("Duflo",   proposal_tax_matrix(duflo_rate)) :
        scenario == "duflo_legacy" ? build_proposal("Duflo_legacy", proposal_tax_matrix(duflo_legacy_rate)) :
        error("unknown scenario $scenario")
    ents = chunk_entities(P, k, n)
    @info "chunk start" scenario k n n_entities = length(ents)
    rho, flags = solve_option_A(P; entities = ents, seed_chunks = false,
                                checkpoint = chunk_path(P.name, k, n))
    write_rho_csv(chunk_path(P.name, k, n), rho, flags)
    @info "chunk done" scenario k n
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

"""
    solve_option_B(P[, rho_init]; ...)

Joint solve over the club members. `rho_init` defaults to the closed-form
prediction of Appendix A, so option B runs on its own without option A having
been solved first; pass option A's vector when it is available, which starts the
secant closer and costs a few iterations less.

`tol` is the largest relative welfare gap accepted for any member (1e-4 = 0.01%).
The inner price calibration has to be at least as tight, otherwise the cap error
moves welfare by more than the gap being solved for.
"""
function solve_option_B(P::Proposal; kwargs...)
    rho0 = zeros(Float64, NB_COUNTRY)
    for c in P.members
        rho0[c] = clamp(predicted_rho([c], P), 0.05, 8.0)
    end
    @info "option B from the closed-form prediction (no option A needed)" P.name
    return solve_option_B(P, rho0; kwargs...)
end

function solve_option_B(P::Proposal, rho_init::Vector{Float64};
                        max_iter = 400, tol = 2e-5, rho_min = -5.0, rho_max = 30.0,
                        fix_level = true, calib_tol = 1e-4, calib_reject = 5e-3,
                        tol_floor = 5e-6)
    m = make_uniform_model(RECYCLE_SHARE, P.members)

    # Everything below is indexed by country but lives on the club: non-members
    # hold no rights, face no price and have no welfare target, so they carry
    # zero weight and are never stepped.
    mem = P.members
    wts = zeros(Float64, NB_COUNTRY)
    for c in mem
        wts[c] = sum(@view P.pop[CALIB_IDX, c])
    end
    wts ./= sum(wts)

    # L is the population-weighted mean of rho over the club, i.e. the cap as a
    # fraction of the club's proposal emissions; sigma is the distribution,
    # normalised so that its own population-weighted mean is 1.
    L     = max(sum(rho_init .* wts), 1e-3)
    sigma = rho_init ./ L

    target = [P.welfare[string(c)] for c in COUNTRIES]
    scale  = abs.(target); scale[scale .== 0] .= 1.0
    price  = copy(P.p_ref)

    L_prev, mbar_prev = NaN, NaN
    L_ok = L
    sigma_prev = copy(sigma); dev_prev = fill(NaN, NB_COUNTRY)
    best_rho, best_rel, best_mbar = L .* sigma, Inf, NaN
    # Adaptive trust region. A fixed step limit makes the secant overshoot once
    # the gaps are small, and the solve then oscillates around a floor instead
    # of converging: shrink the limit on every iteration that fails to improve
    # on the best, and relax it again when one succeeds.
    step_limit = 0.30

    for it in 1:max_iter
        rho = zeros(Float64, NB_COUNTRY)
        for c in mem
            rho[c] = clamp(L * sigma[c], rho_min, rho_max)
        end
        rights = rights_from_rho(rho, P)
        cap    = vec(sum(rights, dims = 2))
        # A zero cap is normal in the years where the proposal itself drives the
        # club's emissions to zero (Wolfram's do from 2091 on): equal-per-capita
        # rights are zero there too, and the calibrator delivers them with the
        # backstop price. Only a non-positive cap in a year where the club still
        # emits something means the level has collapsed. Testing `cap .<= 0`
        # over the whole window instead rejected every iteration for ever, since
        # no rescaling of the level can turn a zero into a positive number.
        material = maximum(P.club_emissions[CALIB_IDX]) * 1e-3
        if any(t -> cap[t] <= 0 && P.club_emissions[t] > material, CALIB_IDX)
            @warn "option B: implied cap non-positive while the club still emits, backing off" P.name it
            L = 0.5 * (L + (isnan(L_prev) ? L * 1.5 : L_prev))
            continue
        end
        price, cerr = calibrate_price_to_cap(m, rights, cap; p_init = price,
                                             tol = calib_tol,
                                             max_iter = (it == 1 ? 30 : 12),
                                             label = "B/$(P.name) it$it")
        # `calib_reject` is an absolute floor, NOT a multiple of calib_tol: the
        # calibrator cannot always reach its target in the late-century years
        # (where the cap is near zero and only the backstop delivers it), and
        # tying the two together makes every iteration fail this test, so the
        # loop spins without ever scoring a vector -- the solve then returns its
        # own starting point with a gap of Inf.
        if cerr > calib_reject
            # The club regime bottoms out at 100% abatement, so a cap below what
            # the backstop delivers simply cannot be hit; welfare then stops
            # responding and any level search walks off into meaningless caps.
            @warn "option B: cap not attainable, raising level" P.name it cerr
            L = min(1.5 * L, L_ok)
            continue
        end
        L_ok = max(L_ok, L)
        run_uniform!(m, rights, price)
        ede = f64(m[:welfare, :cons_EDE_country])
        rel = zeros(Float64, NB_COUNTRY)
        for c in mem
            rel[c] = (npv(@view ede[:, c]) - target[c]) / scale[c]
        end

        mbar    = sum(rel .* wts)      # common component: moved by the cap level
        dev     = rel .- mbar          # distributional component: moved by the shares
        max_rel = maximum(abs, @view rel[mem])
        # With the level held fixed, the population-weighted mean gap `mbar` is
        # whatever that level implies and no rho can remove it; only the spread
        # around it is B's to solve. Convergence is therefore tested on the
        # distributional residual -- testing max|rel| instead puts the target
        # below |mbar| and the solver can never stop.
        @printf("  [B/%s] iter %3d  max gap = %.4f%%  mean = %+.4f%%  spread = %.4f%%  step = %.3f  cap = %.1f%% of proposal (club)\n",
                P.name, it, max_rel * 100, mbar * 100, maximum(abs, @view dev[mem]) * 100, step_limit,
                sum(cap[CALIB_IDX]) / sum(P.club_emissions[CALIB_IDX]) * 100)
        flush(stdout)

        isnan(max_rel) && error("option B: welfare gap is NaN for $(P.name) at iteration $it; " *
                                "the model returned non-finite consumption for some member")
        max_dev = maximum(abs, @view dev[mem])
        # Convergence target. `tol` is the absolute one (set below the welfare
        # differences the tables are meant to resolve); |mbar|/2 tightens it
        # whenever the common gain is itself small, since a spread has to be
        # read against the gain it disperses; `tol_floor` stops the criterion
        # chasing zero when mbar happens to pass through it.
        crit = max(tol_floor, min(tol, abs(mbar) / 2))
        if max_dev < best_rel
            best_rel = max_dev; best_rho = copy(rho); best_mbar = mbar
            step_limit = min(0.30, step_limit * 1.3)
        else
            step_limit = max(0.01, step_limit * 0.6)
        end
        max_dev < crit && break

        # ── level: 1-D secant on the population-weighted mean gap ────────────
        # A looser cap means a lower price, less abatement and higher welfare,
        # so mbar rises with L: a usable slope is positive.
        L_new = if fix_level
            # The aggregate stringency is only weakly identified: over the
            # attainable range the population-weighted mean welfare gap moves by
            # less than 0.05pp while the cap moves by several points, so the
            # indifference conditions do not pin it down. We therefore hold the
            # level at its starting value and solve the distribution, reporting
            # the residual mean gap as the aggregate surplus of club pricing.
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
        for c in mem
            s = if it == 1 || isnan(dev_prev[c]) || abs(sigma[c] - sigma_prev[c]) < 1e-9 ||
                   (dev[c] - dev_prev[c]) / (sigma[c] - sigma_prev[c]) <= 1e-10
                -sign(dev[c]) * 0.08 * max(abs(sigma[c]), 0.2)
            else
                -dev[c] / ((dev[c] - dev_prev[c]) / (sigma[c] - sigma_prev[c]))
            end
            lim = step_limit * max(abs(sigma[c]), 0.2)
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
    isfinite(best_rel) ||
        error("option B produced no usable iteration for $(P.name): every one was rejected before " *
              "its welfare gaps could be scored (see the `non-positive` / `cap not attainable` warnings " *
              "above). The returned vector would be the starting point, not a solve.")
    @printf("  [B/%s] best distributional gap = %.5f%% (target %.5f%%); residual mean gap = %+.4f%%, the aggregate surplus of club pricing at this level\n",
            P.name, best_rel * 100, max(tol_floor, min(tol, abs(best_mbar) / 2)) * 100, best_mbar * 100)
    best_rel > max(tol_floor, min(tol, abs(best_mbar) / 2)) &&
        @warn "option B did not reach its tolerance" P.name best_rel mbar = best_mbar
    return best_rho, best_rel, price
end

# ──────────────────────────────────────────────────────────────────────────────
# VARIANTS
# ──────────────────────────────────────────────────────────────────────────────

struct VariantResult
    label::String
    total_rights::Float64      # NPV-window cumulated club rights (GtCO2)
    total_proposal::Float64    # same for the club's emissions under the proposal
    rights_gain_pct::Float64   # 1 - rights/proposal, in %
    temp_2100::Float64
    world_welfare::Float64     # NPV of global EDE consumption (eta = 1.5)
    welfare_gain_pct::Float64
    world_cons::Float64        # NPV of world mean consumption per capita
    cons_gain_pct::Float64     # the same comparison without inequality aversion
    ede_gain::Vector{Float64}  # per country, % change in EDE consumption vs the proposal
    cons_gain::Vector{Float64} # per country, % change in mean consumption vs the proposal
    rho_eff::Vector{Float64}   # per country, the ratio this variant actually allocates
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

"""
    marginal_utility_weights(P)

u'(c_i) = c_i^{-eta} for each club member, with c_i the NPV of the country's
equally-distributed-equivalent consumption under the proposal. Used to hand a
surplus of rights to the countries where it raises welfare most.
"""
function marginal_utility_weights(P::Proposal)
    w = zeros(Float64, NB_COUNTRY)
    for c in P.members
        ci = P.welfare[string(COUNTRIES[c])]
        w[c] = ci > 0 ? ci^(-ETA) : 0.0
    end
    return w
end

"""
    variant_rights(P, rho, variant; floor_rho)

Four readings of one equivalent allocation.

* **variant 1** -- the allocation at face value. Every member is exactly
  indifferent to the proposal by construction of rho, so the dividend shows up
  as a cap below the proposal's own emissions rather than as welfare.

Variants 2 to 4 hand that dividend back as rights, so that club emissions match
the proposal's and temperature is held fixed. They differ in who receives it.

* **variant 2** -- the surplus allocated so as to minimise the worst relative
  loss against the proposal (see `minimax_loss_rights`).
* **variant 3** -- the surplus distributed in proportion to population times
  marginal utility, i.e. to the members where a tonne of rights buys the most
  welfare.
* **variant 4** -- one common scaling factor, so the surplus goes to whoever
  already holds the most rights. This is regressive, and it can leave a member
  worse off than the proposal it is meant to match: the looser cap lowers the
  club price, and a member holding more rights than its emissions is a net
  seller whose revenue falls with that price.
"""
function variant_rights(P::Proposal, rho::Vector{Float64}, variant::Int; floor_rho = nothing)
    rights = rights_from_rho(rho, P)
    variant == 1 && return rights, vec(sum(rights, dims = 2))
    variant == 4 && return rescale_to(rights, P.club_emissions), copy(P.club_emissions)

    variant == 2 && error("variant 2 is solved by minimax_loss_rights, not here")

    # variant 3: surplus over the equivalent allocation, shared by marginal utility
    mu  = marginal_utility_weights(P)
    out = copy(rights)
    for t in 1:NB_STEPS
        surplus = P.club_emissions[t] - sum(@view rights[t, :])
        w  = [P.pop[t, c] * mu[c] for c in 1:NB_COUNTRY]
        sw = sum(w)
        if surplus <= 0 || sw <= 0
            # nothing to share (or rights already above the proposal's emissions):
            # fall back to uniform rescaling, since sharing a deduction by
            # marginal utility would take most of it from the poorest members
            tot = sum(@view rights[t, :])
            tot != 0 && (out[t, :] .= rights[t, :] .* (P.club_emissions[t] / tot))
        else
            out[t, :] .= rights[t, :] .+ surplus .* w ./ sw
        end
    end
    return out, copy(P.club_emissions)
end

"""
    minimax_loss_rights(P, rho; ...)

Variant 2. The total is the proposal's own club emissions, exactly as in
variants 3 and 4, so ambition is held constant; what is solved for is *who*
holds the rights.

The search is over the whole allocation, not just over the surplus above the
equivalent one. That matters: a no-loss allocation at this total is known to
exist -- grandfathering every member on its own emissions under the proposal
leaves each of them able to reproduce its autarky position and sell the rest at
the club price, which is the dominance result of Section 3 -- but it hands some
members *less* than the equivalent allocation does, so a search that can only
add to that allocation cannot find it. Both starting points are therefore tried,
the equivalent ratios and the grandfathering ones, and the better is kept.

Shares are a per-country multiplier `m` on an equal-per-capita share,
renormalised each year so the cap is met exactly; `m` is therefore a rho. They
are moved towards the worst-off members, maximising the smallest relative gain
against the proposal. The objective does not stop at zero: once no member loses
it keeps raising the floor, and the search stops when that floor has converged
with no member below it.
"""
function minimax_loss_rights(P::Proposal, rho::Vector{Float64};
                             max_iter = 60, tol = 1e-5, label = "", p_init = nothing,
                             model = nothing, calib_tol = 1e-4)
    mem    = P.members
    m      = model === nothing ? make_uniform_model(RECYCLE_SHARE, P.members) : model
    target = [P.welfare[string(c)] for c in COUNTRIES]
    scale  = abs.(target); scale[scale .== 0] .= 1.0

    # rights from a multiplier vector, renormalised each year onto the cap
    function build(mult)
        r = zeros(Float64, NB_STEPS, NB_COUNTRY)
        @inbounds for t in 1:NB_STEPS
            tot = sum(mult[c] * P.pop[t, c] * P.ebar[t] for c in mem)
            tot <= 0 && continue
            k = P.club_emissions[t] / tot
            for c in mem
                r[t, c] = mult[c] * P.pop[t, c] * P.ebar[t] * k
            end
        end
        r
    end

    # start 1: the equivalent ratios. start 2: grandfathering, which theory says
    # leaves no member worse off at this total.
    grand = zeros(Float64, NB_COUNTRY)
    for c in mem
        grand[c] = max(predicted_rho([c], P), 0.0)
    end
    best_overall, best_rights, best_price = Inf, nothing, nothing

    for (start, mult0) in (("equivalent", copy(rho)), ("grandfathered", grand))
        mult  = copy(mult0)
        price = p_init === nothing ? copy(P.p_ref) : copy(p_init)
        best_loss, best_mult, stall = Inf, copy(mult), 0
        for it in 1:max_iter
            rights = build(mult)
            price, _ = calibrate_price_to_cap(m, rights, P.club_emissions; p_init = price,
                                              tol = calib_tol, max_iter = (it == 1 ? 20 : 8),
                                              label = "$label/$start it$it")
            run_uniform!(m, rights, price)
            ede = f64(m[:welfare, :cons_EDE_country])
            g = zeros(Float64, NB_COUNTRY)
            for c in mem
                g[c] = (npv(@view ede[:, c]) - target[c]) / scale[c]
            end
            loss  = maximum(max(0.0, -g[c]) for c in mem)
            nlose = count(c -> g[c] < 0, mem)
            # Annealed step, not a trust region: a trust region that only shrinks
            # on non-improvement collapses to its floor after a few bad steps and
            # the search then sits still for ever.
            step = max(0.01, 0.20 * 0.93^(it - 1))
            @printf("  [%s/%s] iter %2d  worst loss = %.5f%%  losers = %d/%d  step = %.3f\n",
                    label, start, it, loss * 100, nlose, length(mem), step)
            flush(stdout)
            if loss < best_loss - 1e-9
                best_loss = loss; best_mult = copy(mult); stall = 0
            else
                stall += 1
            end
            (loss < tol || stall >= 12) && break

            # Move rights towards the members that are worst off. `build`
            # renormalises onto the cap, so only the *dispersion* of the update
            # survives: scale it by the spread of the gaps, which makes the
            # largest move exp(step) and keeps it meaningful whatever the level.
            gbar   = sum(g[c] for c in mem) / length(mem)
            dev    = [g[c] - gbar for c in mem]
            spread = maximum(abs, dev)
            spread < 1e-12 && break
            for (i, c) in enumerate(mem)
                mult[c] = max(1e-6, mult[c] * exp(-step * dev[i] / spread))
            end
        end
        @printf("  [%s/%s] best worst-case loss = %.5f%%\n", label, start, best_loss * 100)
        if best_loss < best_overall
            best_overall = best_loss
            best_rights  = build(best_mult)
            best_price   = price
        end
    end
    @printf("  [%s] kept the better start: worst-case loss = %.5f%% of proposal welfare\n",
            label, best_overall * 100)
    return best_rights, copy(P.club_emissions), best_price
end

"""
    run_variant(P, rho, variant, label; floor_rho)

Runs one variant: build its rights, calibrate the club price to the cap they
imply, and read the outcome off the model. `floor_rho` is option A's allocation,
used by variant 3.
"""
function run_variant(P::Proposal, rho::Vector{Float64}, variant::Int, label::String;
                     p_init = nothing, model = nothing, floor_rho = nothing)
    m = model === nothing ? make_uniform_model(RECYCLE_SHARE, P.members) : model
    rights, cap = if variant == 2
        r, c, _ = minimax_loss_rights(P, rho; label = "$(label)*", p_init, model = m)
        (r, c)
    else
        variant_rights(P, rho, variant; floor_rho)
    end
    # Warm start: the proposal's own emission-weighted price is a far better
    # opening guess than p*, which was calibrated for a much tighter cap.
    p0 = p_init === nothing ? P.p_ref : p_init
    p, err = calibrate_price_to_cap(m, rights, cap; p_init = p0, label = label)
    run_uniform!(m, rights, p)

    # Does the variant leave every member weakly better off than the proposal?
    # Step 3's dominance result is about grandfathering (rights = own autarky
    # emissions); no variant uses that allocation, so this is a question about
    # the run, not a theorem, and it is worth printing.
    ede  = f64(m[:welfare, :cons_EDE_country])
    cmc  = country_mean_cons(m)
    ede_gain  = zeros(Float64, NB_COUNTRY)
    cons_gain = zeros(Float64, NB_COUNTRY)
    for c in 1:NB_COUNTRY
        e = string(COUNTRIES[c])
        w0, c0 = P.welfare[e], P.cons[e]
        ede_gain[c]  = w0 != 0 ? (npv(@view ede[:, c]) - w0) / abs(w0) * 100 : 0.0
        cons_gain[c] = c0 != 0 ? (cmc[c] - c0) / abs(c0) * 100 : 0.0
    end
    # the ratio the variant actually hands out: rights over an equal-per-capita
    # share of the club's emissions, aggregated exactly as `entity_rho` does.
    # It equals the solved rho in variant 1 and differs in the others, which
    # rescale the allocation or add a surplus to it.
    rho_eff = zeros(Float64, NB_COUNTRY)
    for c in P.members
        num = sum(rights[t, c] for t in CALIB_IDX)
        den = sum(P.pop[t, c] * P.ebar[t] for t in CALIB_IDX)
        rho_eff[c] = den > 0 ? num / den : NaN
    end
    losers = [string(COUNTRIES[c]) for c in P.members if ede_gain[c] < 0]
    @printf("    [%s] %d of %d club members below their proposal welfare%s\n",
            label, length(losers), length(P.members),
            isempty(losers) ? "" : ": " * join(first(losers, 8), ", ") *
                                   (length(losers) > 8 ? ", ..." : ""))

    tot_r = sum(cap[CALIB_IDX])
    tot_p = sum(P.club_emissions[CALIB_IDX])
    ww    = world_welfare_npv(m)
    wc    = npv(mean_consumption(m, population(m)))
    return VariantResult(label, tot_r, tot_p, (1 - tot_r / tot_p) * 100,
                         temperature(m)[YEAR_IDX[2100]], ww,
                         (ww - P.world_welfare) / abs(P.world_welfare) * 100,
                         wc, (wc - P.world_cons) / abs(P.world_cons) * 100,
                         ede_gain, cons_gain, rho_eff, p, err)
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
    idx = intersect(entity_indices(entity), P.members)
    isempty(idx) && return NaN      # outside the club: no equivalent rho exists
    num = 0.0; den = 0.0
    for t in CALIB_IDX, c in idx
        w = P.pop[t, c] * P.ebar[t]
        num += rho[c] * w
        den += w
    end
    return den > 0 ? num / den : NaN
end

fmt(x; d = 3) = isnan(x) ? "--" : replace(string(round(x, digits = d)), "-" => "\$-\$")

"Percentage with a LaTeX minus, e.g. \$-\$0.006\\%."
fmt_pct(x)   = (x < 0 ? "\$-\$" : "") * @sprintf("%.3f\\%%", abs(x))
fmt_pct_1(x) = (x < 0 ? "\$-\$" : "") * @sprintf("%.1f\\%%", abs(x))

"Signed change in 2100 temperature, e.g. \$-\$0.019."
fmt_delta(x) = (x < 0 ? "\$-\$" : "+") * @sprintf("%.3f", abs(x))

"Carbon prices are round numbers; print them as such."
fmt_price(x) = isnan(x) ? "--" : @sprintf("%.0f", x)

# Tables read better with names than with ISO3 codes.
const ENTITY_NAME = Dict("USA" => "United States", "EU27" => "European Union",
                         "CHN" => "China",   "IND" => "India",     "RUS" => "Russia",
                         "TUR" => "Turkey",  "NGA" => "Nigeria",   "COD" => "DR Congo",
                         "BRA" => "Brazil",  "IDN" => "Indonesia")
entity_name(e::AbstractString) = get(ENTITY_NAME, e, e)

"Table/figure label for a proposal (`Duflo_legacy` -> `Duflo (legacy)`)."
function display_name(name::AbstractString)
    base = replace(name, "_legacy" => "")
    cited = base == "Duflo" ? "Banerjee et al." : base == "Wolfram" ? "Wolfram et al." : base
    return endswith(name, "_legacy") ? cited * " (legacy)" : cited
end

"""
    report_order(props)

Rows are ordered by the price the Banerjee schedule asks of each country,
highest tier first, and within a tier by decreasing option-A rho -- so
countries facing the same price sit together, most generously endowed first.
Countries that schedule does not price fall to the bottom.
"""
function report_order(props)
    P = nothing
    for Q in props
        occursin("Duflo", Q.name) && !occursin("legacy", Q.name) && (P = Q)
    end
    P === nothing && (P = last(props))
    r = something(get(RESULTS, ("A", P.name), nothing), get(RESULTS, ("B", P.name), nothing), Some(nothing))
    function key(e)
        idx = intersect(entity_indices(e), P.members)
        isempty(idx) && return (Inf, Inf)                     # unpriced: last
        price = mean(P.tax[YEAR_IDX[2030], idx])
        rho   = r === nothing ? 0.0 : entity_rho(r.rho, e, P)
        return (-price, isnan(rho) ? Inf : -rho)
    end
    return sort(REPORT_COUNTRIES; by = key)
end

"""
    write_table(path, method, props, rhos, v1, v2)

One pair of columns (Delta p_i, rho_i) per proposal, so the table takes two or
three proposals alike (Wolfram, Duflo, and optionally the legacy Duflo).
"""
function write_table(path::String, method::String, props, rhos, v1, v2, v3 = nothing, v4 = nothing)
    n = length(props)
    ncol = 1 + 2n
    # outcome rows: the figure sits in the rho column, so it lines up with the
    # method it belongs to; the price column carries the "from" temperature.
    outcome(label, cell) = "  \\quad " * label * " & " *
                           join([string(first(cell(k)), " & ", last(cell(k))) for k in 1:n], " & ") * " \\\\"
    open(path, "w") do io
        println(io, "\\begin{table}[htbp]")
        println(io, "\\centering")
        println(io, "\\small")
        println(io, "\\caption{Club-price rights ratios \$\\rho_i\$ equivalent to the ",
                    join([display_name(P.name) for P in props], ", ", " and "),
                    " proposals --- ", method, "}")
        println(io, "\\renewcommand{\\arraystretch}{1.15}")
        println(io, "\\begin{tabular}{l", repeat("cc", n), "}")
        println(io, "  \\toprule")
        println(io, "  & ", join(["\\multicolumn{2}{c}{\\textbf{$(display_name(P.name))}}" for P in props], " & "), " \\\\")
        println(io, "  ", join(["\\cmidrule(lr){$(2i)-$(2i+1)}" for i in 1:n], " "))
        println(io, "  \\textbf{Country} & ",
                    join(repeat(["\$p_i\$ \\textbf{2030 (\\\$/t)} & \$\\rho_i\$"], n), " & "), " \\\\")
        println(io, "  \\midrule")
        for e in report_order(props)
            cells = String[]
            for (P, rho) in zip(props, rhos)
                idx = intersect(entity_indices(e), P.members)
                # outside the club: the proposal prices it at 0 and it holds no rights
                isempty(idx) ? push!(cells, "0", "--") :
                    push!(cells, fmt_price(mean(P.tax[YEAR_IDX[2030], idx])),
                                 fmt(entity_rho(rho, e, P)))
            end
            println(io, "  ", entity_name(e), " & ", join(cells, " & "), " \\\\")
        end
        println(io, "  \\midrule")
        println(io, "  \\multicolumn{", ncol, "}{l}{\\textit{Variant 1 --- unscaled rights \$\\rho_i\$ (the allocation sets the cap)}} \\\\")
        println(io, outcome("Gain in rights (\\% of the club's emissions)",
                            k -> ("", fmt_pct_1(v1[k].rights_gain_pct))))
        println(io, outcome("World temperature 2100, change (\$^\\circ\$C)",
                            k -> ("", fmt_delta(v1[k].temp_2100 - props[k].temp_2100))))
        println(io, outcome("World welfare gain, EDE (\\%)",
                            k -> ("", fmt_pct(v1[k].welfare_gain_pct))))
        println(io, outcome("World mean consumption gain (\\%)",
                            k -> ("", fmt_pct(v1[k].cons_gain_pct))))
        println(io, "  \\midrule")
        for (v, k, title) in ((v2, 2, "Variant 2 --- surplus allocated to minimise the largest shortfall"),
                              (v3, 3, "Variant 3 --- surplus shared by marginal utility"),
                              (v4, 4, "Variant 4 --- surplus shared by uniform scaling"))
            v === nothing && continue
            k > 2 && println(io, "  \\midrule")
            println(io, "  \\multicolumn{", ncol, "}{l}{\\textit{", title, "}} \\\\")
            println(io, outcome("World welfare gain, EDE (\\%)",
                                x -> ("", fmt_pct(v[x].welfare_gain_pct))))
            println(io, outcome("World mean consumption gain (\\%)",
                                x -> ("", fmt_pct(v[x].cons_gain_pct))))
        end
        println(io, "  \\bottomrule")
        println(io, "\\end{tabular}")
        println(io, "\\label{tab:equiv_rights_", occursin("option A", method) ? "A" : "B", "}")
        println(io, "\\\\[4pt]")
        println(io, "{\\footnotesize Note: \$p_i\$ is the carbon price the proposal asks of country \$i\$ in ",
                    "2030; the comparison regime instead prices every member of the proposal's own club at ",
                    "one common price (",
                    join([@sprintf("\\\$%.1f/t for %s", P.p_ref[YEAR_IDX[2030]], display_name(P.name))
                          for P in props], ", ", " and "),
                    " in 2030) and leaves non-members unpriced, as the proposal does. \$\\rho_i\$ is the ",
                    "country's allocation as a multiple of an equal-per-capita share of the club's emissions ",
                    "under the proposal; the European Union figure aggregates its members' rights. A country ",
                    "the proposal does not price is outside the club in both regimes: it is shown at \$p_i=0\$ ",
                    "with no equivalent allocation (`--'). \$\\rho_i < 0\$ would mean a country prefers the ",
                    "club regime even while paying a net fee for its allocation. ",
                    "Variant 1 lets the equivalent allocation set the cap, so the ",
                    "club price is recalibrated to deliver exactly those rights; variant 2 rescales the ",
                    "same allocation to the club's own emissions under the proposal, so club emissions are ",
                    "held fixed and only allocative efficiency remains; it is reported both on the ",
                    "inequality-averse global EDE (\$\\eta=1.5\$) and on world mean consumption per ",
                    "capita, which weights every person's consumption equally. The temperature row is the ",
                    "change from the proposal's own 2100 warming, which is ",
                    join([@sprintf("%.2f\\textdegree{}C for %s", P.temp_2100, display_name(P.name))
                          for P in props], ", ", " and "),
                    ". Both proposals are priced ",
                    "from 2025 and capped at the backstop price. The Banerjee et al. proposal prices ",
                    "high-income countries at \\\$", Int(round(DUFLO_HIC_PRICE)), "/t, a tier the report ",
                    "leaves unspecified; the legacy column, where it appears, leaves them unpriced.}")
        println(io, "\\end{table}")
    end
    @info "wrote table" path
end

"""
    ab_tabular(io, props)

The tabular both the slide and the paper use: one price column and one rho
column per method for each proposal, then the outcome rows with each method's
figure in its own column. Only methods that have produced results get a column.
"""
function ab_tabular(io, props)
    got(m, P) = get(RESULTS, (m, P.name), nothing)
    ms = [m for m in ("A", "B") if any(P -> got(m, P) !== nothing, props)]
    isempty(ms) && (ms = ["A"])
    nm = length(ms)
    rho_head = nm == 1 ? "\$\\rho_i\$" : join(["\$\\rho^{$m}\$" for m in ms], " & ")
    n = length(props)
    function outcome(label, left, cell)
        cells = String[]
        for P in props
            push!(cells, left(P))
            for m in ms
                r = got(m, P)
                push!(cells, r === nothing ? "--" : cell(m, P, r))
            end
        end
        return "  \\textbf{" * label * "} & " * join(cells, " & ") * " \\\\"
    end
    println(io, "\\begin{tabular}{l", repeat("c"^(1 + nm), n), "}")
    println(io, "  \\toprule")
    println(io, "  & ", join(["\\multicolumn{$(1 + nm)}{c}{\\textbf{$(display_name(P.name))}}" for P in props], " & "), " \\\\")
    println(io, "  ", join([let a = 2 + (i - 1) * (1 + nm); "\\cmidrule(lr){$a-$(a + nm)}" end for i in 1:n], " "))
    println(io, "  \\textbf{Country} & ", join(repeat(["\$p_i\$ & " * rho_head], n), " & "), " \\\\")
    println(io, "  \\midrule")
    for e in report_order(props)
        cells = String[]
        for P in props
            idx = intersect(entity_indices(e), P.members)
            if isempty(idx)
                push!(cells, "0", fill("--", nm)...)
            else
                push!(cells, fmt_price(mean(P.tax[YEAR_IDX[2030], idx])))
                for m in ms
                    r = got(m, P)
                    push!(cells, r === nothing ? "--" : fmt(entity_rho(r.rho, e, P); d = 2))
                end
            end
        end
        println(io, "  ", entity_name(e), " & ", join(cells, " & "), " \\\\")
    end
    println(io, "  \\midrule")
    println(io, outcome("Gain in rights, variant 1 (\\%)", P -> "",
                        (m, P, r) -> "\\rose{" * fmt_pct_1(r.v1.rights_gain_pct) * "}"))
    println(io, outcome("World temp.~2100, change (\\textdegree{}C)", P -> "",
                        (m, P, r) -> "\\rose{" * fmt_delta(r.v1.temp_2100 - P.temp_2100) * "}"))
    println(io, outcome("World welfare gain, EDE (variant 1)", P -> "",
                        (m, P, r) -> fmt_pct(r.v1.welfare_gain_pct)))
    println(io, outcome("World consumption gain (variant 1)", P -> "",
                        (m, P, r) -> fmt_pct(r.v1.cons_gain_pct)))
    for (k, v) in ((2, :v2), (3, :v3), (4, :v4))
        println(io, outcome("World welfare gain, EDE (variant $k)", P -> "",
                            (m, P, r) -> fmt_pct(getfield(r, v).welfare_gain_pct)))
        println(io, outcome("World consumption gain (variant $k)", P -> "",
                            (m, P, r) -> fmt_pct(getfield(r, v).cons_gain_pct)))
    end
    println(io, "  \\bottomrule")
    println(io, "\\end{tabular}")
    return ms
end

"""
    write_simple_table(path, props)

The slide version: option A only, variants 1 and 2 only, no method superscript
on rho. The two blocks are named by what they do rather than numbered, and the
note carries only the club prices, the reference temperatures and the two rules.
"""
function write_simple_table(path::String, props)
    got(P) = get(RESULTS, ("A", P.name), nothing)
    any(P -> got(P) !== nothing, props) || return
    n = length(props)
    # `rose` colours the whole line, legend included
    function row(label, cell; rose = false)
        paint(x) = rose ? "\\rose{" * x * "}" : x
        cells = [let r = got(P)
                     r === nothing ? " & --" : " & " * paint(cell(P, r))
                 end for P in props]
        return "  " * paint(label) * " & " * join(cells, " & ") * " \\\\"
    end
    open(path, "w") do io
        println(io, "% Generated by src/equivalent_rights_proposals.jl -- do not edit by hand.")
        println(io, "\\centering")
        println(io, "\\scriptsize")
        println(io, "\\renewcommand{\\arraystretch}{0.95}")
        println(io, "\\begin{tabular}{l", repeat("cc", n), "}")
        println(io, "  \\toprule")
        println(io, "  & ", join(["\\multicolumn{2}{c}{\\textbf{$(display_name(P.name))}}" for P in props], " & "), " \\\\")
        println(io, "  ", join(["\\cmidrule(lr){$(2i)-$(2i + 1)}" for i in 1:n], " "))
        println(io, "  \\textbf{Country} & ", join(repeat(["\$p_i\$ & \$\\rho_i\$"], n), " & "), " \\\\")
        println(io, "  \\midrule")
        for e in report_order(props)
            cells = String[]
            for P in props
                idx = intersect(entity_indices(e), P.members)
                r   = got(P)
                if isempty(idx)
                    push!(cells, "0", "--")
                else
                    push!(cells, fmt_price(mean(P.tax[YEAR_IDX[2030], idx])),
                                 r === nothing ? "--" : fmt(entity_rho(r.rho, e, P); d = 2))
                end
            end
            println(io, "  ", entity_name(e), " & ", join(cells, " & "), " \\\\")
        end
        println(io, "  \\midrule")
        println(io, "  \\multicolumn{", 1 + 2n, "}{l}{\\textit{Equivalent rights as solved: the allocation sets the cap}} \\\\")
        println(io, row("World temp.~2100, change (\\textdegree{}C)",
                        (P, r) -> fmt_delta(r.v1.temp_2100 - P.temp_2100)))
        println(io, row("Reduced emissions in the coalition (\\%)",
                        (P, r) -> fmt_pct_1(r.v1.rights_gain_pct); rose = true))
        println(io, row("World welfare gain (\\%)",
                        (P, r) -> fmt_pct(r.v1.welfare_gain_pct); rose = true))
        println(io, row("World consumption gain (\\%)", (P, r) -> fmt_pct(r.v1.cons_gain_pct)))
        println(io, "  \\hline")
        println(io, "  \\multicolumn{", 1 + 2n, "}{l}{\\textit{Rights raised to the coalition's emissions, shared to maximise the smallest gain}} \\\\")
        println(io, row("World welfare gain (\\%)", (P, r) -> fmt_pct(r.v2.welfare_gain_pct)))
        println(io, row("World consumption gain (\\%)", (P, r) -> fmt_pct(r.v2.cons_gain_pct)))
        println(io, "  \\bottomrule")
        println(io, "\\end{tabular}")
        println(io, "")
        println(io, "\\vspace{.2cm}")
        println(io, "{\\tiny \\parbox{0.88\\textwidth}{\$p_i\$: the price the proposal asks of \$i\$ in 2030 ",
                    "(\\\$/t), against a single price for all coalition members of ",
                    join([@sprintf("\\\$%.1f/t (%s)", P.p_ref[YEAR_IDX[2030]], display_name(P.name))
                          for P in props], " and "),
                    "; countries the proposal does not price stay outside the coalition (\$p_i=0\$, no ",
                    "allocation). The proposals themselves reach ",
                    join([@sprintf("%.2f\\textdegree{}C (%s)", P.temp_2100, display_name(P.name))
                          for P in props], " and "),
                    " in 2100. In the first block the equivalent rights are used as they are solved, so the ",
                    "coalition emits less than under the proposal; in the second they are raised to the ",
                    "proposal's own emissions and shared so as to make the worst-off member as well off as ",
                    "possible.}}")
    end
    @info "wrote table" path
end

"The slide version: no float, no caption."
function write_beamer_table(path::String, props)
    open(path, "w") do io
        println(io, "% Generated by src/equivalent_rights_proposals.jl -- do not edit by hand.")
        println(io, "\\centering")
        println(io, "\\scriptsize")
        println(io, "\\renewcommand{\\arraystretch}{0.92}")
        ab_tabular(io, props)
    end
    @info "wrote table" path
end

"The paper version: the same tabular as a float, with caption, label and note."
function write_combined_table(path::String, props)
    open(path, "w") do io
        println(io, "% Generated by src/equivalent_rights_proposals.jl -- do not edit by hand.")
        println(io, "\\begin{table}[htbp]")
        println(io, "\\centering")
        println(io, "\\small")
        println(io, "\\caption{Club-price rights ratios \$\\rho_i\$ equivalent to the ",
                    join([display_name(P.name) for P in props], ", ", " and "), " proposals}")
        println(io, "\\renewcommand{\\arraystretch}{1.15}")
        ab_tabular(io, props)
        println(io, "\\label{tab:equiv_rights}")
        println(io, "\\\\[4pt]")
        println(io, "{\\footnotesize Note: \$p_i\$ is the carbon price the proposal asks of country \$i\$ in ",
                    "2030; the comparison regime instead prices every member of the proposal's own club at ",
                    "one common price (",
                    join([@sprintf("\\\$%.1f/t for %s", P.p_ref[YEAR_IDX[2030]], display_name(P.name))
                          for P in props], ", ", " and "),
                    " in 2030) and leaves non-members unpriced, as the proposal does. \$\\rho_i\$ is the ",
                    "country's allocation as a multiple of an equal-per-capita share of the club's emissions ",
                    "under the proposal, taken at face value in variant 1; \$\\rho^{A}\$ solves each country ",
                    "on its own, \$\\rho^{B}\$ all of them jointly. The European Union figure aggregates its ",
                    "members' rights, and a country the proposal does not price is outside the club in both ",
                    "regimes, shown at \$p_i=0\$ with no equivalent allocation (`--'). Variant 1 lets the ",
                    "equivalent allocation set the cap; variants 2 to 4 return the resulting surplus as rights ",
                    "so that club emissions match the proposal's, sharing it so as to minimise the largest ",
                    "shortfall against the proposal (2), in proportion to population times marginal utility ",
                    "(3), and by uniform scaling (4). The temperature row is the change from the proposal's own 2100 warming, ",
                    "which is ",
                    join([@sprintf("%.2f\\textdegree{}C for %s", P.temp_2100, display_name(P.name))
                          for P in props], ", ", " and "),
                    ". Welfare is reported on the inequality-averse global EDE (\$\\eta=1.5\$) and on world ",
                    "mean consumption per capita, which weights every person's consumption equally.}")
        println(io, "\\end{table}")
    end
    @info "wrote table" path
end

# ──────────────────────────────────────────────────────────────────────────────
# RESULTS STORE
#
# Every (scenario, option) pair writes its tables as soon as it is finished, so
# a long run is readable from the first completed cell onwards and a crash in a
# later one costs nothing that came before.
# ──────────────────────────────────────────────────────────────────────────────

const RESULTS = Dict{Tuple{String,String},NamedTuple}()   # (method, scenario) -> result

method_label(m) = m == "A" ? "option A (per-country solve)" : "option B (simultaneous solve)"

"""
    load_prior_results!(props)

Fill RESULTS with whatever an earlier run left on disk for the options this
session is not solving. Without it a B-only run rewrites the summary CSV from
its own results alone, dropping option A's rows, and the tables lose their A
columns -- the outputs would silently narrow to whatever was solved last.
"""
function load_prior_results!(props)
    FRESH && return
    # per-country gains, if an earlier run wrote them
    prior_gains = Dict{Tuple{String,String,Int,Symbol},Vector{Float64}}()
    gp = joinpath(OUTPUT_BASE, "country_gains.csv")
    if isfile(gp)
        gdf = CSV.read(gp, DataFrame)
        for sub in groupby(gdf, [:method, :scenario, :variant])
            k = (String(sub.method[1]), String(sub.scenario[1]), Int(sub.variant[1]))
            idx = Dict(String(r.country) => i for (i, r) in enumerate(eachrow(sub)))
            for (sym, col) in ((:ede, :ede_gain_pct), (:cons, :cons_gain_pct), (:rho_eff, :rho_variant))
                v = zeros(Float64, NB_COUNTRY)
                for (i, c) in enumerate(COUNTRIES)
                    j = get(idx, string(c), 0)
                    j > 0 && hasproperty(sub, col) && (v[i] = Float64(sub[j, col]))
                end
                prior_gains[(k[1], k[2], k[3], sym)] = v
            end
        end
    end
    path = joinpath(OUTPUT_BASE, "equivalent_rights_variants.csv")
    isfile(path) || return
    df = CSV.read(path, DataFrame)
    for P in props, m in ("A", "B")
        haskey(RESULTS, (m, P.name)) && continue
        rows = df[(String.(df.method) .== m) .& (String.(df.scenario) .== P.name), :]
        nrow(rows) >= 2 || continue
        rp = joinpath(OUTPUT_BASE, "rho_$(lowercase(m))_$(lowercase(P.name)).csv")
        isfile(rp) || continue
        d = Dict(String(r.country) => Float64(r.rho) for r in eachrow(CSV.read(rp, DataFrame)))
        rho = [get(d, string(c), 0.0) for c in COUNTRIES]
        function vr(k)
            r = rows[findfirst(==(k), rows.variant), :]
            VariantResult("$(m)$(k)/$(P.name)", NaN, NaN, r.rights_gain_pct, r.temp_2100,
                          NaN, r.welfare_gain_pct, NaN, r.cons_gain_pct,
                          get(prior_gains, (m, P.name, k, :ede), Float64[]),
                          get(prior_gains, (m, P.name, k, :cons), Float64[]),
                          get(prior_gains, (m, P.name, k, :rho_eff), Float64[]), Float64[], NaN)
        end
        RESULTS[(m, P.name)] = (; rho, v1 = vr(1), v2 = vr(2),
                                v3 = 3 in rows.variant ? vr(3) : vr(2),
                                v4 = 4 in rows.variant ? vr(4) : vr(2))
        @info "reusing results from an earlier run" scenario = P.name option = m
    end
end

"""
    write_losers_table(path, props)

How many club members end up below their proposal welfare under each method and
variant -- the country-by-country counterpart of the world aggregates. Variant 1
makes every member exactly indifferent by construction, so its count is the
numerical zero of the exercise; the others show what each sharing rule costs.
"""
function write_losers_table(path::String, props)
    got(m, P) = get(RESULTS, (m, P.name), nothing)
    ms = [m for m in ("A", "B") if any(P -> got(m, P) !== nothing, props)]
    isempty(ms) && return
    nm, n = length(ms), length(props)
    count_below(v, P, field) = begin
        g = getfield(v, field)
        isempty(g) ? nothing : count(c -> g[c] < 0, P.members)
    end
    cell(x) = x === nothing ? "--" : string(x)
    open(path, "w") do io
        println(io, "% Generated by src/equivalent_rights_proposals.jl -- do not edit by hand.")
        println(io, "\\begin{table}[htbp]")
        println(io, "\\centering")
        println(io, "\\small")
        println(io, "\\caption{Club members left below their welfare under the proposal, by variant}")
        println(io, "\\begin{tabular}{l", repeat("c"^nm, n), "}")
        println(io, "  \\toprule")
        println(io, "  & ", join(["\\multicolumn{$nm}{c}{\\textbf{$(display_name(P.name))}} " *
                                  "(of $(length(P.members)))" for P in props], " & "), " \\\\")
        println(io, "  ", join([let a = 2 + (i - 1) * nm; "\\cmidrule(lr){$a-$(a + nm - 1)}" end for i in 1:n], " "))
        println(io, "  \\textbf{Variant} & ", join(repeat([join(["\\textbf{$m}" for m in ms], " & ")], n), " & "), " \\\\")
        for (fld, title) in ((:ede_gain, "on equally-distributed-equivalent consumption"),
                             (:cons_gain, "on mean consumption per capita"))
            println(io, "  \\midrule")
            println(io, "  \\multicolumn{", 1 + nm * n, "}{l}{\\textit{Members losing ", title, "}} \\\\")
            for (k, f) in ((1, :v1), (2, :v2), (3, :v3), (4, :v4))
                cells = String[]
                for P in props, m in ms
                    r = got(m, P)
                    push!(cells, r === nothing ? "--" : cell(count_below(getfield(r, f), P, fld)))
                end
                println(io, "  \\quad Variant ", k, " & ", join(cells, " & "), " \\\\")
            end
        end
        println(io, "  \\bottomrule")
        println(io, "\\end{tabular}")
        println(io, "\\label{tab:equiv_rights_losers}")
        println(io, "\\\\[4pt]")
        println(io, "{\\footnotesize Note: a member counts as losing when its NPV of consumption over ",
                    "2030--2100 falls short of what it obtains under the proposal itself. Variant 1 makes ",
                    "every member indifferent by construction, so any count there is numerical noise. ",
                    "Variants 2 to 4 return the same surplus of rights under different sharing rules: ",
                    "a split chosen to minimise the largest relative shortfall (2), one proportional to ",
                    "population times marginal utility (3), and uniform scaling (4).}")
        println(io, "\\end{table}")
    end
    @info "wrote table" path
end

"Rewrite everything that can be written from what is in RESULTS right now."
function flush_outputs(props)
    for m in ("A", "B")
        done = [P for P in props if haskey(RESULTS, (m, P.name))]
        isempty(done) && continue
        r = [RESULTS[(m, P.name)] for P in done]
        write_table(joinpath(OUTPUT_BASE, "equivalent_rights_option_$(m).tex"),
                    method_label(m), done, [x.rho for x in r],
                    [x.v1 for x in r], [x.v2 for x in r], [x.v3 for x in r], [x.v4 for x in r])
    end

    rows = NamedTuple[]
    for m in ("A", "B"), P in props
        haskey(RESULTS, (m, P.name)) || continue
        x = RESULTS[(m, P.name)]
        for (variant, v) in ((1, x.v1), (2, x.v2), (3, x.v3), (4, x.v4))
            push!(rows, (method = m, scenario = P.name, variant = variant,
                         club_size = length(P.members),
                         rights_gain_pct = v.rights_gain_pct, temp_2100 = v.temp_2100,
                         temp_2100_proposal = P.temp_2100,
                         welfare_gain_pct = v.welfare_gain_pct,
                         cons_gain_pct = v.cons_gain_pct))
        end
    end
    # per-country gains, long format: one row per method x scenario x variant x country
    rows_c = NamedTuple[]
    for m in ("A", "B"), P in props
        haskey(RESULTS, (m, P.name)) || continue
        x = RESULTS[(m, P.name)]
        for (k, v) in ((1, x.v1), (2, x.v2), (3, x.v3), (4, x.v4))
            (isempty(v.ede_gain) || isempty(v.cons_gain)) && continue
            for c in P.members
                push!(rows_c, (method = m, scenario = P.name, variant = k,
                               country = string(COUNTRIES[c]),
                               rho = x.rho[c],                                   # the solved ratio
                               rho_variant = isempty(v.rho_eff) ? NaN : v.rho_eff[c],  # what this variant allocates
                               ede_gain_pct = v.ede_gain[c], cons_gain_pct = v.cons_gain[c]))
            end
        end
    end
    isempty(rows_c) ||
        CSV.write(joinpath(OUTPUT_BASE, "country_gains.csv"), DataFrame(rows_c))

    write_losers_table(joinpath(OUTPUT_BASE, "equivalent_rights_losers.tex"), props)
    write_beamer_table(joinpath(OUTPUT_BASE, "equivalent_rights_beamer.tex"), props)
    write_simple_table(joinpath(OUTPUT_BASE, "equivalent_rights_simple.tex"), props)
    write_combined_table(joinpath(OUTPUT_BASE, "equivalent_rights_combined.tex"), props)

    summary = DataFrame(rows)
    CSV.write(joinpath(OUTPUT_BASE, "equivalent_rights_variants.csv"), summary)
    return summary
end

"""
Option A's rho vector as written by an earlier run, or `nothing`. Lets a B-only
session start from A's answer instead of the closed-form prediction, which is
what `main` does when both options run in one go.
"""
function saved_rho_A(P::Proposal)
    FRESH && return nothing
    path = joinpath(OUTPUT_BASE, "rho_A_$(lowercase(P.name)).csv")
    isfile(path) || return nothing
    d = Dict(String(r.country) => Float64(r.rho) for r in eachrow(CSV.read(path, DataFrame)))
    v = [get(d, string(c), 0.0) for c in COUNTRIES]
    n = count(c -> haskey(d, string(COUNTRIES[c])), P.members)
    n < length(P.members) ÷ 2 && return nothing      # too partial to be a useful start
    @info "option B warm-started from a saved option-A solve" P.name file = basename(path) members = n
    return v
end

"""
Option A's or B's rho vector from an earlier run, or `nothing`. Lets the
variants be recomputed without re-solving rho, which is what changes when the
definition of a variant changes rather than the equivalence itself.
"""
function saved_rho(method::String, P::Proposal)
    FRESH && return nothing
    path = joinpath(OUTPUT_BASE, "rho_$(lowercase(method))_$(lowercase(P.name)).csv")
    isfile(path) || return nothing
    d = Dict(String(r.country) => Float64(r.rho) for r in eachrow(CSV.read(path, DataFrame)))
    n = count(c -> haskey(d, string(COUNTRIES[c])), P.members)
    n < length(P.members) ÷ 2 && return nothing
    return [get(d, string(c), 0.0) for c in COUNTRIES]
end

"Solve one (scenario, option) cell, run the four variants, and write the tables."
function solve_cell!(P::Proposal, method::String, props; variants_only = false)
    t0 = time()
    if variants_only
        rho = saved_rho(method, P)
        rho === nothing && (@warn "no saved rho to rerun the variants from" P.name method; return nothing)
        @info "rerunning the variants from a saved solve" scenario = P.name option = method
    end
    rho = variants_only ? saved_rho(method, P) : if method == "A"
        d, flags = solve_option_A(P; checkpoint = joinpath(OUTPUT_BASE,
                                 "rho_A_$(lowercase(P.name))_checkpoint.csv"))
        write_rho_csv(joinpath(OUTPUT_BASE, "rho_A_$(lowercase(P.name)).csv"), d, flags)
        [get(d, string(c), 0.0) for c in COUNTRIES]     # non-members hold no rights
    else
        # option A's vector if it ran in this session, else one saved by an
        # earlier run, else the closed-form prediction
        warm = get(RESULTS, ("A", P.name), nothing)
        rho0 = warm !== nothing ? warm.rho : saved_rho_A(P)
        v, gap, pB = rho0 === nothing ? solve_option_B(P) : solve_option_B(P, rho0)
        CSV.write(joinpath(OUTPUT_BASE, "rho_B_$(lowercase(P.name)).csv"),
                  DataFrame(country = string.(COUNTRIES), rho = v,
                            member = [c in P.members for c in 1:NB_COUNTRY],
                            max_rel_welfare_gap = fill(gap, NB_COUNTRY)))
        v
    end

    # kept for reference: option A's allocation, available to any variant that
    # wants it as a reference point
    # variant 3 floors on option A's equivalent allocation: from this session if
    # it ran, else from disk. For option A itself the floor is its own vector,
    # so A3 coincides with A2 by construction.
    a = get(RESULTS, ("A", P.name), nothing)
    floor_rho = a === nothing ? saved_rho_A(P) : a.rho
    floor_rho === nothing &&
        @warn "no option-A allocation to floor variant 3 on; it will repeat variant 2" P.name
    v1 = run_variant(P, rho, 1, "$(method)1/$(P.name)")
    # variant 4 (uniform scaling) is the cheapest way to get a price for the
    # shared cap, so it goes first and warms the other two
    v4 = run_variant(P, rho, 4, "$(method)4/$(P.name)"; p_init = v1.price_path)
    v2 = run_variant(P, rho, 2, "$(method)2/$(P.name)"; p_init = v4.price_path)
    v3 = run_variant(P, rho, 3, "$(method)3/$(P.name)"; p_init = v4.price_path)
    RESULTS[(method, P.name)] = (; rho, v1, v2, v3, v4)
    flush_outputs(props)
    @printf("  [%s/%s] done in %.1f min -- tables updated\n", method, P.name, (time() - t0) / 60)
    flush(stdout)
    return nothing
end

"""
    main(; duflo_legacy, methods)

Runs each scenario through each requested option and writes the tables after
every one of them, so `equivalent_rights_option_A.tex`,
`equivalent_rights_option_B.tex` and `equivalent_rights_variants.csv` are always
up to date with whatever has finished -- nothing waits for the full matrix.

Option A is spread over the worker processes started at load time; option B is
one global model run per iteration and stays sequential. `methods` selects the
options (`["A", "B"]`, `["A"]` or `["B"]`); option B warm-starts from option A
when that has just run, and from the closed-form prediction otherwise.
"""
function main(; duflo_legacy::Bool = get(ENV, "NICE_DUFLO_LEGACY", "0") in ("1", "true"),
                methods::Vector{String} = String.(strip.(split(get(ENV, "NICE_METHODS", "A,B"), ","))),
                variants_only::Bool = get(ENV, "NICE_VARIANTS_ONLY", "0") in ("1", "true"))
    isempty(setdiff(methods, ["A", "B"])) || error("methods must be a subset of [\"A\", \"B\"]")
    @info "Building proposal scenarios" methods workers = nworkers()
    wolfram = build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate))
    duflo   = build_proposal("Duflo",   proposal_tax_matrix(duflo_rate))
    props   = duflo_legacy ?
              [wolfram, duflo, build_proposal("Duflo_legacy", proposal_tax_matrix(duflo_legacy_rate))] :
              [wolfram, duflo]

    load_prior_results!(props)

    for P in props
        @printf("  %s: world emissions 2030 = %.2f GtCO2 (club %.2f), T(2100) = %.3f C, welfare NPV = %.2f\n",
                P.name, P.world_emissions[YEAR_IDX[2030]], P.club_emissions[YEAR_IDX[2030]],
                P.temp_2100, P.world_welfare)
    end

    # scenario-outer, option-inner: B reuses A's vector for the same scenario,
    # and each cell publishes its tables before the next one starts
    for P in props, m in methods
        @info "solving" scenario = P.name option = m variants_only
        solve_cell!(P, m, props; variants_only)
    end

    summary = flush_outputs(props)
    println(summary)
    @info "outputs written" dir = OUTPUT_BASE
    return (; props, results = RESULTS, summary)
end

# ──────────────────────────────────────────────────────────────────────────────
# ENTRY POINT
#
# Runs when the file is executed as a script *and* when it is sent to a REPL
# (VS Code's "Julia: Execute File in REPL", Alt+Enter on the whole file), so the
# usual editor workflow needs no shell. Never on a worker process.
#
#   julia --project=. src/equivalent_rights_proposals.jl          both options
#   julia --project=. src/equivalent_rights_proposals.jl B        option B only
#   julia --project=. src/equivalent_rights_proposals.jl tables       rebuild the tables only
#   julia --project=. src/equivalent_rights_proposals.jl chunk <scen> <k> <n>
# Chunk files left by earlier `chunk` runs are picked up automatically, so there
# is no separate gather step.
#
# Environment: NICE_WORKERS (worker count, default min(4, CPU/2)),
# NICE_METHODS ("A", "B", "A,B"), NICE_DUFLO_LEGACY=1 (third scenario),
# NICE_A_COVERAGE (share of club emissions solved exactly),
# NICE_AUTORUN=0 (load the definitions without running anything).
if myid() == 1 && get(ENV, "NICE_AUTORUN", "1") != "0" &&
   (abspath(PROGRAM_FILE) == abspath(SELF) || isinteractive())
    if length(ARGS) >= 1 && ARGS[1] == "tables"
        # rebuild every table from the results already on disk, solving nothing
        props = [build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate)),
                 build_proposal("Duflo",   proposal_tax_matrix(duflo_rate))]
        load_prior_results!(props)
        println(flush_outputs(props))
    elseif length(ARGS) >= 1 && ARGS[1] == "chunk"
        run_chunk(ARGS[2], parse(Int, ARGS[3]), parse(Int, ARGS[4]))
    elseif length(ARGS) >= 1 && uppercase(ARGS[1]) in ("A", "B")
        main(; methods = [uppercase(ARGS[1])])
    else
        main()
    end
end
