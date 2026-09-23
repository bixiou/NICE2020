################################################################################
# Equivalent emission rights for the Wolfram and Duflo price proposals.
#
# For a proposal S (a schedule of differentiated national carbon prices, run in
# autarky: no cross-border revenue sharing -- except Equal Right, whose revenue
# is recycled equally per capita at the world level), we look for the allocation of
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
#                         Reported: emissions change, world temperature in 2100.
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
#
# RUNNING TIME. Measured on the 15-16 Sept 2026 consumption-target run (4
# workers, 5 scenarios x 2 options); per-cell figures are the "[cell] done in
# ..." lines of logs/cons.log. The unit of cost is one NICE2020 run, ~7 s.
#
#   load + workers .......   3-5 min   packages, Mimi model build, 4 workers
#   proposals ............   0-8 min   0 with the .jls cache, ~2 min each without
#   option A, per cell ...  25-45 min  5 runs per country, 4 countries at a time
#                                      (18 countries in 5.0 min, 110 in 21 min)
#   option B, per cell ...  20-35 min  ~1.5-2 min per outer iteration, 7-29 of them
#   the four variants ....  15-30 min  variant 2 (maximin) is nearly all of it
#   tables only ..........   2-3 min   no model runs; the load is the whole cost
#   ---------------------------------------------------------------------------
#   TOTAL, 8 cells .......  ~4.5 h of compute
#
# Wall clock for that run was ~15 h, but ~9.5 h of it sits inside the A/Wolfram
# cell, which reported 594.5 min for ~25 min of work (18 countries solved in 5.0
# min, and fewer variant runs than the A/Duflo cell that took 47 min): the
# machine was idle or asleep overnight. For an unattended run, wall clock is not
# a measure of compute -- count runs instead.
################################################################################

const ROOT = normpath(joinpath(@__DIR__, ".."))

# data/parameters.jl reads its inputs through paths relative to the project root
# ("data/nice_inputs.json"), so the working directory has to be the root.
cd(ROOT)

using Pkg
Pkg.activate(ROOT)

using Distributed

using Mimi, MimiFAIRv2, DataFrames, CSV, CSVFiles, Statistics, Printf, Dates, Serialization

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

# NICE_TARGET chooses the measure the solvers equalise against the proposal:
# "cons" (default), the NPV of mean consumption per capita, or "ede", the NPV of
# equally-distributed-equivalent consumption. Every result that depends on the
# choice is written under a tagged name -- "_ede" before the extension for the
# EDE target -- so the two sets live side by side and neither overwrites the
# other; files also record the target they were solved on, and a file solved on
# the other target is never reused.
const TARGET = get(ENV, "NICE_TARGET", "cons")
TARGET in ("cons", "ede") || error("NICE_TARGET must be \"cons\" or \"ede\", got \"$TARGET\"")
const TAG = TARGET == "ede" ? "_ede" : ""
@info "solvers target" TARGET

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
# p*: the exponential 1.8C path found by cap_and_share/find_global_exp_carbon_tax_buget_zoom.jl.
# That script searches the start level A and growth rate B of
#     exp_tax_trajectory(tax_start_value = A, g_rate = B, year_tax_start = 2030,
#                        year_tax_end = 2200, ramp_up = 5)
# (src/helper_functions.jl) and keeps the pair that maximises discounted world
# welfare (utility, pure rate of time preference 0.3%) subject to peak warming
# staying below 1.8C, the whole world taxed, no recycling. The path is zero
# before 2025, ramps linearly from 0 in 2025 to A in 2030, is A in 2031, grows
# at B a year to 2200 and is flat afterwards. calibrated_global_exp.csv holds
# that exact vector (src/_write_exp_path.jl writes it from the saved A and B),
# so p* is the path the search validated, with no re-extrapolation.
function read_price_path(file)
    p  = zeros(Float64, NB_STEPS)
    df = CSV.read(joinpath(ROOT, "cap_and_share", "data", "output", file), DataFrame)
    d  = Dict(Int(r.time) => Float64(r.global_tax) for r in eachrow(df))
    first_y, last_y = minimum(keys(d)), maximum(keys(d))
    for (i, y) in enumerate(YEARS)
        p[i] = y < PRICE_START_YEAR ? 0.0 :
               # a file starting after PRICE_START_YEAR is extended back at its own
               # first-year growth rate (the calibrated path starts in 2030)
               y < first_y          ? d[first_y] / (d[first_y + 1] / d[first_y])^(first_y - y) :
               y <= last_y          ? get(d, y, 0.0) : d[last_y]
    end
    return p
end

const P_STAR = read_price_path("calibrated_global_exp.csv")

# The previous benchmark, no longer used. It was calibrated year by year by
# cap_and_share/calibrate_global_tax_club_emissions.jl: in each year, the uniform
# global tax (found by bisection, bracketed around the previous year's tax) at
# which the model's emissions equal the 1.8C cap-and-share trajectory
# cap_and_share/data/input/E_global_cs_2020_2300.csv (31.4 GtCO2 in 2030, zero
# from 2079), and the backstop price once that trajectory reaches zero. No growth
# rule is imposed: $42.6/t in 2030, $206 in 2050, $402 in 2070, the backstop
# (~$471) by 2100, i.e. growth sliding from 16% to ~3% a year. The file starts in
# 2030 and was extended back to 2025 at its 2030-31 growth rate. Results based on
# it are kept in cap_and_share/output/_backup_yearly_price_20260921/.
# const P_STAR_CS = read_price_path("calibrated_global_cs.csv")

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

"""
    make_uniform_model(recycle_share, members; fixed_temp)

Model configured for the *club price + differentiated rights* regime.

`fixed_temp` holds the damage side of the model at a given path of local
temperatures, by disconnecting the damage component from the climate module.
This is what the "reduced emissions" solve needs: Propositions 1 and 2 compare
the two regimes *at the same damages*, so the equivalent allocation must not
credit a member with the damages avoided by the tighter cap. Left endogenous,
the solve keeps cutting emissions as long as the club's avoided damages exceed
its abatement costs -- which, for a club whose price is below its own marginal
damage, they do for a long way -- and the resulting cut would measure the
ambition of the cap rather than the efficiency gain of uniform pricing.
"""
function make_uniform_model(recycle_share, members::Vector{Int}; fixed_temp = nothing)
    m = MimiNICE2020.create_nice2020()
    if fixed_temp !== nothing
        disconnect_param!(m, :damages, :local_temp_anomaly)
        update_param!(m, :damages, :local_temp_anomaly, fixed_temp)
    end
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

"""
    make_autarky_model(recycle_share; world_pc)

Model configured for a proposal: country-specific prices, revenue kept at home.
With `world_pc`, every country's revenue is pooled instead and paid back as an
equal per capita dividend to the whole world population (POLICY_SC is
All_World), priced or not; within each country the dividend is then shared
across deciles by `recycle_share`, like domestic revenue.
"""
function make_autarky_model(recycle_share; world_pc::Bool = false)
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :switch_custom_transfers,                    0)
    update_param!(m, :switch_recycle,                             1)
    update_param!(m, :switch_global_recycling,                    world_pc ? 1 : 0)
    update_param!(m, :revenue_recycle, :global_recycle_share,     world_pc ? ones(NB_COUNTRY) : zeros(NB_COUNTRY))
    update_param!(m, :revenue_recycle, :switch_global_pc_recycle, world_pc ? 1 : 0)
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

const NPV_IDX = [YEAR_IDX[y] for y in YEARS_NPV]
const NPV_DISC = [1 / (1 + DISCOUNT_RATE)^(y - first(YEARS_NPV)) for y in YEARS_NPV]

npv(series::AbstractVector) = sum(series[NPV_IDX] .* NPV_DISC)

"""
    npv_pop(series, pop)

Total-utilitarian NPV: the per-capita `series` weighted by the population alive
in each year. Every criterion below is of this form, because the welfare of a
country is the sum of the welfare of its residents, not the welfare of a
representative one: a year in which the country has more people counts for more.
This is what puts the factor n_it in the dynamic indifference condition of the
paper, and what makes the Hotelling case a statement about shares of the carbon
budget rather than about per-capita emissions.
"""
npv_pop(series::AbstractVector, pop::AbstractVector) =
    sum(series[NPV_IDX] .* pop[NPV_IDX] .* NPV_DISC)

"NPV of each country's total consumption (population-weighted, USD2017)."
function country_cons_npv(m)
    tot = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    pop = population(m)
    return [npv_pop(@view(tot[:, c]), @view(pop[:, c])) for c in 1:NB_COUNTRY]
end

"""
    world_cons_npv(m, pop)

NPV of world total consumption: the utilitarian counterpart of the global EDE,
with the same consumption concept (post damage, abatement and recycling) and no
inequality aversion. The EDE answers "is the world better off once distribution
is priced in", this one answers "is there more consumption in total".
"""
function world_cons_npv(m, pop)
    tot = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    return sum(vec(sum(tot .* pop, dims = 2))[NPV_IDX] .* NPV_DISC)
end

"""
    entity_welfare_npv(m, idx, pop)

NPV of the equally-distributed-equivalent consumption of an entity made of the
country indices `idx`, weighted by its population (total utilitarianism). A
multi-country entity (the EU27) is aggregated with the model's own
population-weighted EDE, so the bloc figure is comparable to a single country's.
"""
function entity_welfare_npv(m, idx::Vector{Int}, pop::Matrix{Float64})
    ede  = f64(m[:welfare, :cons_EDE_country])
    npop = [sum(@view pop[t, idx]) for t in 1:NB_STEPS]
    length(idx) == 1 && return npv_pop(@view(ede[:, idx[1]]), npop)
    agg = [MimiNICE2020.EDE_aggregated(ede[t, idx], pop[t, idx], ETA) for t in 1:NB_STEPS]
    return npv_pop(agg, npop)
end

world_welfare_npv(m) = npv_pop(f64(m[:welfare, :cons_EDE_global]),
                               vec(sum(population(m), dims = 2)))

"What the proposal delivers to `entity` on the measure the solvers equalise."
target_value(P, entity::AbstractString) = TARGET == "cons" ? P.cons[entity] : P.welfare[entity]

"The same measure for every country, read off a model run."
function objective_by_country(m)
    TARGET == "cons" && return country_cons_npv(m)
    ede = f64(m[:welfare, :cons_EDE_country])
    pop = population(m)
    return [npv_pop(@view(ede[:, c]), @view(pop[:, c])) for c in 1:NB_COUNTRY]
end

"""
    produced_under_target(df)

Whether a results file was solved on the current target. Files written before
the target was recorded have no `target` column; those were solved on the EDE.
"""
produced_under_target(df::DataFrame) =
    hasproperty(df, :target) ? all(==(TARGET), String.(df.target)) : TARGET == "ede"

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

# ── Equal Right ─────────────────────────────────────────────────────────────
# Prices from papers/Equal_Right_prices.xlsx: the 2025 charge per country is
# column I of the "Carbon charge rates" sheet (a rate graduated by climate
# vulnerability and income, $12/t to $240/t), and the escalation is the charge
# path of the "Global 2 (1.6)" sheet, column D, which compounds at 16.4% a year.
# Both are extracted to CSV so the model needs no spreadsheet reader; the six
# model countries the report does not list are priced at their income group's
# median charge (see ER_COVERAGE below), so that the schedule covers the world.
#
# Recycling. Equal Right pools the revenues in a global fund that pays a
# universal dividend, so its runs (both escalation paths) recycle revenue
# equally per capita at the world level rather than domestically. The runs
# until 22 Sept 2026 kept it at home like the other proposals; their outputs are
# in cap_and_share/output/_backup_er_domestic_recycling_20260922/, and
# NICE_ER_RECYCLING=domestic reproduces them.
const ER_RECYCLING = get(ENV, "NICE_ER_RECYCLING", "world_pc")
ER_RECYCLING in ("world_pc", "domestic") || error("NICE_ER_RECYCLING must be world_pc or domestic")
"True when the proposal's revenue is recycled equally per capita at the world level."
world_pc_recycling(name::AbstractString) = startswith(name, "EqualRight") && ER_RECYCLING == "world_pc"

# The report lists 173 of the model's 179 economies. The six it leaves out --
# Aruba, French Polynesia, Hong Kong, Macao, Palestine and Taiwan -- are priced
# at the median charge of their World Bank income group among the economies the
# report does list, so that the schedule covers the world and every country is a
# member of the club. The charge is graded by income and by climate
# vulnerability; income is the part of that grading we can observe for the six,
# and the medians are $15/t (LIC), $42 (LMIC), $96 (UMIC) and $240 (HIC).
# NICE_ER_COVERAGE=listed restores the earlier scenario, where the six were
# unpriced and outside the club.
const ER_COVERAGE = get(ENV, "NICE_ER_COVERAGE", "world")
ER_COVERAGE in ("world", "listed") || error("NICE_ER_COVERAGE must be world or listed")

const EQUAL_RIGHT_PRICE = let d = Dict{Symbol,Float64}()
    df = CSV.read(joinpath(ROOT, "cap_and_share", "data", "equal_right_prices.csv"), DataFrame)
    for r in eachrow(df)
        d[Symbol(r.country)] = Float64(r.price_2025)
    end
    if ER_COVERAGE == "world"
        group_of = Dict{Symbol,Vector{Symbol}}(:LIC => LIC_S, :LMIC => LMIC_S,
                                               :UMIC => UMIC_S, :HIC => HIC_S)
        med = Dict(g => median([d[c] for c in cs if haskey(d, c)]) for (g, cs) in group_of)
        for c in COUNTRIES
            haskey(d, c) && continue
            g = findfirst(g -> c in group_of[g], collect(keys(group_of)))
            g === nothing && continue
            key = collect(keys(group_of))[g]
            d[c] = med[key]
            @info "Equal Right: economy not listed in the report, priced at its income group's median" country = c group = key price = d[c]
        end
    end
    d
end

const EQUAL_RIGHT_FACTOR = let d = Dict{Int,Float64}()
    df = CSV.read(joinpath(ROOT, "cap_and_share", "data", "equal_right_path.csv"), DataFrame)
    for r in eachrow(df)
        d[Int(r.time)] = Float64(r.factor)
    end
    d
end

"""
    equal_right_tax_matrix()

The Equal Right schedule: each country's 2025 charge grown along the proposal's
own global charge path. Capped at the backstop like the others -- at 16.4% a
year the top tier passes any plausible backstop within a decade, so the cap is
what keeps the scenario interpretable rather than unbounded negative emissions.
"""
function equal_right_tax_matrix(; escalation = :own)
    tax = zeros(Float64, NB_STEPS, NB_COUNTRY)
    for (ci, c) in enumerate(COUNTRIES)
        p0 = get(EQUAL_RIGHT_PRICE, c, 0.0)
        p0 == 0.0 && continue
        for (t, y) in enumerate(YEARS)
            y < PRICE_START_YEAR && continue
            f = if escalation === :own
                get(EQUAL_RIGHT_FACTOR, y, EQUAL_RIGHT_FACTOR[maximum(keys(EQUAL_RIGHT_FACTOR))])
            else
                # the 5%/year assumed for the other two proposals, so that what
                # distinguishes Equal Right is its schedule of *levels* rather
                # than the speed at which they rise
                y <= PRICE_LEVEL_UNTIL ? 1.0 : (1 + PRICE_GROWTH_RATE)^(y - PRICE_LEVEL_UNTIL)
            end
            tax[t, ci] = min(p0 * f, PBACKTIME[t])
        end
    end
    return tax
end

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

const CACHE_DIR = joinpath(OUTPUT_BASE, "cache")

# Identifies the welfare criteria the cached proposals were built with; see
# `build_proposal`. "popw" = population-weighted NPVs (total utilitarianism).
const CRITERION_VERSION = "popw-2026-09"

# Everything cached below also depends on the recycling shares (set by the
# reference run at p*) and on p* itself; hashing them into the keys means a new
# price path can never be served results computed on an old one.
const RUN_CONFIG = hash((CRITERION_VERSION, round.(RECYCLE_SHARE, digits = 10),
                         round.(P_STAR, digits = 6)))

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
                        collect(CALIB_YEARS), RUN_CONFIG)), base = 16)
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
# Cache key of a proposal run. World per capita recycling enters it only when it
# is on, so the keys (and caches) of the domestically recycled runs are unchanged.
proposal_key(name, tax) = world_pc_recycling(name) ?
    (name, round.(tax, digits = 6), RUN_CONFIG, :world_pc_recycling) :
    (name, round.(tax, digits = 6), RUN_CONFIG)

function build_proposal(name, tax)
    # A proposal is expensive to build (an autarky run plus the p_ref
    # calibration) and depends only on its own price schedule, so cache it:
    # rebuilding the tables then costs no model runs at all.
    #
    # CRITERION_VERSION is part of the key because a Proposal carries the target
    # values the solvers aim at (`welf`, `consd` and their world counterparts),
    # which are criterion-dependent. When the criteria moved from per-capita NPVs
    # to population-weighted ones, a stale cache silently fed the solver targets
    # a million times too small. Bump this string whenever a criterion changes.
    key   = string(hash(proposal_key(name, tax)), base = 16)
    cpath = joinpath(CACHE_DIR, "proposal_$(lowercase(name))_$key.jls")
    if !FRESH && isfile(cpath)
        try
            P = Serialization.deserialize(cpath)::Proposal
            @info "proposal from cache" name file = basename(cpath)
            return P
        catch err
            @warn "could not read the cached proposal, rebuilding" name err
        end
    end
    P = build_proposal_uncached(name, tax)
    mkpath(CACHE_DIR)
    try
        Serialization.serialize(cpath, P)
    catch err
        @warn "could not cache the proposal" name err
    end
    return P
end

function build_proposal_uncached(name, tax)
    @info "Running proposal scenario" name
    members = proposal_members(tax)
    m = run_autarky!(make_autarky_model(RECYCLE_SHARE; world_pc = world_pc_recycling(name)), tax)
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

    cmc  = country_cons_npv(m)
    consd = Dict(string(COUNTRIES[c]) => cmc[c] for c in 1:NB_COUNTRY)

    return Proposal(name, tax, members, pop, ems, we, ce, wp, cp, ce ./ cp, welf, consd,
                    world_welfare_npv(m), world_cons_npv(m, pop),
                    temperature(m)[YEAR_IDX[2100]], p_ref)
end

"""
    proposal_local_temp(P)

The country-level temperature path under the proposal itself, used to hold
damages fixed in the "reduced emissions" solve (see `make_uniform_model`).
One model run, cached on disk like the proposal itself.
"""
function proposal_local_temp(P::Proposal)
    key  = string(hash(proposal_key(P.name, P.tax)), base = 16)
    path = joinpath(CACHE_DIR, "localtemp_$(lowercase(P.name))_$key.jls")
    if !FRESH && isfile(path)
        try
            return Serialization.deserialize(path)::Matrix{Float64}
        catch err
            @warn "could not read the cached local temperatures, recomputing" P.name err
        end
    end
    m = run_autarky!(make_autarky_model(RECYCLE_SHARE; world_pc = world_pc_recycling(P.name)), P.tax)
    T = f64(m[:pattern_scale, :local_temperature])
    mkpath(CACHE_DIR)
    try
        Serialization.serialize(path, T)
    catch err
        @warn "could not cache the local temperatures" P.name err
    end
    return T
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
#
# COST. ~8-12 runs (~1 min) from the default P_STAR start, but only 1-2 runs when
# warm-started from the previous iteration's price, which is what the option B
# and variant loops do: there it is a ~15 s surcharge per iteration, not a solve.
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
        # The model applies min(p, pbacktime), so any price above the backstop is
        # the backstop, with a flat region above it that teaches the secant
        # nothing: a cached price of $9,000/t took fourteen model runs to walk
        # down to it. Clamp the start, and seed zero caps at the backstop itself.
        p[t] = clamp(p[t], 1e-3, PBACKTIME[t])
        cap[t] <= 1e-4 * scale && (p[t] = PBACKTIME[t])
    end
    pprev = similar(p); eprev = fill(NaN, NB_STEPS)
    # Per-year bracket: the highest price known to leave emissions above the cap
    # and the lowest known to push them below it. Near full abatement emissions
    # fall off a cliff as the price rises, so a secant step or a fixed cut can
    # jump from one side to the other for ever -- a tiny positive cap met at
    # e = 0 by one price and missed by 3.8 GtCO2 at 20% less, as Equal Right's
    # near-zero caps did. Keeping every step inside the bracket, and bisecting
    # whenever a step would leave it, rules that cycle out.
    lo = fill(-Inf, NB_STEPS); hi = fill(Inf, NB_STEPS)
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
            e[t] > cap[t] && (lo[t] = max(lo[t], p[t]))     # price too low
            e[t] < cap[t] && (hi[t] = min(hi[t], p[t]))     # price too high
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
            if isfinite(lo[t]) && isfinite(hi[t]) && !(lo[t] < pnew[t] < hi[t])
                pnew[t] = 0.5 * (lo[t] + hi[t])
            end
            # never above the backstop, for the same reason as the start
            pnew[t] = clamp(pnew[t], 1e-3, PBACKTIME[t])
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

"""
First-order prediction of rho for the paper's tables. The Taylor expansion of
Section 3 gives V_i(t) ~ (e_i(t) - e*_i(t)) p(t), so equivalence holds when
sum_t beta_t p(t) (r_i(t) - e_i(t)) = 0: the equivalent rights are the entity's
own emissions under the proposal, valued at the discounted club price. With
r_i(t) = rho pop_i(t) Ebar_S(t) this pins rho down. Unlike `predicted_rho`, which
only discounts, it weights each year by the price at which rights trade in it.
"""
function predicted_rho_priced(entity::AbstractString, P::Proposal)
    idx = intersect(entity_indices(entity), P.members)
    isempty(idx) && return NaN
    w     = NPV_DISC .* P.p_ref[NPV_IDX]
    own   = vec(sum(P.emissions[:, idx], dims = 2))[NPV_IDX]
    equal = (vec(sum(P.pop[:, idx], dims = 2)) .* P.ebar)[NPV_IDX]
    den   = sum(equal .* w)
    return den > 0 ? sum(own .* w) / den : NaN
end

"""
Average-utilitarian variant of `predicted_rho_priced`:
    rho_hat_i = sum_t beta_t p*_t e^A_it / sum_t beta_t p*_t ebar_t,
with e^A_it the entity's emissions per capita under the proposal and ebar_t the
club's. It drops the factor n_it of equation (rhohat_dyn), i.e. it weighs a year
by the discounted price alone rather than also by the number of people alive in
it. The solvers equalise population-weighted NPVs (total utilitarianism), so
`predicted_rho_priced` is the prediction the theory gives and the one the tables
report; this one is kept as a robustness column, and differs mainly for
fast-growing countries, whose late years the totals version weighs more.
"""
function predicted_rho_pc(entity::AbstractString, P::Proposal)
    idx = intersect(entity_indices(entity), P.members)
    isempty(idx) && return NaN
    w   = NPV_DISC .* P.p_ref[NPV_IDX]
    own = (vec(sum(P.emissions[:, idx], dims = 2)) ./ vec(sum(P.pop[:, idx], dims = 2)))[NPV_IDX]
    den = sum(P.ebar[NPV_IDX] .* w)
    return den > 0 ? sum(own .* w) / den : NaN
end

"`predicted_rho` (discounted, not price-weighted) for a reporting entity."
function predicted_rho_paper(entity::AbstractString, P::Proposal)
    idx = intersect(entity_indices(entity), P.members)
    return isempty(idx) ? NaN : predicted_rho(idx, P)
end

"Naive benchmark: 2025 emissions per capita over the world average in 2025."
function predicted_rho_now(entity::AbstractString, P::Proposal)
    idx = intersect(entity_indices(entity), P.members)
    isempty(idx) && return NaN
    t = YEAR_IDX[2025]
    own = sum(P.emissions[t, idx]) / sum(P.pop[t, idx])
    return own / (P.world_emissions[t] / P.world_pop[t])
end

"""
`predicted_rho_now` for any entity, member of the club or not: a property of the
country, not of the proposal, so the targets table gives it once, by the name.
"""
function rho_now_any(entity::AbstractString, P::Proposal)
    idx = entity_indices(entity)
    isempty(idx) && return NaN
    t = YEAR_IDX[2025]
    return (sum(P.emissions[t, idx]) / sum(P.pop[t, idx])) /
           (P.world_emissions[t] / P.world_pop[t])
end

# The three predictions of rho the combined tables can carry, with the sentence
# that defines each in the table note. `:formula` is the one the theory implies.
const PREDICTORS = Dict(
    :pc      => (predicted_rho_pc,
                 "the country's emissions per capita under the proposal over the club's, each year " *
                 "weighted by the discounted uniform price over 2030--2100 (equation~\\eqref{eq:rhohat_dyn})"),
    :formula => (predicted_rho_priced,
                 "the country's own emissions under the proposal over an equal-per-capita share of " *
                 "the club's, each year weighted by the discounted club price over 2030--2100"),
    :paper   => (predicted_rho_paper,
                 "the country's own emissions under the proposal over an equal-per-capita share of " *
                 "the club's, both discounted over 2030--2100"),
    :now     => (predicted_rho_now,
                 "the country's emissions per capita in 2025 over the world average"))

function solve_rho_single(m, entity::String, P::Proposal;
                          tol_rho = 1e-3, max_iter = 14, verbose = true)
    idx    = entity_indices(entity)
    isempty(idx) && return (rho = NaN, flag = :missing, evals = 0)
    TARGET == "cons" && length(idx) != 1 &&
        error("the consumption target is solved country by country, not for $entity")
    target = target_value(P, entity)
    evals  = Ref(0)

    function f(x)
        evals[] += 1
        run_uniform!(m, rights_single_deviation(idx, x, P), P.p_ref)
        obj = TARGET == "cons" ? objective_by_country(m)[idx[1]] : entity_welfare_npv(m, idx, P.pop)
        return obj - target
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

# COST. 5 runs per country, spread over the workers: 18 countries in 5.0 min, 110
# in 21 min on 4 workers. `coverage` is what keeps this from being 179 countries
# -- the tail of small emitters is predicted rather than solved.
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
                      filter(f -> occursin(Regex("^rho_A_$(lowercase(P.name))_chunk\\d+of\\d+$(TAG)\\.csv\$"),
                                           basename(f)),
                             readdir(OUTPUT_BASE; join = true)))
    for f in files
        df = CSV.read(f, DataFrame)
        if !produced_under_target(df)
            @warn "ignoring a checkpoint solved on another target" file = basename(f) TARGET
            continue
        end
        for r in eachrow(df)
            flag = Symbol(r.flag)
            flag === :predicted && continue
            rho[String(r.country)]   = Float64(r.rho)
            flags[String(r.country)] = flag
        end
    end
    isempty(rho) || @info "option A: resuming" P.name solved = length(rho) from = basename.(files)
    return rho
end

function write_rho_csv(path, rho::Dict{String,Float64}, flags::Dict{String,Symbol})
    ks = sort(collect(keys(rho)))
    CSV.write(path, DataFrame(country = ks, rho = [rho[k] for k in ks],
                              flag = [string(flags[k]) for k in ks],
                              target = fill(TARGET, length(ks))))
end

chunk_path(name, k, n) = joinpath(OUTPUT_BASE, "rho_A_$(lowercase(name))_chunk$(k)of$(n)$(TAG).csv")

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
#
# COST. One model run plus a warm-started recalibration per outer iteration,
# ~1.5-2 min; convergence took 7 iterations (Wolfram) to 29 (EqualRight), so
# 20-35 min per cell. Option B is the cheaper option whenever the club is large:
# its cost is set by the number of iterations, not by the number of members.
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

# Variant 1 of the joint solve, "reduced emissions": every member exactly as
# well off as under the proposal. Nested: the inner problem holds the level L
# (population-weighted mean rho, i.e. the cap relative to the proposal's
# emissions) fixed and equalises the members' relative gains -- a well-behaved
# problem, `solve_option_B(fix_level = true)`; the outer problem is a 1-D secant
# on L until that common gain is zero. Solving level and spread in one loop
# (fix_level = false) does not work: the common gain responds weakly to L (a
# few hundredths of a percent over ten points of cap, because the tighter cap is
# largely paid for by avoided damages), so its secant is swamped by the noise of
# the simultaneous share updates and overshoots.
function solve_joint_indifferent(P::Proposal, rho_init::Vector{Float64};
                                 tol_gain = 1e-5, tol_spread = 1e-5, max_outer = 10,
                                 slope_guess = 3e-3, max_level_step = 0.10,
                                 fixed_temp = proposal_local_temp(P))
    wts = zeros(Float64, NB_COUNTRY)
    for c in P.members
        wts[c] = sum(@view P.pop[CALIB_IDX, c])
    end
    wts ./= sum(wts)
    rho = copy(rho_init)
    hist = Tuple{Float64,Float64}[]            # (level, common gain)
    gap, price = Inf, copy(P.p_ref)
    for k in 1:max_outer
        L = sum(rho .* wts)
        rho, gap, price, g = solve_option_B(P, rho; fix_level = true, tol = tol_spread,
                                            tol_floor = tol_spread, label = "B1.$k", max_iter = 100,
                                            fixed_temp = fixed_temp)
        push!(hist, (L, g))
        @printf("  [B1/%s] outer %d  level = %.4f  common gain = %+.5f%%  spread = %.5f%%\n",
                P.name, k, L, g * 100, gap * 100)
        flush(stdout)
        abs(g) < tol_gain && break
        # secant on the last two levels, else the prior slope (a looser cap, a
        # higher gain: the slope is positive)
        s = length(hist) >= 2 && abs(hist[end][1] - hist[end-1][1]) > 1e-9 ?
            (hist[end][2] - hist[end-1][2]) / (hist[end][1] - hist[end-1][1]) : slope_guess
        s <= 1e-6 && (s = slope_guess)
        Lnew = clamp(L - g / s, L - max_level_step, L + max_level_step)
        rho .*= Lnew / L
    end
    abs(hist[end][2]) < tol_gain || @warn "joint indifference: common gain not driven to zero" P.name hist
    return rho, max(gap, abs(hist[end][2])), price
end

"""
    renorm_rights(rho, P)

Rights proportional to `rho_i * pop_i(t) * Ebar_S(t)`, rescaled year by year so
that they add up to the club's own emissions under the proposal. Only the
*relative* rho matter; this is the allocation of the "increased consumption"
variant (the cap is the proposal's, the surplus is shared through the rho).
"""
renorm_rights(rho::Vector{Float64}, P::Proposal) = rescale_to(rights_from_rho(rho, P), P.club_emissions)

# THE TWO JOINT PROBLEMS (Sept 2026 redefinition of the solves).
#   fix_level = false, renorm = false  -> variant 1, "reduced emissions": every
#       member exactly at its proposal level; the cap is the sum of the rights,
#       so the level of rho is solved too and the whole surplus becomes a
#       tighter cap. Converged on max|gap|.
#   renorm = true                       -> variant 2, "increased consumption":
#       the cap is the proposal's own emissions, year by year; the rho are
#       shares, moved until every member gains the same relative amount, which
#       is the maximin allocation. Converged on the spread of the gains.
#   fix_level = true, renorm = false    -> the pre-Sept 2026 joint solve (level
#       held at the starting vector's, spread solved); kept for reference.
function solve_option_B(P::Proposal, rho_init::Vector{Float64};
                        max_iter = 400, tol = 2e-5, rho_min = -5.0, rho_max = 30.0,
                        fix_level = true, renorm = false, calib_tol = 5e-5, calib_reject = 5e-3,
                        tol_floor = 5e-6, level_step = 0.10, label = "B", fixed_temp = nothing)
    renorm && (fix_level = true)          # with renormalised rights the level is moot
    m = make_uniform_model(RECYCLE_SHARE, P.members; fixed_temp)

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

    target = [target_value(P, string(c)) for c in COUNTRIES]
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
        rights = renorm ? renorm_rights(rho, P) : rights_from_rho(rho, P)
        cap    = renorm ? copy(P.club_emissions) : vec(sum(rights, dims = 2))
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
                                             label = "$(label)/$(P.name) it$it")
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
        obj = objective_by_country(m)
        rel = zeros(Float64, NB_COUNTRY)
        for c in mem
            rel[c] = (obj[c] - target[c]) / scale[c]
        end

        mbar    = sum(rel .* wts)      # common component: moved by the cap level
        dev     = rel .- mbar          # distributional component: moved by the shares
        max_rel = maximum(abs, @view rel[mem])
        # With the level held fixed, the population-weighted mean gap `mbar` is
        # whatever that level implies and no rho can remove it; only the spread
        # around it is B's to solve. Convergence is therefore tested on the
        # distributional residual -- testing max|rel| instead puts the target
        # below |mbar| and the solver can never stop.
        @printf("  [%s/%s] iter %3d  max gap = %.4f%%  mean = %+.4f%%  spread = %.4f%%  step = %.3f  cap = %.1f%% of proposal (club)\n",
                label, P.name, it, max_rel * 100, mbar * 100, maximum(abs, @view dev[mem]) * 100, step_limit,
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
        # With a free level (variant 1) the target is every gap at zero, so the
        # criterion is on max|rel| itself, and the absolute `tol` applies.
        crit  = fix_level ? max(tol_floor, min(tol, abs(mbar) / 2)) : tol
        score = fix_level ? max_dev : max_rel
        if score < best_rel
            best_rel = score; best_rho = copy(rho); best_mbar = mbar
            step_limit = min(0.30, step_limit * 1.3)
        else
            step_limit = max(0.01, step_limit * 0.6)
        end
        score < crit && break

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
        elseif isnan(mbar_prev) || abs(L - L_prev) < 1e-9
            # seed the secant: a surplus (mbar > 0) means the cap can be tightened
            L * (1 - sign(mbar) * 0.02)
        elseif !isnan(mbar_prev) && abs(L - L_prev) > 1e-9 &&
               (mbar - mbar_prev) / (L - L_prev) > 1e-9
            L - mbar / ((mbar - mbar_prev) / (L - L_prev))
        else
            L * (1 - clamp(2 * mbar, -0.2, 0.2))
        end
        # trust region on the level: the mean gap responds weakly to the level
        # (a cap change of several points moves it by hundredths of a percent),
        # so an unclamped secant step can leap to caps the backstop cannot meet
        L_new = clamp(L_new, (1 - level_step) * L, (1 + level_step) * L)

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
    target_gap = fix_level ? max(tol_floor, min(tol, abs(best_mbar) / 2)) : tol
    @printf("  [%s/%s] best %s gap = %.5f%% (target %.5f%%); mean gap = %+.4f%%%s\n",
            label, P.name, fix_level ? "distributional" : "absolute", best_rel * 100, target_gap * 100,
            best_mbar * 100, renorm ? ", the common gain at the proposal's cap" :
                             fix_level ? ", the aggregate surplus at this level" : "")
    best_rel > target_gap &&
        @warn "option B did not reach its tolerance" label P.name best_rel mbar = best_mbar
    # the 4th value is the population-weighted mean gap at the kept vector: the
    # common gain when the level is fixed (existing callers take the first three)
    return best_rho, best_rel, price, best_mbar
end

# ──────────────────────────────────────────────────────────────────────────────
# VARIANTS
# ──────────────────────────────────────────────────────────────────────────────

struct VariantResult
    label::String
    total_rights::Float64      # NPV-window cumulated club rights (GtCO2)
    total_proposal::Float64    # same for the club's emissions under the proposal
    rights_gain_pct::Float64      # proposal emissions over equivalent rights, minus 1, in %
    emissions_change_pct::Float64 # equivalent rights over proposal emissions, minus 1, in %
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

COST. The most expensive part of a cell: ~2 runs per iteration (one evaluation,
one warm-started recalibration), up to `max_iter` iterations, times the two
starting points -- 15-30 min, i.e. nearly the whole variant stage. `max_iter` is
the dial to turn when a run has to be shortened.
"""
function minimax_loss_rights(P::Proposal, rho::Vector{Float64};
                             max_iter = 60, tol = 1e-5, label = "", p_init = nothing,
                             model = nothing, calib_tol = 1e-4)
    mem    = P.members
    m      = model === nothing ? make_uniform_model(RECYCLE_SHARE, P.members) : model
    target = [target_value(P, string(c)) for c in COUNTRIES]
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
        best_worst, best_mult, stall = Inf, copy(mult), 0
        for it in 1:max_iter
            rights = build(mult)
            price, _ = calibrate_price_to_cap(m, rights, P.club_emissions; p_init = price,
                                              tol = calib_tol, max_iter = (it == 1 ? 20 : 8),
                                              label = "$label/$start it$it")
            run_uniform!(m, rights, price)
            obj = objective_by_country(m)
            g = zeros(Float64, NB_COUNTRY)
            for c in mem
                g[c] = (obj[c] - target[c]) / scale[c]
            end
            # Maximin on the relative gain: the objective is minus the smallest
            # gain, so it keeps improving after the last loser disappears --
            # clamping it at zero would stop the search the moment nobody loses
            # and leave the worst-off member's gain unmaximised.
            worst = -minimum(g[c] for c in mem)
            nlose = count(c -> g[c] < 0, mem)
            # Annealed step, not a trust region: a trust region that only shrinks
            # on non-improvement collapses to its floor after a few bad steps and
            # the search then sits still for ever.
            step = max(0.01, 0.20 * 0.93^(it - 1))
            @printf("  [%s/%s] iter %2d  smallest gain = %+.5f%%  losers = %d/%d  step = %.3f\n",
                    label, start, it, -worst * 100, nlose, length(mem), step)
            flush(stdout)
            if worst < best_worst - tol
                best_worst = worst; best_mult = copy(mult); stall = 0
            else
                stall += 1
            end
            # stop once the floor has converged *and* nobody loses; give up after
            # a longer stall if some member cannot be brought up at all
            ((stall >= 8 && nlose == 0) || stall >= 20) && break

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
        @printf("  [%s/%s] best smallest gain = %+.5f%%\n", label, start, -best_worst * 100)
        if best_worst < best_overall
            best_overall = best_worst
            best_rights  = build(best_mult)
            best_price   = price
        end
    end
    @printf("  [%s] kept the better start: smallest gain = %+.5f%% of proposal welfare\n",
            label, -best_overall * 100)
    return best_rights, copy(P.club_emissions), best_price
end

"""
    run_variant(P, rho, variant, label; floor_rho)

Runs one variant: build its rights, calibrate the club price to the cap they
imply, and read the outcome off the model. `floor_rho` is option A's allocation,
used by variant 3.
"""
function run_variant(P::Proposal, rho::Vector{Float64}, variant::Int, label::String;
                     p_init = nothing, model = nothing, floor_rho = nothing,
                     calib_tol = 3e-5, fixed_temp = nothing)
    m = model === nothing ? make_uniform_model(RECYCLE_SHARE, P.members; fixed_temp) : model
    rights, cap = if variant == 2
        r, c, _ = minimax_loss_rights(P, rho; label = "$(label)*", p_init, model = m)
        (r, c)
    else
        variant_rights(P, rho, variant; floor_rho)
    end
    # Warm start: the proposal's own emission-weighted price is a far better
    # opening guess than p*, which was calibrated for a much tighter cap.
    p0 = p_init === nothing ? P.p_ref : p_init
    # The cap must be matched at least as tightly as the search that picked the
    # allocation: minimax_loss_rights calibrates at 1e-4 throughout, so leaving
    # this at calibrate_price_to_cap's 2e-3 default reported the winning
    # allocation at a cap error of 0.01-0.09% -- the same order as the gaps
    # between variants and between the two solves, which made those differences
    # unreadable at the resolution they are quoted to.
    p, err = calibrate_price_to_cap(m, rights, cap; p_init = p0, tol = calib_tol, label = label)
    run_uniform!(m, rights, p)

    # Does the variant leave every member weakly better off than the proposal?
    # Step 3's dominance result is about grandfathering (rights = own autarky
    # emissions); no variant uses that allocation, so this is a question about
    # the run, not a theorem, and it is worth printing.
    ede  = f64(m[:welfare, :cons_EDE_country])
    cmc  = country_cons_npv(m)
    # population-weighted, like the targets they are compared with: `P.welfare`
    # is `entity_welfare_npv`, which carries the population of each year
    mpop = population(m)
    ede_gain  = zeros(Float64, NB_COUNTRY)
    cons_gain = zeros(Float64, NB_COUNTRY)
    for c in 1:NB_COUNTRY
        e = string(COUNTRIES[c])
        w0, c0 = P.welfare[e], P.cons[e]
        ede_gain[c]  = w0 != 0 ? (npv_pop(@view(ede[:, c]), @view(mpop[:, c])) - w0) / abs(w0) * 100 : 0.0
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
    tgain  = TARGET == "cons" ? cons_gain : ede_gain
    losers = [string(COUNTRIES[c]) for c in P.members if tgain[c] < 0]
    @printf("    [%s] %d of %d club members below the proposal on %s%s\n",
            label, length(losers), length(P.members), TARGET == "cons" ? "consumption" : "EDE",
            isempty(losers) ? "" : ": " * join(first(losers, 8), ", ") *
                                   (length(losers) > 8 ? ", ..." : ""))

    tot_r = sum(cap[CALIB_IDX])
    tot_p = sum(P.club_emissions[CALIB_IDX])
    ww    = world_welfare_npv(m)
    wc    = world_cons_npv(m, population(m))
    return VariantResult(label, tot_r, tot_p, (tot_p / tot_r - 1) * 100, (tot_r / tot_p - 1) * 100,
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
    cited = base == "Duflo"       ? "Banerjee, Duflo \\& Greenstone" :
            base == "Wolfram"     ? "Wolfram et al."  :
            base == "EqualRight"  ? "Equal Right"     :
            base == "EqualRight5" ? "Equal Right, 5\\%/yr" : base
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
        println(io, outcome("Emissions change in the coalition (\\%)",
                            k -> ("", fmt_pct_1(v1[k].emissions_change_pct))))
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
# `predicted`: name of a proposal whose block opens with a column giving a
# prediction of rho, left of its p_i; `pred_kind` picks it from PREDICTORS.
function ab_tabular(io, props; color = false, predicted = nothing, pred_kind = :formula)
    got(m, P) = get(RESULTS, (m, P.name), nothing)
    # the paper's tables are printed black; only the slide version highlights
    paint(x) = color ? "\\rose{" * x * "}" : x
    ms = [m for m in ("A", "B") if any(P -> got(m, P) !== nothing, props)]
    isempty(ms) && (ms = ["A"])
    nm = length(ms)
    # the appendix names the two solves rather than lettering them
    solve_name(m) = m == "A" ? "\\mathrm{isolated}" : "\\mathrm{joint}"
    rho_head = nm == 1 ? "\$\\rho_i\$" :
               join(["\$\\rho^{$(solve_name(m))}\$" for m in ms], " & ")
    n = length(props)
    haspred(P) = predicted !== nothing && P.name == predicted
    width(P) = 1 + nm + haspred(P)
    function outcome(label, left, cell)
        cells = String[]
        for P in props
            haspred(P) && push!(cells, "")
            push!(cells, left(P))
            for m in ms
                r = got(m, P)
                push!(cells, r === nothing ? "--" : cell(m, P, r))
            end
        end
        return "  \\textbf{" * label * "} & " * join(cells, " & ") * " \\\\"
    end
    println(io, "\\begin{tabular}{l", join("c"^width(P) for P in props), "}")
    println(io, "  \\toprule")
    println(io, "  & ", join(["\\multicolumn{$(width(P))}{c}{\\textbf{$(display_name(P.name))}}" for P in props], " & "), " \\\\")
    starts = cumsum(vcat(2, [width(P) for P in props[1:end-1]]))
    println(io, "  ", join(["\\cmidrule(lr){$a-$(a + width(P) - 1)}" for (a, P) in zip(starts, props)], " "))
    println(io, "  \\textbf{Country} & ", join([(haspred(P) ? "\$\\hat\\rho_i\$ & " : "") * "\$p_i\$ & " * rho_head
                                             for P in props], " & "), " \\\\")
    println(io, "  \\midrule")
    for e in report_order(props)
        cells = String[]
        for P in props
            idx = intersect(entity_indices(e), P.members)
            haspred(P) && push!(cells, fmt(PREDICTORS[pred_kind][1](e, P); d = 2))
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
    ncol = 1 + sum(width, props)
    section(t) = println(io, "  \\multicolumn{$ncol}{l}{\\textit{$t}} \\\\")
    section("Reduced emissions (variant 1)")
    println(io, outcome("\\quad Emissions change in the coalition (\\%)", P -> "",
                        (m, P, r) -> paint(fmt_pct_1(r.v1.emissions_change_pct))))
    println(io, outcome("\\quad World temp.~2100, change (\\textdegree{}C)", P -> "",
                        (m, P, r) -> paint(fmt_delta(r.v1.temp_2100 - P.temp_2100))))
    println(io, outcome("\\quad World welfare gain (\\%)", P -> "",
                        (m, P, r) -> fmt_pct(r.v1.welfare_gain_pct)))
    println(io, outcome("\\quad World consumption gain (\\%)", P -> "",
                        (m, P, r) -> fmt_pct(r.v1.cons_gain_pct)))
    # isolated: v2 is uniform scaling, so v4 repeats it and is not printed
    for (k, v, t) in ((2, :v2, "Increased consumption (variant 2)"),
                      (3, :v3, "Surplus shared by marginal utility (variant 3)"),
                      (4, :v4, "Surplus shared by uniform scaling (variant 4)"))
        section(t)
        println(io, outcome("\\quad World welfare gain (\\%)", P -> "",
                            (m, P, r) -> (m == "A" && k == 4) ? "=(2)" : fmt_pct(getfield(r, v).welfare_gain_pct)))
        println(io, outcome("\\quad World consumption gain (\\%)", P -> "",
                            (m, P, r) -> (m == "A" && k == 4) ? "=(2)" : fmt_pct(getfield(r, v).cons_gain_pct)))
    end
    println(io, "  \\bottomrule")
    println(io, "\\end{tabular}")
    return ms
end

"""
    write_simple_table(path, props)

The slide version: one method (option A by default), variants 1 and 2 only, no
method superscript on rho. The two blocks are named by what they do rather than numbered, and the
note carries only the club prices, the reference temperatures and the two rules.
"""
function write_simple_table(path::String, props; method = "A")
    got(P) = get(RESULTS, (method, P.name), nothing)
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
        # The club price is calibrated so members' emissions equal the cap, so the
        # emissions change is equivalent rights over the proposal's emissions,
        # minus one; with x the gain in rights it equals 1/(1+x) - 1.
        println(io, row("Emissions change in the coalition (\\%)",
                        (P, r) -> fmt_pct_1(r.v1.emissions_change_pct); rose = true))
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
        ab_tabular(io, props; color = true)
    end
    @info "wrote table" path
end

"The paper version: the same tabular as a float, with caption, label and note."
function write_combined_table(path::String, props; label = "tab:equiv_rights", predicted = nothing,
                              pred_kind = :formula)
    open(path, "w") do io
        println(io, "% Generated by src/equivalent_rights_proposals.jl -- do not edit by hand.")
        println(io, "\\begin{table}[htbp]")
        println(io, "\\centering")
        println(io, "\\small")
        println(io, "\\caption{Club-price rights ratios \$\\rho_i\$ equivalent to the ",
                    join([display_name(P.name) for P in props], ", ", " and "), " proposals}")
        println(io, "\\renewcommand{\\arraystretch}{1.15}")
        println(io, "\\resizebox{\\textwidth}{!}{")   # two solves x two proposals is wide
        ab_tabular(io, props; predicted, pred_kind)
        println(io, "}")
        println(io, "\\label{", label, "}")
        println(io, "\\\\[4pt]")
        println(io, "{\\footnotesize Note: \$p_i\$ is the carbon price the proposal asks of country \$i\$ in ",
                    "2030; the comparison regime instead prices every member of the proposal's own club at ",
                    "one common price (",
                    join([@sprintf("\\\$%.1f/t for %s", P.p_ref[YEAR_IDX[2030]], display_name(P.name))
                          for P in props], ", ", " and "),
                    " in 2030) and leaves non-members unpriced, as the proposal does. \$\\rho_i\$ is the ",
                    "country's allocation as a multiple of an equal-per-capita share of the club's emissions ",
                    "under the proposal, taken at face value in variant 1; \$\\rho^{\\mathrm{isolated}}\$ ",
                    "solves each country on its own, \$\\rho^{\\mathrm{joint}}\$ all of them jointly. ",
                    predicted === nothing ? "" :
                    string("\$\\hat\\rho_i\$ is the predicted \$\\rho_i\$ for the ",
                           display_name(predicted), " schedule: ", PREDICTORS[pred_kind][2], ". "),
                    "The European Union figure aggregates its ",
                    "members' rights, and a country the proposal does not price is outside the club in both ",
                    "regimes, shown at \$p_i=0\$ with no equivalent allocation (`--'). Variant 1 (reduced ",
                    "emissions) lets the equivalent allocation set the cap: in the joint solve every member is ",
                    "held exactly at its level under the proposal; in the isolated solve each country's ratio is ",
                    "solved with all others grandfathered, and the ratios are then applied together. Variants 2 to 4 ",
                    "(increased consumption) keep club emissions at the proposal's and share the surplus: ",
                    "equalising the relative gains of all members (joint variant 2), by scaling the isolated ",
                    "allocation up uniformly (isolated variant 2, also variant 4), and in proportion to population ",
                    "times marginal utility (variant 3). The temperature row is the change from the proposal's own 2100 warming, ",
                    "which is ",
                    join([@sprintf("%.2f\\textdegree{}C for %s", P.temp_2100, display_name(P.name))
                          for P in props], ", ", " and "),
                    ". World welfare is the NPV of the inequality-averse global EDE consumption (\$\\eta=1.5\$); ",
                    "world consumption is mean consumption per capita, which weights every person's consumption equally.}")
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
    FRESH && return 0
    # read_results_for prefers the `_er` files, which carry every scenario: the
    # core summary has no Equal Right rows at all, so a `tables` run that read
    # only that one would quietly drop the Equal Right columns.
    n = 0
    for ((m, name), v) in read_results_for(TAG, props)
        haskey(RESULTS, (m, name)) && continue
        RESULTS[(m, name)] = v
        n += 1
        @info "reusing results from an earlier run" scenario = name option = m
    end
    return n
end

"""
    write_losers_table(path, props)

How many club members end up below their proposal welfare under each method and
variant -- the country-by-country counterpart of the world aggregates. Variant 1
makes every member exactly indifferent by construction, so its count is the
numerical zero of the exercise; the others show what each sharing rule costs.
"""
function write_losers_table(path::String, props; label = "tab:equiv_rights_losers")
    got(m, P) = get(RESULTS, (m, P.name), nothing)
    ms = [m for m in ("A", "B") if any(P -> got(m, P) !== nothing, props)]
    isempty(ms) && return
    nm, n = length(ms), length(props)
    count_below(v, P, field) = begin
        g = getfield(v, field)
        isempty(g) ? nothing : count(c -> g[c] < -LOSS_TOL, P.members)
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
        println(io, "  \\textbf{Variant} & ",
                    join(repeat([join(["\\textbf{$(m == "A" ? "isolated" : "joint")}" for m in ms],
                                      " & ")], n), " & "), " \\\\")
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
        println(io, "\\label{", label, "}")
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

# ──────────────────────────────────────────────────────────────────────────────
# THE TWO SOLVES SIDE BY SIDE
#
# The equivalence can be solved so that members keep their mean consumption (the
# default) or their inequality-averse EDE. The two give different rho, and the
# difference is itself a result: matching the EDE takes systematically fewer
# rights, because moving a high-priced member to the lower club price compresses
# its within-country distribution and the EDE counts that compression as part of
# the compensation, while mean consumption does not.
#
# The tables below carry one column per solve, read from the CSVs the two runs
# leave on disk. Only option B is shown: the joint solve is the one whose
# country-level numbers survive being applied to everyone at once, and dropping
# option A leaves room for the two rho columns.
# ──────────────────────────────────────────────────────────────────────────────

"True when `df` comes from a run that targeted `want` (\"cons\" or \"ede\").
Files written before the `target` column existed were all produced on the EDE."
matches_target(df, want::AbstractString) =
    hasproperty(df, :target) ? all(==(want), String.(df.target)) : want == "ede"

"""
    read_csv_safe(path; tries)

`CSV.read`, but `nothing` rather than an exception when the file cannot be read
or comes back without columns. The results of the other target are read off
disk, and a run solving that target may be rewriting them at that very moment;
a half-written file is worth a retry and then a skip, not a crash that loses
the cell just computed.
"""
function read_csv_safe(path::AbstractString; tries = 3)
    for k in 1:tries
        try
            df = CSV.read(path, DataFrame)
            ncol(df) > 0 && return df
        catch err
            k == tries && @warn "could not read a results file, ignoring it" path err
        end
        sleep(0.5)
    end
    return nothing
end

"""
    read_results_for(tag, props)

The results one target left on disk, keyed `(method, scenario)` exactly like
RESULTS: the variant aggregates, the solved rho, and the per-country gains that
the losing-member counts are made of. `tag` is "" for the consumption target and
"_ede" for the EDE one.

The `_er` files carry the Equal Right scenarios *and* the others, so they are
tried first and the core files only fill what they miss. Reading them in that
order is what lets a `tables` run rebuild the Equal Right columns: the core
summary has no rows for those scenarios at all.
"""
function read_results_for(tag::AbstractString, props)
    want  = tag == "_ede" ? "ede" : "cons"
    store = Dict{Tuple{String,String},NamedTuple}()
    gains = Dict{Tuple{String,String,Int,Symbol},Vector{Float64}}()
    for suffix in ("_er", "")
        gp = joinpath(OUTPUT_BASE, "country_gains$(suffix)$(tag).csv")
        isfile(gp) || continue
        gdf = read_csv_safe(gp)
        (gdf === nothing || !matches_target(gdf, want)) && continue
        for sub in groupby(gdf, [:method, :scenario, :variant])
            k = (String(sub.method[1]), String(sub.scenario[1]), Int(sub.variant[1]))
            idx = Dict(String(r.country) => i for (i, r) in enumerate(eachrow(sub)))
            for (sym, col) in ((:ede, :ede_gain_pct), (:cons, :cons_gain_pct),
                               (:rho_eff, :rho_variant))
                haskey(gains, (k[1], k[2], k[3], sym)) && continue
                hasproperty(sub, col) || continue
                v = zeros(Float64, NB_COUNTRY)
                for (i, c) in enumerate(COUNTRIES)
                    j = get(idx, string(c), 0)
                    j > 0 && (v[i] = Float64(sub[j, col]))
                end
                gains[(k[1], k[2], k[3], sym)] = v
            end
        end
    end
    for suffix in ("_er", "")
        path = joinpath(OUTPUT_BASE, "equivalent_rights_variants$(suffix)$(tag).csv")
        isfile(path) || continue
        df = read_csv_safe(path)
        (df === nothing || !matches_target(df, want)) && continue
        for P in props, m in ("A", "B")
            haskey(store, (m, P.name)) && continue
            rows = df[(String.(df.method) .== m) .& (String.(df.scenario) .== P.name), :]
            nrow(rows) >= 2 || continue
            rp = joinpath(OUTPUT_BASE, "rho_$(lowercase(m))_$(lowercase(P.name))$(tag).csv")
            isfile(rp) || continue
            rdf = read_csv_safe(rp)
            (rdf === nothing || !matches_target(rdf, want)) && continue
            d   = Dict(String(r.country) => Float64(r.rho) for r in eachrow(rdf))
            rho = [get(d, string(c), 0.0) for c in COUNTRIES]
            function vr(k)
                i = findfirst(==(k), rows.variant)
                i === nothing && return nothing
                r = rows[i, :]
                # newer summaries store the rights ratio r/p; older ones only 1 - r/p
                ratio = hasproperty(rows, :rights_ratio) ? Float64(r.rights_ratio) :
                                                           1 - Float64(r.rights_gain_pct) / 100
                return VariantResult("$(m)$(k)/$(P.name)", NaN, NaN,
                                     (1 / ratio - 1) * 100, (ratio - 1) * 100, r.temp_2100,
                                     NaN, r.welfare_gain_pct, NaN, r.cons_gain_pct,
                                     get(gains, (m, P.name, k, :ede), Float64[]),
                                     get(gains, (m, P.name, k, :cons), Float64[]),
                                     get(gains, (m, P.name, k, :rho_eff), Float64[]),
                                     Float64[], NaN)
            end
            v1, v2 = vr(1), vr(2)
            (v1 === nothing || v2 === nothing) && continue
            store[(m, P.name)] = (; rho, v1, v2,
                                  v3 = something(vr(3), v2), v4 = something(vr(4), v2))
        end
    end
    return store
end

"""
    write_targets_table(path, props, ede, cons; losers, label)

The paper table that puts the two solves next to each other: per proposal, the
price it asks in 2030 and the option-B rho that makes members indifferent on
their EDE and on their mean consumption. Below it, the two headline variants,
every row read under both solves, and -- with `losers = true` -- how many members
each one leaves short on each measure.

A proposal that has been solved on only one target keeps its other column at
`--`; nothing is invented to fill it.
"""
#     write_main_table(path, props, welf, cons; method, predicted, pred_kind)
#
# The paper's main table (Sept 2026): the bare `tabular`, no float, caption or
# note -- those live in paper.tex, so the note can be edited with the text. Per
# proposal: the 2030 price p, optionally the first-order prediction \\hat\\rho, and
# the equivalent ratio solved on welfare (NPV of EDE consumption) and on mean
# consumption. Below, the two surplus types, every row read under both criteria.
# `method` is "B" (joint, main text) or "A" (isolated, appendix).
const LOSS_TOL = 0.005   # %, threshold for counting a member as losing
function write_main_table(path::String, props, welf, cons; method = "B",
                          predicted = "Duflo", pred_kind = :formula)
    stores = (("welf", welf), ("cons", cons))
    got(store, P) = get(store, (method, P.name), nothing)
    any(P -> any(s -> got(s[2], P) !== nothing, stores), props) || begin
        @warn "no results on either criterion: not writing" path method
        return
    end
    haspred(P) = predicted !== nothing && P.name == predicted
    width(P)   = 3 + haspred(P)
    ncol       = 1 + sum(width, props)
    # summary rows leave the price (and prediction) cells empty, so that each
    # number sits under the rho column of the criterion it was solved on
    function cells_for(f)
        cells = String[]
        for P in props
            push!(cells, "")
            haspred(P) && push!(cells, "")
            for (_, st) in stores
                r = got(st, P)
                push!(cells, r === nothing ? "--" : f(P, r))
            end
        end
        return cells
    end
    row(lbl, f) = "  " * lbl * " & " * join(cells_for(f), " & ") * " \\\\"
    gains(r, v, field) = getfield(getfield(r, v), field)
    # a member "loses" when it falls short of the proposal by more than LOSS_TOL
    # (in %): the joint solve holds members at their level to within ~0.002%,
    # so a zero threshold would count convergence noise as losses
    nlose(v, field)  = (P, r) -> (g = gains(r, v, field); isempty(g) ? "--" : string(count(c -> g[c] < -LOSS_TOL, P.members)))
    floor_(v, field) = (P, r) -> (g = gains(r, v, field); isempty(g) ? "--" : fmt_pct(minimum(g[c] for c in P.members)))
    open(path, "w") do io
        println(io, "% Generated by src/equivalent_rights_proposals.jl (write_main_table) -- do not edit by hand.")
        println(io, "\\begin{tabular}{l", join("c"^width(P) for P in props), "}")
        println(io, "  \\toprule")
        println(io, "  & ", join(["\\multicolumn{$(width(P))}{c}{\\textbf{$(display_name(P.name))}}" for P in props], " & "), " \\\\")
        starts = cumsum(vcat(2, [width(P) for P in props[1:end-1]]))
        println(io, "  ", join(["\\cmidrule(lr){$a-$(a + width(P) - 1)}" for (a, P) in zip(starts, props)], " "))
        # sub-header: within each block, the two criteria columns are the equivalent rights
        println(io, "  & ", join([join(vcat(fill("", 1 + haspred(P)),
                                            ["\\multicolumn{2}{c}{Equivalent rights}"]), " & ") for P in props], " & "), " \\\\")
        println(io, "  ", join(["\\cmidrule(lr){$(a + width(P) - 2)-$(a + width(P) - 1)}" for (a, P) in zip(starts, props)], " "))
        println(io, "  \\textbf{Country} & ",
                join([string("\$p_{2030}\$", haspred(P) ? " & \$\\hat\\rho\$" : "",
                             " & \$\\rho^{\\mathrm{welf}}\$ & \$\\rho^{\\mathrm{cons}}\$") for P in props], " & "), " \\\\")
        println(io, "  \\midrule")
        for e in report_order(props)
            cells = String[]
            for P in props
                idx = intersect(entity_indices(e), P.members)
                if isempty(idx)
                    push!(cells, "0"); haspred(P) && push!(cells, "--"); push!(cells, "--", "--")
                    continue
                end
                push!(cells, fmt_price(mean(P.tax[YEAR_IDX[2030], idx])))
                haspred(P) && push!(cells, fmt(PREDICTORS[pred_kind][1](e, P); d = 2))
                for (_, st) in stores
                    r = got(st, P)
                    push!(cells, r === nothing ? "--" : fmt(entity_rho(r.rho, e, P); d = 2))
                end
            end
            println(io, "  ", entity_name(e), " & ", join(cells, " & "), " \\\\")
        end
        println(io, "  \\midrule")
        println(io, "  \\multicolumn{", ncol, "}{l}{\\textit{Reduced emissions: every member as well off as under the proposal}} \\\\")
        println(io, row("World temp.~2100, change (\\textdegree{}C)", (P, r) -> fmt_delta(r.v1.temp_2100 - P.temp_2100)))
        println(io, row("Emissions change in the coalition (\\%)", (P, r) -> fmt_pct_1(r.v1.emissions_change_pct)))
        println(io, row("World welfare gain (\\%)", (P, r) -> fmt_pct(r.v1.welfare_gain_pct)))
        println(io, row("World consumption gain (\\%)", (P, r) -> fmt_pct(r.v1.cons_gain_pct)))
        println(io, row("\\quad Members losing on welfare", nlose(:v1, :ede_gain)))
        println(io, row("\\quad Members losing on consumption", nlose(:v1, :cons_gain)))
        println(io, "  \\midrule")
        println(io, "  \\multicolumn{", ncol, "}{l}{\\textit{Increased consumption: coalition emissions as under the proposal}} \\\\")
        println(io, row("World welfare gain (\\%)", (P, r) -> fmt_pct(r.v2.welfare_gain_pct)))
        println(io, row("World consumption gain (\\%)", (P, r) -> fmt_pct(r.v2.cons_gain_pct)))
        println(io, row("\\quad Smallest member gain, welfare", floor_(:v2, :ede_gain)))
        println(io, row("\\quad Smallest member gain, consumption", floor_(:v2, :cons_gain)))
        println(io, row("\\quad Members losing on welfare", nlose(:v2, :ede_gain)))
        println(io, row("\\quad Members losing on consumption", nlose(:v2, :cons_gain)))
        println(io, "  \\bottomrule")
        println(io, "\\end{tabular}")
    end
    @info "wrote table" path
end

# `predicted`/`pred_kind` open that proposal's block on a prediction of rho, as in
# `ab_tabular`; `now_col` puts the 2025 relative emissions right of the country.
function write_targets_table(path::String, props, ede, cons;
                             losers = false, label = "tab:equiv_rights_targets",
                             beamer = false, predicted = nothing, pred_kind = :paper,
                             now_col = false)
    stores = (("EDE", ede), ("cons", cons))
    haspred(P) = predicted !== nothing && P.name == predicted
    width(P) = 3 + haspred(P)
    lead = now_col ? [""] : String[]
    got(store, P) = get(store, ("B", P.name), nothing)
    # the dashes are only worth explaining when some column actually has them
    missing_any = any(P -> any(s -> got(s[2], P) === nothing, stores), props)
    any(P -> any(s -> got(s[2], P) !== nothing, stores), props) || begin
        @warn "no option-B results on either target: not writing" path
        return
    end
    n, ncol = length(props), 1 + now_col + sum(width, props)
    # each scenario spans three columns: the price cell stays empty on summary
    # rows, so the two numbers sit under the rho column they were solved for
    # On a highlighted row the slide version prints the consumption solve -- the
    # one the paper now runs on -- in blue, against the rose of the EDE solve.
    function row(lbl, cell; rose = false)
        paint(x)      = rose ? "\\rose{" * x * "}" : x
        paint_cons(x) = rose && beamer ? "\\blue{" * x * "}" : paint(x)
        cells = copy(lead)
        for P in props
            haspred(P) && push!(cells, "")
            push!(cells, "")
            for (which, st) in stores
                r = got(st, P)
                push!(cells, r === nothing ? "--" :
                             (which == "cons" ? paint_cons : paint)(cell(P, r)))
            end
        end
        return "  " * paint(lbl) * " & " * join(cells, " & ") * " \\\\"
    end
    function losers_row(lbl, v, field)
        cells = copy(lead)
        for P in props
            haspred(P) && push!(cells, "")
            push!(cells, "")
            for (_, st) in stores
                r = got(st, P)
                g = r === nothing ? Float64[] : getfield(getfield(r, v), field)
                push!(cells, isempty(g) ? "--" : string(count(c -> g[c] < -LOSS_TOL, P.members)))
            end
        end
        return "  " * lbl * " & " * join(cells, " & ") * " \\\\"
    end
    # the floor itself: what variant 2 maximises, and the number the losing-member
    # counts only summarise
    function floor_row(lbl, v, field)
        cells = copy(lead)
        for P in props
            haspred(P) && push!(cells, "")
            push!(cells, "")
            for (_, st) in stores
                r = got(st, P)
                g = r === nothing ? Float64[] : getfield(getfield(r, v), field)
                push!(cells, isempty(g) ? "--" : fmt_pct(minimum(g[c] for c in P.members)))
            end
        end
        return "  " * lbl * " & " * join(cells, " & ") * " \\\\"
    end
    open(path, "w") do io
        println(io, "% Generated by src/equivalent_rights_proposals.jl -- do not edit by hand.")
        if beamer
            println(io, "\\centering")
            println(io, "\\scriptsize")
            println(io, "\\renewcommand{\\arraystretch}{0.95}")
        else
            println(io, "\\begin{table}[htbp]")
            println(io, "\\centering")
            println(io, "\\small")
            println(io, "\\caption{Equivalent allocations solved on consumption and on the EDE",
                        losers ? ", with the members each variant leaves short" : "", "}")
            println(io, "\\renewcommand{\\arraystretch}{1.15}")
            # three columns per proposal overflow the text block at \small
            println(io, "\\resizebox{\\textwidth}{!}{")
        end
        println(io, "\\begin{tabular}{l", now_col ? "c" : "", join("c"^width(P) for P in props), "}")
        println(io, "  \\toprule")
        println(io, "  & ", now_col ? " & " : "",
                    join(["\\multicolumn{$(width(P))}{c}{\\textbf{$(display_name(P.name))}}"
                          for P in props], " & "), " \\\\")
        starts = cumsum(vcat(2 + now_col, [width(P) for P in props[1:end-1]]))
        println(io, "  ", join(["\\cmidrule(lr){$a-$(a + width(P) - 1)}"
                                for (a, P) in zip(starts, props)], " "))
        println(io, "  \\textbf{Country} & ", now_col ? "\$\\hat\\rho^{2025}_i\$ & " : "",
                    join([(haspred(P) ? "\$\\hat\\rho_i\$ & " : "") *
                          "\$p_i\$ & \$\\rho^{\\mathrm{EDE}}_i\$ & \$\\rho^{\\mathrm{cons}}_i\$"
                          for P in props], " & "), " \\\\")
        println(io, "  \\midrule")
        for e in report_order(props)
            cells = now_col ? [fmt(rho_now_any(e, first(props)); d = 2)] : String[]
            for P in props
                idx = intersect(entity_indices(e), P.members)
                haspred(P) && push!(cells, fmt(PREDICTORS[pred_kind][1](e, P); d = 2))
                if isempty(idx)
                    push!(cells, "0", "--", "--")
                else
                    push!(cells, fmt_price(mean(P.tax[YEAR_IDX[2030], idx])))
                    for (_, st) in stores
                        r = got(st, P)
                        push!(cells, r === nothing ? "--" : fmt(entity_rho(r.rho, e, P); d = 2))
                    end
                end
            end
            println(io, "  ", entity_name(e), " & ", join(cells, " & "), " \\\\")
        end
        println(io, "  \\midrule")
        println(io, "  \\multicolumn{", ncol, "}{l}{\\textit{Variant 1: the equivalent rights as ",
                    "solved, the allocation sets the cap}} \\\\")
        println(io, row("World temp.~2100, change (\\textdegree{}C)",
                        (P, r) -> fmt_delta(r.v1.temp_2100 - P.temp_2100)))
        println(io, row("Emissions change in the coalition (\\%)",
                        (P, r) -> fmt_pct_1(r.v1.emissions_change_pct); rose = beamer))
        println(io, row("World welfare gain, EDE (\\%)", (P, r) -> fmt_pct(r.v1.welfare_gain_pct);
                        rose = beamer))
        println(io, row("World consumption gain (\\%)", (P, r) -> fmt_pct(r.v1.cons_gain_pct)))
        if losers
            println(io, losers_row("\\quad Members losing on the EDE", :v1, :ede_gain))
            println(io, losers_row("\\quad Members losing on mean consumption", :v1, :cons_gain))
        end
        println(io, "  \\hline")
        println(io, "  \\multicolumn{", ncol, "}{l}{\\textit{Variant 2: rights raised to the ",
                    "coalition's emissions, shared to maximise the smallest gain}} \\\\")
        println(io, row("World welfare gain, EDE (\\%)", (P, r) -> fmt_pct(r.v2.welfare_gain_pct)))
        println(io, row("World consumption gain (\\%)", (P, r) -> fmt_pct(r.v2.cons_gain_pct)))
        if !beamer
            println(io, floor_row("\\quad Smallest member gain, mean consumption", :v2, :cons_gain))
            println(io, floor_row("\\quad Smallest member gain, EDE", :v2, :ede_gain))
        end
        if losers
            println(io, losers_row("\\quad Members losing on the EDE", :v2, :ede_gain))
            println(io, losers_row("\\quad Members losing on mean consumption", :v2, :cons_gain))
        end
        println(io, "  \\bottomrule")
        println(io, "\\end{tabular}")
        beamer || println(io, "}")          # close \resizebox
        if beamer
            println(io, "")
            println(io, "\\vspace{.2cm}")
            println(io, "{\\tiny \\parbox{0.88\\textwidth}{")
            # the paper's note runs off the bottom of a slide: keep this one short
            println(io, "\$p_i\$: the price the proposal asks of \$i\$ in 2030 (\\\$/t), against one ",
                        "club price for all coalition members of ",
                        join([@sprintf("\\\$%.1f/t (%s)", P.p_ref[YEAR_IDX[2030]], display_name(P.name))
                              for P in props], ", ", " and "),
                        ". \$\\rho^{\\mathrm{EDE}}_i\$ and \$\\rho^{\\mathrm{cons}}_i\$ hold each member ",
                        "at, respectively, its equally-distributed-equivalent and its mean consumption ",
                        "per capita",
                        missing_any ? "; `--' marks a proposal not solved on that target" : "",
                        ".}")
        else
            println(io, "\\label{", label, "}")
            println(io, "\\\\[4pt]")
            println(io, "{\\footnotesize ")
        end
        beamer || println(io, "Note: option B (the joint solve) throughout. \$p_i\$ is the ",
                    "carbon price the proposal asks of country \$i\$ in 2030, against one common ",
                    "club price of ",
                    join([@sprintf("\\\$%.1f/t (%s)", P.p_ref[YEAR_IDX[2030]], display_name(P.name))
                          for P in props], ", ", " and "),
                    ". \$\\rho^{\\mathrm{EDE}}_i\$ and \$\\rho^{\\mathrm{cons}}_i\$ are the ",
                    "allocations that leave \$i\$ exactly as well off as under the proposal when ",
                    "the solver targets its equally-distributed-equivalent consumption and its ",
                    "mean consumption per capita respectively, each as a multiple of an equal per ",
                    "capita share of the club's emissions under the proposal; every row below is ",
                    "likewise read under both solves. ",
                    predicted === nothing ? "" :
                    string("\$\\hat\\rho_i\$ is the predicted \$\\rho_i\$ for the ",
                           display_name(predicted), " schedule: ", PREDICTORS[pred_kind][2], ". "),
                    now_col ? "\$\\hat\\rho^{2025}_i\$ is a naive prediction of \$\\rho_i\$, common to \
                               all schedules: $(PREDICTORS[:now][2]). " : "",
                    "The two columns are not a like-for-like ",
                    "comparison of aggregates: in variant 1 they imply different caps, so the world ",
                    "rows differ mainly through avoided damage, while in variant 2 the cap is common ",
                    "and each column raises the floor on its own measure, which a maximin does at ",
                    "some cost to the aggregate. For the schedules shown here matching the EDE ",
                    "takes fewer rights, because moving a high-priced member to the club price ",
                    "compresses its within-country distribution and the EDE counts that compression ",
                    "as compensation; the direction is not universal, and reverses under Equal ",
                    "Right's own 16.4\\%/yr path, whose club price is far above the others'. Countries ",
                    "the proposal does not price are outside the club in both regimes ",
                    "(\$p_i=0\$, no allocation). The proposals reach ",
                    join([@sprintf("%.2f\\textdegree{}C (%s)", P.temp_2100, display_name(P.name))
                          for P in props], ", ", " and "),
                    " in 2100. ",
                    losers ? "A member counts as losing when its NPV over 2030--2100 falls short of \
                              what the proposal itself gives it on that measure; variant 1 makes \
                              every member indifferent on the measure it was solved for, so a count \
                              there is the within-country residue described in the text. " : "",
                    missing_any ? "A column of `--' means that proposal has not been solved on \
                                   that target." : "", "}")
        println(io, beamer ? "}" : "\\end{table}")
    end
    @info "wrote table" path
end

"""
    write_target_tables(props)

The four cross-target tables: Wolfram and Banerjee alone, and the two of them
with Equal Right at 5\\%/yr, each with and without the losing-member counts.
They are written once per run, not once per target, so they pick the other
target's results off disk whichever way round this session was run.
"""
function write_target_tables(props)
    ede  = TARGET == "ede"  ? RESULTS : read_results_for("_ede", props)
    cons = TARGET == "cons" ? RESULTS : read_results_for("", props)
    core = [P for P in props if !is_equal_right(P)]
    isempty(core) && return
    sets = [(core, "")]
    er5  = [P for P in props if P.name == "EqualRight5"]
    isempty(er5) || push!(sets, (vcat(core, er5), "_er5"))
    for (ps, sfx) in sets, lo in (false, true)
        lsfx = lo ? "_losers" : ""
        write_targets_table(joinpath(OUTPUT_BASE, "equivalent_rights_targets$(sfx)$(lsfx).tex"),
                            ps, ede, cons; losers = lo,
                            label = "tab:equiv_rights_targets$(sfx)$(lsfx)")
    end
    # the slide version of the paper's main table: no float, no losing-member
    # counts, and the highlighted rows split rose (EDE) against blue (consumption)
    # the paper's Table 4 and its two variants: `_pred` opens the Banerjee et al.
    # block on the paper's predicted_rho, `_pred_formula` on the price-weighted
    # first-order formula, `_pred_now` gives 2025 relative emissions by the country
    if !isempty(er5) && any(P -> P.name == "Duflo", core)
        write_pred_target_tables(vcat(core, er5), ede, cons)
        # Sept 2026 paper tables: main text (joint), appendix (isolated, and the
        # Equal Right schedule on its own escalation path)
        write_main_table(joinpath(OUTPUT_BASE, "equivalent_rights_main.tex"), vcat(core, er5), ede, cons)
        write_main_table(joinpath(OUTPUT_BASE, "equivalent_rights_main_isolated.tex"), vcat(core, er5), ede, cons;
                         method = "A")
    end
    er = [P for P in props if P.name == "EqualRight"]
    if !isempty(er)
        for m in ("A", "B")
            write_main_table(joinpath(OUTPUT_BASE, "equivalent_rights_equalright_$(m == "A" ? "isolated" : "joint").tex"),
                             vcat(er, er5), ede, cons; method = m, predicted = nothing)
        end
    end
    isempty(er5) || write_targets_table(
        joinpath(OUTPUT_BASE, "equivalent_rights_targets_er5_beamer.tex"),
        vcat(core, er5), ede, cons; beamer = true)
end

"The three predicted-rho versions of the paper's Table 4 (the `_er5_losers` table)."
function write_pred_target_tables(props, ede, cons)
    for (stem, kw) in (("pred", (predicted = "Duflo", pred_kind = :paper)),
                       ("pred_formula", (predicted = "Duflo", pred_kind = :formula)),
                       ("pred_now", (now_col = true,)))
        write_targets_table(joinpath(OUTPUT_BASE, "equivalent_rights_targets_er5_losers_$(stem).tex"),
                            props, ede, cons; losers = true,
                            label = "tab:equiv_rights_targets_er5_losers", kw...)
    end
end

is_equal_right(P::Proposal) = startswith(P.name, "EqualRight")

"""
    flush_outputs(props)

Writes two sets of tables: the established ones, over the proposals the paper
has always carried, and a duplicate set suffixed `_er` that adds the Equal Right
scenarios. The first keeps its structure so existing text and slides are
undisturbed; the second is where Equal Right appears.
"""
function flush_outputs(props)
    core = [P for P in props if !is_equal_right(P)]
    if length(core) < length(props)
        # the appendix's Equal Right table carries only the Equal Right schedules:
        # Wolfram and Banerjee have their own table just above it
        write_table_set(props, "_er"; combined_props = [P for P in props if is_equal_right(P)])
    end
    out = write_table_set(core, "")
    write_target_tables(props)
    return out
end

# COST. No model runs at all: a `tables` invocation is ~2-3 min of Julia package
# load and Mimi model build (plus a cached proposal read), then seconds of I/O.
# That load, not the tables, is why regenerating them is not instant.
"Rewrite everything that can be written from what is in RESULTS right now."
function write_table_set(props, suffix; combined_props = props)
    any(m -> any(P -> haskey(RESULTS, (m, P.name)), props), ("A", "B")) || begin
        @warn "no results for these proposals: not rewriting their tables" suffix
        return DataFrame()
    end
    for m in ("A", "B")
        done = [P for P in props if haskey(RESULTS, (m, P.name))]
        isempty(done) && continue
        r = [RESULTS[(m, P.name)] for P in done]
        write_table(joinpath(OUTPUT_BASE, "equivalent_rights_option_$(m)$(suffix)$(TAG).tex"),
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
                         rights_gain_pct = v.rights_gain_pct,
                         emissions_change_pct = v.emissions_change_pct,
                         rights_ratio = 1 + v.emissions_change_pct / 100,
                         temp_2100 = v.temp_2100,
                         temp_2100_proposal = P.temp_2100,
                         welfare_gain_pct = v.welfare_gain_pct,
                         cons_gain_pct = v.cons_gain_pct, target = TARGET))
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
                               ede_gain_pct = v.ede_gain[c], cons_gain_pct = v.cons_gain[c],
                               target = TARGET))
            end
        end
    end
    isempty(rows_c) ||
        CSV.write(joinpath(OUTPUT_BASE, "country_gains$(suffix)$(TAG).csv"), DataFrame(rows_c))

    write_losers_table(joinpath(OUTPUT_BASE, "equivalent_rights_losers$(suffix)$(TAG).tex"), props;
                       label = "tab:equiv_rights_losers$(suffix)$(TAG)")
    write_beamer_table(joinpath(OUTPUT_BASE, "equivalent_rights_beamer$(suffix)$(TAG).tex"), props)
    write_simple_table(joinpath(OUTPUT_BASE, "equivalent_rights_simple$(suffix)$(TAG).tex"), props)
    write_simple_table(joinpath(OUTPUT_BASE, "equivalent_rights_simple$(suffix)_b$(TAG).tex"), props;
                       method = "B")
    write_combined_table(joinpath(OUTPUT_BASE, "equivalent_rights_combined$(suffix)$(TAG).tex"),
                         combined_props; label = "tab:equiv_rights$(suffix)$(TAG)")
    # the paper's versions: same table, with the Banerjee et al. block opening on
    # a prediction of rho -- `_pred` the paper's predicted_rho, `_pred_formula`
    # the price-weighted first-order formula, `_pred_now` 2025 relative emissions
    if any(P -> P.name == "Duflo", combined_props)
        for (kind, stem) in ((:paper, "pred"), (:formula, "pred_formula"), (:now, "pred_now"))
            write_combined_table(joinpath(OUTPUT_BASE, "equivalent_rights_combined_$(stem)$(suffix)$(TAG).tex"),
                                 combined_props; label = "tab:equiv_rights$(suffix)$(TAG)",
                                 predicted = "Duflo", pred_kind = kind)
        end
    end

    summary = DataFrame(rows)
    # Writing an empty summary would overwrite a good one with nothing, which is
    # exactly what happens when a session rebuilds tables but loads no results.
    if isempty(rows)
        @warn "no results to write: leaving the existing outputs alone" suffix
        return summary
    end
    CSV.write(joinpath(OUTPUT_BASE, "equivalent_rights_variants$(suffix)$(TAG).csv"), summary)
    return summary
end

"""
Option A's rho vector as written by an earlier run, or `nothing`. Lets a B-only
session start from A's answer instead of the closed-form prediction, which is
what `main` does when both options run in one go.
"""
function saved_rho_A(P::Proposal)
    FRESH && return nothing
    path = joinpath(OUTPUT_BASE, "rho_A_$(lowercase(P.name))$(TAG).csv")
    isfile(path) || return nothing
    df = CSV.read(path, DataFrame)
    produced_under_target(df) || return nothing
    d = Dict(String(r.country) => Float64(r.rho) for r in eachrow(df))
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
    path = joinpath(OUTPUT_BASE, "rho_$(lowercase(method))_$(lowercase(P.name))$(TAG).csv")
    isfile(path) || return nothing
    df = CSV.read(path, DataFrame)
    produced_under_target(df) || return nothing
    d = Dict(String(r.country) => Float64(r.rho) for r in eachrow(df))
    n = count(c -> haskey(d, string(COUNTRIES[c])), P.members)
    n < length(P.members) ÷ 2 && return nothing
    return [get(d, string(c), 0.0) for c in COUNTRIES]
end

# COST. 25-45 min for an option A cell, 20-35 min for an option B one. The cell
# prints its own elapsed time ("[A/Duflo] done in 47.0 min -- tables updated") as
# it publishes its tables, so a log is a timing record.
"Solve one (scenario, option) cell, run the four variants, and write the tables."
function solve_cell!(P::Proposal, method::String, props; variants_only = false)
    t0 = time()
    if variants_only
        rho = saved_rho(method, P)
        rho === nothing && (@warn "no saved rho to rerun the variants from" P.name method; return nothing)
        @info "rerunning the variants from a saved solve" scenario = P.name option = method
    end
    # Sept 2026 definitions (criterion x way x surplus type):
    #   isolated (A): rho solved country by country, others grandfathered.
    #       v1 "reduced emissions"     the allocation at face value, its sum the cap;
    #       v2 "increased consumption" the same allocation scaled up uniformly to
    #                                  the proposal's emissions (= uniform scaling).
    #   joint (B):    v1 every member exactly indifferent, level of rho solved, so
    #                    the whole surplus becomes a tighter cap;
    #                 v2 cap = proposal's emissions, rho moved until all members
    #                    gain the same relative amount (maximin).
    #   v3, v4 (appendix, both ways): the v2 surplus shared instead in proportion
    #       to population x marginal utility (v3) or by uniform scaling (v4).
    rho = variants_only ? saved_rho(method, P) : if method == "A"
        d, flags = solve_option_A(P; checkpoint = joinpath(OUTPUT_BASE,
                                 "rho_A_$(lowercase(P.name))_checkpoint$(TAG).csv"))
        write_rho_csv(joinpath(OUTPUT_BASE, "rho_A_$(lowercase(P.name))$(TAG).csv"), d, flags)
        [get(d, string(c), 0.0) for c in COUNTRIES]     # non-members hold no rights
    else
        # warm start: a previous joint solve if one is on disk (the pre-Sept 2026
        # one solved the same spread at a slightly looser level, so it is close),
        # else option A's vector, else the closed-form prediction
        warm = get(RESULTS, ("A", P.name), nothing)
        rho0 = something(saved_rho("B", P), warm === nothing ? saved_rho_A(P) : warm.rho, Some(nothing))
        rho0 === nothing && (rho0 = [c in P.members ? clamp(predicted_rho([c], P), 0.05, 8.0) : 0.0 for c in 1:NB_COUNTRY])
        v, gap, pB = solve_joint_indifferent(P, rho0)
        CSV.write(joinpath(OUTPUT_BASE, "rho_B_$(lowercase(P.name))$(TAG).csv"),
                  DataFrame(country = string.(COUNTRIES), rho = v,
                            member = [c in P.members for c in 1:NB_COUNTRY],
                            max_rel_welfare_gap = fill(gap, NB_COUNTRY),
                            level = fill("free", NB_COUNTRY),
                            target = fill(TARGET, NB_COUNTRY)))
        v
    end

    v1 = run_variant(P, rho, 1, "$(method)1/$(P.name)")
    # uniform scaling: variant 2 of the isolated solve, variant 4 of the joint one
    vs = run_variant(P, rho, 4, "$(method)4/$(P.name)"; p_init = v1.price_path)
    v2 = if method == "A"
        vs
    else
        # equal gains at the proposal's cap, started from the variant-1 shares
        rho2 = variants_only && isfile(joinpath(OUTPUT_BASE, "rho_B2_$(lowercase(P.name))$(TAG).csv")) ?
               let df = CSV.read(joinpath(OUTPUT_BASE, "rho_B2_$(lowercase(P.name))$(TAG).csv"), DataFrame)
                   d = Dict(String(r.country) => Float64(r.rho) for r in eachrow(df))
                   [get(d, string(c), 0.0) for c in COUNTRIES]
               end : first(solve_option_B(P, rho; renorm = true, label = "B2", max_iter = 80))
        CSV.write(joinpath(OUTPUT_BASE, "rho_B2_$(lowercase(P.name))$(TAG).csv"),
                  DataFrame(country = string.(COUNTRIES), rho = rho2,
                            member = [c in P.members for c in 1:NB_COUNTRY],
                            target = fill(TARGET, NB_COUNTRY)))
        run_variant(P, rho2, 4, "$(method)2/$(P.name)"; p_init = vs.price_path)
    end
    v3 = run_variant(P, rho, 3, "$(method)3/$(P.name)"; p_init = vs.price_path)
    v4 = vs
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
                variants_only::Bool = get(ENV, "NICE_VARIANTS_ONLY", "0") in ("1", "true"),
                equal_right::Bool = get(ENV, "NICE_EQUAL_RIGHT", "1") in ("1", "true"))
    isempty(setdiff(methods, ["A", "B"])) || error("methods must be a subset of [\"A\", \"B\"]")
    @info "Building proposal scenarios" methods workers = nworkers()
    props = [build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate)),
             build_proposal("Duflo",   proposal_tax_matrix(duflo_rate))]
    duflo_legacy && push!(props, build_proposal("Duflo_legacy", proposal_tax_matrix(duflo_legacy_rate)))
    if equal_right
        push!(props, build_proposal("EqualRight",  equal_right_tax_matrix(escalation = :own)))
        push!(props, build_proposal("EqualRight5", equal_right_tax_matrix(escalation = :common)))
    end

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
# NICE_ER_RECYCLING ("world_pc", the default, or "domestic": Equal Right's recycling),
# NICE_AUTORUN=0 (load the definitions without running anything).
if myid() == 1 && get(ENV, "NICE_AUTORUN", "1") != "0" &&
   (abspath(PROGRAM_FILE) == abspath(SELF) || isinteractive())
    if length(ARGS) >= 1 && ARGS[1] == "tables"
        # rebuild every table from the results already on disk, solving nothing
        props = [build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate)),
                 build_proposal("Duflo",   proposal_tax_matrix(duflo_rate))]
        if get(ENV, "NICE_EQUAL_RIGHT", "1") in ("1", "true")
            push!(props, build_proposal("EqualRight",  equal_right_tax_matrix(escalation = :own)))
            push!(props, build_proposal("EqualRight5", equal_right_tax_matrix(escalation = :common)))
        end
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
