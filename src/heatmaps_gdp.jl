################################################################################
# Empirical test of Appendix A (uniform_price.pdf):
# correspondence between autarky differentiated prices and uniform price
# with differentiated emission rights.
#
# Scenario A — Autarky:
#   Country i faces p_i = π_i × p*; RoW price adjusted to fix global cap.
#   No international transfers; each country recycles revenue domestically.
#
# Scenario U — Uniform price with varying rights:
#   All countries face p*; country i gets rights r_i = ρ × pop_share × E*.
#   Net transfer = p* × (r_i − E*_i).
#
# Heatmap: relative GDP = (GDP_U − GDP_A) / |GDP_A| × 100
###############################################################################
cd("/Users/constance/Documents/stage/NICE2020")

using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()
using Mimi, MimiFAIRv2, DataFrames, CSV, Statistics, Plots, Plots.Measures, LaTeXStrings

include("nice2020_module.jl")
include("helper_functions.jl")

# ─────────────────────────────────────────────────────────────────────────────
# SETUP
# ─────────────────────────────────────────────────────────────────────────────
base_model = MimiNICE2020.create_nice2020()
nb_steps   = length(dim_keys(base_model, :time))
nb_country = length(dim_keys(base_model, :country))
all_countries = collect(dim_keys(base_model, :country))  # precomputed once

# global carbon tax path from the calibrated cap-and-share scenario
p_star_path = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output",
                           "calibrated_global_cs.csv"), DataFrame)
df_tax.time       = Int.(df_tax.time)
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
for (i, y) in enumerate(collect(dim_keys(base_model, :time)))
    p_star_path[i] = get(tax_dict, y, 0.0)
end

# ─────────────────────────────────────────────────────────────────────────────
# REFERENCE RUN (scenario 5: global cap-and-share)
# Fixes the global emissions cap E* (to get to 1.8°C) and provides reference populations
# ─────────────────────────────────────────────────────────────────────────────
cs_ref = MimiNICE2020.create_nice2020()
update_param!(cs_ref, :switch_custom_transfers,                  0)
update_param!(cs_ref, :switch_recycle,                           1)
update_param!(cs_ref, :switch_global_recycling,                  1)
update_param!(cs_ref, :revenue_recycle, :global_recycle_share,   ones(nb_country))
update_param!(cs_ref, :revenue_recycle, :switch_global_pc_recycle, 1)
update_param!(cs_ref, :switch_footprint,                         1)
update_param!(cs_ref, :switch_transfers_affect_growth,           1)
update_param!(cs_ref, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
update_param!(cs_ref, :abatement, :control_regime,               1)
update_param!(cs_ref, :abatement, :global_carbon_tax,            p_star_path)
run(cs_ref)

# ── this is to make sure the tax doesn't affect intra-country inequality ────
# so we use emissions-proportional recycling shares (distributionally neutral) 
# tax_burden_distr[t,c,q] = share of country c's carbon tax borne by quintile q,
# proportional to that quintile's emissions (via CO2 income elasticity)
# using this as recycle_share makes each quintile receive back exactly what it paid,
# neutralising within-country redistribution so the heatmap reflects only
# inter-country welfare differences
let burden_df = getdataframe(cs_ref, :quantile_recycle => :tax_burden_distr)
    global all_quantiles          = sort(unique(burden_df.quantile))
    global nb_quantile_val        = length(all_quantiles)
    global recycle_share_neutral  = zeros(Float64, nb_country, nb_quantile_val)
    for (ci, csym) in enumerate(all_countries)
        c_str  = string(csym)
        c_rows = filter(r -> string(r.country) == c_str, burden_df)
        for (qi, q) in enumerate(all_quantiles)
            vals = filter(!isnan, Float64.(filter(r -> r.quantile == q, c_rows).tax_burden_distr))
            recycle_share_neutral[ci, qi] = isempty(vals) ? 1.0/nb_quantile_val : mean(vals)
        end
    end
end

### now we get our shared reference dataframes for population and emissions, and build lookups

pop_df       = getdataframe(cs_ref, :grosseconomy => :l)
emissions_df = getdataframe(cs_ref, :emissions    => :E_gtco2)
unique_years = sort(unique(emissions_df.time))

@assert length(unique_years) == nb_steps

pop_lookup       = Dict((r.time, r.country) => r.l       for r in eachrow(pop_df))
emissions_lookup = Dict((r.time, r.country) => r.E_gtco2 for r in eachrow(emissions_df))

global_cap = [sum(filter(r -> r.time == y, emissions_df).E_gtco2) for y in unique_years]
global_pop = [sum(filter(r -> r.time == y, pop_df).l)             for y in unique_years]

# ─── Negishi redistribution weights: w_q ∝ c_q^η (inverse marginal utility) ─
# Carbon tax revenue is recycled to each quintile proportional to 1/U'(c) = c^η.
# This preserves the initial consumption ranking (regressive: richer quintiles
# receive proportionally more). Weights are computed from the reference run
# consumption before tax recycling, averaged over 2025–2035.
η_welfare = try Float64(Mimi.get_param(base_model, :welfare, :elasmu)) catch; 1.5 end

let conso_df = getdataframe(cs_ref, :quantile_recycle => :conso_pc_post_damage_abatement)
    ref_years = Set(filter(y -> 2025 <= y <= 2035, unique_years))
    global recycle_share_negishi = zeros(Float64, nb_country, nb_quantile_val)
    for (ci, csym) in enumerate(all_countries)
        c_str  = string(csym)
        c_rows = filter(r -> string(r.country) == c_str && r.time in ref_years, conso_df)
        weights = zeros(nb_quantile_val)
        for (qi, q) in enumerate(all_quantiles)
            vals = filter(!isnan, Float64.(filter(r -> r.quantile == q, c_rows).conso_pc_post_damage_abatement))
            avg  = isempty(vals) ? 0.0 : mean(vals)
            weights[qi] = avg > 0 ? avg^η_welfare : 0.0
        end
        s = sum(weights)
        recycle_share_negishi[ci, :] = s > 0 ? weights ./ s : fill(1.0/nb_quantile_val, nb_quantile_val)
    end
end
println("Negishi weights computed (η = $η_welfare)")

const DISCOUNT_RATE = 0.03
const YEARS_NPV     = 2030:2100
const OUTPUT_BASE   = joinpath(@__DIR__, "..", "cap_and_share", "output")

target_countries    = ["USA", "COD", "CHN", "IND", "EU27", "RUS", "NGA", "TUR"]
invisible_countries = ["BRA", "MEX", "DEU", "FRA"]
eu27_countries      = Symbol.(split("AUT BEL BGR HRV CYP CZE DNK EST FIN FRA DEU GRC HUN IRL ITA LVA LTU LUX MLT NLD POL PRT ROU SVK SVN ESP SWE"))

pi_vals    = [0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0]
ratio_vals = [0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0]

# abatement exponents — used to compute analytical p_{-i} and hatch flags
θ2_abat    = try Float64(Mimi.get_param(base_model, :abatement, :θ2)) catch; 2.6 end
α_abat     = 1.0 / (θ2_abat - 1.0)
pback = try collect(Float64, Mimi.get_param(base_model, :abatement, :pbacktime)) catch; nothing end


# ─────────────────────────────────────────────────────────────────────────────
# HELPER FUNCTIONS
# ─────────────────────────────────────────────────────────────────────────────

# extract one country/group time series from the wide selected_country_output.csv
function wide_to_long(df_wide, country_name, prefix)
    row = filter(r -> r.country == country_name, df_wide)
    isempty(row) && error("$country_name not found in selected_country_output.csv")
    cols   = filter(c -> startswith(string(c), prefix * "_"), names(df_wide))
    times  = parse.(Int, replace.(string.(cols), prefix * "_" => ""))
    values = [row[1, col] for col in cols]
    return DataFrame(:time => times, Symbol(prefix) => values)
end

# write the three CSVs the heatmap reads per scenario folder
function save_country_series!(folder, country_name)
    df = CSV.read(joinpath(folder, "selected_country_output.csv"), DataFrame)
    CSV.write(joinpath(folder, "gross_output.csv"),
              wide_to_long(df, country_name, "gross_output"))
    CSV.write(joinpath(folder, "consumption_EDE.csv"),
              wide_to_long(df, country_name, "consumption_ede"))
    df_ems = wide_to_long(df, country_name, "co2_emissions")
    df_ems[!, :co2_emissions] = map(x -> abs(x) < 1e-10 ? 0.0 : x, df_ems.co2_emissions)
    CSV.write(joinpath(folder, "emissions.csv"), df_ems)
    CSV.write(joinpath(folder, "country_carbon_tax.csv"),
              wide_to_long(df, country_name, "country_carbon_tax"))
end

# ═════════════════════════════════════════════════════════════════════════════
# SCENARIO A — AUTARKY WITH NEGISHI RECYCLING

# For each country i and each price factor π_i, we set that country's carbon
# tax to π_i × p*, then back out the rest-of-world price p_{-i} from the
# identity p^* = e_i/E^* p_i + e_{-i}/E^* p_{-i}, where e_i is country i's emissions and E^* is the global cap
# and p_i = π_i × p*
# this is equivalent to the autarky scenario in the uniform-price paper
# from this, we get p_{-i} = p^* x (E^* - e_i pi_i) / e_{-i}

# BUT this identity assumes a linear relationship between carbon prices and emissions
# it implies that if averaging the prices based on emission weights results in the exact same global abatement
# however, this is mathematically impossible in the model because the MAC curve is NOT linear,
#and the emissions weights are endogenous to the tax itself
# if we plug this simple p_{−i} into the model, the Rest of the World will reduce its emissions by the wrong amount
# and the sum of E_i + E_{−i} will miss the global_cap
# hence, tthe Newtooon iterations and bisection
# ═════════════════════════════════════════════════════════════════════════════

pi_vals_negishi = [0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0, 2.25, 2.5, 2.75, 3.0, 3.25, 3.5, 3.75, 4.0, 4.25, 4.5, 4.75]

for country_name in [target_countries; invisible_countries]
    is_eu27        = (country_name == "EU27")
    target_symbols = is_eu27 ? eu27_countries : [Symbol(country_name)]
    target_indices = findall(x -> x in target_symbols, all_countries)

    e_i_ref = [sum(get(emissions_lookup, (y, s), 0.0) for s in target_symbols) for y in unique_years]
    omega_i = e_i_ref ./ max.(global_cap, 1e-10)

    println("\nAutarky Negishi | $country_name")

    for pi_i in pi_vals_negishi
        pi_str = replace(string(round(pi_i; digits=2)), "." => "p")
        folder = joinpath(OUTPUT_BASE, country_name, "autarky_negishi_$pi_str")
        isdir(folder) && isfile(joinpath(folder, "consumption_EDE.csv")) && continue

        println("\nAutarky Negishi | $country_name | π = $pi_i")

        if !isnothing(pback)
            mu_ref   = [pback[t] > 0 && p_star_path[t] > 0 ?
                        (min(p_star_path[t], pback[t]) / pback[t])^α_abat : 0.0
                        for t in 1:nb_steps]
            mu_i     = [min(pi_i^α_abat * mu_ref[t], 1.0) for t in 1:nb_steps]
            eff_pi_α = [mu_ref[t] > 1e-10 ? mu_i[t] / mu_ref[t] : pi_i^α_abat
                        for t in 1:nb_steps]
        else
            eff_pi_α = fill(pi_i^α_abat, nb_steps)
        end
        denom     = max.(1.0 .- omega_i, 1e-10)
        raw_ratio = (1.0 .- eff_pi_α .* omega_i) ./ denom
        p_minus_i = p_star_path .* sign.(raw_ratio) .* abs.(raw_ratio) .^ (θ2_abat - 1.0)
        p_minus_i[unique_years .< 2030] .= 0.0

        m = MimiNICE2020.create_nice2020()
        update_param!(m, :quantile_recycle, :elasticity_slope,        0)
        update_param!(m, :switch_custom_transfers,                    0)
        update_param!(m, :switch_recycle,                             0)
        update_param!(m, :switch_global_recycling,                    0)
        update_param!(m, :revenue_recycle, :global_recycle_share,     zeros(nb_country))
        update_param!(m, :revenue_recycle, :switch_global_pc_recycle, 0)
        update_param!(m, :switch_footprint,                           1)
        update_param!(m, :switch_transfers_affect_growth,             1)
        update_param!(m, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
        update_param!(m, :abatement, :control_regime,                 4)
        update_param!(m, :quantile_recycle, :recycle_share,           recycle_share_negishi) # or recycle_share_neutral for the other scenario

        max_err = Inf
        glob_e  = nothing
        e_lk    = nothing
        for iter in 1:4
            tax_mat = Float64[c in target_indices ? pi_i * p_star_path[t] : p_minus_i[t]
                              for t in 1:nb_steps, c in 1:nb_country]
            update_param!(m, :abatement, :direct_country_tax, tax_mat)
            run(m)

            glob_e = getdataframe(m, :emissions => :E_Global_gtco2)
            ctry_e = getdataframe(m, :emissions => :E_gtco2)
            e_lk   = Dict((r.time, r.country) => r.E_gtco2 for r in eachrow(ctry_e))

            max_err = 0.0
            for (t_idx, y) in enumerate(unique_years)
                E_tot    = only(filter(r -> r.time == y, glob_e)).E_Global_gtco2
                E_target = global_cap[t_idx]
                E_target < 0.1 && continue
                abs_err  = abs(E_tot - E_target)
                max_err  = max(max_err, abs_err / cap_norm)
                abs_err / cap_norm < 0.001 && continue

                E_i          = sum(get(e_lk, (y, s), 0.0) for s in target_symbols)
                E_row_actual = E_tot - E_i
                E_row_target = E_target - E_i
                abs(E_row_actual) <= 1e-10 && continue

                if !isnothing(pback) && pback[t_idx] > 0
                    r         = p_minus_i[t_idx] / pback[t_idx]
                    mu_cur    = sign(r) * abs(r)^α_abat
                    abs(1.0 - mu_cur) < 1e-10 && continue
                    sigma_eff = E_row_actual / (1.0 - mu_cur)
                    mu_new    = 1.0 - E_row_target / sigma_eff
                    (isnan(mu_new) || isinf(mu_new)) && continue
                    p_minus_i[t_idx] = pback[t_idx] * sign(mu_new) * abs(mu_new)^(θ2_abat - 1.0)
                end
            end

            println("  [Newton $iter]  max rel. error = $(round(max_err * 100; digits=3))%")
            max_err < 0.001 && (println("  ✓ converged"); break)
        end

        if max_err >= 0.001
            @warn "$country_name π=$pi_i Newton did not converge (Negishi), trying bisection"
            p_low  = !isnothing(pback) ? -copy(pback)  : fill(-1000.0, nb_steps)
            p_high = !isnothing(pback) ? 10.0 .* pback : fill(10000.0, nb_steps)

            for (t_idx, y) in enumerate(unique_years)
                global_cap[t_idx] < 0.1 && continue
                E_tot    = only(filter(r -> r.time == y, glob_e)).E_Global_gtco2
                E_target = global_cap[t_idx]
                if abs(E_tot - E_target) / cap_norm < 0.001
                    p_low[t_idx] = p_high[t_idx] = p_minus_i[t_idx]
                    continue
                end
                E_i          = sum(get(e_lk, (y, s), 0.0) for s in target_symbols)
                E_row_actual = E_tot - E_i
                E_row_target = E_target - E_i
                if E_row_actual > E_row_target
                    p_low[t_idx]  = max(p_low[t_idx],  p_minus_i[t_idx])
                else
                    p_high[t_idx] = min(p_high[t_idx], p_minus_i[t_idx])
                end
            end

            for bis_iter in 1:30
                for t in 1:nb_steps
                    global_cap[t] >= 0.1 && p_low[t] < p_high[t] &&
                        (p_minus_i[t] = 0.5 * (p_low[t] + p_high[t]))
                end
                tax_mat = Float64[c in target_indices ? pi_i * p_star_path[t] : p_minus_i[t]
                                  for t in 1:nb_steps, c in 1:nb_country]
                update_param!(m, :abatement, :direct_country_tax, tax_mat)
                run(m)

                glob_e = getdataframe(m, :emissions => :E_Global_gtco2)
                ctry_e = getdataframe(m, :emissions => :E_gtco2)
                e_lk   = Dict((r.time, r.country) => r.E_gtco2 for r in eachrow(ctry_e))

                max_err = 0.0
                for (t_idx, y) in enumerate(unique_years)
                    global_cap[t_idx] < 0.1 && continue
                    E_tot    = only(filter(r -> r.time == y, glob_e)).E_Global_gtco2
                    E_target = global_cap[t_idx]
                    max_err  = max(max_err, abs(E_tot - E_target) / cap_norm)
                    E_i          = sum(get(e_lk, (y, s), 0.0) for s in target_symbols)
                    E_row_actual = E_tot - E_i
                    E_row_target = E_target - E_i
                    if E_row_actual > E_row_target
                        p_low[t_idx]  = p_minus_i[t_idx]
                    else
                        p_high[t_idx] = p_minus_i[t_idx]
                    end
                end

                println("  [Bisection $bis_iter]  max_err = $(round(max_err * 100; digits=3))%")
                max_err < 0.001 && (println("  ✓ bisection converged"); break)
                bis_iter == 30 && @warn "$country_name π=$pi_i did not converge after bisection (Negishi, max_err=$(round(max_err*100;digits=3))%)"
            end
        end

        mkpath(folder)
        save_nice2020_reduced_output(m, folder)
        save_country_series!(folder, country_name)

        df_ede = CSV.read(joinpath(folder, "consumption_EDE.csv"), DataFrame)
        println("  [Check Output] EDE | Min: $(round(minimum(df_ede.consumption_ede); digits=2)) | Max: $(round(maximum(df_ede.consumption_ede); digits=2)) | Mean: $(round(mean(df_ede.consumption_ede); digits=2))")

        m = nothing
    end
end

# ═════════════════════════════════════════════════════════════════════════════
# SCENARIO U — UNIFORM PRICE WITH NEGISHI RECYCLING

# The global cap stays fixed (same total emissions as scenario 5).  We vary
# the rights allocated to country i by a factor rho relative to its population
# share, then redistribute the remaining rights to other countries in
# proportion to their baseline emissions.
# ═════════════════════════════════════════════════════════════════════════════

for country_name in [target_countries; invisible_countries]
    is_eu27        = (country_name == "EU27")
    target_symbols = is_eu27 ? eu27_countries : [Symbol(country_name)]
    target_indices = findall(x -> x in target_symbols, all_countries)

    target_pop  = [sum(get(pop_lookup, (y, s), 0.0) for s in target_symbols) for y in unique_years]
    pop_share_i = target_pop ./ global_pop

    println("\nUniform Negishi | $country_name")

    for ratio in ratio_vals
        rat_str = replace(string(round(ratio; digits=2)), "." => "p")
        folder  = joinpath(OUTPUT_BASE, country_name, "uniform_negishi_ratio_$rat_str")
        isdir(folder) && isfile(joinpath(folder, "consumption_EDE.csv")) && continue

        rights_i = ratio .* pop_share_i .* global_cap

        println("\nUniform Negishi | $country_name | ρ = $ratio")

        rights_mat = zeros(Float64, nb_steps, nb_country)
        for t in 1:nb_steps
            target_pop[t] == 0 && continue
            y = unique_years[t]
            for idx in target_indices
                s = all_countries[idx]
                rights_mat[t, idx] = get(pop_lookup, (y, s), 0.0) / target_pop[t] * rights_i[t]
            end
            rem       = max(0.0, global_cap[t] - rights_i[t])
            other_ems = sum(get(emissions_lookup, (y, s), 0.0)
                            for s in all_countries if !(s in target_symbols))
            for c in 1:nb_country
                c in target_indices && continue
                s = all_countries[c]
                rights_mat[t, c] = other_ems > 0 ?
                    get(emissions_lookup, (y, s), 0.0) / other_ems * rem :
                    rem / (nb_country - length(target_indices))
            end
        end

        m = MimiNICE2020.create_nice2020()
        update_param!(m, :switch_custom_transfers,                    1)
        update_param!(m, :switch_recycle,                             1)
        update_param!(m, :switch_global_recycling,                    1)
        update_param!(m, :revenue_recycle, :global_recycle_share,     ones(nb_country))
        update_param!(m, :revenue_recycle, :switch_global_pc_recycle, 1)
        update_param!(m, :revenue_recycle, :rights_proposed,          rights_mat)
        update_param!(m, :switch_footprint,                           1)
        update_param!(m, :switch_transfers_affect_growth,             1)
        update_param!(m, :abatement, :control_regime,                 1)
        update_param!(m, :abatement, :global_carbon_tax,              p_star_path)
        update_param!(m, :quantile_recycle, :recycle_share,           recycle_share_negishi)
        run(m)

        mkpath(folder)
        save_nice2020_reduced_output(m, folder)
        save_country_series!(folder, country_name)

        df_ede = CSV.read(joinpath(folder, "consumption_EDE.csv"), DataFrame)
        println("  [Check Output] EDE | Min: $(round(minimum(df_ede.consumption_ede); digits=2)) | Max: $(round(maximum(df_ede.consumption_ede); digits=2)) | Mean: $(round(mean(df_ede.consumption_ede); digits=2))")

        m = nothing
    end
end

#= Checking the result for USA | ρ=0.02:
- Rights = 0.026 GtCO2 — USA pop share is ~4.2%, so ρ=0.02 × 4.2% × E* gives a tiny allocation
- Emissions = 4.937 GtCO2 — USA emissions under p* after abatement. Pre-policy US emissions are ~5.5 GtCO2, so ~4.9 GtCO2 with
  a meaningful carbon price in 2030 is plausible.
- Net transfer = -209.12 bn USD — USA has to pay for nearly all its emissions
- The implied carbon price is:  p* = 209.12 bn / (4.937 - 0.026) GtCO2 = 209.12 / 4.911 ≈ 42.6 $/tCO2 -> checks out
=#

# ═════════════════════════════════════════════════════════════════════════════
# HEATMAP — NPV OF RELATIVE GDP GAIN: UNIFORM vs AUTARKY
#
# For each (rho, pi) pair we compute relative variation of the discounted values for both scenarios
# ((NPV_uniform - NPV_autarky) / NPV_autarky) × 100  over YEARS_NPV
# Positive values mean the uniform scenario is better (in terms of GDP) for country i; negative values mean autarky is.
# ═════════════════════════════════════════════════════════════════════════════

parse_pval(name, sep) = parse(Float64, replace(split(basename(name), sep)[end], "p" => "."))
npv_go(df) = net_present_value(df, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "gross_output")

function log_heatmap_plot(pi_hm, ratio_hm, results, clims, color_scheme; kwargs...)
    lpi  = log10.(pi_hm)
    lrat = log10.(ratio_hm)
    n_pi    = length(pi_hm)
    n_ratio = length(ratio_hm)
    dpi  = n_pi  > 1 ? (lpi[end]  - lpi[1])  / (2 * (n_pi  - 1)) : 0.5
    drat = n_ratio > 1 ? (lrat[end] - lrat[1]) / (2 * (n_ratio - 1)) : 0.5
    p = heatmap(lpi, lrat, results;
        color  = color_scheme,
        clims  = clims,
        xticks = (lpi, string.(pi_hm)),
        yticks = (lrat, string.(ratio_hm)),
        xlims  = (lpi[1]  - dpi,  lpi[end]  + dpi),
        ylims  = (lrat[1] - drat, lrat[end] + drat),
        kwargs...
    )
    return p, lpi, lrat
end

function try_parse_pval(name, sep) # to ignore the negishi folders in these heatmaps
    raw_str = split(basename(name), sep)[end]
    cleaned = replace(raw_str, "p" => ".")
    return tryparse(Float64, cleaned)
end


## example of numbers for autarky, USA, pi=2.0 
country = "USA"
pi_i    = 2.0
pi_str  = replace(string(round(pi_i; digits=2)), "." => "p")
folder  = joinpath(OUTPUT_BASE, country, "autarky_negishi_$pi_str")

tax_df = CSV.read(joinpath(folder, "country_carbon_tax.csv"), DataFrame)
ems_df = CSV.read(joinpath(folder, "emissions.csv"), DataFrame)

table_years = 2030:10:2100
rows = [(
    year,
    round(get(tax_dict, year, 0.0); digits=1),                           # p*
    round(only(filter(r -> r.time == year, tax_df)).country_carbon_tax; digits=1),  # p_i
    round(only(filter(r -> r.time == year, ems_df)).co2_emissions; digits=2),       # E_i
) for year in table_years]

tbl = DataFrame(rows, [:Year, :p_star, :p_i, :E_i])
println(tbl)
using PrettyTables
pretty_table(tbl; backend=:latex)

table_years = 2030:10:2100

p_star_vals = [round(get(tax_dict, y, 0.0);                                              digits=1) for y in table_years]
p_i_vals    = [round(only(filter(r -> r.time == y, tax_df)).country_carbon_tax;          digits=1) for y in table_years]
E_i_vals    = [round(only(filter(r -> r.time == y, ems_df)).co2_emissions;               digits=2) for y in table_years]

tbl = DataFrame(
    :Variable => ["p* (USD/tCO₂)", "p_i (USD/tCO₂)", "E_i (GtCO₂)"],
    [Symbol(y) => [p_star_vals[i], p_i_vals[i], E_i_vals[i]]
    for (i, y) in enumerate(table_years)]...
)

pretty_table(tbl; backend=:latex)

## example of numbers for uniform price, COD, rho=5

country = "COD"
ratio   = 5.0

rat_str        = replace(string(round(ratio; digits=2)), "." => "p")
folder         = joinpath(OUTPUT_BASE, country, "uniform_negishi_ratio_$rat_str")

ems_df = CSV.read(joinpath(folder, "emissions.csv"), DataFrame)

# recalcul des droits (même formule que dans la boucle de simulation)
t_syms        = [Symbol(country)]   # remplacer par eu27_countries si EU27
t_pop         = [sum(get(pop_lookup, (y, s), 0.0) for s in t_syms) for y in unique_years]
pop_share_vec = t_pop ./ global_pop
rights_vec    = ratio .* pop_share_vec .* global_cap
year_idx      = Dict(y => i for (i, y) in enumerate(unique_years))

table_years = 2030:10:2100
tbl = DataFrame(
    :Variable => ["p* (USD/tCO₂)", "r_i (GtCO₂)", "E_i (GtCO₂)", "Transfert (bn USD)"],
    [Symbol(y) => [
        round(get(tax_dict, y, 0.0); digits=1),
        round(rights_vec[year_idx[y]]; digits=3),
        round(only(filter(r -> r.time == y, ems_df)).co2_emissions; digits=3),
        round(get(tax_dict, y, 0.0) *
            (rights_vec[year_idx[y]] -
                only(filter(r -> r.time == y, ems_df)).co2_emissions); digits=1),
    ] for y in table_years]...
)

pretty_table(tbl; backend=:latex)

# ═════════════════════════════════════════════════════════════════════════════
# WELFARE HEATMAP — NPV OF RELATIVE EDE GAIN: UNIFORM vs AUTARKY (NEGISHI)
#
# Reads consumption_EDE.csv from the Negishi scenario folders.
# EDE uses Atkinson equal weights; the Negishi recycling determines the
# within-country distribution of carbon revenue, not the welfare evaluation.
# ═════════════════════════════════════════════════════════════════════════════

function load_welfare_heatmap(country, omega_i, pop_share_i, target_pis)
    cpath   = joinpath(OUTPUT_BASE, country)
    a_all   = sort(filter(d -> isdir(d) && startswith(basename(d), "autarky_negishi_"),
                         readdir(cpath; join=true)); by = d -> parse_pval(d, "autarky_negishi_"))
    a_pairs = filter(x -> any(isapprox(x[2], t; rtol=1e-3) for t in target_pis),
                    [(d, parse_pval(d, "autarky_negishi_")) for d in a_all])
    a_dirs  = first.(a_pairs)
    u_dirs  = sort(filter(d -> isdir(d) && startswith(basename(d), "uniform_negishi_ratio_"),
                         readdir(cpath; join=true)); by = d -> parse_pval(d, "uniform_negishi_ratio_"))
    (isempty(a_dirs) || isempty(u_dirs)) && return nothing

    pi_hm    = last.(a_pairs)
    ratio_hm = parse_pval.(u_dirs, "uniform_negishi_ratio_")

    read_ede_npv(d) = begin
        path = joinpath(d, "consumption_EDE.csv")
        !isfile(path) && return (NaN, false)
        df    = CSV.read(path, DataFrame)
        rows  = filter(r -> first(YEARS_NPV) <= r.time <= last(YEARS_NPV), df)
        valid = filter(r -> !isnan(r.consumption_ede), rows)
        nrow(valid) == 0 && return (NaN, false)
        val = net_present_value(valid, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "consumption_ede")
        return (val, nrow(valid) < nrow(rows))
    end

    a_results   = read_ede_npv.(a_dirs)
    a_npvs      = first.(a_results)
    a_truncated = last.(a_results)
    u_results   = read_ede_npv.(u_dirs)
    u_npvs      = first.(u_results)

    results = Float64[
        isnan(u_npvs[i]) || isnan(a_npvs[j]) || abs(a_npvs[j]) < 1e-6 ? NaN :
            (u_npvs[i] - a_npvs[j]) / abs(a_npvs[j]) * 100
        for i in eachindex(u_dirs), j in eachindex(a_dirs)
    ]

    autarky_subsidy = [any((1.0 .- π^α_abat .* omega_i) .< 0.0) for π in pi_hm]
    uniform_bad     = [any(pop_share_i .* ρ .> 1.0)              for ρ in ratio_hm]

    return (results           = results,
            pi_hm             = pi_hm,
            ratio_hm          = ratio_hm,
            autarky_truncated = a_truncated,
            autarky_subsidy   = autarky_subsidy,
            uniform_bad       = uniform_bad)
end

pi_vals_heatmap = [0.1, 0.3, 0.5, 0.75, 1.0, 1.25, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0]

store_welfare = Dict(c => begin
    t_syms = (c == "EU27") ? eu27_countries : [Symbol(c)]
    ω  = [sum(get(emissions_lookup,(y,s),0.0) for s in t_syms) for y in unique_years] ./
         max.(global_cap, 1e-10)
    ps = [sum(get(pop_lookup,(y,s),0.0) for s in t_syms) for y in unique_years] ./ global_pop
    load_welfare_heatmap(c, ω, ps, pi_vals_heatmap)
end for c in target_countries)
filter!(p -> !isnothing(p.second), store_welfare)

all_welf_vals  = vcat([filter(!isnan, vec(d.results)) for d in values(store_welfare)]...)
clim_welf      = isempty(all_welf_vals) ? 5.0 : quantile(abs.(all_welf_vals), 0.95)
shared_clims_w = (-clim_welf, clim_welf)

for country in target_countries
    !haskey(store_welfare, country) && continue
    d = store_welfare[country]

    # apply supervisor range: π ∈ [0.5, 2.0], ρ ≤ 10
    pi_mask    = (d.pi_hm .>= 0.3) .& (d.pi_hm .<= 5.0)
    ratio_mask = d.ratio_hm .<= 10.0
    pi_sub    = d.pi_hm[pi_mask]
    ratio_sub = d.ratio_hm[ratio_mask]
    res_sub   = d.results[ratio_mask, pi_mask]
    asub      = d.autarky_subsidy[pi_mask]
    ubad      = d.uniform_bad[ratio_mask]
    n_ratio, n_pi = size(res_sub)

    p, lpi, lrat = log_heatmap_plot(pi_sub, ratio_sub, res_sub, shared_clims_w, cgrad(:RdBu);
        xlabel          = "\n" * L"Autarky: Price factor $\pi_i$  ($p_i = \pi_i \cdot p^*$)",
        ylabel          = L"Uniform price: Rights factor $\rho_i$  ($r_i = \rho_i \cdot e^*_i$)",
        colorbar_title  = "\nWelfare in Uniform relative to Autarky\n(% of EDE consumption NPV)",
        size=(800, 600), right_margin=16mm, left_margin=6mm,
        bottom_margin=10mm, top_margin=4mm, frame=:axes, tickdir=:out,
        tickfontsize=11, guidefontsize=12, colorbar_titlefontsize=10, legendfontsize=9,
    )

    # white cell borders — drawn over heatmap, under contour/scatter
    for bx in [(lpi[i] + lpi[i+1]) / 2 for i in 1:n_pi-1]
        vline!(p, [bx]; color=:white, lw=0.8, label="")
    end
    for by in [(lrat[i] + lrat[i+1]) / 2 for i in 1:n_ratio-1]
        hline!(p, [by]; color=:white, lw=0.8, label="")
    end

    contour!(p, lpi, lrat, res_sub;
        levels=[0.0], color=:black, lw=1.5, label="Indifference")

    subsidy_xy = [(lpi[j], lrat[i])
                for i in 1:n_ratio, j in 1:n_pi
                if asub[j] && !ubad[i]]
    bad_xy     = [(lpi[j], lrat[i])
                for i in 1:n_ratio, j in 1:n_pi if ubad[i]]

    !isempty(subsidy_xy) && scatter!(p, first.(subsidy_xy), last.(subsidy_xy);
      markershape=:circle, markersize=4, markercolor=:white, markeralpha=0.7,
      markerstrokecolor=:dimgrey, markerstrokewidth=1, label="RoW subsidized (autarky)")

    !isempty(bad_xy) && scatter!(p, first.(bad_xy), last.(bad_xy);
      markershape=:x, markersize=5, markercolor=:dimgrey,
      markerstrokecolor=:black, markerstrokewidth=1, label="RoW rights < 0 (uniform)")

    j1 = findfirst(==(1.0), pi_sub)
    if !isnothing(j1)
        col = res_sub[:, j1]
        for i in 1:length(col)-1
            if !isnan(col[i]) && !isnan(col[i+1]) && col[i] * col[i+1] <= 0
                t    = col[i] / (col[i] - col[i+1])
                lρ   = lrat[i] + t * (lrat[i+1] - lrat[i])
                lpi1 = lpi[j1]
                xl, yl = xlims(p)[1], ylims(p)[1]
                plot!(p, [lpi1, lpi1], [yl, lρ]; color=:grey, lw=1.2, linestyle=:dot, label="")
                plot!(p, [xl, lpi1],   [lρ, lρ]; color=:grey, lw=1.2, linestyle=:dot, label="")
                scatter!(p, [lpi1], [lρ]; markershape=:cross, markersize=6,
                         markercolor=:black, markeralpha=0.8, markerstrokecolor=:black,
                         markerstrokewidth=1.5, label="")
                break
            end
        end
    end

    savefig(p, joinpath(OUTPUT_BASE, country, "Welfare_Heatmap_$(country).pdf"))
    savefig(p, joinpath(OUTPUT_BASE, country, "Welfare_Heatmap_$(country).png"))
    println("Saved: Welfare_Heatmap_$country (.pdf + .png)")
end

# ═════════════════════════════════════════════════════════════════════════════
# GDP HEATMAP — NPV OF RELATIVE GDP GAIN: UNIFORM vs AUTARKY (NEGISHI)
#
# Reads gross_output.csv from the Negishi scenario folders.
# ═════════════════════════════════════════════════════════════════════════════

function load_gdp_negishi_heatmap(country, omega_i, pop_share_i, target_pis)
    cpath   = joinpath(OUTPUT_BASE, country)
    a_all   = sort(filter(d -> isdir(d) && startswith(basename(d), "autarky_negishi_"),
                         readdir(cpath; join=true)); by = d -> parse_pval(d, "autarky_negishi_"))
    a_pairs = filter(x -> any(isapprox(x[2], t; rtol=1e-3) for t in target_pis),
                    [(d, parse_pval(d, "autarky_negishi_")) for d in a_all])
    a_dirs  = first.(a_pairs)
    u_dirs  = sort(filter(d -> isdir(d) && startswith(basename(d), "uniform_negishi_ratio_"),
                         readdir(cpath; join=true)); by = d -> parse_pval(d, "uniform_negishi_ratio_"))
    (isempty(a_dirs) || isempty(u_dirs)) && return nothing

    pi_hm    = last.(a_pairs)
    ratio_hm = parse_pval.(u_dirs, "uniform_negishi_ratio_")

    read_gdp_npv(d) = begin
        path = joinpath(d, "gross_output.csv")
        !isfile(path) && return (NaN, false)
        df    = CSV.read(path, DataFrame)
        rows  = filter(r -> first(YEARS_NPV) <= r.time <= last(YEARS_NPV), df)
        valid = filter(r -> !isnan(r.gross_output), rows)
        nrow(valid) == 0 && return (NaN, false)
        val = net_present_value(valid, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "gross_output")
        return (val, nrow(valid) < nrow(rows))
    end

    a_results   = read_gdp_npv.(a_dirs)
    a_npvs      = first.(a_results)
    a_truncated = last.(a_results)
    u_results   = read_gdp_npv.(u_dirs)
    u_npvs      = first.(u_results)

    results = Float64[
        isnan(u_npvs[i]) || isnan(a_npvs[j]) || abs(a_npvs[j]) < 1e-6 ? NaN :
            (u_npvs[i] - a_npvs[j]) / abs(a_npvs[j]) * 100
        for i in eachindex(u_dirs), j in eachindex(a_dirs)
    ]

    autarky_subsidy = [any((1.0 .- π^α_abat .* omega_i) .< 0.0) for π in pi_hm]
    uniform_bad     = [any(pop_share_i .* ρ .> 1.0)              for ρ in ratio_hm]

    return (results           = results,
            pi_hm             = pi_hm,
            ratio_hm          = ratio_hm,
            autarky_truncated = a_truncated,
            autarky_subsidy   = autarky_subsidy,
            uniform_bad       = uniform_bad)
end

store_gdp_negishi = Dict(c => begin
    t_syms = (c == "EU27") ? eu27_countries : [Symbol(c)]
    ω  = [sum(get(emissions_lookup,(y,s),0.0) for s in t_syms) for y in unique_years] ./
         max.(global_cap, 1e-10)
    ps = [sum(get(pop_lookup,(y,s),0.0) for s in t_syms) for y in unique_years] ./ global_pop
    load_gdp_negishi_heatmap(c, ω, ps, pi_vals_negishi)
end for c in target_countries)
filter!(p -> !isnothing(p.second), store_gdp_negishi)

all_gdp_vals  = vcat([filter(!isnan, vec(d.results)) for d in values(store_gdp_negishi)]...)
clim_gdp      = isempty(all_gdp_vals) ? 5.0 : quantile(abs.(all_gdp_vals), 0.95)
shared_clims_g = (-clim_gdp, clim_gdp)

for country in target_countries
    !haskey(store_gdp_negishi, country) && continue
    d = store_gdp_negishi[country]

    # subset to display range
    pi_mask    = (d.pi_hm    .>= 0.3) .& (d.pi_hm    .<= 5.0)
    ratio_mask =  d.ratio_hm .<= 10.0

    pi_sub    = d.pi_hm[pi_mask]       # linear values
    ratio_sub = d.ratio_hm[ratio_mask]
    res_sub   = d.results[ratio_mask, pi_mask]
    asub      = d.autarky_subsidy[pi_mask]
    ubad      = d.uniform_bad[ratio_mask]
    n_ratio, n_pi = size(res_sub)

    # axis positions
    # π is the policy-relevant axis; linear keeps π=1 visually central.
    # ρ spans 0→10 so log helps, but linear is also fine.
    x_pos = pi_sub                          # linear x positions
    y_pos = log.(ratio_sub)                 # log y positions (swap to ratio_sub if preferred)

    xtick_vals = pi_vals_negishi
    xtick_pos  = xtick_vals                 # same as vals since linear
    xtick_labs = string.(xtick_vals)

    ytick_vals = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
    ytick_pos  = ytick_vals
    ytick_labs = string.(ytick_vals)

    # apply supervisor range: π ∈ [0.5, 2.0], ρ ≤ 10
    # pi_mask    = (d.pi_hm .>= 0.3) .& (d.pi_hm .<= 5.0)
    # ratio_mask = d.ratio_hm .<= 10.0
    # pi_sub    = d.pi_hm[pi_mask]
    # ratio_sub = d.ratio_hm[ratio_mask]
    # res_sub   = d.results[ratio_mask, pi_mask]
    # asub      = d.autarky_subsidy[pi_mask]
    # ubad      = d.uniform_bad[ratio_mask]
    # n_ratio, n_pi = size(res_sub)

    p, lpi, lrat = log_heatmap_plot(pi_sub, ratio_sub, res_sub, shared_clims_g, cgrad(:RdBu);
        xlabel          = "\n" * L"Autarky: Price factor $\pi_i$  ($p_i = \pi_i \cdot p^*$)",
        ylabel          = L"Uniform price: Rights ratio $\rho_i$  ($r_i = \rho_i \cdot e^*_i$)",
        colorbar_title  = "\nNPV GDP gain (%)\n(Uniform / Autarky, Negishi)",
        xticks         = (xtick_pos, xtick_labs),
        yticks         = (ytick_pos, ytick_labs),
        size           = (780, 580),
        right_margin   = 14Plots.mm,
        left_margin    = 8Plots.mm,
        bottom_margin  = 10Plots.mm,
        top_margin     = 6Plots.mm,
        frame          = :axes,
        tickdir        = :out,
        tickfontsize   = 10,
        guidefontsize  = 11,
        colorbar_titlefontsize = 9,
        legendfontsize = 9,
        legend         = :topright,
        dpi            = 300,
    )

    # white cell borders — drawn over heatmap, under contour/scatter
    for bx in [(lpi[i] + lpi[i+1]) / 2 for i in 1:n_pi-1]
        vline!(p, [bx]; color=:white, lw=0.8, label="")
    end
    for by in [(lrat[i] + lrat[i+1]) / 2 for i in 1:n_ratio-1]
        hline!(p, [by]; color=:white, lw=0.8, label="")
    end

    contour!(p, lpi, lrat, res_sub;
        levels=[0.0], color=:black, lw=1.5, label="Indifference")

    subsidy_xy = [(lpi[j], lrat[i])
                for i in 1:n_ratio, j in 1:n_pi
                if asub[j] && !ubad[i]]
    bad_xy     = [(lpi[j], lrat[i])
                for i in 1:n_ratio, j in 1:n_pi if ubad[i]]

    !isempty(subsidy_xy) && scatter!(p, first.(subsidy_xy), last.(subsidy_xy);
      markershape=:circle, markersize=4, markercolor=:white, markeralpha=0.7,
      markerstrokecolor=:dimgrey, markerstrokewidth=1, label="RoW subsidized (autarky)")

    !isempty(bad_xy) && scatter!(p, first.(bad_xy), last.(bad_xy);
      markershape=:x, markersize=5, markercolor=:dimgrey,
      markerstrokecolor=:black, markerstrokewidth=1, label="RoW rights < 0 (uniform)")

    j1 = findfirst(==(1.0), pi_sub)
    if !isnothing(j1)
        col = res_sub[:, j1]
        for i in 1:length(col)-1
            if !isnan(col[i]) && !isnan(col[i+1]) && col[i] * col[i+1] <= 0
                t    = col[i] / (col[i] - col[i+1])
                lρ   = lrat[i] + t * (lrat[i+1] - lrat[i])
                lpi1 = lpi[j1]
                xl, yl = xlims(p)[1], ylims(p)[1]
                plot!(p, [lpi1, lpi1], [yl, lρ]; color=:grey, lw=1.2, linestyle=:dot, label="")
                plot!(p, [xl, lpi1],   [lρ, lρ]; color=:grey, lw=1.2, linestyle=:dot, label="")
                scatter!(p, [lpi1], [lρ]; markershape=:cross, markersize=6,
                         markercolor=:black, markeralpha=0.8, markerstrokecolor=:black,
                         markerstrokewidth=1.5, label="")
                break
            end
        end
    end

    savefig(p, joinpath(OUTPUT_BASE, country, "GDP_Negishi_Heatmap_$(country).pdf"))
    savefig(p, joinpath(OUTPUT_BASE, country, "GDP_Negishi_Heatmap_$(country).png"))
    println("Saved: GDP_Negishi_Heatmap_$country (.pdf + .png)")
end

# ═════════════════════════════════════════════════════════════════════════════
# WELFARE HEATMAP — NPV OF RELATIVE EDE GAIN: UNIFORM vs AUTARKY (NEGISHI)

# Reads consumption_EDE.csv from the Negishi scenario folders.
# EDE uses Atkinson equal weights; the Negishi recycling determines the
# within-country distribution of carbon revenue, not the welfare evaluation.
#
# π-axis (x): LINEAR 
# ρ-axis (y): LOG 
# ═════════════════════════════════════════════════════════════════════════════

function load_welfare_heatmap(country, omega_i, pop_share_i, target_pis)
    cpath   = joinpath(OUTPUT_BASE, country)
    a_all   = sort(filter(d -> isdir(d) && startswith(basename(d), "autarky_negishi_"),
                         readdir(cpath; join=true)); by = d -> parse_pval(d, "autarky_negishi_"))
    a_pairs = filter(x -> any(isapprox(x[2], t; rtol=1e-3) for t in target_pis),
                    [(d, parse_pval(d, "autarky_negishi_")) for d in a_all])
    a_dirs  = first.(a_pairs)
    u_dirs  = sort(filter(d -> isdir(d) && startswith(basename(d), "uniform_negishi_ratio_"),
                         readdir(cpath; join=true)); by = d -> parse_pval(d, "uniform_negishi_ratio_"))
    (isempty(a_dirs) || isempty(u_dirs)) && return nothing

    pi_hm    = last.(a_pairs)
    ratio_hm = parse_pval.(u_dirs, "uniform_negishi_ratio_")

    read_ede_npv(d) = begin
        path = joinpath(d, "consumption_EDE.csv")
        !isfile(path) && return (NaN, false)
        df    = CSV.read(path, DataFrame)
        rows  = filter(r -> first(YEARS_NPV) <= r.time <= last(YEARS_NPV), df)
        valid = filter(r -> !isnan(r.consumption_ede), rows)
        nrow(valid) == 0 && return (NaN, false)
        val = net_present_value(valid, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "consumption_ede")
        return (val, nrow(valid) < nrow(rows))
    end

    a_results   = read_ede_npv.(a_dirs)
    a_npvs      = first.(a_results)
    a_truncated = last.(a_results)
    u_results   = read_ede_npv.(u_dirs)
    u_npvs      = first.(u_results)

    results = Float64[
        isnan(u_npvs[i]) || isnan(a_npvs[j]) || abs(a_npvs[j]) < 1e-6 ? NaN :
            (u_npvs[i] - a_npvs[j]) / abs(a_npvs[j]) * 100
        for i in eachindex(u_dirs), j in eachindex(a_dirs)
    ]

    autarky_subsidy = [any((1.0 .- π^α_abat .* omega_i) .< 0.0) for π in pi_hm]
    uniform_bad     = [any(pop_share_i .* ρ .> 1.0)              for ρ in ratio_hm]

    return (results           = results,
            pi_hm             = pi_hm,
            ratio_hm          = ratio_hm,
            autarky_truncated = a_truncated,
            autarky_subsidy   = autarky_subsidy,
            uniform_bad       = uniform_bad)
end

store_welfare = Dict(c => begin
    t_syms = (c == "EU27") ? eu27_countries : [Symbol(c)]
    ω  = [sum(get(emissions_lookup,(y,s),0.0) for s in t_syms) for y in unique_years] ./
         max.(global_cap, 1e-10)
    ps = [sum(get(pop_lookup,(y,s),0.0) for s in t_syms) for y in unique_years] ./ global_pop
    load_welfare_heatmap(c, ω, ps, pi_vals_negishi)
end for c in target_countries)
filter!(p -> !isnothing(p.second), store_welfare)

all_welf_vals  = vcat([filter(!isnan, vec(d.results)) for d in values(store_welfare)]...)
clim_welf      = isempty(all_welf_vals) ? 5.0 : quantile(abs.(all_welf_vals), 0.95)
shared_clims_w = (-clim_welf, clim_welf)

for country in target_countries
    !haskey(store_welfare, country) && continue
    d = store_welfare[country]

    # ── 1. Subset to the display range ───────────────────────────────────────
    pi_mask    = (d.pi_hm .>= 0.3) .& (d.pi_hm .<= 5.0)
    ratio_mask = d.ratio_hm .<= 10.0
    pi_sub    = d.pi_hm[pi_mask]        # linear x coordinates
    ratio_sub = d.ratio_hm[ratio_mask]  # raw ρ values (plotted on log y)
    res_sub   = d.results[ratio_mask, pi_mask]
    asub      = d.autarky_subsidy[pi_mask]
    ubad      = d.uniform_bad[ratio_mask]
    n_ratio, n_pi = size(res_sub)

    # ── 2. Log-y coordinates for the heatmap and overlays ────────────────────
    # x stays linear (pi_sub); y is log10(ratio_sub) so heatmap cells are even
    lrat = log10.(ratio_sub)

    pi_step   = length(pi_sub) > 1 ? (pi_sub[end] - pi_sub[end-1]) / 2 : 0.5
    lrat_step = length(lrat)   > 1 ? (lrat[end]   - lrat[end-1])   / 2 : 0.5

    # ── 3. Build the heatmap ─────────────────────────────────────────────────
    # heatmap() treats x/y as cell centres; with linear x the spacing is uniform
    p = heatmap(pi_sub, lrat, res_sub;
        clims          = shared_clims_w,
        color          = cgrad(:RdBu),
        xlabel         = "\n" * L"Autarky: price factor $\pi_i$  ($p_i = \pi_i \cdot p^*$)",
        ylabel         = "Uniform price:\n"* L"rights factor $\rho_i$  ($r_i = \rho_i \cdot e_i^*$)",
        colorbar_title = "\nWelfare in Uniform relative to Autarky\n(% of EDE consumption NPV)",
        size           = (720, 360),
        right_margin   = 18mm,
        left_margin    = 8mm,
        bottom_margin  = 12mm,
        top_margin     = 6mm,
        frame          = :axes, 
        tickdir        = :out,
        tickfontsize   = 10,
        guidefontsize  = 12,
        colorbar_titlefontsize = 10,
        legendfontsize = 9,
        legend         = :topright,
        xlims = (minimum(pi_sub) - pi_step,   maximum(pi_sub) + pi_step),
        ylims = (minimum(lrat)   - lrat_step, maximum(lrat)   + lrat_step)
    )

    # ── 4. Explicit x-ticks: show every 0.5 increment, label with π symbol ───
    xtick_vals   = filter(v -> 0.3 <= v < 5.0, 0.25:0.25:5.0)
    xtick_labels = [isinteger(v) || v in [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5] ?
                    string(v) : "" for v in xtick_vals]
    plot!(p; xticks = (collect(xtick_vals), xtick_labels))

    # ── 5. y-ticks: show the raw ρ values on the log scale ───────────────────
    ytick_vals   = [0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0]
    ytick_labels = ["0.02","0.05","0.1","0.2","0.5","1","2","5","10"]
    plot!(p; yticks = (log10.(ytick_vals), ytick_labels))

    # ── 6. Thin white cell borders (x now linear, so use midpoints of pi_sub) ─
    for bx in [(pi_sub[i] + pi_sub[i+1]) / 2 for i in 1:n_pi-1]
        vline!(p, [bx]; color=:white, lw=0.5, label="")
    end
    for by in [(lrat[i] + lrat[i+1]) / 2 for i in 1:n_ratio-1]
        hline!(p, [by]; color=:white, lw=0.5, label="")
    end

    # ── 7. Indifference contour (zero welfare change) ────────────────────────
    contour!(p, pi_sub, lrat, res_sub;
        levels = [0.0],
        color  = :black,
        lw     = 2.0,
        label  = "Indifference (ΔW = 0)",
    )

    # ── 8. Flag markers (coordinates: linear x, log10 y) ─────────────────────
    subsidy_xy = [(pi_sub[j], lrat[i])
                  for i in 1:n_ratio, j in 1:n_pi
                  if asub[j] && !ubad[i]]
    bad_xy     = [(pi_sub[j], lrat[i])
                  for i in 1:n_ratio, j in 1:n_pi if ubad[i]]

    !isempty(subsidy_xy) && scatter!(p, first.(subsidy_xy), last.(subsidy_xy);
        markershape       = :circle,
        markersize        = 2,
        markercolor       = :white,
        markeralpha       = 0.7,
        markerstrokecolor = :dimgrey,
        markerstrokewidth = 1,
        label             = L"\mathrm{RoW\ price} < 0 \ \mathrm{(autarky)}",
    )

    !isempty(bad_xy) && scatter!(p, first.(bad_xy), last.(bad_xy);
        markershape       = :xcross,
        markersize        = 4,
        markercolor       = :dimgrey,
        markerstrokecolor = :black,
        markerstrokewidth = 1,
        label             = L"\mathrm{RoW\ rights} < 0 \ \mathrm{(uniform)}",
    )

    # ── 9. Crosshair at π = 1 (the theoretically neutral autarky price) ───────
    # Find the column index for π = 1, interpolate ρ where ΔW crosses zero
    j1 = findfirst(v -> isapprox(v, 1.0; atol=1e-3), pi_sub)
    if !isnothing(j1)
        col = res_sub[:, j1]
        for i in 1:length(col)-1
            if !isnan(col[i]) && !isnan(col[i+1]) && col[i] * col[i+1] <= 0
                t    = col[i] / (col[i] - col[i+1])
                lρ   = lrat[i] + t * (lrat[i+1] - lrat[i])
                ρ_star = 10^lρ
                xl   = xlims(p)[1]
                yl   = ylims(p)[1]
                # vertical dotted line down from the crosshair to x-axis
                plot!(p, [pi_sub[j1], pi_sub[j1]], [yl, lρ];
                      color=:grey, lw=1.2, linestyle=:dot, label="")
                # horizontal dotted line left from the crosshair to y-axis
                plot!(p, [xl, pi_sub[j1]], [lρ, lρ];
                      color=:grey, lw=1.2, linestyle=:dot, label="")
                scatter!(p, [pi_sub[j1]], [lρ];
                        markershape       = :diamond,
                        markersize        = 5,
                        markercolor       = :black,
                        markeralpha       = 0.9,
                        markerstrokecolor = :black,
                        markerstrokewidth = 1.2,
                        label             = L"\rho_i \ \mathrm{at} \ \pi_i = 1\ (\rho_1)")
                annotate!(p, xl + 0.75, lρ + 0.15,
                      text(L"$\rho_1 = $ %$(round(ρ_star, digits=2))", :left, 8, :grey20))
                break
            end
        end
    end

    # ── 11. Save ──────────────────────────────────────────────────────────────
    savefig(p, joinpath(OUTPUT_BASE, country, "Welfare_Heatmap_$(country).pdf"))
    println("Saved: Welfare_Heatmap_$country")
end

# ═════════════════════════════════════════════════════════════════════════════
# SUMMARY TABLE — break-even ρ_1 at π=1 for each country
# ═════════════════════════════════════════════════════════════════════════════

# reference year for shares (2030, first policy year)
ref_year = 2030
t_ref    = findfirst(==(ref_year), unique_years)

summary_rows = []

for country in target_countries
    !haskey(store_welfare, country) && continue
    d = store_welfare[country]

    t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]

    # emission share and population share at ref_year
    e_i   = sum(get(emissions_lookup, (ref_year, s), 0.0) for s in t_syms)
    pop_i = sum(get(pop_lookup,       (ref_year, s), 0.0) for s in t_syms)
    e_world   = global_cap[t_ref]
    pop_world = global_pop[t_ref]

    ems_share  = round(100 * e_i / e_world;   digits=1)  # %
    pop_share  = round(100 * pop_i / pop_world; digits=1) # %
    pc_ems = round((e_i * 1e9) / (pop_i * 1e3); digits=2) 
    pc_ems_rel = round((e_i / pop_i) / (e_world / pop_world); digits=2) # ratio to world avg

    # find ρ* at π=1 from the heatmap data
    pi_mask    = (d.pi_hm .>= 0.3) .& (d.pi_hm .<= 5.0)
    ratio_mask = d.ratio_hm .<= 10.0
    pi_sub     = d.pi_hm[pi_mask]
    ratio_sub  = d.ratio_hm[ratio_mask]
    res_sub    = d.results[ratio_mask, pi_mask]
    lrat       = log10.(ratio_sub)

    ρ_star = NaN
    j1 = findfirst(v -> isapprox(v, 1.0; atol=1e-3), pi_sub)
    if !isnothing(j1)
        col = res_sub[:, j1]
        for i in 1:length(col)-1
            if !isnan(col[i]) && !isnan(col[i+1]) && col[i] * col[i+1] <= 0
                t     = col[i] / (col[i] - col[i+1])
                lρ    = lrat[i] + t * (lrat[i+1] - lrat[i])
                ρ_star = round(10^lρ; digits=2)
                break
            end
        end
    end

    push!(summary_rows, (
        country    = country,
        ems_share  = ems_share,
        pop_share  = pop_share,
        pc_ems     = pc_ems,
        pc_ems_rel = pc_ems_rel,
        rho_star   = ρ_star,
    ))
end

summary_df = DataFrame(summary_rows)
println(summary_df)

io = IOBuffer()
println(io, "\\begin{table}[h]")
println(io, "\\centering")
println(io, "\\caption{Break-even rights factor \$\\rho^*\$ at \$\\pi_i = 1\$ by country}")
println(io, "\\renewcommand{\\arraystretch}{1.2}")
println(io, "\\begin{tabular}{lrrrr}")
println(io, "  \\toprule")
println(io, "  \\textbf{Country} & \\textbf{Emission share (\\%)} & \\textbf{Population share (\\%)} & \\textbf{Per-capita emissions (tC02)} & \$\\rho^*\$ at \$\\pi_i=1\$ \\\\")
println(io, "  \\midrule")
for r in eachrow(summary_df)
    println(io, "  $(r.country) & $(r.ems_share) & $(r.pop_share) & $(r.pc_ems) & $(r.rho_star) \\\\")
end
println(io, "  \\bottomrule")
println(io, "\\end{tabular}")
println(io, "\\end{table}")

latex_str = String(take!(io))
println(latex_str)

# optionally save to file
open(joinpath(OUTPUT_BASE, "summary_table.tex"), "w") do f
    write(f, latex_str)
end