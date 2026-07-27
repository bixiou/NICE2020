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
# Heatmap: relative NPV consumption EDE = (NPV_U − NPV_A) / |NPV_A| × 100
###############################################################################
# change this line to the path of the NICE2020 project
cd("/Users/constance/Documents/stage/NICE2020")

using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()
using Mimi, MimiFAIRv2, DataFrames, CSV, Statistics, Plots, Plots.Measures, LaTeXStrings, PrettyTables

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
# p_star_path[t] = uniform world carbon price p* in year t (in $/tCO2)
# reference price throughout the script
# we also keep the last calibrated value for extrapolation beyond the calibration period
p_star_path = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output",
                           "calibrated_global_cs.csv"), DataFrame)
df_tax.time       = Int.(df_tax.time)
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
last_calibrated_year = maximum(df_tax.time)
last_calibrated_tax  = tax_dict[last_calibrated_year]
for (i, y) in enumerate(collect(dim_keys(base_model, :time)))
    p_star_path[i] = y <= last_calibrated_year ? get(tax_dict, y, 0.0) : last_calibrated_tax
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

pop_lookup       = Dict((r.time, r.country) => r.l       for r in eachrow(pop_df))
emissions_lookup = Dict((r.time, r.country) => r.E_gtco2 for r in eachrow(emissions_df))

# global_cap[t] = total world emissions in year t under the reference run
# this is the emissions cap E* that Scenario A and Scenario U must both respect
global_cap = [sum(filter(r -> r.time == y, emissions_df).E_gtco2) for y in unique_years]
global_pop = [sum(filter(r -> r.time == y, pop_df).l)             for y in unique_years]

# ─── Negishi redistribution weights: w_q ∝ c_q^η (inverse marginal utility) ─
# Carbon tax revenue is recycled to each quintile proportional to 1/U'(c) = c^η.
# This preserves the initial consumption ranking (regressive: richer quintiles
# receive proportionally more). Weights are computed from the reference run
# consumption before tax recycling, averaged over 2025–2035.

# elasticity of marginal utility of consumption
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

# abatement exponents — used to compute analytical p_{-i}
# θ2_abat: curvature parameter of the marginal abatement cost curve 
# α_abat is just a convenient reformulation (1/(θ2-1))
θ2_abat    = try Float64(Mimi.get_param(base_model, :abatement, :θ2)) catch; 2.6 end
α_abat     = 1.0 / (θ2_abat - 1.0)
# "pback" = backstop price path: the price at which abatement would reach 100%
pback = try collect(Float64, Mimi.get_param(base_model, :abatement, :pbacktime)) catch; nothing end
 


# ═════════════════════════════════════════════════════════════════════════════
# WELFARE HEATMAP — NPV OF RELATIVE EDE GAIN: UNIFORM vs AUTARKY (NEGISHI)

# Reads consumption_EDE.csv from the Negishi scenario folders.
# EDE uses Atkinson equal weights; the Negishi recycling determines the
# within-country distribution of carbon revenue, not the welfare evaluation.
#
# π-axis (x): LINEAR 
# ρ-axis (y): LOG 
# ═════════════════════════════════════════════════════════════════════════════

parse_pval(name, sep) = parse(Float64, replace(split(basename(name), sep)[end], "p" => "."))
npv_go(df) = net_present_value(df, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "gross_output")

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
#### predicted rho_1 value (NPV)

# reference year for shares (2030, first policy year)

ref_year = 2030
t_ref    = findfirst(==(ref_year), unique_years)

summary_rows = []

discount_rate = 0.03
β = [1 / (1 + discount_rate)^(y - 2030) for y in unique_years]

for country in target_countries
    !haskey(store_welfare, country) && continue
    d = store_welfare[country]

    t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]

    # 1. 2030 Snapshot metrics (for context columns)
    e_i       = sum(get(emissions_lookup, (ref_year, s), 0.0) for s in t_syms)
    pop_i     = sum(get(pop_lookup,       (ref_year, s), 0.0) for s in t_syms)
    e_world   = global_cap[t_ref]
    pop_world = global_pop[t_ref]

    ems_share  = round(100 * e_i / e_world;   digits=1)  # %
    pop_share  = round(100 * pop_i / pop_world; digits=1) # %
    pc_ems     = round((e_i * 1e9) / (pop_i * 1e3); digits=2) 
    pc_ems_rel = round((e_i / pop_i) / (e_world / pop_world); digits=2) 

    # 2. Time-series calculations for dynamic NPV Predicted rho_1
    folder_autarky_1 = joinpath(OUTPUT_BASE, country, "autarky_negishi_1p0")

    if isdir(folder_autarky_1)
        ems_autarky = CSV.read(joinpath(folder_autarky_1, "emissions.csv"), DataFrame)
    else
        ems_autarky = CSV.read(joinpath(OUTPUT_BASE, country, "uniform_negishi_ratio_1p0", "emissions.csv"), DataFrame)
    end

    e_star_i = Float64[]
    base_rights_t = Float64[]

    for (t_idx, y) in enumerate(unique_years)
        row_y = filter(r -> r.time == y, ems_autarky)
        push!(e_star_i, nrow(row_y) > 0 ? row_y.co2_emissions[1] : 0.0)
        
        p_i_t = sum(get(pop_lookup, (y, s), 0.0) for s in t_syms)
        p_world_t = global_pop[t_idx]
        share_t = p_world_t > 0 ? p_i_t / p_world_t : 0.0
        push!(base_rights_t, share_t * global_cap[t_idx])
    end

    npv_e_star_i    = sum(e_star_i[t] * p_star_path[t] * β[t] for t in 1:length(unique_years))
    npv_base_rights = sum(base_rights_t[t] * p_star_path[t] * β[t] for t in 1:length(unique_years))

    predicted_rho_npv = npv_base_rights > 0 ? npv_e_star_i / npv_base_rights : 0.0

    # 3. Find empirical ρ* at π=1 from the heatmap data
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

    # Push to summary structure
    push!(summary_rows, (
        country            = country,
        ems_share          = ems_share,
        pop_share          = pop_share,
        pc_ems             = pc_ems,
        predicted_rho_star = round(predicted_rho_npv; digits=2),
        pc_ems_rel         = pc_ems_rel,
        rho_star           = ρ_star,
    ))
end

summary_df = DataFrame(summary_rows)
sorted_df = sort(summary_df, order(:pc_ems, rev=true))
println(sorted_df)

# Write out clean LaTeX
io = IOBuffer()
println(io, "\\begin{table}[h]")
println(io, "\\centering")
println(io, "\\small")
println(io, "\\caption{Non-losing rights allocation factor \$\\rho_i\$ at \$\\pi_i = 1\$ (\$\\rho_1\$) by country}")
println(io, "\\renewcommand{\\arraystretch}{1.2}")
println(io, "\\begin{tabular}{lrrrr}")
println(io, "  \\toprule")
println(io, "  \\textbf{Country} & \\textbf{Emission share} & \\textbf{Population share} & \\textbf{Emissions p.c} & \\textbf{Predicted} & \$\\rho_1\$ \\\\")
println(io, "  & \\textbf{in 2030 (\\%)} & \\textbf{in 2030 (\\%)} & \\textbf{in 2030 (tCO\$_2\$)} & \\textbf{\$\\rho_1\$} & \\\\")
println(io, "  \\midrule")
for r in eachrow(sorted_df)
    println(io, "  $(r.country) & $(r.ems_share) & $(r.pop_share) & $(r.pc_ems) & $(r.predicted_rho_star) & $(r.rho_star) \\\\")
end
println(io, "  \\bottomrule")
println(io, "\\end{tabular}")
println(io, "\\label{tab:summary}")
println(io, "\\end{table}")

latex_str = String(take!(io))
println(latex_str)

# graph predicted rho against 


# ═════════════════════════════════════════════════════════════════════════════
# country-specific tables
# ════════════════════════════════════════════════════════════════════════════

function generate_autarky_table(country::String, pi_i::Float64; output_base::String=".", tax_dict::Dict=Dict())
    # Format the pi_i string for the folder name
    pi_str = replace(string(round(pi_i; digits=2)), "." => "p")
    folder = joinpath(output_base, country, "autarky_negishi_$pi_str")

    # Read the dataframes
    tax_df     = CSV.read(joinpath(folder, "country_carbon_tax.csv"), DataFrame)
    ems_df     = CSV.read(joinpath(folder, "emissions.csv"), DataFrame)
    p_minus_df = CSV.read(joinpath(folder, "p_minus_i.csv"), DataFrame)

    table_years = 2030:10:2100

    # Extract values
    p_star_vals  = [round(get(tax_dict, y, 0.0); digits=1) for y in table_years]
    p_i_vals     = [round(only(filter(r -> r.time == y, tax_df)).country_carbon_tax; digits=1) for y in table_years]
    E_i_vals     = [round(only(filter(r -> r.time == y, ems_df)).co2_emissions; digits=2) for y in table_years]
    p_minus_vals = [round(only(filter(r -> r.time == y, p_minus_df)).p_minus_i; digits=1) for y in table_years]

    # Add p_{-i} to the final table construction (transposed format)
    tbl = DataFrame(
        :Variable => [
            raw"$p^*$ (USD/tCO$_2$)", 
            raw"$p_i$ (USD/tCO$_2$)", 
            raw"$p_{-i}$ (USD/tCO$_2$)",
            raw"$e_i$ (GtCO$_2$)"
        ],
        [Symbol(y) => [p_star_vals[i], p_i_vals[i], p_minus_vals[i], E_i_vals[i]]
        for (i, y) in enumerate(table_years)]...
    )

    # Output to console
    println("--- Transposed Table for $country with π = $pi_i ---")
    println(tbl)
    println("\nLaTeX format:")
    pretty_table(tbl; backend=:latex)
    
    return tbl
end

usa_table = generate_autarky_table("USA", 2.0, output_base=OUTPUT_BASE, tax_dict=tax_dict)

ind_table = generate_autarky_table("IND", 3.25, output_base=OUTPUT_BASE, tax_dict=tax_dict)



## example of numbers for uniform price, COD, rho=5

country = "COD"
ratio   = 0.5

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
    :Variable => [L"$p^*$ (USD/tCO$_2$)", L"$r_i$ (GtCO$_2$)", L"$e_i$ (GtCO$_2$)", "Transfer (bn USD)"],
    [Symbol(y) => [
        round(get(tax_dict, y, 0.0); digits=1),
        round(rights_vec[year_idx[y]]; digits=3),
        round(only(filter(r -> r.time == y, ems_df)).co2_emissions; digits=3),
        round(get(tax_dict, y, 0.0) *
            (rights_vec[year_idx[y]] -
            only(filter(r -> r.time == y, ems_df)).co2_emissions); digits=1),
    ] for y in table_years]...
)

using PrettyTables
pretty_table(tbl; backend=:latex)

#################################################################################################################################
# WOLFRAM / DUFLO EQUIVALENCE — uniform-price rights ratios equivalent to two differentiated-pricing proposals from the literature
#
# for a set of countries, we try to find the rights ratio rho_i such that a country's welfare under the uniform-price regime
# (with country i receiving rights_i(t) = rho_i * pop_share_i(t) * global_cap(t)) exactly matches its welfare under a 
#specific autarky scenario (Wolfram or Duflo)
# rho_i is therefore the uniform-price-regime rights allocation that makes country i indifferent to the autarky alternative
#
# what this code actually does:
# - for each country, rho_i is found by bisection on welfare: when solving for country i,
# every other target country is held at the default population/reference-emissions pro-rata split and
# not at its own already-solved rho (approximation of a simultaneous solve for the largest countries 
# but not guaranteed to be identical)
# - rights are then distributed within a target country pro rata on population and the residual global cap
# is distributed to the rest of the world pro rata on reference (autarky-scenario) emissions
#
# country coverage:
# - Wolfram uses a coalition list (`countries_wolfram`): only these countries are taxed, and only for these does a
# Wolfram-equivalent rho make sense, countries outside this list get a "n/a" in the Wolfram column of the final table
# - Duflo applies to all LIC/LMIC/UMIC countries in the world (HIC pay no domestic tax)
################################################################################

# ──────────────────────────────────────────────────────────────────────────
# helpers
# ──────────────────────────────────────────────────────────────────────────

include("../data/parameters.jl")

global eu27_countries = Symbol.(eu27_countries)

# country/time lookups extracted from a specific model run
struct ScenarioData
    pop_lookup::Dict{Tuple{Int,Symbol}, Float64}
    emissions_lookup::Dict{Tuple{Int,Symbol}, Float64}
    global_cap::Vector{Float64}
    global_pop::Vector{Float64}
    unique_years::Vector{Int}
    all_countries::Vector{Symbol}
end

function build_scenario_data(m)
    pop_df       = getdataframe(m, :grosseconomy => :l)
    emissions_df = getdataframe(m, :emissions    => :E_gtco2)
    unique_years = sort(unique(emissions_df.time))
    @assert length(unique_years) == nb_steps

    pop_lookup       = Dict((r.time, r.country) => r.l       for r in eachrow(pop_df))
    emissions_lookup = Dict((r.time, r.country) => r.E_gtco2 for r in eachrow(emissions_df))
    global_cap = [sum(filter(r -> r.time == y, emissions_df).E_gtco2) for y in unique_years]
    global_pop = [sum(filter(r -> r.time == y, pop_df).l)             for y in unique_years]
    all_countries = collect(dim_keys(m, :country))

    return ScenarioData(pop_lookup, emissions_lookup, global_cap, global_pop, unique_years, all_countries)
end

# NPV welfare (cons_EDE) of a single country under a given model run `m`
function country_welfare_npv(m, country, sd::ScenarioData)
    η_welfare = try Float64(Mimi.get_param(m, :welfare, :elasmu)) catch; 1.5 end
    t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]
    cons_EDE = filter(row -> row.country in t_syms, getdataframe(m, :welfare => :cons_EDE_country))
    println("$country: raw eltype = $(eltype(cons_EDE.cons_EDE_country))")

    if country == "EU27"
        if isempty(cons_EDE)
            @warn "No EU27 member rows found" t_syms model_countries=unique(getdataframe(m, :welfare => :cons_EDE_country).country)
        end
        cons_EDE = combine(groupby(cons_EDE, :time)) do dd
            if isempty(dd)
                return DataFrame(cons_EDE_country = Float64[])
            end
            pop_t = [get(sd.pop_lookup, (dd.time[1], r.country), 0.0) for r in eachrow(dd)]
            result = MimiNICE2020.EDE_aggregated(Float64.(dd.cons_EDE_country), pop_t, η_welfare)
            println("$country @ t=$(dd.time[1]): EDE_aggregated returned $(typeof(result)) = $result")
            DataFrame(cons_EDE_country = result)
        end
        println("$country: post-combine eltype = $(eltype(cons_EDE.cons_EDE_country))")
    end

    return net_present_value(cons_EDE, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "cons_EDE_country")
end

# computes the NPV welfare for a single country when the uniform-price regime uses a per-country rights allocation defined by `ratio`
# - `ratio`: scalar rho applied to the country's population share x global cap 
# - remaining global cap is assigned to the rest of the world pro rata on reference (autarky-scenario) emissions
# (if those are zero, split evenly)
# - runs a NICE2020 model under the uniform cap-and-share regime (control_regime = 1, tax = p_star_path, rights = constructed matrix)
# and returns NPV welfare for the target country
function welfare_for_ratio(country, ratio, sd::ScenarioData, p_star_path; verbose=true)
    t0 = time()
    is_eu27        = (country == "EU27")
    target_symbols = is_eu27 ? eu27_countries : [Symbol(country)]
    target_indices = findall(x -> x in target_symbols, sd.all_countries)

    target_pop  = [sum(get(sd.pop_lookup, (y, s), 0.0) for s in target_symbols) for y in sd.unique_years]
    pop_share_i = target_pop ./ sd.global_pop
    rights_i    = ratio .* pop_share_i .* sd.global_cap

    verbose && println("    [build] rho=$(round(ratio, digits=4)) | rights_i(2030)=$(round(rights_i[findfirst(==(2030), sd.unique_years)], digits=4)) GtCO2 | pop_share(2030)=$(round(pop_share_i[findfirst(==(2030), sd.unique_years)]*100, digits=2))%")

    rights_mat = zeros(Float64, nb_steps, nb_country)
    for t in 1:nb_steps
        target_pop[t] == 0 && continue
        y = sd.unique_years[t]
        for idx in target_indices
            s = sd.all_countries[idx]
            rights_mat[t, idx] = get(sd.pop_lookup, (y, s), 0.0) / target_pop[t] * rights_i[t]
        end
        # negative global_cap is allowed (CDR)
        rem = sd.global_cap[t] >= 0 ? max(0.0, sd.global_cap[t] - rights_i[t]) : (sd.global_cap[t] - rights_i[t])
        other_ems = sum(get(sd.emissions_lookup, (y, s), 0.0)
                         for s in sd.all_countries if !(s in target_symbols))
        for c in 1:nb_country
            c in target_indices && continue
            s = sd.all_countries[c]
            if other_ems > 0
                rights_mat[t, c] = get(sd.emissions_lookup, (y, s), 0.0) / other_ems * rem
            else
                denom = nb_country - length(target_indices)
                rights_mat[t, c] = denom > 0 ? rem / denom : 0.0
            end
        end
    end

    neg_count = count(<(0), rights_mat)
    neg_count > 0 && verbose && println("    [warn] $neg_count negative entries in rights_mat (RoW rights < 0)")

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
    update_param!(m, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
    run(m)
    t_run = round(time() - t0, digits=2)

    cons_EDE = filter(row -> row.country in target_symbols, getdataframe(m, :welfare => :cons_EDE_country))
    if is_eu27
        cons_EDE = combine(groupby(cons_EDE, :time)) do dd
            pop_t = [get(sd.pop_lookup, (dd.time[1], r.country), 0.0) for r in eachrow(dd)]
            DataFrame(cons_EDE_country = MimiNICE2020.EDE_aggregated(Float64.(dd.cons_EDE_country), pop_t, η_welfare))
        end
    end

    n_nan = count(isnan, cons_EDE.cons_EDE_country)
    n_nan > 0 && verbose && println("    [warn] $n_nan NaN values in cons_EDE_country for $country")

    welfare = net_present_value(cons_EDE, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "cons_EDE_country")
    verbose && println("    [done] rho=$(round(ratio, digits=4)) -> welfare=$(round(welfare, digits=2)) | run took $(t_run)s")
    return welfare
end


# finds the scalar rho that makes a country's welfare under the uniform rights allocation equal to `target_welfare`
# by bisection (each evaluation needs a full model run, so bisection keeps this simpler and still robust)
# `lo`/`hi` must bracket the solution
# out-of-bracket results are flagged ":below_range" / ":above_range" and the corresponding bound is returned as the (approximate) value
function find_rho(country, target_welfare, sd::ScenarioData, p_star_path;
                   lo=-5.0, hi=20.0, tol=1e-3, max_iter=25)
    println("\n========== Bisection for $country (target welfare = $(round(target_welfare, digits=2))) ==========")
    t_total = time() 

    w_lo = welfare_for_ratio(country, lo, sd, p_star_path)
    w_hi = welfare_for_ratio(country, hi, sd, p_star_path)
    println("  Bounds check: w(rho=$lo)=$(round(w_lo, digits=2))  |  target=$(round(target_welfare, digits=2))  |  w(rho=$hi)=$(round(w_hi, digits=2))")

    if target_welfare < w_lo
        println("  WARNING: target BELOW w_lo -- even the minimal rho=$lo already exceeds the autarky welfare. Flagging as 'below range' (rho* < $lo).")
        return (rho = lo, flag = :below_range)
    end
    if target_welfare > w_hi
        println("  WARNING: target ABOVE w_hi -- even rho=$hi does not reach the autarky welfare. Flagging as 'above range' (rho* > $hi).")
        return (rho = hi, flag = :above_range)
    end
    println("  Target is properly bracketed.")

    for iter in 1:max_iter
        mid = (lo + hi) / 2
        w_mid = welfare_for_ratio(country, mid, sd, p_star_path)
        gap = abs(hi - lo)
        direction = w_mid > target_welfare ? "hi down" : "lo up"
        println("  [iter $iter/$max_iter] rho=$(round(mid, digits=4))  ->  welfare=$(round(w_mid, digits=2))  (target=$(round(target_welfare, digits=2)), gap=$(round(gap, digits=5)), $direction)")
        w_mid > target_welfare ? (hi = mid) : (lo = mid)
        gap < tol && (println("  Converged after $iter iterations."); break)
    end

    rho_final = (lo + hi) / 2
    println("  -> Final rho for $country: $(round(rho_final, digits=4))  (total time: $(round(time() - t_total, digits=1))s)")
    return (rho = rho_final, flag = :converged)
end

# builds a rights matrix from each country's rho: each target country gets rights_i(t) = rho_i * pop_share_i(t) * global_cap(t),
# split within the country pro rata on population
# the remaining global cap goes to the rest of the world pro rata on reference (autarky-scenario) emissions
function build_combined_rights_mat(rho_dict, countries_to_include, sd::ScenarioData)
    rights_mat = zeros(Float64, nb_steps, nb_country)
    assigned_indices = Int[]

    for country in countries_to_include
        haskey(rho_dict, country) || continue
        is_eu27        = (country == "EU27")
        target_symbols = is_eu27 ? eu27_countries : [Symbol(country)]
        target_indices = findall(x -> x in target_symbols, sd.all_countries)
        for idx in target_indices
            idx in assigned_indices || push!(assigned_indices, idx)
        end

        ratio = rho_dict[country].rho
        target_pop  = [sum(get(sd.pop_lookup, (y, s), 0.0) for s in target_symbols) for y in sd.unique_years]
        pop_share_i = target_pop ./ sd.global_pop
        rights_i    = ratio .* pop_share_i .* sd.global_cap

        for t in 1:nb_steps
            target_pop[t] == 0 && continue
            y = sd.unique_years[t]
            for idx in target_indices
                s = sd.all_countries[idx]
                rights_mat[t, idx] = get(sd.pop_lookup, (y, s), 0.0) / target_pop[t] * rights_i[t]
            end
        end
    end

    for t in 1:nb_steps
        y = sd.unique_years[t]
        assigned_rights_t = sum(rights_mat[t, idx] for idx in assigned_indices; init=0.0)
        rem = sd.global_cap[t] >= 0 ? max(0.0, sd.global_cap[t] - assigned_rights_t) : (sd.global_cap[t] - assigned_rights_t)
        other_ems = sum(get(sd.emissions_lookup, (y, s), 0.0)
                         for (idx, s) in enumerate(sd.all_countries) if !(idx in assigned_indices))
        n_rest = nb_country - length(assigned_indices)
        for c in 1:nb_country
            c in assigned_indices && continue
            s = sd.all_countries[c]
            rights_mat[t, c] = other_ems > 0 ? get(sd.emissions_lookup, (y, s), 0.0) / other_ems * rem :
                                                (n_rest > 0 ? rem / n_rest : 0.0)
        end
    end

    return rights_mat
end

# runs the uniform-price regime with a given rights matrix
function run_uniform_with_rights(rights_mat, p_star_path)
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
    update_param!(m, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
    run(m)
    return m
end

# rescales a rights matrix year by year so that world rights exactly equal world emissions of the reference autarky scenario
function rescale_rights_to_match_cap(rights_mat, global_cap)
    rescaled = copy(rights_mat)
    for t in 1:size(rights_mat, 1)
        row_sum = sum(@view rights_mat[t, :])
        row_sum > 0 && (rescaled[t, :] .*= global_cap[t] / row_sum)
    end
    return rescaled
end

# sanity check
function diagnose_rights_mat(rights_mat, global_cap, unique_years, label)
    println("\n========== Diagnostic: $label ==========")
    neg_count = count(<(0), rights_mat)
    if neg_count > 0
        neg_vals = filter(<(0), rights_mat)
        println("  WARNING: $neg_count negative entries found (worst: $(round(minimum(neg_vals), digits=4)), sum: $(round(sum(neg_vals), digits=4)))")
    else
        println("  OK: no negative entries")
    end
    println("\n  Per-year deviation from global_cap[t] (years with |deviation| > 0.5% only):")
    println("  year | row_sum | global_cap | deviation | deviation_%")
    for t in 1:length(unique_years)
        row_sum = sum(@view rights_mat[t, :])
        cap_t   = global_cap[t]
        dev     = row_sum - cap_t
        dev_pct = cap_t != 0 ? dev / cap_t * 100 : NaN
        abs(dev_pct) > 0.5 && println("  $(unique_years[t]) | $(round(row_sum, digits=4)) | $(round(cap_t, digits=4)) | $(round(dev, digits=4)) | $(round(dev_pct, digits=2))%")
    end
    total_rights, total_cap = sum(rights_mat), sum(global_cap)
    println("\n  TOTAL rights_mat: $(round(total_rights, digits=4))  |  TOTAL global_cap: $(round(total_cap, digits=4))  |  deviation: $(round((total_rights-total_cap)/total_cap*100, digits=2))%")
    return neg_count
end

# ── gain en droits ──────────────────────────────────────────
# by construction `build_combined_rights_mat` always allocates exactly the residual global cap to the rest of the world
# so world rights always equal world autarky emissions, regardless of which rho's were used
# the quantity below is how much rights are reallocayed across countries relative to the autarky benchmark:
# sum of |country-level deltas| / 2 as a % of world rights

# eg: in autarky scenario, country A emits 100 tonnes and country B emits 100 tonnes -> total = 200 tonnes
# to convince both countries to move to a uniform-price regime, the rights allocation is set so that country A receives 140 tonnes and country B receives 60 tonnes
# - country A got an additional 40 tonnes ($\Delta_A = +40$)
# - country B lost 40 tonnes ($\Delta_B = -40$)
# the formula sums these absolute deltas and divides by 2 to avoid double-counting, then divides by the total to get a percentage:
# $$\text{Reallocation} = \frac{|\Delta_A| + |\Delta_B|}{2 \times \text{Total}} = \frac{40 + 40}{2 \times 200} = \frac{40}{200} = 20\%$$
# we end up with the proportion of global carbon budget that has been redistributed from one country to another from one scenario to another

function world_reallocation_pct(country_gains_df, global_cap::Vector{Float64})
    # use sum(abs.(global_cap)) so that net world emissions
    # can go negative in heavy-CDR years, and a signed (or near-zero)
    # denominator makes a "% of world rights" statistic meaningless or sign-flipped.
    gross_total_rights = sum(abs.(global_cap))
    @assert gross_total_rights > 0 "gross_total_rights must be positive; check global_cap"
    return sum(abs.(country_gains_df.delta)) / 2 / gross_total_rights * 100
end

# formatting for table
format_rho(result) = if result.flag == :below_range
    "\$<\$ $(round(result.rho, digits=4))"
elseif result.flag == :above_range
    "\$>\$ $(round(result.rho, digits=4))"
else
    string(round(result.rho, digits=3))
end

# NPV/emissions-weighted average tax rate a country actually paid under its autarky scenario,
# vs. the uniform price path
function avg_tax_rate(country, sd::ScenarioData, tax_schedule::Matrix{Float64})
    # tax_schedule: [time, country] matrix, e.g. diff_country_tax_wolfram or diff_country_tax_duflo
    t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]
    t_idx  = findall(x -> x in t_syms, sd.all_countries)
    num = 0.0
    denom = 0.0
    for (t, y) in enumerate(sd.unique_years)
        e_t = sum(get(sd.emissions_lookup, (y, s), 0.0) for s in t_syms)
        rate_t = length(t_idx) > 0 ? sum(tax_schedule[t, idx] for idx in t_idx) / length(t_idx) : 0.0
        num   += e_t * rate_t
        denom += e_t
    end
    return denom > 0 ? num / denom : NaN
end

function avg_uniform_price(sd::ScenarioData, p_star_path::Vector{Float64})
    # same weighting, using world emissions since the uniform price is the same for everyone
    num = sum(sd.global_cap[t] * p_star_path[t] for t in 1:length(sd.unique_years))
    denom = sum(sd.global_cap)
    return denom != 0 ? num / denom : NaN
end

####################################################################################################################
# WOLFRAM: "Building a Climate Coalition" — $25/t LIC & LMIC, $50/t UMIC, $75/t HIC for 2025-30 within the coalition, then +5%/year
#####################################################################################################################

tax_lic_lmic_wolfram = 25.0  # $/tCO2, 2025-2030
tax_umic_wolfram     = 50.0  # $/tCO2, 2025-2030
tax_hic_wolfram      = 75.0  # $/tCO2, 2025-2030

nice2020_wolfram = MimiNICE2020.create_nice2020()
years     = collect(dim_keys(nice2020_wolfram, :time))
countries = collect(dim_keys(nice2020_wolfram, :country))

diff_country_tax_wolfram = zeros(Float64, length(years), length(countries))

# LIC_LMIC / UMIC / HIC income-tier lists loaded from parameters.jl
LIC_LMIC = Symbol.(LIC_LMIC)
UMIC     = Symbol.(UMIC)
HIC      = Symbol.(HIC)

# Wolfram's country list

countries_wolfram = ["AUS", "BRA", "CAN", "CHE", "CHN", "CMR", "DZA", "EGY", "EU27",
                        "GBR", "GHA", "IDN", "IND", "ISL", "KEN", "LIE", "MOZ",
                        "NOR", "TGO", "THA", "UGA", "ZMB"]


participating_symbols = Symbol[]
for c in countries_wolfram
    if c == "EU27"
        append!(participating_symbols, Symbol.(eu27_countries))
    else
        push!(participating_symbols, Symbol(c))
    end
end

# 2025-2030: income-tier tax
years_index_2530 = findall(y -> 2025 <= y <= 2030, years)
for t in years_index_2530, (c_idx, country) in enumerate(countries)
    if country in participating_symbols
        diff_country_tax_wolfram[t, c_idx] = country in LIC_LMIC ? tax_lic_lmic_wolfram :
                                              country in UMIC     ? tax_umic_wolfram     :
                                              country in HIC      ? tax_hic_wolfram      : 0.0
    end
    # non-participants (or unclassified participants) stay at 0.0
end

# post-2030: tax grows at 5%/year
years_index_post2030 = findall(y -> y > 2030, years)
wolfram_growth_rate = 0.05
for t in years_index_post2030, (c_idx, country) in enumerate(countries)
    diff_country_tax_wolfram[t, c_idx] = diff_country_tax_wolfram[t-1, c_idx] * (1 + wolfram_growth_rate)
end

update_param!(nice2020_wolfram, :switch_custom_transfers,       0)
update_param!(nice2020_wolfram, :switch_recycle,                1)
update_param!(nice2020_wolfram, :switch_global_recycling,       0)
update_param!(nice2020_wolfram, :revenue_recycle, :global_recycle_share, zeros(nb_country))
update_param!(nice2020_wolfram, :revenue_recycle, :switch_global_pc_recycle, 0)
update_param!(nice2020_wolfram, :abatement, :control_regime,    4)
update_param!(nice2020_wolfram, :abatement, :direct_country_tax, diff_country_tax_wolfram)
update_param!(nice2020_wolfram, :switch_footprint,               1)
update_param!(nice2020_wolfram, :switch_transfers_affect_growth, 1)
update_param!(nice2020_wolfram, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
run(nice2020_wolfram)

MimiNICE2020.save_nice2020_reduced_output(nice2020_wolfram, joinpath(OUTPUT_BASE, "Wolfram_prices"))

sd_wolfram = build_scenario_data(nice2020_wolfram)

# global cap-and-share tax path used as the uniform price for every B1/B2 run below for both Wolfram and Duflo
p_star_path = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output",
                           "calibrated_global_cs.csv"), DataFrame)
df_tax.time       = Int.(df_tax.time)
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
last_calibrated_year = maximum(df_tax.time)
last_calibrated_tax  = tax_dict[last_calibrated_year]
for (i, y) in enumerate(collect(dim_keys(base_model, :time)))
    p_star_path[i] = y <= last_calibrated_year ? get(tax_dict, y, 0.0) : last_calibrated_tax
end


η_welfare = try Float64(Mimi.get_param(base_model, :welfare, :elasmu)) catch; 1.5 end

let conso_df = getdataframe(nice2020_wolfram, :quantile_recycle => :conso_pc_post_damage_abatement)
    ref_years = Set(filter(y -> 2025 <= y <= 2035, sd_wolfram.unique_years))
    all_quantiles = collect(dim_keys(nice2020_wolfram, :quantile))
    nq = length(all_quantiles)
    global recycle_share_negishi = zeros(Float64, nb_country, nq)
    
    eta_val = try Float64(Mimi.get_param(nice2020_wolfram, :welfare, :elasmu)) catch; 1.5 end
    
    for (ci, csym) in enumerate(sd_wolfram.all_countries)
        c_str  = string(csym)
        c_rows = filter(r -> string(r.country) == c_str && r.time in ref_years, conso_df)
        weights = zeros(nq)
        for (qi, q) in enumerate(all_quantiles)
            vals = filter(!isnan, Float64.(filter(r -> r.quantile == q, c_rows).conso_pc_post_damage_abatement))
            avg  = isempty(vals) ? 0.0 : mean(vals)
            weights[qi] = avg > 0 ? avg^eta_val : 0.0
        end
        s = sum(weights)
        recycle_share_negishi[ci, :] = s > 0 ? weights ./ s : fill(1.0 / nq, nq)
    end
end

model_countries = Set(collect(dim_keys(nice2020_wolfram, :country)))
missing_countries = [c for c in countries_wolfram if c != "EU27" && !(Symbol(c) in model_countries)]
if !isempty(missing_countries)
    @warn "Countries not present in model, skipping" missing_countries
end
countries_wolfram_valid = filter(c -> c == "EU27" || Symbol(c) in model_countries, countries_wolfram)

welfare_wolfram = Dict{String, Float64}(
    country => country_welfare_npv(nice2020_wolfram, country, sd_wolfram) for country in countries_wolfram_valid
)

println(welfare_wolfram)

rho_wolfram = Dict{String, NamedTuple}(
    country => find_rho(country, welfare_wolfram[country], sd_wolfram, p_star_path) for country in countries_wolfram_valid
)
println(rho_wolfram)

##############################################################################################################
# DUFLO: Banerjee-Duflo-Greenstone "Grand Bargain" — $10/t LIC, $30/t LMIC, $50/t UMIC, 2025-30 then +5%/year
# no domestic tax for HIC, applies globally 
##############################################################################################################

tax_lic_duflo  = 10.0  # $/tCO2, 2025-2030
tax_lmic_duflo = 30.0  # $/tCO2, 2025-2030
tax_umic_duflo = 50.0  # $/tCO2, 2025-2030
# HIC: no tax under this proposal (they are assumed to pay via other instruments/transfers which are not modelled here
# since this table focuses on the autarky-vs-uniform comparison)

nice2020_duflo = MimiNICE2020.create_nice2020()
years     = collect(dim_keys(nice2020_duflo, :time))
countries = collect(dim_keys(nice2020_duflo, :country))

diff_country_tax_duflo = zeros(Float64, length(years), length(countries))

LIC  = Symbol.(LIC)
LMIC = Symbol.(LMIC)
UMIC = Symbol.(UMIC)

# 2025-2030: tax by income tier
years_index_2530 = findall(y -> 2025 <= y <= 2030, years)
for t in years_index_2530, (c_idx, country) in enumerate(countries)
    diff_country_tax_duflo[t, c_idx] = country in LIC  ? tax_lic_duflo  :
                                        country in LMIC ? tax_lmic_duflo :
                                        country in UMIC ? tax_umic_duflo : 0.0  # HIC: no tax
end

years_index_post2030 = findall(y -> y > 2030, years)
duflo_growth_rate = 0.05
for t in years_index_post2030, (c_idx, country) in enumerate(countries)
    diff_country_tax_duflo[t, c_idx] = diff_country_tax_duflo[t-1, c_idx] * (1 + duflo_growth_rate)
end

update_param!(nice2020_duflo, :switch_custom_transfers,       0)
update_param!(nice2020_duflo, :switch_recycle,                1)
update_param!(nice2020_duflo, :switch_global_recycling,       0)
update_param!(nice2020_duflo, :revenue_recycle, :global_recycle_share, zeros(nb_country))
update_param!(nice2020_duflo, :revenue_recycle, :switch_global_pc_recycle, 0)
update_param!(nice2020_duflo, :abatement, :control_regime,    4)
update_param!(nice2020_duflo, :abatement, :direct_country_tax, diff_country_tax_duflo)
update_param!(nice2020_duflo, :switch_footprint,               1)
update_param!(nice2020_duflo, :switch_transfers_affect_growth, 1)
update_param!(nice2020_duflo, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
run(nice2020_duflo)

MimiNICE2020.save_nice2020_reduced_output(nice2020_duflo, joinpath(OUTPUT_BASE, "Duflo_prices"))

sd_duflo = build_scenario_data(nice2020_duflo)

# ── Welfare targets & equivalent rho ─────────────────────
target_countries = ["USA", "COD", "CHN", "IND", "EU27", "RUS", "NGA", "TUR"]

report_countries = sort(collect(union(target_countries, countries_wolfram_valid)))

welfare_duflo = Dict{String, Float64}(
    country => country_welfare_npv(nice2020_duflo, country, sd_duflo) for country in report_countries
)
println(welfare_duflo)

rho_duflo = Dict{String, NamedTuple}(
    country => find_rho(country, welfare_duflo[country], sd_duflo, p_star_path) for country in report_countries
)
println(rho_duflo)

################################################################################
# DIAGNOSTIC
#
# rho_wolfram / duflo were each solved holding other target countries at the default RoW pro-rata split instead of their own solved rho
# we check here whether that shortcut is a good approximation of a simultaneous solve
# we build the combined rights matrix (all rho's applied at once) and re-check each country's welfare in that combined run
# against its original target
# we look for a close match (a few %)
# large gaps for China, India, EU27 (the biggest countries in the list so the most likely to interact with each other through
# global transfer recycling) would be a sign to implement a Gauss-Seidel refinement:
#   rho_current = copy(rho_wolfram)
#   for round in 1:3, country in countries_wolfram
#       # re-solve country's rho holding every OTHER target country at
#       # rho_current[other] (via build_combined_rights_mat minus `country`)
#       # instead of at the RoW pro-rata default, then update rho_current
#   end
################################################################################

function check_combined_welfare(rho_dict, countries_to_check, sd::ScenarioData, target_welfare_dict, p_star_path, label)
    combined_mat = build_combined_rights_mat(rho_dict, countries_to_check, sd)
    m_combined   = run_uniform_with_rights(combined_mat, p_star_path)
    println("\n========== Sequential vs. simultaneous diagnostic: $label ==========")
    for country in countries_to_check
        haskey(rho_dict, country) || continue
        achieved = country_welfare_npv(m_combined, country, sd)
        target   = target_welfare_dict[country]
        gap_pct  = target != 0 ? (achieved - target) / abs(target) * 100 : NaN
        println("  $country: achieved=$(round(achieved,digits=2))  target=$(round(target,digits=2))  gap=$(round(gap_pct,digits=2))%")
    end
    return m_combined, combined_mat
end

m_wolfram_combined, rights_mat_wolfram_b1 = check_combined_welfare(
    rho_wolfram, countries_wolfram_valid, sd_wolfram, welfare_wolfram, p_star_path, "Wolfram"
)
m_duflo_combined, rights_mat_duflo_b1 = check_combined_welfare(
    rho_duflo, report_countries, sd_duflo, welfare_duflo, p_star_path, "Duflo"
)

################################################################################
# VARIANT B1 (unscaled rights) — world rights/temperature vs. the autarky scenario, and per-country rights reallocation
################################################################################

function summarize_variant_b1(m_autarky, m_b1, rights_mat_b1, sd::ScenarioData,
                                countries_to_check, label)
    total_rights_b1     = sum(rights_mat_b1)
    total_emissions_ref = sum(sd.global_cap)   # world emissions of the autarky scenario

    temp_autarky_2100 = only(filter(r -> r.time == 2100, getdataframe(m_autarky, :temperature => :T))).T
    temp_b1_2100      = only(filter(r -> r.time == 2100, getdataframe(m_b1,      :temperature => :T))).T

    println("\nVariant B1 ($label):")
    println("  World rights (B1) vs. $label world emissions: $(round((total_rights_b1-total_emissions_ref)/total_emissions_ref*100, digits=4))% (expected ~0 by construction -- see note on world_reallocation_pct)")
    println("  World temperature in 2100 -- $label: $(round(temp_autarky_2100, digits=3))C | uniform (B1): $(round(temp_b1_2100, digits=3))C")

    # per-country totals and reallocation
    global_total_rights = sum(sd.global_cap)
    rows = NamedTuple[]
    covered_indices = Int[]
    #for country in countries_to_check
        #t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]
        #t_idx  = findall(x -> x in t_syms, sd.all_countries)
        #w = sum(get(sd.emissions_lookup, (y, s), 0.0) for y in sd.unique_years for s in t_syms)
        #u = sum(rights_mat_b1[t, idx] for (t, y) in enumerate(sd.unique_years) for idx in t_idx)
        #delta = u - w
        #push!(rows, (country=country, autarky_total=w, uniform_total=u, delta=delta,
                      #delta_pct_global = global_total_rights != 0 ? delta / global_total_rights * 100 : NaN))
    #end
    for s in sd.all_countries
        c_str = string(s)
        # Check if this country belongs to your reporting groups (like EU27 aggregation)
        # or handle every model country individually:
        t_idx = findall(x -> x == s, sd.all_countries)
        w = sum(get(sd.emissions_lookup, (y, s), 0.0) for y in sd.unique_years)
        u = sum(rights_mat_b1[t, idx] for (t, y) in enumerate(sd.unique_years) for idx in t_idx)
        delta = u - w
        push!(rows, (country=s, autarky_total=w, uniform_total=u, delta=delta))
    end
    covered_indices = unique(covered_indices)

    # Rest-of-world aggregate: without this row the deltas among `countries_to_check`
    # have no reason to sum to zero, since whatever they don't take is absorbed by
    # everyone else -- the /2 trick below is only valid over the FULL set of countries.
    row_idx = setdiff(1:nb_country, covered_indices)
    w_row = sum(get(sd.emissions_lookup, (y, sd.all_countries[idx]), 0.0)
                for y in sd.unique_years for idx in row_idx)
    u_row = sum(rights_mat_b1[t, idx] for (t, y) in enumerate(sd.unique_years) for idx in row_idx)
    push!(rows, (country="Rest of World", autarky_total=w_row, uniform_total=u_row, delta=u_row - w_row))

    country_gains_df = DataFrame(rows)

    # sanity check: with RoW included, deltas MUST sum to ~0 by construction
    total_delta = sum(country_gains_df.delta)
    if abs(total_delta) > 1e-6 * max(abs(total_rights_b1), 1.0)
        @warn "Deltas do not sum to zero -- reallocation accounting is incomplete" total_delta label
    end

    realloc_pct = world_reallocation_pct(country_gains_df, sd.global_cap)
    println("  World rights REALLOCATION (sum of |country deltas| / 2, % of gross world rights): $(round(realloc_pct, digits=2))%")

    return (temp_autarky_2100=temp_autarky_2100, temp_b1_2100=temp_b1_2100,
            country_gains_df=country_gains_df, reallocation_pct=realloc_pct)
end

res_b1_wolfram = summarize_variant_b1(nice2020_wolfram, m_wolfram_combined, rights_mat_wolfram_b1,
                                       sd_wolfram, countries_wolfram_valid, "Wolfram")
CSV.write(joinpath(OUTPUT_BASE, "equivalent_rights_gain_wolfram.csv"), res_b1_wolfram.country_gains_df)

res_b1_duflo = summarize_variant_b1(nice2020_duflo, m_duflo_combined, rights_mat_duflo_b1,
                                     sd_duflo, report_countries, "Duflo")
CSV.write(joinpath(OUTPUT_BASE, "equivalent_rights_gain_duflo.csv"), res_b1_duflo.country_gains_df)

################################################################################
# VARIANT B2 (rescaled rights) — rescale rights year by year so world emissions exactly match the autarky scenario
# then compare world welfare
################################################################################

function summarize_variant_b2(m_autarky, rights_mat_b1, sd::ScenarioData, p_star_path, label)
    rights_mat_b2 = rescale_rights_to_match_cap(rights_mat_b1, sd.global_cap)
    diagnose_rights_mat(rights_mat_b2, sd.global_cap, sd.unique_years, "$label (B2, rescaled)")
    m_b2 = run_uniform_with_rights(rights_mat_b2, p_star_path)

    welfare_world_autarky = net_present_value(getdataframe(m_autarky, :welfare => :cons_EDE_global),
                                               first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "cons_EDE_global")
    welfare_world_b2 = net_present_value(getdataframe(m_b2, :welfare => :cons_EDE_global),
                                          first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "cons_EDE_global")
    gain_pct = (welfare_world_b2 - welfare_world_autarky) / abs(welfare_world_autarky) * 100

    println("\nVariant B2 ($label):")
    println("  World welfare NPV -- $label: $(round(welfare_world_autarky, digits=2)) | uniform, rescaled (B2): $(round(welfare_world_b2, digits=2))")
    println("  World welfare gain: $(round(gain_pct, digits=2))%")

    return (welfare_world_autarky=welfare_world_autarky, welfare_world_b2=welfare_world_b2, gain_pct=gain_pct)
end

res_b2_wolfram = summarize_variant_b2(nice2020_wolfram, rights_mat_wolfram_b1, sd_wolfram, p_star_path, "Wolfram")
res_b2_duflo   = summarize_variant_b2(nice2020_duflo,   rights_mat_duflo_b1,   sd_duflo,   p_star_path, "Duflo")

################################################################################
# TABLE — per-country equivalent rho (Wolfram, Duflo) + world-level B1/B2 summary figures
################################################################################

function tax_diff_in_2030(country_str::String, sd::ScenarioData, diff_country_tax, p_star_path)
    # Trouver l'index de l'année 2030
    idx_2030 = findfirst(==(2030), sd.unique_years)
    isnothing(idx_2030) && return missing
    
    # Gérer les pays ou le groupe EU27
    target_symbols = (country_str == "EU27") ? eu27_countries : [Symbol(country_str)]
    target_indices = findall(x -> x in target_symbols, sd.all_countries)
    isempty(target_indices) && return missing
    
    # Taxe d'autarcie moyenne du groupe en 2030 (utile si EU27 a des taxes différentes en interne, sinon c'est juste la taxe du pays)
    autarky_tax_2030 = mean(diff_country_tax[idx_2030, idx] for idx in target_indices)
    
    # Prix uniforme mondial en 2030
    uniform_price_2030 = p_star_path[idx_2030]
    
    # On retourne la différence
    return autarky_tax_2030 - uniform_price_2030
end

table_rows = [
    (country              = country,
     welfare_wolfram      = haskey(rho_wolfram, country) ? round(welfare_wolfram[country], digits=2) : missing,
     tax_diff_wolfram     = haskey(rho_wolfram, country) ? round(tax_diff_in_2030(country, sd_wolfram, diff_country_tax_wolfram, p_star_path), digits=1) : missing,
     rho_wolfram_display  = haskey(rho_wolfram, country) ? format_rho(rho_wolfram[country]) : "",
     rho_wolfram_numeric  = haskey(rho_wolfram, country) ? rho_wolfram[country].rho : missing,
     flag_wolfram         = haskey(rho_wolfram, country) ? string(rho_wolfram[country].flag) : "not_in_coalition",
     welfare_duflo        = round(welfare_duflo[country], digits=2),
     tax_diff_duflo       = round(tax_diff_in_2030(country, sd_duflo, diff_country_tax_duflo, p_star_path), digits=1),
     rho_duflo_display    = format_rho(rho_duflo[country]),
     rho_duflo_numeric    = rho_duflo[country].rho,
     flag_duflo           = string(rho_duflo[country].flag))
    for country in report_countries
]

table_df = DataFrame(table_rows)
CSV.write(joinpath(OUTPUT_BASE, "equivalent_rights_table.csv"), table_df)

summary_df = DataFrame(
    scenario                       = ["Wolfram", "Duflo"],
    world_rights_reallocation_pct  = [res_b1_wolfram.reallocation_pct, res_b1_duflo.reallocation_pct],
    temp_autarky_2100              = [res_b1_wolfram.temp_autarky_2100, res_b1_duflo.temp_autarky_2100],
    temp_uniform_2100_b1           = [res_b1_wolfram.temp_b1_2100, res_b1_duflo.temp_b1_2100],
    world_welfare_gain_pct_b2      = [res_b2_wolfram.gain_pct, res_b2_duflo.gain_pct],
)
CSV.write(joinpath(OUTPUT_BASE, "equivalent_rights_global_gains.csv"), summary_df)

# ─────────────────────────────────────────────────────────────────────────────
# LaTeX export
# ─────────────────────────────────────────────────────────────────────────────
io = IOBuffer()
println(io, "\\begin{table}[h]")
println(io, "\\centering")
println(io, "\\small")
println(io, "\\caption{Uniform-price rights ratio \$\\rho_i\$ equivalent to the Wolfram and Duflo autarky scenarios}")
println(io, "\\renewcommand{\\arraystretch}{1.2}")
println(io, "\\begin{tabular}{lcccccc}")
println(io, "  \\toprule")
println(io, "  & \\multicolumn{3}{c}{\\textbf{Wolfram}} & \\multicolumn{3}{c}{\\textbf{Duflo}} \\\\")
println(io, "  \\cmidrule(lr){2-4} \\cmidrule(lr){5-7}")
println(io, raw"  \textbf{Country} & $\Delta$ \textbf{Tax in 2030 ($/t)} & \textbf{Welfare} & $\rho_{\text{Wolfram}}$ & $\Delta$ \textbf{Tax in 2030 ($/t)} & \textbf{Welfare} & $\rho_{\text{Duflo}}$ " * "\\\\")
println(io, "  \\midrule")
for r in eachrow(table_df)
    format_diff(val) = ismissing(val) ? "" : (val > 0 ? "+$(val)" : string(val))
    w_cell = ismissing(r.welfare_wolfram) ? "" : string(r.welfare_wolfram)
    println(io, "  $(r.country) & $(format_diff(r.tax_diff_wolfram)) & $w_cell & $(r.rho_wolfram_display) & $(format_diff(r.tax_diff_duflo)) & $(r.welfare_duflo) & $(r.rho_duflo_display) \\\\")
end
println(io, "  \\midrule")
println(io, "  \\multicolumn{2}{r}\\textbf{World rights reallocation, B1 (\\%)} & \\multicolumn{2}{c}{$(round(res_b1_wolfram.reallocation_pct, digits=2))\\%} & & \\multicolumn{2}{c}{$(round(res_b1_duflo.reallocation_pct, digits=2))\\%} \\\\")
println(io, "  \\multicolumn{2}{r}\\textbf{World temp. 2100, autarky \$\\to\$ uniform B1 (\$^\\circ\$C)} & \\multicolumn{2}{c}{$(round(res_b1_wolfram.temp_autarky_2100, digits=3)) \$\\to\$ $(round(res_b1_wolfram.temp_b1_2100, digits=3))} & & \\multicolumn{2}{c}{$(round(res_b1_duflo.temp_autarky_2100, digits=3)) \$\\to\$ $(round(res_b1_duflo.temp_b1_2100, digits=3))} \\\\")
println(io, "  \\multicolumn{2}{r}\\textbf{World welfare gain, uniform B2 (\\%)} & \\multicolumn{2}{c}{$(round(res_b2_wolfram.gain_pct, digits=2))\\%} & & \\multicolumn{2}{c}{$(round(res_b2_duflo.gain_pct, digits=2))\\%} \\\\")
println(io, "  \\bottomrule")
println(io, "\\end{tabular}")
println(io, "\\label{tab:equivalent_rights}")
println(io, "\\\\[4pt]")
println(io, raw"{\footnotesize Note: '<' (resp. '>') indicates that welfare under uniform pricing already exceeds (resp. never reaches) the autarky benchmark over the tested raw $\rho$ range; the true equivalent ratio lies below (resp. above) the reported bound. 'n/a' means the country is outside the Wolfram coalition, so no Wolfram-equivalent $\rho$ is defined. $\Delta$ Tax in 2030 represents the difference between the domestic autarky tax and the global uniform price in 2030 (Positive = Autarky was more expensive; Negative = Uniform regime is more expensive).}")
println(io, "\\end{table}")

latex_str = String(take!(io))
println(latex_str)
open(joinpath(OUTPUT_BASE, "equivalent_rights_table.tex"), "w") do f
    write(f, latex_str)
end

###### finding equivalent rights for ALL countries
 
function get_autarky_welfare_in_memory(country_name, pi_i, sd::ScenarioData, p_star_path, pback, θ2_abat, α_abat, recycle_share_negishi)
    # setup autarky parameters
    is_eu27 = (country_name == "EU27")
    target_symbols = is_eu27 ? eu27_countries : [Symbol(country_name)]
    target_indices = findall(x -> x in target_symbols, sd.all_countries)
    
    # calculate p_minus_i
    e_i_ref = [sum(get(sd.emissions_lookup, (y, s), 0.0) for s in target_symbols) for y in sd.unique_years]
    omega_i = e_i_ref ./ max.(sd.global_cap, 1e-10)
    
    eff_pi_α = fill(pi_i^α_abat, length(sd.unique_years))
    denom     = max.(1.0 .- omega_i, 1e-10)
    raw_ratio = (1.0 .- eff_pi_α .* omega_i) ./ denom
    p_minus_i = p_star_path .* sign.(raw_ratio) .* abs.(raw_ratio) .^ (θ2_abat - 1.0)

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
    update_param!(m, :quantile_recycle, :recycle_share,           recycle_share_negishi)

    tax_mat = Float64[c in target_indices ? pi_i * p_star_path[t] : p_minus_i[t] for t in 1:length(sd.unique_years), c in 1:nb_country]
    update_param!(m, :abatement, :direct_country_tax, tax_mat)
    run(m)
    
    # compute NPV in-memory
    welfare = country_welfare_npv(m, country_name, sd)

    # also extract this country's own emissions path from the same run, so callers
    # can build autarky_emissions_all (used later by predicted_rho_from_emissions
    # and the emissions-gap diagnostic plots) without a second model run
    e_lk = Dict((r.time, r.country) => r.E_gtco2 for r in eachrow(getdataframe(m, :emissions => :E_gtco2)))
    emissions_series = [sum(get(e_lk, (y, s), 0.0) for s in target_symbols) for y in sd.unique_years]

    return (welfare = welfare, emissions = emissions_series)
end
 
# ─────────────────────────────────────────────────────────────────────────────
# STAGE 1 — fast per-country warm start (PARTIAL EQUILIBRIUM)
#
# Same approximation as find_rho: while solving country i, every other country
# sits at the reference pro-rata split, not at its own (unknown) equilibrium rho.
# Only used to seed Stage 2 with good starting values, not as a final answer.
#
# Illinois algorithm (modified regula falsi) instead of plain bisection: uses the
# welfare *values*, not just their sign, so it converges superlinearly.
# Typically ~8-12 model runs per country instead of bisection's ~25.
# ─────────────────────────────────────────────────────────────────────────────
function find_rho_fast(country, target_welfare, sd::ScenarioData, p_star_path;
                        lo=-5.0, hi=20.0, tol=1e-3, max_iter=15)
    f(x) = welfare_for_ratio(country, x, sd, p_star_path; verbose=false) - target_welfare
 
    f_lo = f(lo)
    f_hi = f(hi)
 
    f_lo > 0 && return (rho = lo, flag = :below_range)   # even minimal rho exceeds target
    f_hi < 0 && return (rho = hi, flag = :above_range)   # even maximal rho falls short
 
    side = 0
    mid = lo
    for iter in 1:max_iter
        mid = (f_hi*lo - f_lo*hi) / (f_hi - f_lo)
        f_mid = f(mid)
        abs(hi - lo) < tol && break
 
        if f_mid * f_hi > 0
            hi, f_hi = mid, f_mid
            side == -1 && (f_lo /= 2)   # Illinois damping — prevents regula falsi stalling
            side = -1
        else
            lo, f_lo = mid, f_mid
            side == 1 && (f_hi /= 2)
            side = 1
        end
    end
    return (rho = mid, flag = :converged)
end

# ScenarioData for the reference cap-and-share run (cs_ref),
# matching the pop/emissions lookups already built manually from cs_ref
baseline_sd = build_scenario_data(cs_ref)
 
println("\n================ STAGE 1: per-country warm start (partial equilibrium) ================")
 
target_welfare_all = Dict{String, Float64}()
autarky_emissions_all = Dict{String, Vector{Float64}}()
all_rho1_results    = Dict{String, Float64}()
all_rho1_flags      = Dict{String, Symbol}()

all_countries = ["TUR"]
 
for country_sym in all_countries
    country = string(country_sym)
 
    autarky_result = get_autarky_welfare_in_memory(
        country, 1.0, baseline_sd, p_star_path, pback, θ2_abat, α_abat, recycle_share_negishi
    )
    target_welfare_all[country]    = autarky_result.welfare
    autarky_emissions_all[country] = autarky_result.emissions
 
    res = find_rho_fast(country, target_welfare_all[country], baseline_sd, p_star_path)
    all_rho1_results[country] = res.rho
    all_rho1_flags[country]   = res.flag
 
    GC.gc()
end
 
println("Stage 1 done: $(length(all_rho1_results)) countries warm-started.")

using JLD2
@save "stage1_checkpoint.jld2" all_rho1_results target_welfare_all autarky_emissions_all all_rho1_flags baseline_sd
 
# ─────────────────────────────────────────────────────────────────────────────
# STAGE 2 — joint simultaneous solve (GENERAL EQUILIBRIUM)
#
# Instead of N independent bisections (each holding every other country fixed
# at the reference split), solve for rho_1,...,rho_N together: each outer
# iteration builds ONE joint rights matrix from the current guesses and runs
# ONE global model, extracting every country's welfare from that single run.
# So the cost per outer iteration is 1 run, not N — cheaper per iteration than
# a single country's Stage-1 bisection used to be.
#
# Update rule: per-country secant step (Gauss-Seidel style, using the gap and
# rho from this iteration and the previous one), damped for stability.
# ─────────────────────────────────────────────────────────────────────────────
 
# builds a joint rights matrix for ALL countries at once from a Dict of rho values,
# then rescales each year so rights still sum exactly to global_cap[t]
# (no leftover "rest of world" group here — every model country is assigned directly)
function build_rights_mat_all(rho::Dict{String,Float64}, sd::ScenarioData)
    raw = zeros(Float64, nb_steps, nb_country)
    for (ci, csym) in enumerate(sd.all_countries)
        c = string(csym)
        r = get(rho, c, 1.0)
        for t in 1:nb_steps
            pop_share = get(sd.pop_lookup, (sd.unique_years[t], csym), 0.0) / max(sd.global_pop[t], 1e-10)
            raw[t, ci] = r * pop_share * sd.global_cap[t]
        end
    end
    rights_mat = similar(raw)
    for t in 1:nb_steps
        s = sum(@view raw[t, :])
        rights_mat[t, :] = s > 0 ? raw[t, :] .* (sd.global_cap[t] / s) : raw[t, :]
    end
    return rights_mat
end
 
# extracts NPV welfare for many countries from a SINGLE model run
# (one getdataframe call instead of one per country)
function welfare_all_countries_npv(m, countries::Vector{String}, sd::ScenarioData)
    η_welfare_local = try Float64(Mimi.get_param(m, :welfare, :elasmu)) catch; 1.5 end
    full_cons_EDE = getdataframe(m, :welfare => :cons_EDE_country)
    welfare = Dict{String, Float64}()
    for country in countries
        t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]
        cons_EDE = filter(row -> row.country in t_syms, full_cons_EDE)
        if country == "EU27"
            cons_EDE = combine(groupby(cons_EDE, :time)) do dd
                isempty(dd) && return DataFrame(cons_EDE_country = Float64[])
                pop_t = [get(sd.pop_lookup, (dd.time[1], r.country), 0.0) for r in eachrow(dd)]
                DataFrame(cons_EDE_country = MimiNICE2020.EDE_aggregated(Float64.(dd.cons_EDE_country), pop_t, η_welfare_local))
            end
        end
        welfare[country] = net_present_value(cons_EDE, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE, "cons_EDE_country")
    end
    return welfare
end

function solve_rho_simultaneous(rho_init::Dict{String,Float64}, target_welfare::Dict{String,Float64},
                                 sd::ScenarioData, p_star_path;
                                 max_outer=40, tol=1e-2, damping=0.7, rho_min=1e-4, rho_max=20.0)
 
    countries = collect(keys(rho_init))
    rho      = copy(rho_init)
    rho_prev = Dict(c => rho_init[c] * 1.05 for c in countries)   # nudge for the first secant step
    gap      = Dict{String,Float64}()
    gap_prev = Dict{String,Float64}()
 
    for outer in 1:max_outer
        rights_mat = build_rights_mat_all(rho, sd)
 
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
        update_param!(m, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
        run(m)
 
        welfare_now = welfare_all_countries_npv(m, countries, sd)
 
        max_gap = 0.0
        for c in countries
            gap[c] = welfare_now[c] - target_welfare[c]
            max_gap = max(max_gap, abs(gap[c]))
        end
        println("  [outer $outer/$max_outer] max |welfare gap| = $(round(max_gap, digits=4))")
 
        global rho_debug, gap_debug, rho_prev_debug
        rho_debug      = copy(rho)
        gap_debug      = copy(gap)
        rho_prev_debug = copy(rho_prev)

        if max_gap < tol
            println("Stage 2 converged after $outer joint iterations.")
            return rho
        end
 
        for c in countries
            rho_new = if outer == 1 || rho[c] == rho_prev[c]
                rho[c] * (gap[c] > 0 ? 0.95 : 1.05)
            else
                slope = (gap[c] - gap_prev[c]) / (rho[c] - rho_prev[c])
                abs(slope) > 1e-8 ? rho[c] - gap[c] / slope : rho[c]
            end
            rho_new = clamp(rho_new, rho_min, rho_max)

            rho_prev[c] = rho[c]
            rho[c]      = rho[c] + damping * (rho_new - rho[c])
        end
        gap_prev = copy(gap)
 
        GC.gc()
    end
 
    println("Stage 2: max_outer reached without full convergence — returning best available estimate.")
    return rho
end
 
println("\n================ STAGE 2: joint simultaneous solve (general equilibrium) ================")
 
rho_simultaneous = solve_rho_simultaneous(all_rho1_results, target_welfare_all, baseline_sd, p_star_path)
 
# ─────────────────────────────────────────────────────────────────────────────
# SAVE RESULTS
# ─────────────────────────────────────────────────────────────────────────────
all_countries_rho_df = DataFrame(
    country               = [string(c) for c in all_countries],
    target_welfare        = [target_welfare_all[string(c)] for c in all_countries],
    rho_warm_start         = [all_rho1_results[string(c)] for c in all_countries],
    rho_warm_start_flag    = [string(all_rho1_flags[string(c)]) for c in all_countries],
    rho_simultaneous       = [rho_simultaneous[string(c)] for c in all_countries],
)
all_countries_out_path = joinpath(OUTPUT_BASE, "equivalent_rights_all_countries.csv")
CSV.write(all_countries_out_path, all_countries_rho_df)
println("Saved: $all_countries_out_path")

# ═════════════════════════════════════════════════════════════════════════════
# PREDICTED (analytical, NPV-of-emissions) vs EMPIRICAL (simultaneous) RHO
# ═════════════════════════════════════════════════════════════════════════════
#
# This graph shows where prediction fails the most
# the per-country diagnostic plot shows where in time the emissions paths diverge

function predicted_rho_from_emissions(country, emissions_series::Vector{Float64}, sd::ScenarioData, p_star_path)
    t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]
    β_local = [1 / (1 + DISCOUNT_RATE)^(y - first(YEARS_NPV)) for y in sd.unique_years]
 
    base_rights_t = Float64[]
    for (t_idx, y) in enumerate(sd.unique_years)
        p_i_t     = sum(get(sd.pop_lookup, (y, s), 0.0) for s in t_syms)
        p_world_t = sd.global_pop[t_idx]
        share_t   = p_world_t > 0 ? p_i_t / p_world_t : 0.0
        push!(base_rights_t, share_t * sd.global_cap[t_idx])
    end
 
    npv_e_star = sum(emissions_series[t] * p_star_path[t] * β_local[t] for t in 1:length(sd.unique_years))
    npv_rights = sum(base_rights_t[t]    * p_star_path[t] * β_local[t] for t in 1:length(sd.unique_years))
 
    return npv_rights > 0 ? npv_e_star / npv_rights : NaN
end
 
predicted_rho_all = Dict(
    country => predicted_rho_from_emissions(country, autarky_emissions_all[country], baseline_sd, p_star_path)
    for country in string.(all_countries)
)
 
# one extra run at the converged simultaneous rho, so the uniform emissions we
# compare against autarky are the true general-equilibrium counterpart, not a
# partial-equilibrium stand-in
function extract_uniform_emissions_all(rho::Dict{String,Float64}, sd::ScenarioData, p_star_path)
    rights_mat = build_rights_mat_all(rho, sd)
 
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
    update_param!(m, :policy_scenario, MimiNICE2020.scenario_index[:All_World])
    run(m)
 
    e_lk = Dict((r.time, r.country) => r.E_gtco2 for r in eachrow(getdataframe(m, :emissions => :E_gtco2)))
    emissions = Dict{String, Vector{Float64}}()
    for csym in sd.all_countries
        c = string(csym)
        t_syms = (c == "EU27") ? eu27_countries : [csym]
        emissions[c] = [sum(get(e_lk, (y, s), 0.0) for s in t_syms) for y in sd.unique_years]
    end
    return emissions
end
 
println("\n================ Extracting uniform emissions at converged simultaneous rho ================")
uniform_emissions_all = extract_uniform_emissions_all(rho_simultaneous, baseline_sd, p_star_path)

# ── comparison table: predicted vs empirical, plus context for plotting ────────
ref_year_cmp = 2030
t_ref_cmp    = findfirst(==(ref_year_cmp), baseline_sd.unique_years)
 
rho_compare_df = DataFrame(
    country       = String[],
    predicted_rho = Float64[],
    empirical_rho = Float64[],   # simultaneous, general-equilibrium solve
    warm_start_rho= Float64[],   # Stage-1 partial-equilibrium solve, for reference
    flag          = String[],
    pop_share_pct = Float64[],
)
for csym in all_countries
    c = string(csym)
    t_syms   = (c == "EU27") ? eu27_countries : [csym]
    pop_i    = sum(get(baseline_sd.pop_lookup, (ref_year_cmp, s), 0.0) for s in t_syms)
    pop_pct  = 100 * pop_i / baseline_sd.global_pop[t_ref_cmp]
    push!(rho_compare_df, (
        c,
        predicted_rho_all[c],
        rho_simultaneous[c],
        all_rho1_results[c],
        string(all_rho1_flags[c]),
        pop_pct,
    ))
end
# drop degenerate rows (zero population, or a formula that returned NaN)
filter!(r -> isfinite(r.predicted_rho) && r.predicted_rho > 0 && isfinite(r.empirical_rho), rho_compare_df)
 
CSV.write(joinpath(OUTPUT_BASE, "predicted_vs_empirical_rho.csv"), rho_compare_df)
 
# ─────────────────────────────────────────────────────────────────────────────
# PLOT 1 — predicted vs empirical rho, all countries (log-log, house style)
# ─────────────────────────────────────────────────────────────────────────────
function plot_predicted_vs_empirical_rho(df::DataFrame; highlight_countries=target_countries,
                                          output_base::String=OUTPUT_BASE)
    lo = min(minimum(df.predicted_rho), minimum(df.empirical_rho)) * 0.7
    hi = max(maximum(df.predicted_rho), maximum(df.empirical_rho)) * 1.4
 
    # fit quality on the log scale (how well the closed-form shortcut tracks the real solve)
    r2 = cor(log10.(df.predicted_rho), log10.(df.empirical_rho))^2
 
    p = plot(
        xlabel = "\n" * L"Predicted $\rho_1$ (NPV of autarky emissions / NPV of population rights)",
        ylabel = L"Empirical $\rho_1$ (simultaneous, general-equilibrium solve)" * "\n",
        xscale = :log10, yscale = :log10,
        xlims  = (lo, hi), ylims = (lo, hi),
        size   = (640, 620),
        left_margin = 8mm, bottom_margin = 12mm, right_margin = 6mm, top_margin = 6mm,
        frame = :axes, tickdir = :out,
        tickfontsize = 10, guidefontsize = 12, legendfontsize = 9,
        legend = :topleft,
        aspect_ratio = :equal,
    )
 
    plot!(p, [lo, hi], [lo, hi]; color=:grey40, lw=1.4, linestyle=:dash,
          label="Predicted = Empirical")
 
    style_for(flag) = flag == "converged" ?
        (markershape=:circle,   markercolor=:steelblue, markeralpha=0.75, label="Converged") :
        (markershape=:utriangle, markercolor=:grey60,   markeralpha=0.55, label="Hit search bound (less reliable)")
 
    for flag in unique(df.flag)
        sub = df[df.flag .== flag, :]
        st  = style_for(flag)
        scatter!(p, sub.predicted_rho, sub.empirical_rho;
            markershape       = st.markershape,
            markercolor       = st.markercolor,
            markeralpha       = st.markeralpha,
            markersize        = 3.5 .+ 9 .* sqrt.(sub.pop_share_pct ./ maximum(df.pop_share_pct)),
            markerstrokewidth = 0.5,
            markerstrokecolor = :white,
            label             = st.label,
        )
    end
 
    # this is to label only a subset of countries and avoid crowdedness (target countries and biggest divergences)
    df.logdiff = abs.(log10.(df.empirical_rho) .- log10.(df.predicted_rho))
    biggest_gap = first(sort(df, :logdiff, rev=true), min(8, nrow(df))).country
    to_label = unique(vcat(intersect(highlight_countries, df.country), biggest_gap))
 
    for c in to_label
        row = df[df.country .== c, :]
        isempty(row) && continue
        annotate!(p, row.predicted_rho[1] * 1.06, row.empirical_rho[1],
                  text(c, :left, 7, :grey15))
    end
 
    annotate!(p, lo * 1.15, hi / 1.3,
              text("R² = $(round(r2, digits=3)) (log-log fit)", :left, 9, :grey20))
 
    savefig(p, joinpath(output_base, "Predicted_vs_Empirical_Rho.pdf"))
    println("Saved: Predicted_vs_Empirical_Rho.pdf")
    return p
end
 
plot_predicted_vs_empirical_rho(rho_compare_df)
 
# ─────────────────────────────────────────────────────────────────────────────
# PLOT 2 — per-country emissions-gap diagnostic (autarky vs simultaneous-uniform)
# ─────────────────────────────────────────────────────────────────────────────
function plot_emissions_gap_diagnostic(country::String, sd::ScenarioData, p_star_path;
                                        autarky_emissions::Vector{Float64},
                                        uniform_emissions::Vector{Float64},
                                        predicted_rho::Float64, empirical_rho::Float64,
                                        output_base::String=OUTPUT_BASE)
    years  = sd.unique_years
    t_syms = (country == "EU27") ? eu27_countries : [Symbol(country)]
    pop_t  = [sum(get(sd.pop_lookup, (y, s), 0.0) for s in t_syms) for y in years]
 
    pc_autarky = (autarky_emissions .* 1e9) ./ max.(pop_t .* 1e3, 1e-10)
    pc_uniform = (uniform_emissions .* 1e9) ./ max.(pop_t .* 1e3, 1e-10)
 
    # ── left panel: per-capita emissions paths, shaded gap ─────────────────────
    p1 = plot(years, pc_autarky;
        label = "Autarky", lw = 2.2, color = :firebrick,
        xlabel = "", ylabel = L"tCO$_2$ / person" * "\n",
        frame = :axes, tickdir = :out, tickfontsize = 9, guidefontsize = 11,
        legendfontsize = 8, legend = :topright,
        title = "Per-capita emissions", titlefontsize = 11,
    )
    plot!(p1, years, pc_uniform; label="Uniform (simultaneous)", lw=2.2, color=:steelblue, linestyle=:dash)
    plot!(p1, years, pc_uniform; fillrange=pc_autarky, fillalpha=0.12, fillcolor=:grey40,
          linewidth=0, label="")
 
    # ── right panel: year-by-year NPV-weighted contribution to the emissions gap ─
    β_local = [1 / (1 + DISCOUNT_RATE)^(y - first(YEARS_NPV)) for y in years]
    discounted_gap = (uniform_emissions .- autarky_emissions) .* p_star_path .* β_local
    npv_gap = sum(discounted_gap)
 
    # split into two full-width series (rather than a per-bar color vector) so bar
    # widths stay consistent across Plots.jl/GR versions
    gap_pos = [g >= 0 ? g : 0.0 for g in discounted_gap]
    gap_neg = [g <  0 ? g : 0.0 for g in discounted_gap]
 
    p2 = bar(years, gap_pos;
        label = "Uniform emits more", color = :steelblue, linewidth = 0,
        xlabel = "", ylabel = "Discounted \$ gap\n(Uniform \$-\$ Autarky)",
        frame = :axes, tickdir = :out, tickfontsize = 9, guidefontsize = 11,
        legendfontsize = 8, legend = :topright,
        title = "Where the gap is concentrated", titlefontsize = 11,
    )
    bar!(p2, years, gap_neg; label="Autarky emits more", color=:firebrick, linewidth=0)
    hline!(p2, [0.0]; color=:grey30, lw=1.0, label="")
 
    p_final = plot(p1, p2, layout=(1,2), size=(1040, 420),
        left_margin=8mm, right_margin=6mm, bottom_margin=10mm, top_margin=10mm,
        plot_title = "$country — predicted ρ₁=$(round(predicted_rho,digits=2))" *
                     "  vs  empirical ρ₁=$(round(empirical_rho,digits=2))" *
                     "  (NPV gap: $(round(npv_gap, digits=2)))",
        plot_titlefontsize = 11,
    )
 
    country_dir = joinpath(output_base, country)
    mkpath(country_dir)
    save_path = joinpath(country_dir, "Emissions_Gap_Diagnostic_$(country).pdf")
    savefig(p_final, save_path)
    println("Saved: Emissions_Gap_Diagnostic_$country")
    return p_final
end
 
# run the diagnostic for the usual reporting countries + the biggest predicted/empirical divergences
diagnostic_countries = unique(vcat(
    intersect(target_countries, rho_compare_df.country),
    first(sort(rho_compare_df, :logdiff, rev=true), min(10, nrow(rho_compare_df))).country
))
 
println("\n================ Emissions-gap diagnostic plots ================")
for country in diagnostic_countries
    plot_emissions_gap_diagnostic(
        country, baseline_sd, p_star_path;
        autarky_emissions = autarky_emissions_all[country],
        uniform_emissions = uniform_emissions_all[country],
        predicted_rho      = predicted_rho_all[country],
        empirical_rho      = rho_simultaneous[country],
    )
end


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

# used later to turn absolute emissions errors into relative error
cap_norm = maximum(global_cap)

# loop over every country we want to test and over every price factor π_i
# for each (country, π_i) pair we:
#  1. guess a rest-of-world price p_{-i} using the linear formula
#  2. run the model
#  3. check whether total world emissions still match the target cap
#  4. if not, adjust p_{-i} and repeat (Newton's method, then bisection as backup)
#  5. once emissions match the cap, save the results

for country_name in [target_countries; invisible_countries]
    is_eu27        = (country_name == "EU27")
    target_symbols = is_eu27 ? eu27_countries : [Symbol(country_name)]
    target_indices = findall(x -> x in target_symbols, all_countries)

    e_i_ref = [sum(get(emissions_lookup, (y, s), 0.0) for s in target_symbols) for y in unique_years] # country's emissions in the reference run
    omega_i = e_i_ref ./ max.(global_cap, 1e-10) # share of world emissions each year

    println("\nAutarky Negishi | $country_name")

    for pi_i in pi_vals_negishi
        pi_str = replace(string(round(pi_i; digits=2)), "." => "p")
        folder = joinpath(OUTPUT_BASE, country_name, "autarky_negishi_$pi_str")
        isdir(folder) && isfile(joinpath(folder, "consumption_EDE.csv")) && continue

        println("\nAutarky Negishi | $country_name | π = $pi_i")

        # we get a first guess for the RoW price p_{-i} by inverting the MAC curve to translate 
        # how much this country would abate at price π_i into an equivalent price for
        # the RoW, so that if the relationship were linear, world emissions would exactly hit the target
        if !isnothing(pback)
            # this is the fraction of emissions that get abated under the eference price p*
            mu_ref   = [pback[t] > 0 && p_star_path[t] > 0 ?
                        (min(p_star_path[t], pback[t]) / pback[t])^α_abat : 0.0
                        for t in 1:nb_steps]
            # this is the fraction that gets abated if the country chooses its own price p_i
            mu_i     = [min(pi_i^α_abat * mu_ref[t], 1.0) for t in 1:nb_steps]
            eff_pi_α = [mu_ref[t] > 1e-10 ? mu_i[t] / mu_ref[t] : pi_i^α_abat
                        for t in 1:nb_steps]
        else
            eff_pi_α = fill(pi_i^α_abat, nb_steps)
        end

        # linear-approximation formula:
        # p_{-i} = p* x [(E* - e_i*pi_i) / e_{-i}]
        # using shares (omega_i) and the MAC curvature exponent so it works with the model's non-linear MAC curve
        denom     = max.(1.0 .- omega_i, 1e-10) # RoW share of world emissions
        raw_ratio = (1.0 .- eff_pi_α .* omega_i) ./ denom
        p_minus_i = p_star_path .* sign.(raw_ratio) .* abs.(raw_ratio) .^ (θ2_abat - 1.0)
        p_minus_i[unique_years .< 2030] .= 0.0

        # configure the model for a direct country tax autarky run 
        # control_regime = 4 means that each country/region gets its own carbon tax
        # revenue is not recycled globally
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
        update_param!(m, :quantile_recycle, :recycle_share,           recycle_share_negishi)

        # Newton-style iterations to correct p_{-i}
        # the formula for p_{-i} is only exact if the MAC curve is linear (which it isn't in the real model)
        # we run the model, measure how far off total world emissions are from the target cap
        # and move p_{-i} to close the gap
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

        CSV.write(joinpath(folder, "p_minus_i.csv"), DataFrame(time = unique_years, p_minus_i = p_minus_i))

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

ratio_vals = [0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2.0, 5.0, 10.0, 20.0]

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