######################################################################################################################
# This file finds a suitable global carbon tax trajectory for the NICE2020 model
# with an emissions constraint instead of a temperature constraint.
######################################################################################################################

# Activate the project and make sure all packages we need are installed.
using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

# Load required Julia packages.
using Mimi, MimiFAIRv2, DataFrames, CSVFiles

# --- Tax and budget start parameters ---
const tax_start_year        = 2025     # tax start year (2025, 2030…)
const evaluation_end_year   = 2100     # the end of the year for budgeting and well-being
const emission_budget_limit = 1000.0   # budget max GtCO2 from tax_start_year to evaluation_end_year
rho = 0.015                       # discount rate
# ---------------------------------------------------------

# ——————————————————————————————————————————————————————————————
# Choice of Scenario
# ——————————————————————————————————————————————————————————————
include(joinpath(@__DIR__, "..", "data", "parameters.jl"))

const scenario_name     = :All_World # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership)

const policy_scenario   = scenario_index[scenario_name]

const participation_vec = club_countries_binary[policy_scenario, :]
# ——————————————————————————————————————————————————————————————

# Make directory to optionally save tested pathways outputs
output_directory_test_2deg_global = joinpath(@__DIR__, "..", "test_2deg_global_exp")
mkpath(output_directory_test_2deg_global)

println("Test global carbon tax runs")

# Define a function that creates a global tax trajectory from starting level and growth rate,
# runs the model with this carbon tax trajectory and outputs yearly emissions and welfare
function test_global_exp_c_tax(tax_start_value_test, g_rate_test)
    # we pass tax_start_year instead of 2020
    full_co2_tax = exp_tax_trajectory(tax_start_value = tax_start_value_test, g_rate = g_rate_test, year_tax_start = tax_start_year, year_tax_end = 2200)
    # ---  add zeros before tax_start_year to always have nb_steps elements
    n_pre = tax_start_year - 2020
    full_co2_tax = vcat(zeros(n_pre), full_co2_tax)[1:length(dim_keys(nice_v2, :time))]
    # ---------------------------------------------------------

    update_param!(nice_v2, :abatement, :global_carbon_tax, full_co2_tax)
    run(nice_v2)

    # — Country-by-country aggregation on selected club only —
    emissions_matrix = nice_v2[:emissions, :E_gtco2]        # (time × country)
    welfare_matrix   = nice_v2[:welfare,   :welfare_country]# (time × country)

    emissions = emissions_matrix * participation_vec
    welfare   = welfare_matrix   * participation_vec

    return emissions, welfare
end

# Load NICE2020 source code.
include(joinpath(@__DIR__, "..", "src", "nice2020_module.jl"))
include(joinpath(@__DIR__, "..", "src", "helper_functions.jl"))

# Get baseline instance of the model
nice_v2 = MimiNICE2020.create_nice2020()
update_param!(nice_v2, :switch_recycle, 0)             # Switch carbon taxation recycling off 
update_param!(nice_v2, :abatement, :control_regime, 1) # 1 = global_carbon_tax

# We update policy_scenario so that the model knows which country club to apply.
update_param!(nice_v2, :policy_scenario, policy_scenario)

# Get number of time steps in the model
nb_steps   = length(dim_keys(nice_v2, :time))
years_vec  = collect(2020:2020+nb_steps-1)

mask = (years_vec .>= tax_start_year) .& (years_vec .<= evaluation_end_year)
discount = (1 .+ rho) .^ collect(0:(evaluation_end_year - tax_start_year))

# === Zoom-progressive search with carbon budget constraint ===
const n_zoom    = 3       # number of zoom iterations
const n_points  = 11      # 11×11 grids
# initial bounds 
const start_first  = 0
const start_last   = 2000
const g_rate_first = 0.0
const g_rate_last  = 0.20
start_min, start_max = start_first, start_last
rate_min,  rate_max  = g_rate_first, g_rate_last

best_welfare = -Inf
best_params  = (NaN, NaN)

for zoom in 1:n_zoom
    println("Zoom niveau $zoom: domaine A∈[$start_min,$start_max], B∈[$rate_min,$rate_max]")
    A_vals = range(start_min, stop=start_max, length=n_points)
    B_vals = range(rate_min,  stop=rate_max,  length=n_points)

    # grille de scores
    welfare_grid = fill(-Inf, n_points, n_points)

    for (i, A) in enumerate(A_vals), (j, B) in enumerate(B_vals)
        # simulation
        emissions, welfare = test_global_exp_c_tax(A, B)

        # we restrict to the years [tax_start_year, evaluation_end_year]
        emis_zoom = emissions[mask]
        wel_zoom  = welfare[mask] ./ discount

        # carbon budget constraint
        if sum(emis_zoom) <= emission_budget_limit
            welfare_grid[i, j] = sum(wel_zoom)
        end
        # sinon reste -Inf
    end

    # detect the max in the grid
    max_idx = argmax(welfare_grid)
    ci = CartesianIndices(welfare_grid)[max_idx]
    i_max, j_max = Tuple(ci)
    best_A, best_B = A_vals[i_max], B_vals[j_max]
    best_val       = welfare_grid[i_max, j_max]
    println("  → meilleur A=$(best_A), B=$(best_B), welfare=$best_val")

    # memorize if it's the best overall
    if best_val > best_welfare
        best_welfare = best_val
        best_params  = (best_A, best_B)
    end

    # determining the mini-rectangle around max and its neighbors
    i_min = max(i_max-1, 1); i_max_ = min(i_max+1, n_points)
    j_min = max(j_max-1, 1); j_max_ = min(j_max+1, n_points)
    A_neighbors = [A_vals[ii] for ii in i_min:i_max_]
    B_neighbors = [B_vals[jj] for jj in j_min:j_max_]

    start_min, start_max = minimum(A_neighbors), maximum(A_neighbors)
    rate_min,  rate_max  = minimum(B_neighbors), maximum(B_neighbors)
end

# --- Checking total emissions for the optimum path ---
emissions_opt, welfare_opt = test_global_exp_c_tax(best_params[1], best_params[2])

# We keep only the period [tax_start_year, evaluation_end_year]
emis_budget = emissions_opt[mask]

total_emis_opt = sum(emis_budget)
println("Total emissions of  ", tax_start_year, " to ", evaluation_end_year,
        " for the scenario : ", total_emis_opt, " GtCO2")

println("\n=== Final result ===")
println("Best path: initial tax = $(best_params[1]), growth = $(best_params[2])")
println("Total discounted welfare = $best_welfare")

# Saving the results
save(joinpath("data","uniform_exp_tax_path_params.csv"),
     DataFrame(path=collect(best_params)); header=false)

# Save selected carbon tax pathway to CSV
tax_path = parse.(Float64, split(tax_path[1], '_'))
save(joinpath("data","uniform_exp_tax_path_params.csv"),
     DataFrame(path=tax_path); header=false)

# Extract corresponding welfare value
welfare_value_path = tot_welfare_disc[tot_welfare_disc.value .== maximum(tot_welfare_disc.value), :value]
