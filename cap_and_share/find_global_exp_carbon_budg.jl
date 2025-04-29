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

const scenario_name     = :Partnership # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership)

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

# Run function over ranges for start rate and growth rate
start_first   = 125
start_step    = 1
start_last    = 128
g_rate_first  = 0.022
g_rate_step   = 0.001
g_rate_last   = 0.025
nb_tests      = length(collect(start_first:start_step:start_last)) *
                length(collect(g_rate_first:g_rate_step:g_rate_last))
println("Number of trajectories tested: ", nb_tests)

## Store results in arrays
emissions_all   = Array{Float64}(undef, nb_steps, nb_tests)
welfare_all     = Array{Float64}(undef, nb_steps, nb_tests)
c_tax_paths     = Array{String}(undef, nb_tests)

function global_c_tax_loop(emissions_all, welfare_all, c_tax_paths)
    i = 0
    for start in start_first:start_step:start_last, g in g_rate_first:g_rate_step:g_rate_last
        i += 1
        println("Run $i/$nb_tests")

        emissions_all[:, i], welfare_all[:, i] = test_global_exp_c_tax(start, g)
        c_tax_paths[i] = string(start, "_", g)
    end
    return emissions_all, welfare_all, c_tax_paths
end

emissions_all, welfare_all, c_tax_paths = global_c_tax_loop(emissions_all, welfare_all, c_tax_paths)

######################################################
## Sélection selon 2 critères sur [tax_start_year, evaluation_end_year]
######################################################

# --- from 2020-2100 to tax_start_year-evaluation_end_year ---
mask      = (years_vec .>= tax_start_year) .& (years_vec .<= evaluation_end_year)
emissions = emissions_all[mask, :]
welfare   = welfare_all[mask, :]
# ---------------------------------------------------------

# Create vector for discounting à partir de la date de début
discount     = (1 .+ rho) .^ collect(0:evaluation_end_year-tax_start_year)

# Apply discount rate 
welfare_disc = welfare ./ discount 

# Change to DataFrame format
emissions_df    = DataFrame(emissions,    Symbol.(c_tax_paths))
welfare_df_disc = DataFrame(welfare_disc, Symbol.(c_tax_paths))

# Compute total emissions per trajectory
tot_emissions = combine(emissions_df, names(emissions_df) .=> sum .=> names(emissions_df))

# --- use emission_budget_limit to filter ---
valid                  = sum.(eachcol(emissions_df)) .<= emission_budget_limit
emissions_constrained  = emissions_df[:, valid]
# ---------------------------------------------------------

# Sum global welfare and keep only valid columns
tot_welfare_disc = combine(welfare_df_disc, names(welfare_df_disc) .=> sum .=> names(welfare_df_disc))
select!(tot_welfare_disc, names(emissions_constrained))

# Finding the optimum trajectory
tot_welfare_disc = stack(tot_welfare_disc)
tax_path         = tot_welfare_disc[tot_welfare_disc.value .== maximum(tot_welfare_disc.value), :variable]
println("Selected global carbon tax pathway: ", tax_path)

# Save selected carbon tax pathway to CSV
tax_path = parse.(Float64, split(tax_path[1], '_'))
save(joinpath("data","uniform_exp_tax_path_params.csv"),
     DataFrame(path=tax_path); header=false)

# Extract corresponding welfare value
welfare_value_path = tot_welfare_disc[tot_welfare_disc.value .== maximum(tot_welfare_disc.value), :value]
