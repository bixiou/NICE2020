
######################################################################################################################
# This file finds a suitable global carbon tax trajectory for the NICE2020 model
######################################################################################################################

# Activate the project and make sure all packages we need are installed.
using Pkg
#Pkg.activate(joinpath(@__DIR__, "..", ".."))
Pkg.activate(joinpath(@__DIR__, ".."))
#Pkg.resolve() # To resolve inconsistencies between Manifest.toml and Project.toml
Pkg.instantiate()

# Load required Julia packages.
using Mimi, MimiFAIRv2, DataFrames, CSVFiles

# Make directory to optionally save tested pathways outputs
output_directory_test_2deg_global = joinpath(@__DIR__, "..", "test_2deg_global_exp")
mkpath(output_directory_test_2deg_global)

println("Test global carbon tax runs")

# Define a function that creates a global tax trajectory from starting level and growth rate,
# runs the model with this carbon tax trajectory and outputs yearly global temperature and emissions
function test_global_exp_c_tax(tax_start_value_test, g_rate_test)

    full_co2_tax = exp_tax_trajectory(tax_start_value = tax_start_value_test, g_rate=g_rate_test, year_tax_start=2020, year_tax_end=2200)

    update_param!(nice_v2, :abatement, :global_carbon_tax, full_co2_tax)

    run(nice_v2)

    temperature = nice_v2[:temperature, :T]
    welfare     = nice_v2[:welfare, :welfare_global]


    return temperature, welfare
end

# Load NICE2020 source code.
#include(joinpath("..", "nice_v2_module.jl"))
#include(joinpath("..", "helper_functions.jl"))

include(joinpath(@__DIR__, "..", "src", "nice2020_module.jl"))
include(joinpath(@__DIR__, "..", "src", "helper_functions.jl"))
# Get baseline instance of the model and set the model to run in "global carbon tax" control regime and no revenue recycling
#nice_v2 = nice_v2_module.create_nice_v2()
nice_v2 = MimiNICE2020.create_nice2020()
#update_param!(nice_v2, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice_v2, :switch_recycle, 0) # Switch carbon taxation recycling off 
update_param!(nice_v2, :abatement, :control_regime, 1)

# Get number of time steps in the model
nb_steps   = length(dim_keys(nice_v2, :time))

# Run function over ranges for start rate and growth rate
start_first = 240
start_step = 1
start_last = 250
g_rate_first = 0.022
g_rate_step  = 0.001
g_rate_last  = 0.025
nb_tests = length(collect(start_first:start_step:start_last)) * length(collect(g_rate_first:g_rate_step:g_rate_last))
println("Number of trajectories tested: ", nb_tests)

years_vec = collect(2020:1:(2020+nb_steps-1))

## Store results in arrays with dimensions model_years x number of tested trajectories,
## and store names of tested tax paths as strings in a vector

# Initialize arrays
temperature = Array{Float64}(undef, length(years_vec), nb_tests)
welfare     = Array{Float64}(undef,  length(years_vec), nb_tests)
c_tax_paths = Array{String}(undef, nb_tests)


function global_c_tax_loop(temperature, welfare, c_tax_paths)
    i=0
    for start in start_first:start_step:start_last, g in g_rate_first:g_rate_step:g_rate_last
    
        i += 1
        println("Run $i/$nb_tests" )

        temperature[:, i],  welfare[:, i] = test_global_exp_c_tax(start, g)
        
        #c_tax_paths[1,i], c_tax_paths[2,i] = start, increase
        c_tax_paths[i] = string(start, "_", g)
    end

    return temperature, welfare, c_tax_paths
end

temperature, welfare, c_tax_paths = global_c_tax_loop(temperature, welfare, c_tax_paths)

######################################################
## Select the carbon tax pathway with 2 criteria:
# 1. temperature increase below 2°C from 2020 to 2120
# 2. highest total welfare from 2020 to the end_date, 
# with welfare discounted at rate rho
#####################################################

end_date_welfare = 2100
rho = 0.015

# Keep values between 2020 and 2120
temperature = temperature[years_vec .<= 2100 , :]
welfare = welfare[years_vec .<= end_date_welfare , :]

# Create vector for discounting
discount = (1+rho) .^ (collect(0:1:(end_date_welfare-2020)))

# Apply discount rate 
welfare_disc = welfare ./ discount 

# Change to dataframe format with one carbon tax trajectory per column
temperature = DataFrame(temperature, Symbol.(c_tax_paths))
welfare_disc = DataFrame(welfare_disc, Symbol.(c_tax_paths))

# Drop trajectories that lead to temperatures above 2.01°C by 2120
temperature_constrained = temperature[!, Not(any.(>(1.81), eachcol(temperature)))] 

# Sum global welfare over 2020-2100 
tot_welfare_disc = combine(welfare_disc, names(welfare_disc) .=> sum .=> names(welfare_disc))

# Remove trajectories that violate <2°C constraint from the total discounted welfare dataframe 
select!(tot_welfare_disc, names(temperature_constrained))

# Find the highest welfare value and extract corresponding trajectory
tot_welfare_disc = stack(tot_welfare_disc) #reshape wide to long, so welfare -> :value and name of tax trajetory -> :variable
tax_path = tot_welfare_disc[tot_welfare_disc.value .== maximum(tot_welfare_disc.value, init=-Inf), :variable]
println("Selected global carbon tax pathway: ", tax_path )

# Save selected carbon tax pathway to csv in data folder
tax_path = parse.(Float64,split(tax_path[1],'_')) #Extract the initial level and increase rate from string
save(joinpath( "data","uniform_exp_tax_path_params.csv"), DataFrame(path=tax_path); header=false)

# Extract corresponding welfare value
welfare_value_path = tot_welfare_disc[tot_welfare_disc.value .== maximum(tot_welfare_disc.value, init=-Inf), :value]

# Optionally Save emissions, temperature and welfare outputs for tested runs
#=save(joinpath(output_directory_test_2deg_global, "temperature.csv"), temperature)
save(joinpath(output_directory_test_2deg_global, "temperature_constrained.csv"), temperature_constrained)
save(joinpath(output_directory_test_2deg_global, "welfare.csv"), welfare)
save(joinpath(output_directory_test_2deg_global, "tot_discounted_welfare_constrained.csv"), tot_welfare_disc)
save(joinpath(output_directory_test_2deg_global,"uniform_exp_tax_path_welfare.csv"), DataFrame(value=welfare_value_path); header=false)
=#