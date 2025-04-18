#########################################################
# This file produces example runs for the NICE2020 model
#########################################################

#create your own "chemin.txt" to find NICE2020
chemin = read("chemin.txt", String) |> strip  
cd(chemin)  


# Activate the project and make sure all packages we need
# are installed.
using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
#Pkg.resolve() # To resolve inconsistencies between Manifest.toml and Project.toml
Pkg.instantiate()

# Load required Julia packages.
using Mimi, MimiFAIRv2, DataFrames, CSVFiles

println("Load NICE2020 source code.")
# Load NICE2020 source code.
include("nice2020_module.jl")

# ------------------------------------------------------------------------------------------------
# RETRIEVE NECESSARY PARAMETERS FROM THE BASE MODEL 
# ------------------------------------------------------------------------------------------------

println("Creating an instance of the NICE2020 model and retrieving some necessary parameters.")

base_model = MimiNICE2020.create_nice2020()

nb_steps   = length(dim_keys(base_model, :time))
nb_country = length(dim_keys(base_model, :country))
nb_quantile = length(dim_keys(base_model, :quantile))

# Share of recycled carbon tax revenue that each region-quantile pair receives (row = country, column = quantile)
recycle_share = ones(nb_country,nb_quantile) .* 1/nb_quantile

#---------------------------------------------------------
# CARBON TAX PATHWAY
# ----------------------------------------------------

#Example linear uniform carbon tax pathway (not optimised), 2017 USD per tCO2
global_co2_tax = MimiNICE2020.linear_tax_trajectory(tax_start_value = 90, increase_value=7, year_tax_start=2020, year_tax_end=2200)

#------------
# DIRECTORIES
#------------

output_directory_bau = joinpath(@__DIR__, "..", "results", "bau_no_policy_at_all")
mkpath(output_directory_bau)

output_directory_uniform = joinpath(@__DIR__, "..", "results", "uniform_tax_example")
mkpath(output_directory_uniform)

#---------------------------------------------------------------------------------------------------
#0- Run a baseline version of the model without CO2 mitigation.
#---------------------------------------------------------------------------------------------------

println("--0-- Baseline model without CO2 mitigation")

println("Creating an instance of the NICE2020 model and updating some parameters.")

# Get an instance of the BAU no-policy model. This includes the user-specifications but has no CO2 mitigation policy (will be used to calculte global CO2 policy).
bau_model = MimiNICE2020.create_nice2020()

update_param!(bau_model, :abatement, :control_regime, 3) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(bau_model, :abatement, :μ_input, zeros(nb_steps, nb_country))

println("Running the updated model and saving the output in the directory: ", output_directory_bau,)

run(bau_model)

# Save the bau (see helper functions for saving function details)
MimiNICE2020.save_nice2020_results(bau_model, output_directory_bau, revenue_recycling=false)


# ----------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------
#1- Uniform global carbon tax (non-optimized), with revenues not recycled (returned to households) 
# ----------------------------------------------------------------------------------------------
# ----------------------------------------------------------------------------------------------

println("--1-- Example run with uniform global carbon tax (non-optimized), with revenues not recycled (returned to households)")

println("Creating an instance of the NICE2020 model and updating some parameters.")

# Get baseline instance of the model.
nice2020_uniform_tax = MimiNICE2020.create_nice2020()

switch_recycle  = 0 # OFF   Recycle revenues to households
switch_scenario = :Partnership  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership)

# Set uniform global carbon tax rates and run model.
update_param!(nice2020_uniform_tax, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_uniform_tax, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_uniform_tax, :switch_recycle, switch_recycle)
update_param!(nice2020_uniform_tax, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

println("Running the updated model and saving the output in the directory: ", output_directory_uniform,)

run(nice2020_uniform_tax)

# Save the run (see helper functions for saving function details)
MimiNICE2020.save_nice2020_results(nice2020_uniform_tax, output_directory_uniform, revenue_recycling=false)


# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------
#2- Uniform global carbon tax (non-optimized), with revenues recycled within countries
# -------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------

println("--2-- Example run with global carbon tax (non-optimized), with revenues recycled within countries")

println("Updating some parameters of the previously created NICE2020 instance.")

switch_recycle                  = 1 # ON     Recycle revenues to households
switch_scope_recycle            = 0 # OFF    Carbon tax revenues recycled at country level (0) or globally (1)
switch_global_pc_recycle        = 0 # OFF    Carbon tax revenues recycled on an equal per capita basis
switch_scenario                 = :Partnership  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership)


# Set uniform taxes, revenue recycling switches and run the model
update_param!(nice2020_uniform_tax, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_uniform_tax, :abatement, :global_carbon_tax, global_co2_tax)

update_param!(nice2020_uniform_tax, :switch_recycle, switch_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :switch_scope_recycle, switch_scope_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
update_param!(nice2020_uniform_tax, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

println("Selected Scenario : ", switch_scenario)

println("Running the updated model and saving the output in the directory: ", output_directory_uniform,)

run(nice2020_uniform_tax)

# Save the recycle run (see helper functions for saving function details)
MimiNICE2020.save_nice2020_results(nice2020_uniform_tax, output_directory_uniform, revenue_recycling=true, recycling_type=1)


#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#3- Uniform global carbon tax (non-optimized), with revenues recycled globally (equal per capita)
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

println("--3-- Example run with global carbon tax (non-optimized), with revenues recycled globally (equal per capita)")

println("Updating some parameters of the previously created NICE2020 instance.")

switch_recycle                  = 1 # ON     Recycle revenues to households
switch_scope_recycle            = 1 # ON     Carbon tax revenues recycled globally
switch_global_pc_recycle        = 1 # ON    Carbon tax revenues recycled on an equal per capita basis
switch_scenario                 = :Partnership  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership)

# Rule for share of global tax revenues recycled at global level (switch_recycle and switch_scope_recycle must be ON)
global_recycle_share            = 1 # 100%   Share of tax revenues recycled globally 


# Set uniform taxes, revenue recycling switches and run the model
update_param!(nice2020_uniform_tax, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_uniform_tax, :abatement, :global_carbon_tax, global_co2_tax)

update_param!(nice2020_uniform_tax, :switch_recycle, switch_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :switch_scope_recycle, switch_scope_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_uniform_tax, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

println("Selected Scenario : ", switch_scenario)

println("Running the updated model and saving the output in the directory: ", output_directory_uniform,)

run(nice2020_uniform_tax)

# Save the recycle run (see helper functions for saving function details)
MimiNICE2020.save_nice2020_results(nice2020_uniform_tax, output_directory_uniform, revenue_recycling=true, recycling_type=2)


#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------
#4- Uniform global carbon tax (non-optimized), with revenues recycled globally (equal per capita)
#   Changing the value of the inequality aversion η
#------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------

println("--4-- Example run changing the parameter η, with global carbon tax (non-optimized) and revenues recycled globally (equal per capita)")

switch_recycle                  = 1 # ON     Recycle revenues to households
switch_scope_recycle            = 1 # ON     Carbon tax revenues recycled globally
switch_global_pc_recycle        = 1 # ON    Carbon tax revenues recycled on an equal per capita basis
switch_scenario                 = :Partnership  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership)

# Rule for share of global tax revenues recycled at global level (switch_recycle and switch_scope_recycle must be ON)
global_recycle_share            = 1 # 100%   Share of tax revenues recycled globally 

# Set inform taxes, revenue recycling switches and run the model
update_param!(nice2020_uniform_tax, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_uniform_tax, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_uniform_tax, :switch_recycle, switch_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :switch_scope_recycle, switch_scope_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
update_param!(nice2020_uniform_tax, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share )
update_param!(nice2020_uniform_tax, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

# CHANGE THE VALUE OF THE η PARAMETER
# Note that η is a shared parameter, it enters both in the abatement and the welfare  components

# Print the current value of η in the welfare component
println("In the welfare component η=", nice2020_uniform_tax[:welfare, :η], ", in the abatement component η=", nice2020_uniform_tax[:abatement, :η])

println("Updating the η parameter in all connected components (welfare and abatement) and running the model.")

# Update the η parameter in all components it is used in
update_param!(nice2020_uniform_tax, :η, 1) 
run(nice2020_uniform_tax)
println("In the welfare component η=", nice2020_uniform_tax[:welfare, :η], ", in the abatement component η=", nice2020_uniform_tax[:abatement, :η])

println("Updating the η parameter in only the welfare component and running the model.")

# This updates only the η parameter in the welfare component, not in the abatement component
disconnect_param!(nice2020_uniform_tax, :welfare, :η)
update_param!(nice2020_uniform_tax, :welfare, :η, 2) 
run(nice2020_uniform_tax)

println("In the welfare component η=", nice2020_uniform_tax[:welfare, :η], ", in the abatement component η=", nice2020_uniform_tax[:abatement, :η])


println("All done!")
