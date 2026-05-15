#########################################################
# This file reproduces runs for "From Global Policies to Phase Out Fossil Fuels To a Sustainable Union"
#########################################################

#create your own "path.txt" to find NICE2020
# path = read("path.txt", String) |> strip  
cd("/Users/theop/Desktop/NICE2020")  

# Activate the project and make sure packages are installed.
using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
#Pkg.resolve() # To resolve inconsistencies between Manifest.toml and Project.toml
Pkg.instantiate()
using Mimi, MimiFAIRv2, DataFrames, CSVFiles, CSV

include("nice2020_module.jl") 
# Creating an instance of the NICE2020 model and retrieving some necessary parameters

base_model = MimiNICE2020.create_nice2020()

nb_steps   = length(dim_keys(base_model, :time))
nb_country = length(dim_keys(base_model, :country))
nb_quantile = length(dim_keys(base_model, :quantile))
# Share of recycled carbon tax revenue that each region-quantile pair receives (row = country, column = quantile)
recycle_share = ones(nb_country,nb_quantile) .* 1/nb_quantile

###########################
# 1. BAU scenario (no abatement)
###########################

bau_model = MimiNICE2020.create_nice2020()
update_param!(bau_model, :abatement, :control_regime, 3) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(bau_model, :abatement, :μ_input, zeros(nb_steps, nb_country))
run(bau_model)
MimiNICE2020.save_nice2020_output(bau_model, joinpath(@__DIR__, "..", "cap_and_share", "output", "bau"))

###########################
# 2. FFU (Fossil-Free Union), with rights_proposed defined in R 
###########################

include("nice2020_module.jl") 
# RIGHTS PROPOSED 
rights_path = joinpath(@__DIR__, "..", "cap_and_share", "data", "input", "ffu_rights_proposed_allocation_below_bau.csv") #  non_losing_rights ffu_rights_proposed_allocation
df_rigths = CSV.read(rights_path, DataFrame)
year_cols  = filter(c -> startswith(string(c), "rights_proposed_"), names(df_rigths))
df_rigths_long  = stack(df_rigths, year_cols; variable_name = :year_str, value_name    = :rights_proposed)
df_rigths_long.rights_proposed .= df_rigths_long.rights_proposed ./ 1e9
df_rigths_long.time = parse.(Int, replace.(df_rigths_long.year_str, "rights_proposed_" => ""))
select!(df_rigths_long, Not(:year_str))
rename!(df_rigths_long, "code" => "country")
years_model     = collect(dim_keys(base_model, :time))     # ex. 2020:2300
countries_model = dim_keys(base_model, :country)           # ex. 179 codes
T = length(years_model)
C = length(countries_model)
rights_proposed_mat = zeros(Float64, T, C)
idx_year    = Dict(y => i for (i,y) in enumerate(years_model))
idx_country = Dict(string(c) => j for (j,c) in enumerate(countries_model))
for row in eachrow(df_rigths_long)
    y = row.time
    c = string(row.country)
    if haskey(idx_year, y) && haskey(idx_country, c)
        i = idx_year[y]
        j = idx_country[c]
        rights_proposed_mat[i, j] = row.rights_proposed
    end
end

# Rights proposed csv name (used to save results)
filename = basename(rights_path)              # "ffu_rights_proposed_allocation.csv"
basename_without_ext = splitext(filename)[1]  # "ffu_rights_proposed_allocation"
prefix = replace(basename_without_ext, "_rights_proposed_allocation" => "")  # "ffu"

# CARBON TAX PATHWAY from the Union's 1.9°C scenario
years = collect(dim_keys(base_model, :time))
global_co2_tax = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_tax_ffu.csv"), DataFrame) # below_bau_calibrated_global_tax_union calibrated_global_tax_union
df_tax.time       = Int.(df_tax.time)   # be sure it is Int
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
for (i, y) in enumerate(years)
    if haskey(tax_dict, y)
        global_co2_tax[i] = tax_dict[y]
    else
        global_co2_tax[i] = 0.0
    end
end

nice2020_ffu = MimiNICE2020.create_nice2020()
switch_recycle                  = 1 # ON     Recycle revenues to households
switch_global_recycling         = 1 # ON     Carbon tax revenues recycled globally
switch_global_pc_recycle        = 1 # ON    Carbon tax revenues recycled on an equal per capita basis
switch_scenario                 = :Union  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
switch_transfers_affect_growth  = 1 # Can compute economic data including redistributive effect 
switch_custom_transfers         = 1
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)

update_param!(nice2020_ffu, :switch_custom_transfers, switch_custom_transfers)
# Rule for share of global tax revenues recycled at global level (switch_recycle and switch_global_recycling must be ON)
global_recycle_share            = 1 # 100%   Share of tax revenues recycled globally 

# Set uniform taxes, revenue recycling switches and run the model
update_param!(nice2020_ffu, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_ffu, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_ffu, :switch_footprint, switch_footprint)
update_param!(nice2020_ffu, :revenue_recycle, :rights_proposed, rights_proposed_mat)

update_param!(nice2020_ffu, :switch_recycle, switch_recycle)
update_param!(nice2020_ffu, :switch_global_recycling, switch_global_recycling)
update_param!(nice2020_ffu, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
update_param!(nice2020_ffu, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_ffu, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
update_param!(nice2020_ffu, :switch_transfers_affect_growth, switch_transfers_affect_growth)

run(nice2020_ffu)

MimiNICE2020.save_nice2020_output(nice2020_ffu, joinpath(@__DIR__, "..", "cap_and_share", "output", "ffu"))
#run(`powershell -c "[console]::beep(1000, 300)"`)

###########################
# 3. global_price_ffu: Global (all countries) carbon pricing with price equal to Union's one.
###########################
include("nice2020_module.jl") 

# CARBON TAX PATHWAY from the Union's 1.9°C scenario
years = collect(dim_keys(base_model, :time))
global_co2_tax = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_tax_ffu.csv"), DataFrame) # below_bau_calibrated_global_tax_union calibrated_global_tax_union
df_tax.time       = Int.(df_tax.time)   # be sure it is Int
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
for (i, y) in enumerate(years)
    if haskey(tax_dict, y)
        global_co2_tax[i] = tax_dict[y]
    else
        global_co2_tax[i] = 0.0
    end
end

nice2020_global_price_ffu = MimiNICE2020.create_nice2020()

switch_recycle  = 1 # ON   
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_global_price_ffu, :switch_custom_transfers, 0)
switch_transfers_affect_growth           = 1 # Can compute economic data including redistributive effect 
switch_global_recycling        = 1
switch_global_pc_recycle        = 1
global_recycle_share            = 1
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)
switch_transfers_affect_growth    = 1 # Can compute economic data including redistributive effect 

switch_custom_transfers = 0        # 
update_param!(nice2020_global_price_ffu, :switch_custom_transfers, switch_custom_transfers)

update_param!(nice2020_global_price_ffu, :switch_recycle, switch_recycle)
update_param!(nice2020_global_price_ffu, :switch_global_recycling, switch_global_recycling)
update_param!(nice2020_global_price_ffu, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_global_price_ffu, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
# Set uniform global carbon tax rates and run model.
update_param!(nice2020_global_price_ffu, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_global_price_ffu, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_global_price_ffu, :switch_footprint, switch_footprint)
update_param!(nice2020_global_price_ffu, :switch_recycle, switch_recycle)
update_param!(nice2020_global_price_ffu, :switch_transfers_affect_growth, switch_transfers_affect_growth)
update_param!(nice2020_global_price_ffu, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
#update_param!(nice2020_global_price_ffu, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(nice2020_global_price_ffu)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_price_ffu, output_directory_uniform, revenue_recycling=false)
MimiNICE2020.save_nice2020_output(nice2020_global_price_ffu, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_price_ffu"))
#run(`powershell -c "[console]::beep(1000, 300)"`)

###########################
# 4. non_losing: Within-country carbon pricing, with non-losing rights
###########################

# NON-LOSING RIGHTS
rights_path = joinpath(@__DIR__, "..", "cap_and_share", "data", "input", "non_losing_rights.csv") 
df_rigths = CSV.read(rights_path, DataFrame)
year_cols  = filter(c -> startswith(string(c), "rights_proposed_"), names(df_rigths))
df_rigths_long  = stack(df_rigths, year_cols; variable_name = :year_str, value_name    = :rights_proposed)
df_rigths_long.rights_proposed .= df_rigths_long.rights_proposed ./ 1e9
df_rigths_long.time = parse.(Int, replace.(df_rigths_long.year_str, "rights_proposed_" => ""))
select!(df_rigths_long, Not(:year_str))
rename!(df_rigths_long, "code" => "country")
years_model     = collect(dim_keys(base_model, :time))     # ex. 2020:2300
countries_model = dim_keys(base_model, :country)           # ex. 179 codes
T = length(years_model)
C = length(countries_model)
rights_mat = zeros(Float64, T, C)
idx_year    = Dict(y => i for (i,y) in enumerate(years_model))
idx_country = Dict(string(c) => j for (j,c) in enumerate(countries_model))
for row in eachrow(df_rigths_long)
    y = row.time
    c = string(row.country)
    if haskey(idx_year, y) && haskey(idx_country, c)
        i = idx_year[y]
        j = idx_country[c]
        rights_mat[i, j] = row.rights_proposed
    end
end

nice2020_non_losing = MimiNICE2020.create_nice2020()

switch_recycle                  = 1 # ON     Recycle revenues to households
switch_global_recycling        = 0 # OFF    Carbon tax revenues recycled at country level (0) or globally (1)
switch_global_pc_recycle        = 0 # OFF    Carbon tax revenues recycled on an equal per capita basis
switch_scenario                 = :Union  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
switch_transfers_affect_growth    = 1 # Can compute economic data including redistributive effect 
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)

switch_custom_transfers = 0

update_param!(nice2020_non_losing, :abatement, :control_regime, 5) # Switch to specify emissions pathways
update_param!(nice2020_non_losing, :abatement, :rights_mat, rights_mat)
update_param!(nice2020_non_losing, :switch_footprint, switch_footprint)

update_param!(nice2020_non_losing, :switch_recycle, switch_recycle)
update_param!(nice2020_non_losing, :switch_global_recycling, switch_global_recycling)
update_param!(nice2020_non_losing, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
update_param!(nice2020_non_losing, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
update_param!(nice2020_non_losing, :switch_transfers_affect_growth, switch_transfers_affect_growth)
update_param!(nice2020_non_losing, :switch_custom_transfers, switch_custom_transfers)

run(nice2020_non_losing)

MimiNICE2020.save_nice2020_output(nice2020_non_losing, joinpath(@__DIR__, "..", "cap_and_share", "output", "non_losing"))
#run(`powershell -c "[console]::beep(1000, 300)"`)

###########################
# 5. global_cap_share: Global (all countries) egalitarian carbon pricing with 1.8°C carbon budget (corresponding to FFU's budget).
###########################

# CARBON TAX PATHWAY 
years = collect(dim_keys(base_model, :time))
global_co2_tax = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_cs.csv"), DataFrame) # calibrated_global_tax_ffu below_bau_calibrated_global_tax_union calibrated_global_tax_union
df_tax.time       = Int.(df_tax.time)   # be sure it is Int
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
for (i, y) in enumerate(years)
    if haskey(tax_dict, y)
        global_co2_tax[i] = tax_dict[y]
    else
        global_co2_tax[i] = 0.0
    end
end

nice2020_global_cap_share = MimiNICE2020.create_nice2020()

switch_recycle  = 1 # ON   
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_global_cap_share, :switch_custom_transfers, 0)
switch_transfers_affect_growth           = 1 # Can compute economic data including redistributive effect 
switch_global_recycling        = 1
switch_global_pc_recycle        = 1
global_recycle_share            = 1
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)
switch_transfers_affect_growth    = 1 # Can compute economic data including redistributive effect 

switch_custom_transfers = 0        # 
update_param!(nice2020_global_cap_share, :switch_custom_transfers, switch_custom_transfers)

update_param!(nice2020_global_cap_share, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share, :switch_global_recycling, switch_global_recycling)
update_param!(nice2020_global_cap_share, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_global_cap_share, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
# Set uniform global carbon tax rates and run model.
update_param!(nice2020_global_cap_share, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_global_cap_share, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_global_cap_share, :switch_footprint, switch_footprint)
update_param!(nice2020_global_cap_share, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share, :switch_transfers_affect_growth, switch_transfers_affect_growth)
update_param!(nice2020_global_cap_share, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
# update_param!(nice2020_global_cap_share, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(nice2020_global_cap_share)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, output_directory_uniform, revenue_recycling=false)
MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share"))
#run(`powershell -c "[console]::beep(1000, 300)"`)
include("helper_functions.jl")


MimiNICE2020.save_nice2020_reduced_output(nice2020_global_cap_share, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share_reduced"))

###########################
# 6. IMF: IMF proposal - $25/t LIC & LMIC, $50 UMIC, $75 HIC starting from 2025 (2025-2300)
###########################

#We load the list of LIC, LMIC, UMIC and HIC countries from parameters.jl
include("../data/parameters.jl")

#Creation of the differenciated tax
tax_lic_lmic = 25.0 # $/tCO2 for 2025-2030 (checked that same unit as global_co2_tax)
tax_umic = 50.0 # $/tCO2 for 2025-2030
tax_hic = 75.0 # $/tCO2 for 2025-2030

nice2020_IMF = MimiNICE2020.create_nice2020()

# Creation of the country x year matrix of carbon tax rates

years = collect(dim_keys(nice2020_IMF, :time))
countries = collect(dim_keys(nice2020_IMF, :country))

diff_country_tax_1 = zeros(Float64, length(years), length(countries))

LIC_LMIC = Symbol.(LIC_LMIC)
UMIC     = Symbol.(UMIC)
HIC      = Symbol.(HIC)

years_index = findall(y -> 2025 <= y, years)

for t in years_index
    for (c_idx, country) in enumerate(countries)
        if country in LIC_LMIC
            diff_country_tax_1[t, c_idx] = tax_lic_lmic
        elseif country in UMIC
            diff_country_tax_1[t, c_idx] = tax_umic
        elseif country in HIC
            diff_country_tax_1[t, c_idx] = tax_hic
        else
            diff_country_tax_1[t, c_idx] = 0.0  # si pays non classé
        end
    end
end


global_recycle_share            = 0
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_IMF, :switch_custom_transfers, 0)
update_param!(nice2020_IMF, :switch_recycle, 1)
update_param!(nice2020_IMF, :switch_global_recycling, 0)
update_param!(nice2020_IMF, :revenue_recycle, :global_recycle_share, ones(nb_country) * global_recycle_share) 
update_param!(nice2020_IMF, :revenue_recycle, :switch_global_pc_recycle, 0)

update_param!(nice2020_IMF, :abatement, :control_regime, 4) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_IMF, :abatement, :direct_country_tax, diff_country_tax_1)
update_param!(nice2020_IMF, :switch_footprint, 0)
update_param!(nice2020_IMF, :switch_transfers_affect_growth, 1)
update_param!(nice2020_IMF, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

run(nice2020_IMF)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, output_directory_uniform, revenue_recycling=false)
dir_imf_1=joinpath(@__DIR__, "..", "cap_and_share", "output", "IMF")
#mkpath(dir_imf_1)  # create directory if it does not exist
MimiNICE2020.save_nice2020_output(nice2020_IMF, joinpath(@__DIR__, "..", "cap_and_share", "output", "IMF"))

###########################
# 7. IMF_2: IMF proposal - $25/t LIC & LMIC, $50 UMIC, $75 HIC for 2025-30, increasing at x% beyond that, where x is chosen to get us to 2+/-.1°C
###########################

#We load the list of LIC, LMIC, UMIC and HIC countries from parameters.jl
include("../data/parameters.jl")

#Creation of the differenciated tax
tax_lic_lmic = 25.0 # $/tCO2 for 2025-2030 (checked that same unit as global_co2_tax)
tax_umic = 50.0 # $/tCO2 for 2025-2030
tax_hic = 75.0 # $/tCO2 for 2025-2030

nice2020_IMF_2 = MimiNICE2020.create_nice2020()

# Creation of the country x year matrix of carbon tax rates

years = collect(dim_keys(nice2020_IMF_2, :time))
countries = collect(dim_keys(nice2020_IMF_2, :country))

diff_country_tax_2 = zeros(Float64, length(years), length(countries))

LIC_LMIC = Symbol.(LIC_LMIC)
UMIC     = Symbol.(UMIC)
HIC      = Symbol.(HIC)

years_index = findall(y -> 2025 <= y <= 2030, years)

for t in years_index
    for (c_idx, country) in enumerate(countries)
        if country in LIC_LMIC
            diff_country_tax_2[t, c_idx] = tax_lic_lmic
        elseif country in UMIC
            diff_country_tax_2[t, c_idx] = tax_umic
        elseif country in HIC
            diff_country_tax_2[t, c_idx] = tax_hic
        else
            diff_country_tax_2[t, c_idx] = 0.0  # si pays non classé
        end
    end
end

years_index_post2030 = findall(y -> y > 2030, years)

# Growth rate of the tax beyond 2030, chosen to reach approx 2°C => target of 2.02°C in 2100
growth_rate = 0.036
for t in years_index_post2030
    for (c_idx, country) in enumerate(countries)
        diff_country_tax_2[t, c_idx] = diff_country_tax_2[t-1, c_idx] * (1 + growth_rate)
    end
end



global_recycle_share            = 0
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_IMF_2, :switch_custom_transfers, 0)
update_param!(nice2020_IMF_2, :switch_recycle, 1)
update_param!(nice2020_IMF_2, :switch_global_recycling, 0)
update_param!(nice2020_IMF_2, :revenue_recycle, :global_recycle_share, ones(nb_country) * global_recycle_share) 
update_param!(nice2020_IMF_2, :revenue_recycle, :switch_global_pc_recycle, 0)

update_param!(nice2020_IMF_2, :abatement, :control_regime, 4) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_IMF_2, :abatement, :direct_country_tax, diff_country_tax_2)
update_param!(nice2020_IMF_2, :switch_footprint, 0)
update_param!(nice2020_IMF_2, :switch_transfers_affect_growth, 1)
update_param!(nice2020_IMF_2, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

run(nice2020_IMF_2)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, output_directory_uniform, revenue_recycling=false)
dir=joinpath(@__DIR__, "..", "cap_and_share", "output", "IMF_2")
# mkpath(dir)  # create directory if it does not exist
MimiNICE2020.save_nice2020_output(nice2020_IMF_2, joinpath(@__DIR__, "..", "cap_and_share", "output", "IMF_2"))

###########################
# 8. Stoft: scenario cap_and_share but with global_recycle_share = 0.1
###########################

# CARBON TAX PATHWAY 
years = collect(dim_keys(base_model, :time))
global_co2_tax = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_cs.csv"), DataFrame) # calibrated_global_tax_ffu below_bau_calibrated_global_tax_union calibrated_global_tax_union
df_tax.time       = Int.(df_tax.time)   # be sure it is Int
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
for (i, y) in enumerate(years)
    if haskey(tax_dict, y)
        global_co2_tax[i] = tax_dict[y]
    else
        global_co2_tax[i] = 0.0
    end
end

nice2020_stoft = MimiNICE2020.create_nice2020()
  
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
global_recycle_share            = 0.1

update_param!(nice2020_stoft, :switch_custom_transfers, 0)
update_param!(nice2020_stoft, :switch_recycle, 1)
update_param!(nice2020_stoft, :switch_global_recycling, 1)
update_param!(nice2020_stoft, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_stoft, :revenue_recycle, :switch_global_pc_recycle, 1)
# Set uniform global carbon tax rates and run model.
update_param!(nice2020_stoft, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_stoft, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_stoft, :switch_footprint, 1)
update_param!(nice2020_stoft, :switch_transfers_affect_growth, 1)
update_param!(nice2020_stoft, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
# update_param!(nice2020_stoft, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(nice2020_stoft)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, output_directory_uniform, revenue_recycling=false)
dir_b=joinpath(@__DIR__, "..", "cap_and_share", "output", "stoft")
# mkpath(dir_b) # => to execute only once to create the directory
MimiNICE2020.save_nice2020_output(nice2020_stoft, joinpath(@__DIR__, "..", "cap_and_share", "output", "stoft"))

###########################
#9: FFU_SU: FFU scenario with a tax on consumption and redistribution of 1% of the PIB
###########################

include("nice2020_module.jl") 
# RIGHTS PROPOSED 
rights_path = joinpath(@__DIR__, "..", "cap_and_share", "data", "input", "ffu_rights_proposed_allocation_below_bau.csv") #  non_losing_rights ffu_rights_proposed_allocation
df_rigths = CSV.read(rights_path, DataFrame)
year_cols  = filter(c -> startswith(string(c), "rights_proposed_"), names(df_rigths))
df_rigths_long  = stack(df_rigths, year_cols; variable_name = :year_str, value_name    = :rights_proposed)
df_rigths_long.rights_proposed .= df_rigths_long.rights_proposed ./ 1e9
df_rigths_long.time = parse.(Int, replace.(df_rigths_long.year_str, "rights_proposed_" => ""))
select!(df_rigths_long, Not(:year_str))
rename!(df_rigths_long, "code" => "country")
years_model     = collect(dim_keys(base_model, :time))     # ex. 2020:2300
countries_model = dim_keys(base_model, :country)           # ex. 179 codes
T = length(years_model)
C = length(countries_model)
rights_proposed_mat = zeros(Float64, T, C)
idx_year    = Dict(y => i for (i,y) in enumerate(years_model))
idx_country = Dict(string(c) => j for (j,c) in enumerate(countries_model))
for row in eachrow(df_rigths_long)
    y = row.time
    c = string(row.country)
    if haskey(idx_year, y) && haskey(idx_country, c)
        i = idx_year[y]
        j = idx_country[c]
        rights_proposed_mat[i, j] = row.rights_proposed
    end
end

# Rights proposed csv name (used to save results)
filename = basename(rights_path)              # "ffu_rights_proposed_allocation.csv"
basename_without_ext = splitext(filename)[1]  # "ffu_rights_proposed_allocation"
prefix = replace(basename_without_ext, "_rights_proposed_allocation" => "")  # "ffu"

# CARBON TAX PATHWAY from the Union's 1.9°C scenario
years = collect(dim_keys(base_model, :time))
global_co2_tax = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_tax_ffu.csv"), DataFrame) # below_bau_calibrated_global_tax_union calibrated_global_tax_union
df_tax.time       = Int.(df_tax.time)   # be sure it is Int
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
for (i, y) in enumerate(years)
    if haskey(tax_dict, y)
        global_co2_tax[i] = tax_dict[y]
    else
        global_co2_tax[i] = 0.0
    end
end

nice2020_ffu_su = MimiNICE2020.create_nice2020()

switch_scenario                 = :Union  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)

update_param!(nice2020_ffu_su, :switch_custom_transfers, 1)
# Rule for share of global tax revenues recycled at global level (switch_recycle and switch_global_recycling must be ON)
global_recycle_share            = 1 # 100%   Share of tax revenues recycled globally 

# Set uniform taxes, revenue recycling switches and run the model
update_param!(nice2020_ffu_su, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_ffu_su, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_ffu_su, :switch_footprint, 1)
update_param!(nice2020_ffu_su, :revenue_recycle, :rights_proposed, rights_proposed_mat)

update_param!(nice2020_ffu_su, :switch_recycle, 1)
update_param!(nice2020_ffu_su, :switch_global_recycling, 1)
update_param!(nice2020_ffu_su, :revenue_recycle, :switch_global_pc_recycle, 1)
update_param!(nice2020_ffu_su, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_ffu_su, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
update_param!(nice2020_ffu_su, :switch_transfers_affect_growth, 1)
update_param!(nice2020_ffu_su, :switch_consumption_tax, 1)
update_param!(nice2020_ffu_su, :quantile_recycle, :rate_ninth, 0.01)
update_param!(nice2020_ffu_su, :quantile_recycle, :rate_tenth, 0.06)
update_param!(nice2020_ffu_su, :neteconomy, :rate_pib, 0.01)
update_param!(nice2020_ffu_su, :quantile_recycle, :inefficiency_rate, 0.1)


run(nice2020_ffu_su)

dir_su=joinpath(@__DIR__, "..", "cap_and_share", "output", "ffu_su")
#mkpath(dir_su) # => to execute only once to create the directory
MimiNICE2020.save_nice2020_output(nice2020_ffu_su, joinpath(@__DIR__, "..", "cap_and_share", "output", "ffu_su"))

###########################
getdataframe(nice2020_ffu_su, :quantile_recycle, :new_conso_pc)
getdataframe(nice2020_ffu_su, :quantile_recycle, :conso_pc_base)
getdataframe(nice2020_ffu_su, :neteconomy, :pib_contrib)
println(getdataframe(nice2020_ffu_su, :quantile_recycle, :net_surplus))
println(getdataframe(nice2020_ffu_su, :quantile_recycle, :net_transfer_pib))

df = filter(row->row.time == 2050, getdataframe(nice2020_ffu_su, :quantile_recycle, :net_transfer_pib))
df_pos = filter(:net_transfer_pib => x -> x > 0, df)

df2 = filter(row -> row.time == 2050, getdataframe(nice2020_ffu_su, :quantile_recycle, :net_surplus))
df_neg = filter(:net_surplus => x -> x < 0, df2)
println(df_neg)

df3 = filter(row -> row.time == 2050, getdataframe(nice2020_ffu_su, :quantile_recycle, :net_surplus_per_pib))
df_neg = filter(:net_surplus_per_pib => x -> x < 0, df3)
println(df_neg)

println(getdataframe(nice2020_ffu_su, :quantile_recycle=>(:net_transfer_pib, :tot_tax_cons_country)))
println(filter(row -> row.time == 2050,getdataframe(nice2020_ffu_su, :quantile_recycle => (:tot_tax_cons_country, :net_transfer_pib))))
println(getdataframe(nice2020_ffu_su, :quantile_recycle=>(:recycle_pib, :pib_contrib)))

println(getdataframe(nice2020_ffu_su, :quantile_recycle=>(:conso_pc_base,:new_conso_pc)))

###########################
#10: CSU: Cap and Share Union : same participating countries as in FFU but with an egalitarian repartition of rights
###########################

# CARBON TAX PATHWAY from the Union's 1.9°C scenario
years = collect(dim_keys(base_model, :time))
global_co2_tax = zeros(Float64, nb_steps)
df_tax = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_tax_ffu.csv"), DataFrame) # below_bau_calibrated_global_tax_union calibrated_global_tax_union
df_tax.time       = Int.(df_tax.time)   # be sure it is Int
df_tax.global_tax = Float64.(df_tax.global_tax)
tax_dict = Dict(row.time => row.global_tax for row in eachrow(df_tax))
for (i, y) in enumerate(years)
    if haskey(tax_dict, y)
        global_co2_tax[i] = tax_dict[y]
    else
        global_co2_tax[i] = 0.0
    end
end

nice2020_csu = MimiNICE2020.create_nice2020()

switch_scenario                 = :Union  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
global_recycle_share            = 1 # 100%   Share of tax revenues recycled globally 

update_param!(nice2020_csu, :switch_custom_transfers, 0) # Difference with the FFU scenario
update_param!(nice2020_csu, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_csu, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_csu, :switch_footprint, 1)
update_param!(nice2020_csu, :switch_recycle, 1)
update_param!(nice2020_csu, :switch_global_recycling, 1)
update_param!(nice2020_csu, :revenue_recycle, :switch_global_pc_recycle, 1)
update_param!(nice2020_csu, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_csu, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
update_param!(nice2020_csu, :switch_transfers_affect_growth, 1)
#update_param!(nice2020_ffu_su, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(nice2020_csu)

dir_d=joinpath(@__DIR__, "..", "cap_and_share", "output", "csu")
#mkpath(dir_d) # => to execute only once to create the directory
MimiNICE2020.save_nice2020_output(nice2020_csu, joinpath(@__DIR__, "..", "cap_and_share", "output", "csu"))

###########################
# 11. global_cap_share_2c: Global (all countries) egalitarian carbon pricing with 2°C carbon budget
###########################

# CARBON TAX PATHWAY 
global_co2_tax = exp_tax_trajectory(tax_start_value = 184, g_rate = .002, year_tax_start = 2030, year_tax_end = 2200, ramp_up = 0) # 2°C
# global_co2_tax = exp_tax_trajectory(tax_start_value = 176, g_rate = .0168, year_tax_start = 2030, year_tax_end = 2200, ramp_up = 0) # 1.8°C

nice2020_global_cap_share_2c = MimiNICE2020.create_nice2020()

switch_recycle  = 1 # ON   
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_global_cap_share_2c, :switch_custom_transfers, 0)
switch_transfers_affect_growth           = 1 # Can compute economic data including redistributive effect 
switch_global_recycling        = 1
switch_global_pc_recycle        = 1
global_recycle_share            = 1
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)
switch_transfers_affect_growth    = 1 # Can compute economic data including redistributive effect 

switch_custom_transfers = 0        # 
update_param!(nice2020_global_cap_share_2c, :switch_custom_transfers, switch_custom_transfers)

update_param!(nice2020_global_cap_share_2c, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share_2c, :switch_global_recycling, switch_global_recycling)
update_param!(nice2020_global_cap_share_2c, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_global_cap_share_2c, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
# Set uniform global carbon tax rates and run model.
update_param!(nice2020_global_cap_share_2c, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_global_cap_share_2c, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_global_cap_share_2c, :switch_footprint, switch_footprint)
update_param!(nice2020_global_cap_share_2c, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share_2c, :switch_transfers_affect_growth, switch_transfers_affect_growth)
update_param!(nice2020_global_cap_share_2c, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
# update_param!(nice2020_global_cap_share_2c, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(nice2020_global_cap_share_2c)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share_2c, output_directory_uniform, revenue_recycling=false)
MimiNICE2020.save_nice2020_output(nice2020_global_cap_share_2c, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share_2c"))
#run(`powershell -c "[console]::beep(1000, 300)"`)


###########################
# 11. global_cap_share_18: Global (all countries) egalitarian carbon pricing with 2°C carbon budget
###########################

# CARBON TAX PATHWAY 
global_co2_tax = exp_tax_trajectory(tax_start_value = 176, g_rate = .0168, year_tax_start = 2030, year_tax_end = 2200, ramp_up = 0) # 1.8°C

nice2020_global_cap_share_2c = MimiNICE2020.create_nice2020()

switch_recycle  = 1 # ON   
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_global_cap_share_2c, :switch_custom_transfers, 0)
switch_transfers_affect_growth           = 1 # Can compute economic data including redistributive effect 
switch_global_recycling        = 1
switch_global_pc_recycle        = 1
global_recycle_share            = 1
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)
switch_transfers_affect_growth    = 1 # Can compute economic data including redistributive effect 

switch_custom_transfers = 0        # 
update_param!(nice2020_global_cap_share_2c, :switch_custom_transfers, switch_custom_transfers)

update_param!(nice2020_global_cap_share_2c, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share_2c, :switch_global_recycling, switch_global_recycling)
update_param!(nice2020_global_cap_share_2c, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_global_cap_share_2c, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
# Set uniform global carbon tax rates and run model.
update_param!(nice2020_global_cap_share_2c, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_global_cap_share_2c, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_global_cap_share_2c, :switch_footprint, switch_footprint)
update_param!(nice2020_global_cap_share_2c, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share_2c, :switch_transfers_affect_growth, switch_transfers_affect_growth)
update_param!(nice2020_global_cap_share_2c, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
# update_param!(nice2020_global_cap_share_2c, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(nice2020_global_cap_share_2c)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share_2c, output_directory_uniform, revenue_recycling=false)
MimiNICE2020.save_nice2020_output(nice2020_global_cap_share_2c, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share_18"))
#run(`powershell -c "[console]::beep(1000, 300)"`)

###########################
# 12. global_cap_share_15: Global (all countries) egalitarian carbon pricing with 1.5°C carbon budget
###########################

# CARBON TAX PATHWAY 
global_co2_tax = exp_tax_trajectory(tax_start_value = 408, g_rate = .0088, year_tax_start = 2030, year_tax_end = 2200, ramp_up = 5)

nice2020_global_cap_share_15 = MimiNICE2020.create_nice2020()

switch_recycle  = 1 # ON   
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_global_cap_share_15, :switch_custom_transfers, 0)
switch_transfers_affect_growth           = 1 # Can compute economic data including redistributive effect 
switch_global_recycling        = 1
switch_global_pc_recycle        = 1
global_recycle_share            = 1
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)
switch_transfers_affect_growth    = 1 # Can compute economic data including redistributive effect 

switch_custom_transfers = 0        # 
update_param!(nice2020_global_cap_share_15, :switch_custom_transfers, switch_custom_transfers)

update_param!(nice2020_global_cap_share_15, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share_15, :switch_global_recycling, switch_global_recycling)
update_param!(nice2020_global_cap_share_15, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(nice2020_global_cap_share_15, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
# Set uniform global carbon tax rates and run model.
update_param!(nice2020_global_cap_share_15, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_global_cap_share_15, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(nice2020_global_cap_share_15, :switch_footprint, switch_footprint)
update_param!(nice2020_global_cap_share_15, :switch_recycle, switch_recycle)
update_param!(nice2020_global_cap_share_15, :switch_transfers_affect_growth, switch_transfers_affect_growth)
update_param!(nice2020_global_cap_share_15, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
# update_param!(nice2020_global_cap_share_15, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(nice2020_global_cap_share_15)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share_15, output_directory_uniform, revenue_recycling=false)
MimiNICE2020.save_nice2020_output(nice2020_global_cap_share_15, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share_15"))
#run(`powershell -c "[console]::beep(1000, 300)"`)


###########################
# 13. global_cap_share_18_rampup: Global (all countries) egalitarian carbon pricing with 2°C carbon budget
###########################

# CARBON TAX PATHWAY 
global_co2_tax = exp_tax_trajectory(tax_start_value = 216, g_rate = .0128, year_tax_start = 2030, year_tax_end = 2200, ramp_up = 5) # 1.8°C 

global_cap_share_18_rampup = MimiNICE2020.create_nice2020()

switch_recycle  = 1 # ON   
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(global_cap_share_18_rampup, :switch_custom_transfers, 0)
switch_transfers_affect_growth           = 1 # Can compute economic data including redistributive effect 
switch_global_recycling        = 1
switch_global_pc_recycle        = 1
global_recycle_share            = 1
switch_footprint             = 1 # Switch for footprint calculation (1: ON, 0: OFF)
switch_transfers_affect_growth    = 1 # Can compute economic data including redistributive effect 

switch_custom_transfers = 0        # 
update_param!(global_cap_share_18_rampup, :switch_custom_transfers, switch_custom_transfers)

update_param!(global_cap_share_18_rampup, :switch_recycle, switch_recycle)
update_param!(global_cap_share_18_rampup, :switch_global_recycling, switch_global_recycling)
update_param!(global_cap_share_18_rampup, :revenue_recycle, :global_recycle_share,  ones(nb_country) * global_recycle_share ) 
update_param!(global_cap_share_18_rampup, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
# Set uniform global carbon tax rates and run model.
update_param!(global_cap_share_18_rampup, :abatement, :control_regime, 1) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(global_cap_share_18_rampup, :abatement, :global_carbon_tax, global_co2_tax)
update_param!(global_cap_share_18_rampup, :switch_footprint, switch_footprint)
update_param!(global_cap_share_18_rampup, :switch_recycle, switch_recycle)
update_param!(global_cap_share_18_rampup, :switch_transfers_affect_growth, switch_transfers_affect_growth)
update_param!(global_cap_share_18_rampup, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
# update_param!(global_cap_share_18_rampup, :revenue_recycle, :rights_proposed, rights_proposed_mat)

run(global_cap_share_18_rampup)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(global_cap_share_18_rampup, output_directory_uniform, revenue_recycling=false)
MimiNICE2020.save_nice2020_output(global_cap_share_18_rampup, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share_18_rampup"))
#run(`powershell -c "[console]::beep(1000, 300)"`)


###########################
#Year at which consumption_EDE becomes higher than consumption_EDE in the BAU scenario :
###########################

include("helper_functions.jl")

#Creation of a complete dataframe with all scenarios
scenarios = [nice2020_ffu, nice2020_global_cap_share, nice2020_IMF_2, nice2020_stoft]
scenario_names = ["FFU", "Global_Cap_Share", "IMF_2", "Stoft"]
df_temp = DataFrame()
df_final = DataFrame()
for (i, m) in enumerate(scenarios)
    df_temp = year_EDE_higher_than_BAU(m)
    df_temp[!, :scenario] .= scenario_names[i]
    if i == 1
        df_final = df_temp
    else
        append!(df_final, df_temp)
    end
end
println(df_final)

df_imf_2 = unstack(filter(row -> row.scenario == "IMF_2", df_final), :scenario, :year)
df_ffu = unstack(filter(row -> row.scenario == "FFU", df_final), :scenario, :year)
df_capshare = unstack(filter(row -> row.scenario == "Global_Cap_Share", df_final), :scenario, :year)
df_stoft = unstack(filter(row -> row.scenario == "Stoft", df_final), :scenario, :year)
df_merged = innerjoin(df_ffu, df_capshare, df_imf_2, df_stoft, on=:country)

path_year = joinpath(@__DIR__, "..", "cap_and_share", "output", "year_EDE_higher_than_BAU.csv")
CSV.write(path_year, df_merged)


###########################
#Code to retrieve the needed values :
###########################

include("helper_functions.jl")

countries_wanted = (:IND, :NGA, :CHN, :USA, :FRA, :COD, :RUS)
years_wanted = (2030, 2050, 2100)
scenarios = [bau_model, nice2020_global_cap_share, nice2020_ffu, nice2020_IMF, nice2020_IMF_2, nice2020_stoft, nice2020_csu, nice2020_non_losing, nice2020_ffu_su]
names_scenarios = ["BAU", "Global_Cap_Share", "FFU", "IMF", "IMF_2", "Stoft", "CSU", "Non-losing", "FFU_SU"]

results = build_results_csv(scenarios, names_scenarios, countries_wanted, years_wanted)

path = joinpath(@__DIR__, "..", "cap_and_share", "output", "comparison_output.csv")
CSV.write(path, results)

###########################
#Compute net present value of consumption EDE from 2030 to 2100 with a rate of 3% :
###########################

include("helper_functions.jl")
countries_wanted = (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :COD, :RUS)
years = dim_keys(base_model, :time)
scenarios = [bau_model, nice2020_global_cap_share, nice2020_ffu, nice2020_IMF, nice2020_IMF_2, nice2020_stoft, nice2020_csu, nice2020_non_losing, nice2020_ffu_su]
names_scenarios = ["BAU", "Global_Cap_Share", "FFU", "IMF", "IMF_2", "Stoft", "CSU", "Non-losing", "FFU_SU"]
data_npv = DataFrame(scenario = String[], country = String[], value = Float64[])

for c in countries_wanted
    for (i, m) in enumerate(scenarios)
        cons_EDE = filter(row -> row.country == c, getdataframe(m, :welfare=>:cons_EDE_country))
        npv_val = net_present_value(cons_EDE, 2030, 2100, 0.03, "cons_EDE_country")
        push!(data_npv,(String(names_scenarios[i]), String(c), npv_val))
    end
end

for (i, m) in enumerate(scenarios)
    cons_EDE = getdataframe(m, :welfare=>:cons_EDE_global)
    npv_val = net_present_value(cons_EDE, 2030, 2100, 0.03, "cons_EDE_global")
    push!(data_npv,(String(names_scenarios[i]), "Global", npv_val))
end

df_npv = unstack(filter(row -> row.scenario == names_scenarios[1], data_npv), :scenario, :value)

for i in 2:length(scenarios)
    df_tempo = unstack(filter(row -> row.scenario == names_scenarios[i], data_npv), :scenario, :value)
    df_npv = innerjoin(df_npv, df_tempo, on=:country)
end

path_npv = joinpath(@__DIR__, "..", "cap_and_share", "output", "net_present_value_cons_EDE.csv")
CSV.write(path_npv, df_npv)


###########################
#Decomposition of welfare gains
###########################
include("helper_functions.jl")
countries_wanted = [:IND, :NGA, :CHN, :USA, :COD, :RUS]
welfare_year = 2050

test = [only(filter(row -> row.time == 2050 && row.country == c, getdataframe(nice2020_global_cap_share, :welfare => :cons_EDE_country))).cons_EDE_country for c in countries_wanted]
println(test)

welfare_gains(nice2020_global_cap_share, bau_model, welfare_year, [:FRA])

welfare_gains(nice2020_global_cap_share, bau_model, welfare_year, countries_wanted)

#TEST DIFF ede_country_level et EDE_aggregated
ede_country_level = only(filter(row -> row.time == 2050 && row.country == :FRA, getdataframe(nice2020_global_cap_share, :welfare=>:cons_EDE_country)).cons_EDE_country)
η = try
    Mimi.get_param(nice2020_global_cap_share, :welfare, :η)
catch
    1.5  # default value
end
population_fra = only(filter(row -> row.time == 2050 && row.country == :FRA, getdataframe(nice2020_global_cap_share, :welfare=>:l)).l)
ede_agg = MimiNICE2020.EDE_aggregated([ede_country_level], [population_fra], η)

println(ede_country_level*population_fra)
println(population_fra)
###########################################

welfare_gains_path = joinpath(
    @__DIR__,
    "..",
    "cap_and_share",
    "output",
    "welfare_gains_$(welfare_year)_global_cap_share_vs_bau.csv"
)

welfare_gains_df = write_welfare_gains_csv(
    nice2020_global_cap_share,
    bau_model,
    welfare_year,
    countries_wanted,
    welfare_gains_path;
    scenario1_name = "Global_Cap_Share",
    scenario2_name = "BAU",
    include_global = true,
    global_label = "Global",
    eu_label = "European Union (27)"
)

println("Welfare gains components saved to $(welfare_gains_path)")

###########################
# NPV of welfare gains decomposition (2030-2100, discount rate 3%)
###########################

# List of years for NPV calculation
years_npv = collect(2030:2100)
countries_npv = [:IND, :NGA, :CHN, :USA, :COD, :RUS]

# DataFrame to store yearly welfare gains for all countries
df_yearly_gains = DataFrame(
    Country = String[],
    time = Int[],
    damages_avoided = Float64[],
    transfer_diff = Float64[],
    growth = Float64[],
    abat_cost = Float64[],
    reduction_inequalities = Float64[],
    total_welfare_gains = Float64[],
    residual_tot = Float64[]
)

# Calculate yearly welfare gains for each country
for c in countries_npv
    for y in years_npv
        damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, total_welfare_gains, residual_tot = welfare_gains(nice2020_global_cap_share, bau_model, y, [c])
        push!(df_yearly_gains, (
            Country = string(c),
            time = y,
            damages_avoided = damages_avoided,
            transfer_diff = transfer_diff,
            growth = growth,
            abat_cost = abat_cost,
            reduction_inequalities = reduction_inequalities,
            total_welfare_gains = total_welfare_gains,
            residual_tot = residual_tot
        ))
    end
end

# Add Global aggregation
all_countries_model = collect(dim_keys(nice2020_global_cap_share, :country))
for y in years_npv
    damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, total_welfare_gains, residual_tot = welfare_gains(nice2020_global_cap_share, bau_model, y, all_countries_model)
    push!(df_yearly_gains, (
        Country = "Global",
        time = y,
        damages_avoided = damages_avoided,
        transfer_diff = transfer_diff,
        growth = growth,
        abat_cost = abat_cost,
        reduction_inequalities = reduction_inequalities,
        total_welfare_gains = total_welfare_gains,
        residual_tot = residual_tot
    ))
end

# Add EU27 aggregation
eu27_countries = Symbol.(["AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"])
for y in years_npv
    damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, total_welfare_gains, residual_tot = welfare_gains(nice2020_global_cap_share, bau_model, y, eu27_countries)
    push!(df_yearly_gains, (
        Country = "European Union (27)",
        time = y,
        damages_avoided = damages_avoided,
        transfer_diff = transfer_diff,
        growth = growth,
        abat_cost = abat_cost,
        reduction_inequalities = reduction_inequalities,
        total_welfare_gains = total_welfare_gains,
        residual_tot = residual_tot
    ))
end

# Calculate NPV for each country and component
df_npv_gains = DataFrame(
    Country = String[],
    Scenario1 = String[],
    Scenario2 = String[],
    damages_avoided_npv = Float64[],
    transfer_diff_npv = Float64[],
    growth_npv = Float64[],
    abat_cost_npv = Float64[],
    reduction_inequalities_npv = Float64[],
    total_welfare_gains_npv = Float64[],
    residual_tot_npv = Float64[]
)

# Get unique countries (including Global and EU27)
unique_countries = unique(df_yearly_gains.Country)

for country in unique_countries
    df_country = filter(row -> row.Country == country, df_yearly_gains)
    
    # Calculate NPV for each component
    npv_damages = net_present_value(df_country, 2030, 2100, 0.03, "damages_avoided")
    npv_transfer = net_present_value(df_country, 2030, 2100, 0.03, "transfer_diff")
    npv_growth = net_present_value(df_country, 2030, 2100, 0.03, "growth")
    npv_abat = net_present_value(df_country, 2030, 2100, 0.03, "abat_cost")
    npv_ineq = net_present_value(df_country, 2030, 2100, 0.03, "reduction_inequalities")
    npv_total = net_present_value(df_country, 2030, 2100, 0.03, "total_welfare_gains")
    npv_residual = net_present_value(df_country, 2030, 2100, 0.03, "residual_tot")
    
    push!(df_npv_gains, (
        Country = country,
        Scenario1 = "Global_Cap_Share",
        Scenario2 = "BAU",
        damages_avoided_npv = npv_damages,
        transfer_diff_npv = npv_transfer,
        growth_npv = npv_growth,
        abat_cost_npv = npv_abat,
        reduction_inequalities_npv = npv_ineq,
        total_welfare_gains_npv = npv_total,
        residual_tot_npv = npv_residual
    ))
end

# Save NPV welfare gains to CSV
npv_gains_path = joinpath(
    @__DIR__,
    "..",
    "cap_and_share",
    "output",
    "welfare_gains_npv_2030_2100_global_cap_share_vs_bau.csv"
)

CSV.write(npv_gains_path, df_npv_gains)
println("NPV welfare gains components saved to $(npv_gains_path)")

################################
# AUTARCHY SCENARIOS FOR DIFFERENT PRICE FACTOR VALUES
# UNIFORM GLOBAL CARBON TAX WITH VARYING RIGHTS COMPARED TO POPULATION
# HEATMAP OF THE EDE VARIATION FOR THESE 2 SCENARIOS
################################

using Statistics

years_ref = collect(dim_keys(base_model, :time))
p_star_path = zeros(Float64, nb_steps)
df_tax_n5 = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_cs.csv"), DataFrame)
df_tax_n5.time = Int.(df_tax_n5.time)
df_tax_n5.global_tax = Float64.(df_tax_n5.global_tax)
tax_dict_n5 = Dict(row.time => row.global_tax for row in eachrow(df_tax_n5))

for (i, y) in enumerate(years_ref)
    p_star_path[i] = get(tax_dict_n5, y, 0.0)
end

# i get my global parameters from scenario 5: global_cap_share
pop_df = getdataframe(nice2020_global_cap_share, :grosseconomy => :l)
gcs_emissions_df = getdataframe(nice2020_global_cap_share, :emissions => :E_gtco2)
unique_years = sort(unique(gcs_emissions_df.time))
pop_lookup = Dict((row.time, row.country) => row.l for row in eachrow(pop_df))
emissions_lookup = Dict((row.time, row.country) => row.E_gtco2 for row in eachrow(gcs_emissions_df))
global_rights = [sum(filter(r -> r.time == y, gcs_emissions_df).E_gtco2) for y in unique_years]
global_pop_total = [sum(filter(r -> r.time == y, pop_df).l) for y in unique_years]

const EU27_LIST = [
    :AUT, :BEL, :BGR, :HRV, :CYP, :CZE, :DNK, :EST, :FIN, :FRA, 
    :DEU, :GRC, :HUN, :IRL, :ITA, :LVA, :LTU, :LUX, :MLT, :NLD, 
    :POL, :PRT, :ROU, :SVK, :SVN, :ESP, :SWE, :GBR
]

# countries to process
target_countries = ["USA", "COG", "CHN", "IND", "EU27", "RUS", "NGA"]

println("=== Running autarchy scenario ===")

for country_name in target_countries
    # this is to handle the case of EUE which is not a country in the model but a group of countries
    # I will compute the average price path weighted by emissions of the countries in the EU28 list
    # for the output i will sum the consumption_EDE and emissions of these countries to get the EUE consumption_EDE and emissions
    is_eue = (country_name == "EU27")
    target_symbols = is_eue ? EU27_LIST : [Symbol(country_name)]
    target_indices = findall(x -> x in target_symbols, dim_keys(base_model, :country))

    # omega_i (emissions weight) to get the autarchy price path for country i and rest of world for any pi_i value:
    # p^* = omega_i p_i + (1 - omega_i) p_{-i}, with p_i = pi_i p^*
    omega_i = [sum(get(emissions_lookup, (y, s), 0.0) for s in target_symbols) / 
               sum(get(emissions_lookup, (y, s), 0.0) for s in dim_keys(base_model, :country)) 
               for y in unique_years]

    # now I loop over different pi_i values to get different autarchy paths for country i, and rest of world coordinates accordingly
    for pi_i in range(0, 2.0, step=0.2)
        pi_str = replace(string(round(pi_i, digits=2)), "." => "p")
        folder = joinpath(@__DIR__, "..", "cap_and_share", "output", country_name, "autarchy_$pi_str")
        
        # --- skip if already in folder ---
        if isdir(folder) && isfile(joinpath(folder, "consumption_EDE.csv"))
            println("   ⏩ Skipping Autarchy for $country_name at pi=$pi_i (Already exists)")
            continue
        end

        println("   🚀 Running Autarchy: $country_name | pi=$pi_i")
        denom = 1.0 .- omega_i
        p_minus_i_path = ifelse.(denom .> 1e-10, p_star_path .* (1.0 .- omega_i .* pi_i) ./ denom, 0.0)
        
        tax_mat = zeros(Float64, nb_steps, nb_country)
        for t in 1:nb_steps, c in 1:nb_country
            tax_mat[t, c] = (c in target_indices) ? (pi_i * p_star_path[t]) : p_minus_i_path[t]
        end

        nice2020_autarchy = MimiNICE2020.create_nice2020()
        update_param!(nice2020_autarchy, :switch_custom_transfers, 0)
        update_param!(nice2020_autarchy, :switch_recycle, switch_recycle)
        update_param!(nice2020_autarchy, :switch_global_recycling, switch_global_recycling)
        update_param!(nice2020_autarchy, :revenue_recycle, :global_recycle_share, ones(nb_country) * global_recycle_share)
        update_param!(nice2020_autarchy, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
        update_param!(nice2020_autarchy, :switch_footprint, switch_footprint)
        update_param!(nice2020_autarchy, :abatement, :control_regime, 4)
        update_param!(nice2020_autarchy, :abatement, :direct_country_tax, tax_mat)
        update_param!(nice2020_autarchy, :switch_transfers_affect_growth, switch_transfers_affect_growth)
        update_param!(nice2020_autarchy, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

        run(nice2020_autarchy)

        # here i reduce the output so as to only get consumption_EDE for country i, emissions for country i and the carbon tax paid by country i and other countries
        # i still need to use the initial saving function because i need specific dataframes from the model output, but i will only save the relevant dataframes in the output folder and not the whole model output to save space
        # creating the folder
        pi_i_str = replace(string(round(pi_i, digits=2)), "." => "p")
        output_folder = joinpath(@__DIR__, "..", "cap_and_share", "output", country_name, "autarchy_$(pi_i_str)")
        mkpath(output_folder)

        # extracting EDE
        df_ede = getdataframe(nice2020_autarchy, :welfare, :cons_EDE_country)
        df_res_ede = filter(row -> row.country in target_symbols, df_ede)
        # this allows me to deal with EU countries by summing their consumption_EDE weighted by their population to get the EUE consumption_EDE
        if is_eue
            df_res_ede[!, :cons_EDE_country] = Float64.(df_res_ede.cons_EDE_country)
            df_save_ede = combine(groupby(df_res_ede, :time), :cons_EDE_country => mean => :cons_EDE_country)
            df_save_ede[!, :country] .= "EU27"
        else
            df_save_ede = df_res_ede
        end
        CSV.write(joinpath(output_folder, "consumption_EDE.csv"), df_save_ede)

        # extracting emissions
        df_emissions = getdataframe(nice2020_autarchy, :emissions, :E_gtco2)
        df_res_em = filter(row -> row.country in target_symbols, df_emissions)
        
        if is_eue
            df_res_em[!, :E_gtco2] = Float64.(df_res_em.E_gtco2)
            df_save_em = combine(groupby(df_res_em, :time), :E_gtco2 => sum => :E_gtco2)
            df_save_em[!, :country] .= "EU27"
        else
            df_save_em = df_res_em
        end
        CSV.write(joinpath(output_folder, "emissions.csv"), df_save_em)

        # extracting carbon tax
        df_tax = getdataframe(nice2020_autarchy, :abatement, :country_carbon_tax)
        df_res_tax = filter(row -> row.country in target_symbols, df_tax)
        
        if is_eue
            df_res_tax[!, :country_carbon_tax] = Float64.(df_res_tax.country_carbon_tax)
            df_save_tax = combine(groupby(df_res_tax, :time), :country_carbon_tax => mean => :country_carbon_tax)
            df_save_tax[!, :country] .= "EU27"
        else
            df_save_tax = df_res_tax
        end
        CSV.write(joinpath(output_folder, "country_carbon_tax.csv"), df_save_tax)

        nice2020_autarchy = nothing
        GC.gc()
    end
end

println("\n=== Running uniform price scenario ===")

# again, start from scenario 5 (global_cap_share)
# vary the ratio : (rights_share_i) / (population_share_i) for country i
# global emissions cap stays the same so the rest of world's rights compensate any variation of country i's rights

for country_name in target_countries
    println("\n📍 Running for $country_name")
    
    is_eue = (country_name == "EU27")
    target_symbols = is_eue ? EU27_LIST : [Symbol(country_name)]
    target_indices = findall(x -> x in target_symbols, dim_keys(base_model, :country))

    # population shares
    target_group_pop = [sum(get(pop_lookup, (y, s), 0.0) for s in target_symbols) for y in unique_years]
    pop_share_target = target_group_pop ./ global_pop_total

    # now I loop over ratio values to get different paths for country i's rights (ratio = 1 means same rights as in global_cap_share, ratio < 1 means less rights, ratio > 1 means more rights)
    for ratio in 0.5:0.4:4.5
        rat_str = replace(string(round(ratio, digits=2)), "." => "p")
        folder = joinpath(@__DIR__, "..", "cap_and_share", "output", country_name, "uniform_ratio_$rat_str")
        
        # skip if the folder already exists
        if isdir(folder) && isfile(joinpath(folder, "consumption_EDE.csv"))
            println("   ⏩ Skip : $country_name | ratio $ratio (already exists)")
            continue
        end

        println("   🚀 Start : $country_name | ratio $ratio")
        
        # rights_share_target = ratio × pop_share_target
        # target_rights_total = rights_share_target × global_rights
        target_rights_total = (ratio .* pop_share_target) .* global_rights
        
        # remaining global rights and redistribute proportionally to other countries
        rights_mat = zeros(Float64, nb_steps, nb_country)
        println("      🛠️ Construction of the rights matrix...")
        for t in 1:nb_steps
            y = unique_years[t]
            if target_group_pop[t] == 0 continue end
            
            # target group rights
            for idx in target_indices
                s = dim_keys(base_model, :country)[idx]
                p_share = get(pop_lookup, (y, s), 0.0) / target_group_pop[t]
                rights_mat[t, idx] = p_share * target_rights_total[t]
            end
            
            # other countries' rights
            rem_rights = global_rights[t] - target_rights_total[t]
            other_ems = sum(get(emissions_lookup, (y, s), 0.0) for s in dim_keys(base_model, :country) if !(s in target_symbols))
            
            if other_ems <= 0
                other_indices = [i for i in 1:nb_country if !(i in target_indices)]
                for c_idx in other_indices
                    rights_mat[t, c_idx] = rem_rights / length(other_indices)
                end
            else
                for c_idx in 1:nb_country
                    if !(c_idx in target_indices)
                        s = dim_keys(base_model, :country)[c_idx]
                        rights_mat[t, c_idx] = (get(emissions_lookup, (y, s), 0.0) / other_ems) * rem_rights
                    end
                end
            end
        end

        println("      📡 Mimi run...")
        nice2020_uniform_varying = MimiNICE2020.create_nice2020()
        update_param!(nice2020_uniform_varying, :switch_custom_transfers, 0)
        update_param!(nice2020_uniform_varying, :switch_recycle, 1)
        update_param!(nice2020_uniform_varying, :switch_global_recycling, switch_global_recycling)
        update_param!(nice2020_uniform_varying, :revenue_recycle, :global_recycle_share, ones(nb_country) * global_recycle_share)
        update_param!(nice2020_uniform_varying, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
        update_param!(nice2020_uniform_varying, :switch_footprint, switch_footprint)
        update_param!(nice2020_uniform_varying, :abatement, :control_regime, 5)
        update_param!(nice2020_uniform_varying, :abatement, :rights_mat, rights_mat)    
        update_param!(nice2020_uniform_varying, :switch_transfers_affect_growth, switch_transfers_affect_growth)
        update_param!(nice2020_uniform_varying, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])
        
        @time run(nice2020_uniform_varying)
        println("      ✅ Run done!")

        print("      💾 Saving results...")
        mkpath(folder)

        # extracting cons EDE
        df_ede = getdataframe(nice2020_uniform_varying, :welfare, :cons_EDE_country)
        df_res_ede = filter(row -> row.country in target_symbols, df_ede)
        if is_eue
            df_res_ede[!, :cons_EDE_country] = Float64.(df_res_ede.cons_EDE_country)
            df_save_ede = combine(groupby(df_res_ede, :time), :cons_EDE_country => mean => :cons_EDE_country)
            df_save_ede[!, :country] .= "EU27"
        else
            df_save_ede = df_res_ede
        end
        CSV.write(joinpath(folder, "consumption_EDE.csv"), df_save_ede)

        # extracting emissions
        df_emissions = getdataframe(nice2020_uniform_varying, :emissions, :E_gtco2)
        df_res_em = filter(row -> row.country in target_symbols, df_emissions)
        if is_eue
            df_res_em[!, :E_gtco2] = Float64.(df_res_em.E_gtco2)
            df_save_em = combine(groupby(df_res_em, :time), :E_gtco2 => sum => :E_gtco2)
            df_save_em[!, :country] .= "EU27"
        else
            df_save_em = df_res_em
        end
        CSV.write(joinpath(folder, "emissions.csv"), df_save_em)

        # extracting carbon tax
        df_tax = getdataframe(nice2020_uniform_varying, :abatement, :country_carbon_tax)
        df_res_tax = filter(row -> row.country in target_symbols, df_tax)
        if is_eue
            df_res_tax[!, :country_carbon_tax] = Float64.(df_res_tax.country_carbon_tax)
            df_save_tax = combine(groupby(df_res_tax, :time), :country_carbon_tax => mean => :country_carbon_tax)
            df_save_tax[!, :country] .= "EU27"
        else
            df_save_tax = df_res_tax
        end
        CSV.write(joinpath(folder, "carbon_tax.csv"), df_save_tax)
        
        println(" OK.")

        nice2020_uniform_varying = nothing
        GC.gc()
    end
end

#### now the heatmap
using Plots, CSV, DataFrames, Plots.Measures, Statistics, LaTeXStrings

# --- 1. SETUP ---
output_base = joinpath(@__DIR__, "..", "cap_and_share", "output")
discount_rate = 0.03
years_npv = 2030:2100

# --- 2. HELPER FUNCTIONS ---
# get the 2030 global price as reference
function read_p_star0()
    path = joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_cs.csv")
    if !isfile(path)
        @warn "Cannot find calibrated global price file: $path"
        return 1.0
    end
    df = CSV.read(path, DataFrame)
    df.time = Int.(df.time)
    df.global_tax = Float64.(df.global_tax)
    row = filter(row -> row.time == 2030, df)
    return only(row).global_tax
end

# find the folders
function find_output_dirs(base, prefix)
    dirs = String[]
    for (root, subdirs, _) in walkdir(base)
        for d in subdirs
            if startswith(d, prefix)
                push!(dirs, joinpath(root, d))
            end
        end
    end
    sort(dirs)
end

# convert folder names to get the actual pi/ratio values
function parse_decimal_value(dir_name, sep)
    token = split(basename(dir_name), sep)[end]
    token = replace(token, "p" => ".")
    return parse(Float64, token)
end

function read_ede_series(dir, country)
    path = joinpath(dir, "consumption_EDE.csv")
    if !isfile(path) 
        return nothing 
    end
    df = CSV.read(path, DataFrame)
    # CSV reads Symbols as Strings, so just compare as strings
    return filter(row -> string(row.country) == country, df)
end

# NPV for the RELATIVE variation (%)
function calculate_relative_npv(series_uniform, series_autarchy, years, rate)
    npv_sum = 0.0
    count_years = 0
    
    for y in years
        # Safe filtering
        row_u = filter(r -> r.time == y, series_uniform)
        row_a = filter(r -> r.time == y, series_autarchy)
        
        # skip if year is missing in either scenario
        if isempty(row_u) || isempty(row_a)
            continue
        end
        
        val_u = row_u.cons_EDE_country[1]
        val_a = row_a.cons_EDE_country[1]
        
        # Check for NaN or 0 in the data itself to avoid Inf
        if isnan(val_u) || isnan(val_a) || val_a == 0
            continue
        end

        # Relative Gain: ((Uniform - Autarchy) / Autarchy) * 100
        relative_gain = ((val_u - val_a) / val_a) * 100
        
        # NPV Discounting
        npv_sum += relative_gain / ((1 + rate)^(y - 2030))
        count_years += 1
    end
    
    # If we found no data at all, return NaN; otherwise return the sum
    return count_years == 0 ? NaN : npv_sum
end

p_star0 = read_p_star0()

theme(:vibrant) # Ou :scientific pour un look plus sobre
default(
    fontfamily = "Computer Modern", # Police académique standard
    titlefontsize = 12,
    guidefontsize = 10,
    tickfontsize = 8,
    legendfontsize = 9,
    dpi = 300 # Haute résolution pour l'export
)

# --- LOOP ---
for country in target_countries
    println("\n📊 >>> GENERATING HEATMAP FOR: $country <<<")
    
    country_path = joinpath(output_base, country)
    
    # 1. Get raw directory lists
    raw_autarchy_dirs = find_output_dirs(country_path, "autarchy_")
    raw_uniform_dirs = find_output_dirs(country_path, "uniform_ratio_")

    # 2. Sort them NUMERICALLY (Crucial step!)
    # We create a pair of (value, directory) then sort by value
    autarchy_pairs = sort([(parse_decimal_value(d, "autarchy_"), d) for d in raw_autarchy_dirs])
    uniform_pairs = sort([(parse_decimal_value(d, "uniform_ratio_"), d) for d in raw_uniform_dirs])

    # 3. Extract the sorted values and the sorted directory paths
    pi_vals = [p[1] for p in autarchy_pairs]
    autarchy_dirs = [p[2] for p in autarchy_pairs]

    ratio_vals = [p[1] for p in uniform_pairs]
    uniform_dirs = [p[2] for p in uniform_pairs]

    # construct NPV matrix
    # Rows = Ratios (Y), Cols = Pi (X)
    results = zeros(length(ratio_vals), length(pi_vals))
    
    for (i, d_u) in enumerate(uniform_dirs), (j, d_a) in enumerate(autarchy_dirs)
        s_u = read_ede_series(d_u, country)
        s_a = read_ede_series(d_a, country)
        if isnothing(s_u) || isempty(s_u) || isnothing(s_a) || isempty(s_a)
            @warn "Missing data for ratio index $i or autarchy index $j"
            results[i, j] = NaN
            continue
        end
        # Calculate the cumulative discounted % gain
        results[i, j] = calculate_relative_npv(s_u, s_a, years_npv, discount_rate)
    end

    valid_res = filter(!isnan, results)
    if isempty(valid_res)
        @warn "      No valid data for $country heatmap."
        continue
    end
    
    lim = maximum(abs.(valid_res))

    my_cgrad = cgrad([:blue, :white, :red], [0, 0.5, 1])

    p = heatmap(pi_vals, ratio_vals, results,
        xlabel = L"Price Factor \pi_i (p_i = \pi_i \times p_0^*)",
        ylabel = L"Rights Ratio (\rho_i)",
        title = "$country : Variation de l'EDE (2030-2100)\nUniforme vs Autarcie",
        colorbar_title = L"Gain cumulé (%)",
        titlealign = :left,
        color = my_cgrad,
        clims = (-lim, lim),
        right_margin = 12mm,  # Espace pour le titre de la colorbar
        left_margin = 8mm,
        bottom_margin = 15mm,
        top_margin = 8mm,
        size = (900, 600),
        frame = :box, # Cadre complet autour du graph
        grid = false  # On enlève la grille pour une heatmap
    )

    # 1. Ajout de la courbe de niveau 0 (la "frontière")
    # On la rend plus élégante : tirets épais
    contour!(p, pi_vals, ratio_vals, results, 
        levels = [0.0], 
        color = :black, 
        lw = 1.5, 
        linestyle = :dash,
        label = "Seuil d'indifférence" # Apparaîtra si tu actives la légende
    )

    # 2. Amélioration de la note de bas de page
    # Au lieu d'utiliser des coordonnées DATA, on utilise des coordonnées RELATIVES (ann)
    # ou un titre de bas de page (plot_title)
    note_text = "Note : Prix mondial 2030 (p_0^*) = $(round(p_star0, digits=2)) USD/tCO₂"
    annotate!(p, [(0.5, -0.3, text(note_text, 8, :gray30, :center, :rel))])

    # 3. Optionnel : Ajout d'un point ou d'une étoile sur un point d'intérêt
    # scatter!(p, [1.0], [1.0], marker=:star, ms=8, label="Benchmark", color=:yellow)

    save_path = joinpath(country_path, "Heatmap_$(country)_Pro.png")
    savefig(p, save_path)
end