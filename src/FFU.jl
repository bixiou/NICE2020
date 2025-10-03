#########################################################
# This file reproduces runs of "From Global Policies to Phase Out Fossil Fuels To a Sustainable Union"
#########################################################

#create your own "path.txt" to find NICE2020
# path = read("path.txt", String) |> strip  
#cd("C:/Users/fabre/Documents/www/NICE2020/")  

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
# update_param!(nice2020_global_price_ffu, :revenue_recycle, :rights_proposed, rights_proposed_mat)

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
update_param!(nice2020_ffu_su, :quantile_recycle, :rate_tenth, 0.05)
update_param!(nice2020_ffu_su, :neteconomy, :rate_pib, 0.01)
update_param!(nice2020_ffu_su, :neteconomy, :inefficiency_rate, 0.1)


run(nice2020_ffu_su)

dir_su=joinpath(@__DIR__, "..", "cap_and_share", "output", "ffu_su")
#mkpath(dir_su) # => to execute only once to create the directory
MimiNICE2020.save_nice2020_output(nice2020_ffu_su, joinpath(@__DIR__, "..", "cap_and_share", "output", "ffu_su"))

getdataframe(nice2020_ffu_su, :quantile_recycle, :new_conso_pc)
getdataframe(nice2020_ffu_su, :quantile_recycle, :conso_pc_base)
getdataframe(nice2020_ffu_su, :neteconomy, :pib_contrib)
println(getdataframe(nice2020_ffu_su, :quantile_recycle, :net_surplus))



println(getdataframe(nice2020_ffu_su, :quantile_recycle=>(:recycle_pib, :pib_contrib, :net_surplus)))
println(getdataframe(nice2020_ffu_su, :quantile_recycle => (:tot_tax_cons_country, :pib_contrib)))
println(getdataframe(nice2020_ffu_su, :quantile_recycle=>(:recycle_pib, :pib_contrib)))


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
#Year at which consumption_EDE becomes higher than consumption_EDE in the BAU scenario :
###########################

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

countries_wanted = (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :COD, :RUS)
years_wanted = (2030, 2050, 2100)
scenarios = [bau_model, nice2020_global_cap_share, nice2020_ffu, nice2020_IMF, nice2020_IMF_2, nice2020_stoft, nice2020_csu, nice2020_non_losing]
names_scenarios = ["BAU", "Global_Cap_Share", "FFU", "IMF", "IMF_2", "Stoft", "CSU", "Non-losing"]

results = build_results_csv(scenarios, names_scenarios, countries_wanted, years_wanted)

path = joinpath(@__DIR__, "..", "cap_and_share", "output", "comparison_output.csv")
CSV.write(path, results)

###########################
#Compute net present value of consumption EDE from 2030 to 2100 with a rate of 3% :
###########################

include("helper_functions.jl")
countries_wanted = (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :COD, :RUS)
years = dim_keys(base_model, :time)
scenarios = [bau_model, nice2020_global_cap_share, nice2020_ffu, nice2020_IMF, nice2020_IMF_2, nice2020_stoft, nice2020_csu, nice2020_non_losing]
names_scenarios = ["BAU", "Global_Cap_Share", "FFU", "IMF", "IMF_2", "Stoft", "CSU", "Non-losing"]
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