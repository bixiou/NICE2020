#########################################################
# This file reproduces runs for "From Global Policies to Phase Out Fossil Fuels To a Sustainable Union"
#########################################################

#create your own "path.txt" to find NICE2020
# path = read("path.txt", String) |> strip  
cd("/Users/constance/Documents/stage/NICE2020")  

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


###########################
# AUTARCHY: country i acts independently, rest of world coordinates -- ex with China
###########################

include("nice2020_module.jl") 
include("helper_functions.jl")

country_i_name = "CHN"  # other countries: "USA", "IND", "EUE" (will have to create that), "RUS", "COG", "NGA"
country_i_idx = findfirst(==(Symbol(country_i_name)), dim_keys(base_model, :country))

# 1. parameters based on scenario n5: global_cap_share
years_ref = collect(dim_keys(base_model, :time))
p_star_path = zeros(Float64, nb_steps)
df_tax_n5 = CSV.read(joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_cs.csv"), DataFrame)
df_tax_n5.time = Int.(df_tax_n5.time)
df_tax_n5.global_tax = Float64.(df_tax_n5.global_tax)
tax_dict_n5 = Dict(row.time => row.global_tax for row in eachrow(df_tax_n5))
for (i, y) in enumerate(years_ref)
    p_star_path[i] = get(tax_dict_n5, y, 0.0)
end

# i now have the global price path

# 2. compute omega_i the emission weight of country i (invariant across pi_i values), which will allow me to compute the autarchy price path for country i and rest of world for any pi_i value:
# p^* = omega_i p_i + (1 - omega_i) p_{-i}, with p_i = pi_i p^*
emissions_18_df = getdataframe(nice2020_global_cap_share, :emissions => :E_gtco2)
unique_years = sort(unique(emissions_18_df.time))
omega_i = zeros(Float64, nb_steps)
for (t_idx, year) in enumerate(unique_years)
    row_i = filter(row -> row.time == year && row.country == Symbol(country_i_name), emissions_18_df)
    row_global = filter(row -> row.time == year, emissions_18_df)
    e_i = only(row_i).E_gtco2
    E_global = sum(row_global.E_gtco2)
    omega_i[t_idx] = e_i / E_global
end

switch_recycle = 1
switch_global_recycling = 1
switch_global_pc_recycle = 1
switch_scenario = :All_World
switch_transfers_affect_growth = 1
switch_footprint = 1

# 3. loop over pi_i values to get different autarchy paths for country i
for pi_i in range(0.0, 1.0, length=11) 
    # p^* = omega_i p_i + (1 - omega_i) p_{-i}, with p_i = pi_i p^*
    # => p_{-i} = p^* (1 - omega_i pi_i)/(1 - omega_i)
    p_minus_i_path = zeros(Float64, nb_steps)
    for t in 1:nb_steps
        p_minus_i_path[t] = p_star_path[t] * (1 - omega_i[t] * pi_i) / (1 - omega_i[t])
    end

    direct_country_tax_autarchy = zeros(Float64, nb_steps, nb_country)
    for t in 1:nb_steps
        for c in 1:nb_country
            if c == country_i_idx
                direct_country_tax_autarchy[t, c] = pi_i * p_star_path[t]
            else
                direct_country_tax_autarchy[t, c] = p_minus_i_path[t]
            end
        end
    end

    nice2020_autarchy = MimiNICE2020.create_nice2020()
    update_param!(nice2020_autarchy, :switch_custom_transfers, 0)
    update_param!(nice2020_autarchy, :switch_recycle, switch_recycle)
    update_param!(nice2020_autarchy, :switch_global_recycling, switch_global_recycling)
    update_param!(nice2020_autarchy, :revenue_recycle, :global_recycle_share, ones(nb_country) * global_recycle_share)
    update_param!(nice2020_autarchy, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
    update_param!(nice2020_autarchy, :switch_footprint, switch_footprint)
    update_param!(nice2020_autarchy, :abatement, :control_regime, 4)
    update_param!(nice2020_autarchy, :abatement, :direct_country_tax, direct_country_tax_autarchy)
    update_param!(nice2020_autarchy, :switch_transfers_affect_growth, switch_transfers_affect_growth)
    update_param!(nice2020_autarchy, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

    run(nice2020_autarchy)

    pi_i_str = replace(string(round(pi_i, digits=2)), "." => "p")
    output_dir_autarchy = joinpath(@__DIR__, "..", "cap_and_share", "output", country_i_name, "autarchy_$(pi_i_str)")
    MimiNICE2020.save_nice2020_output(nice2020_autarchy, output_dir_autarchy)
    println("Autarchy pi_i=$(round(pi_i, digits=2)) saved to: $output_dir_autarchy")
end
############################

############################
# UNIFORM PRICE with varying emission rights -- ex with China
############################

# again, start from scenario n5 (global_cap_share)
# vary the ratio: (rights_share_i) / (population_share_i) for country i
# global emissions cap stays the same so the rest of world's rights compensate any variation of country i's rights

include("nice2020_module.jl")

country_i_name = "CHN"  # country i
country_i_idx = findfirst(==(Symbol(country_i_name)), dim_keys(base_model, :country))

# 1. population shares from the model (invariant across ratio values)
pop_df = getdataframe(nice2020_global_cap_share, :grosseconomy => :l)
unique_years_pop = sort(unique(pop_df.time))

global_pop = zeros(Float64, nb_steps)
country_i_pop = zeros(Float64, nb_steps)
for (t_idx, year) in enumerate(unique_years_pop)
    row_global = filter(row -> row.time == year, pop_df)
    row_i = filter(row -> row.time == year && row.country == Symbol(country_i_name), pop_df)
    global_pop[t_idx] = sum(row_global.l)
    country_i_pop[t_idx] = only(row_i).l
end

pop_share_i = country_i_pop ./ global_pop

# 2. total global emission rights (invariant across ratio values)
gcs_emissions_df = getdataframe(nice2020_global_cap_share, :emissions => :E_gtco2)

global_rights = zeros(Float64, nb_steps)
for (t_idx, year) in enumerate(unique_years_pop)
    row_gcs = filter(row -> row.time == year, gcs_emissions_df)
    global_rights[t_idx] = sum(row_gcs.E_gtco2)
end

switch_global_recycling = 1
switch_global_pc_recycle = 1
switch_scenario = :All_World
switch_transfers_affect_growth = 1
switch_footprint = 1
global_recycle_share = 1

# 3. now I loop over ratio values to get different paths for country i's rights (ratio = 1 means same rights as in global_cap_share, ratio < 1 means less rights, ratio > 1 means more rights)
for ratio in 0.5:0.5:4.0  # can change the scale and granularity
    # rights_share_i = ratio × pop_share_i
    # country_i_rights = rights_share_i × global_rights
    rights_share_i = ratio .* pop_share_i
    country_i_rights_new = rights_share_i .* global_rights

    # remaining global rights and redistribute proportionally to other countries
    rights_mat_varying = zeros(Float64, nb_steps, nb_country)
    for t in 1:nb_steps
        rights_mat_varying[t, country_i_idx] = country_i_rights_new[t]
        remaining_rights = global_rights[t] - country_i_rights_new[t]
        row_gcs_t = filter(row -> row.time == unique_years_pop[min(t, length(unique_years_pop))], gcs_emissions_df)
        emissions_by_country = Dict(row.country => row.E_gtco2 for row in eachrow(row_gcs_t))
        other_emissions = 0.0
        for c in dim_keys(base_model, :country)
            if c != Symbol(country_i_name)
                other_emissions += get(emissions_by_country, c, 0.0)
            end
        end
        for (c_idx, c) in enumerate(dim_keys(base_model, :country))
            if c_idx != country_i_idx
                if other_emissions > 0
                    country_emissions = get(emissions_by_country, c, 0.0)
                    rights_mat_varying[t, c_idx] = (country_emissions / other_emissions) * remaining_rights
                else
                    rights_mat_varying[t, c_idx] = remaining_rights / (nb_country - 1)
                end
            end
        end
    end

    nice2020_uniform_varying = MimiNICE2020.create_nice2020()
    update_param!(nice2020_uniform_varying, :switch_custom_transfers, 0)
    update_param!(nice2020_uniform_varying, :switch_recycle, 1)
    update_param!(nice2020_uniform_varying, :switch_global_recycling, switch_global_recycling)
    update_param!(nice2020_uniform_varying, :revenue_recycle, :global_recycle_share, ones(nb_country) * global_recycle_share)
    update_param!(nice2020_uniform_varying, :revenue_recycle, :switch_global_pc_recycle, switch_global_pc_recycle)
    update_param!(nice2020_uniform_varying, :switch_footprint, switch_footprint)
    update_param!(nice2020_uniform_varying, :abatement, :control_regime, 5)
    update_param!(nice2020_uniform_varying, :abatement, :rights_mat, rights_mat_varying)
    update_param!(nice2020_uniform_varying, :switch_transfers_affect_growth, switch_transfers_affect_growth)
    update_param!(nice2020_uniform_varying, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

    run(nice2020_uniform_varying)

    ratio_str = replace(string(round(ratio, digits=2)), "." => "p")
    output_dir_uniform_varying = joinpath(@__DIR__, "..", "cap_and_share", "output", country_i_name, "uniform_varying_rights_ratio_$(ratio_str)")
    MimiNICE2020.save_nice2020_output(nice2020_uniform_varying, output_dir_uniform_varying)
    println("Uniform varying rights ratio=$(round(ratio, digits=2)) saved to: $output_dir_uniform_varying")
end
############################

############################
# HEATMAP: EDE difference for China at 2030 and 2050
# X axis: p_i,0 = pi_i × p*_0
# Y axis: rho_i = rights share / population share
# Color: EDE_uniform(rho_i) - EDE_autarchy(pi_i)
#        Positive = uniform better, negative = autarchy better
############################

using Plots, CSV, DataFrames

output_base = joinpath(@__DIR__, "..", "cap_and_share", "output")
country = "CHN"
heatmap_years = [2030, 2050]

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

function parse_decimal_value(dir_name, sep)
    token = split(basename(dir_name), sep)[end]
    token = replace(token, "p" => ".")
    return parse(Float64, token)
end

function read_ede_year(dir, country, year)
    path = joinpath(dir, "country_output", "consumption_EDE.csv")
    if !isfile(path)
        @warn "File not found: $path"
        return NaN
    end
        df = CSV.read(path, DataFrame)
    row = filter(row -> string(row.country) == country && row.time == year, df)
    if isempty(row)
        @warn "No EDE row for $country in $path at year $year"
            return NaN
        end
    return only(row).cons_EDE_country
end

function read_p_star0()
    path = joinpath(@__DIR__, "..", "cap_and_share", "data", "output", "calibrated_global_cs.csv")
    if !isfile(path)
        @warn "Cannot find calibrated global price file: $path"
        return 1.0
    end
    df = CSV.read(path, DataFrame)
    df.time = Int.(df.time)
    df.global_tax = Float64.(df.global_tax)
    row = filter(row -> row.time == 2020, df)
    if isempty(row)
        @warn "No 2020 price in calibrated global price file"
        return 1.0
    end
    return only(row).global_tax
end
 
# 1. Collect autarchy folders and pi_i values
full_autarchy_dirs = find_output_dirs(joinpath(output_base, country), "autarchy_")
pi_info = sort([(parse_decimal_value(d, "autarchy_"), d) for d in full_autarchy_dirs], by = first)
pi_i_vals = [info[1] for info in pi_info]
autarchy_dirs = [info[2] for info in pi_info]

# 2. Collect uniform folders and ratio values
full_uniform_dirs = find_output_dirs(output_base, "uniform_varying_rights_ratio_")
ratio_info = sort([(parse_decimal_value(d, "_ratio_"), d) for d in full_uniform_dirs], by = first)
ratio_vals = [info[1] for info in ratio_info]
uniform_dirs = [info[2] for info in ratio_info]

p_star0 = read_p_star0()
p_i0_vals = pi_i_vals .* p_star0

for year in heatmap_years
    autarchy_ede = [read_ede_year(d, country, year) for d in autarchy_dirs]
    uniform_ede = [read_ede_year(d, country, year) for d in uniform_dirs]

    valid_values = filter(!isnan, vcat(autarchy_ede, uniform_ede))
    if isempty(valid_values)
        @error "No valid EDE data for $country at year $year"
        continue
    end

    ede_diff = [uniform_ede[i] - autarchy_ede[j] for i in 1:length(ratio_vals), j in 1:length(pi_i_vals)]
valid_vals = filter(!isnan, vec(ede_diff))
maxval = maximum(abs.(valid_vals))

p = heatmap(
        p_i0_vals, ratio_vals, ede_diff,
        xlabel = "pi_i (p_i = pi_i × p*_0)",
        ylabel = "rho_i (rights share / population share)",
        title = "CHN: ΔEDE uniform - autarchy in $year",
        color = cgrad(:RdBu, rev = false),
        clims = (-maxval, maxval),
    colorbar_title = "ΔEDE",
        xticks = (p_i0_vals, string.(round.(p_i0_vals, digits = 3))),
        yticks = (ratio_vals, string.(round.(ratio_vals, digits = 2))),
        size = (800, 520),
        margin = 8Plots.mm,
    )
    contour!(p_i0_vals, ratio_vals, ede_diff, levels = [0.0], color = :black, linewidth = 2, label = "")

out_path = joinpath(output_base, country, "$(country)_EDE_heatmap_$(year).png")
savefig(p, out_path)
end

