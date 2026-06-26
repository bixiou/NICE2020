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

MimiNICE2020.save_nice2020_reduced_output(nice2020_global_cap_share, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share_reduced"))

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, output_directory_uniform, revenue_recycling=false)
MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, joinpath(@__DIR__, "..", "cap_and_share", "output", "global_cap_share"))
#run(`powershell -c "[console]::beep(1000, 300)"`)
include("helper_functions.jl")


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

cap_norm = maximum(global_cap)

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
# GDP HEATMAP — NPV OF RELATIVE GDP GAIN: UNIFORM vs AUTARKY (NEGISHI)
#
# Reads gross_output.csv from the Negishi scenario folders.
# ═════════════════════════════════════════════════════════════════════════════

#=function load_gdp_negishi_heatmap(country, omega_i, pop_share_i, target_pis)
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
=#

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
    folder_uniform = joinpath(OUTPUT_BASE, country, "uniform_negishi_ratio_1p0")
    ems_uniform = CSV.read(joinpath(folder_uniform, "emissions.csv"), DataFrame)

    # Robust year-filtering to prevent "0 rows found" errors
    e_star_i = Float64[]
    base_rights_t = Float64[]
    for (t_idx, y) in enumerate(unique_years)
        # Handle Emissions
        row_y = filter(r -> r.time == y, ems_uniform)
        push!(e_star_i, nrow(row_y) > 0 ? row_y.co2_emissions[1] : 0.0)
        
        # Handle time-varying population share base
        p_i_t = sum(get(pop_lookup, (y, s), 0.0) for s in t_syms)
        p_world_t = global_pop[t_idx]
        share_t = p_world_t > 0 ? p_i_t / p_world_t : 0.0
        push!(base_rights_t, share_t * global_cap[t_idx])
    end

    # Calculate exact NPV ratios
    npv_e_star_i    = sum(e_star_i .* β)
    npv_base_rights = sum(base_rights_t .* β)
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
        predicted_rho_star = round(predicted_rho_npv; digits=2), # Real NPV prediction!
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
