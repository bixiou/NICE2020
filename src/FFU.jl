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
# 6. differenciated_prices: FMI proposal - $25/t LIC & LMIC, $50 UMIC, $75 HIC pour 2025-30, increasing at x% beyond that, where x is chosen to get us to 2+/-.1°C
###########################

#We load the list of LIC, LMIC, UMIC and HIC countries from parameters.jl
include("../data/parameters.jl")

#Creation of the differenciated tax
tax_lic_lmic = 25.0 # $/tCO2 for 2025-2030 (checked that same unit as global_co2_tax)
tax_umic = 50.0 # $/tCO2 for 2025-2030
tax_hic = 75.0 # $/tCO2 for 2025-2030

nice2020_differenciated_prices = MimiNICE2020.create_nice2020()

# Creation of the country x year matrix of carbon tax rates

years = collect(dim_keys(nice2020_differenciated_prices, :time))
countries = collect(dim_keys(nice2020_differenciated_prices, :country))

diff_country_tax = zeros(Float64, length(years), length(countries))

LIC_LMIC = Symbol.(LIC_LMIC)
UMIC     = Symbol.(UMIC)
HIC      = Symbol.(HIC)

years_index = findall(y -> 2025 <= y <= 2030, years)

for t in years_index
    for (c_idx, country) in enumerate(countries)
        if country in LIC_LMIC
            diff_country_tax[t, c_idx] = tax_lic_lmic
        elseif country in UMIC
            diff_country_tax[t, c_idx] = tax_umic
        elseif country in HIC
            diff_country_tax[t, c_idx] = tax_hic
        else
            diff_country_tax[t, c_idx] = 0.0  # si pays non classé
        end
    end
end

years_index_post2030 = findall(y -> y > 2030, years)
# Growth rate of the tax beyond 2030, chosen to reach approx 2°C => target of 2.02°C in 2100
growth_rate = 0.036
for t in years_index_post2030
    for (c_idx, country) in enumerate(countries)
        diff_country_tax[t, c_idx] = diff_country_tax[t-1, c_idx] * (1 + growth_rate)
    end
end



global_recycle_share            = 0
switch_scenario = :All_World  # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Africa_Eu)
update_param!(nice2020_differenciated_prices, :switch_custom_transfers, 0)
update_param!(nice2020_differenciated_prices, :switch_recycle, 1)
update_param!(nice2020_differenciated_prices, :switch_global_recycling, 0)
update_param!(nice2020_differenciated_prices, :revenue_recycle, :global_recycle_share, ones(nb_country) * global_recycle_share) 
update_param!(nice2020_differenciated_prices, :revenue_recycle, :switch_global_pc_recycle, 0)

update_param!(nice2020_differenciated_prices, :abatement, :control_regime, 4) # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
update_param!(nice2020_differenciated_prices, :abatement, :direct_country_tax, diff_country_tax)
update_param!(nice2020_differenciated_prices, :switch_footprint, 0)
update_param!(nice2020_differenciated_prices, :switch_transfers_affect_growth, 1)
update_param!(nice2020_differenciated_prices, :policy_scenario, MimiNICE2020.scenario_index[switch_scenario])

run(nice2020_differenciated_prices)

# Save the run (see helper functions for saving function details)
#MimiNICE2020.save_nice2020_output(nice2020_global_cap_share, output_directory_uniform, revenue_recycling=false)
dir=joinpath(@__DIR__, "..", "cap_and_share", "output", "differenciated_prices")
#mkpath(dir)  # create directory if it does not exist
MimiNICE2020.save_nice2020_output(nice2020_differenciated_prices, joinpath(@__DIR__, "..", "cap_and_share", "output", "differenciated_prices"))

###########################
# 7. Stoft: scenario cap_and_share but with global_recycle_share = 0.1
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
dir_b=joinpath(@__DIR__, "..", "cap_and_share", "output", "Cramton_Stoft")
mkpath(dir_b)
MimiNICE2020.save_nice2020_output(nice2020_stoft, joinpath(@__DIR__, "..", "cap_and_share", "output", "Cramton_Stoft"))

###########################
#Year at which consumption_EDE becomes higher than consumption_capita :
###########################

#On compare la consommation EDE (exprimé en 10^3 $/pers/year) avec la consommation par tête (exprimé en 10^3 $/pers/year) pour chaque pays et chaque scénario.

#Creation of a function that returns the year at which cons_EDE_country > cons_capita

function year_EDE_higher_than_capita(model)
    countries = dim_keys(model, :country)
    years = dim_keys(model, :time)
    df = DataFrame(country = String[], year = Int64[])
    conso_pc = getdataframe(model, :quantile_recycle, :CPC_post)
    conso_pc_global = getdataframe(model, :quantile_recycle, :CPC_post_global)
    conso_EDE = getdataframe(model, :welfare, :cons_EDE_country)
    conso_EDE_global = getdataframe(model, :welfare, :cons_EDE_global)
    for t in years
        for c in countries
            conso_pc_val = conso_pc[(conso_pc.time .== t) .& (conso_pc.country .== c), :CPC_post][1]
            conso_EDE_val = conso_EDE[(conso_EDE.time .== t) .& (conso_EDE.country .== c), :cons_EDE_country][1]
            if conso_EDE_val > conso_pc_val
                push!(df, (string(c), t))
                break
            end
        end
    end

    for t in years
        conso_pc_global_val = conso_pc_global[conso_pc_global.time .== t, :CPC_post_global][1]
        conso_EDE_global_val = conso_EDE_global[conso_EDE_global.time .== t, :cons_EDE_global][1]
        if conso_EDE_global_val > conso_pc_global_val
            push!(df, ("Global", t))
            break
        end
    end
    
    return df
end

println(year_EDE_higher_than_capita(nice2020_ffu))

#TEST (à supprimer lundi)
conso_pc_test = getdataframe(nice2020_ffu, :quantile_recycle, :CPC_post)
conso_EDE_test = getdataframe(nice2020_ffu, :welfare, :cons_EDE_country)
conso_pc_val_test = conso_pc_test[(conso_pc_test.time .== 2100) .& (conso_pc_test.country .== :FRA), :CPC_post][1]
conso_EDE_val_test = conso_EDE_test[(conso_EDE_test.time .== 2100) .& (conso_EDE_test.country .== :FRA), :cons_EDE_country][1]

countries = dim_keys(nice2020_ffu, :country)
years = dim_keys(nice2020_ffu, :time)
df_test = DataFrame(country = String[], year = Int64[])
conso_pc_test = getdataframe(nice2020_ffu, :quantile_recycle, :CPC_post)
conso_pc_global_test = getdataframe(nice2020_ffu, :quantile_recycle, :CPC_post_global)
conso_EDE_test = getdataframe(nice2020_ffu, :welfare, :cons_EDE_country)
conso_EDE_global_test = getdataframe(nice2020_ffu, :welfare, :cons_EDE_global)
for t in years
    for c in countries
        conso_pc_val = conso_pc_test[(conso_pc_test.time .== t) .& (conso_pc_test.country .== c), :CPC_post][1]
        conso_EDE_val = conso_EDE_test[(conso_EDE_test.time .== t) .& (conso_EDE_test.country .== c), :cons_EDE_country][1]
        if conso_EDE_val < conso_pc_val
            push!(df_test, (string(c), t))
        end
    end
end
println(df_test)

#en fait peut être qu'il faut comparer la consommation EDE d'un des scénarios avec la consommation EDE BAU 
# à partir du moment où ça devient supérieur, ça veut dire qu'il y a moins d'inégalités dans le scénario que dans le BAU

###########################
#Code to retrieve the needed values :
###########################

#a) cons_EDE_country
imf_cons = getdataframe(nice2020_differenciated_prices, :welfare=>:cons_EDE_country)
filtre_imf_cons = filter(row -> row.time in (2030, 2050, 2100) && row.country in (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :DEU, :COD, :RUS), imf_cons)
println(filtre_imf_cons)

bau_cons = getdataframe(bau_model, :welfare=>:cons_EDE_country)
filtre_bau_cons = filter(row -> row.time in (2030, 2050, 2100) && row.country in (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :DEU, :COD, :RUS), bau_cons)
println(filtre_bau_cons)

ffu_cons = getdataframe(nice2020_ffu, :welfare=>:cons_EDE_country)
filtre_ffu_cons = filter(row -> row.time in (2030, 2050, 2100) && row.country in (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :DEU, :COD, :RUS), ffu_cons)
println(filtre_ffu_cons)

global_cap_share_cons = getdataframe(nice2020_global_cap_share, :welfare=>:cons_EDE_country)
filtre_global_cap_share_cons = filter(row -> row.time in (2030, 2050, 2100) && row.country in (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :DEU, :COD, :RUS), global_cap_share_cons)
println(filtre_global_cap_share_cons)

#b) cons_EDE_global
global_cons_imf = getdataframe(nice2020_differenciated_prices, :welfare=>:cons_EDE_global)[[11, 31, 81], :]
global_cons_bau = getdataframe(bau_model, :welfare=>:cons_EDE_global)[[11, 31, 81], :]
global_cons_ffu = getdataframe(nice2020_ffu, :welfare=>:cons_EDE_global)[[11, 31, 81], :]
global_cons_cap_share = getdataframe(nice2020_global_cap_share, :welfare=>:cons_EDE_global)[[11, 31, 81], :]

# c) Global temperature in 2100
imf_temp = getdataframe(nice2020_differenciated_prices, :temperature=>:T)[81, :]
bau_temp = getdataframe(bau_model, :temperature=>:T)[81, :]
capshare_temp = getdataframe(nice2020_global_cap_share, :temperature=>:T)[81, :]
ffu_temp = getdataframe(nice2020_ffu, :temperature=>:T)[81, :]


#d) Transfers in India in 2050
india_2050_bau = filter(row -> row.time == 2050 && row.country == :IND, getdataframe(bau_model, :revenue_recycle=>:transfer))
india_2050_global = filter(row -> row.time == 2050 && row.country == :IND, getdataframe(nice2020_global_cap_share, :revenue_recycle=>:transfer))
india_2050_ffu = filter(row -> row.time == 2050 && row.country == :IND, getdataframe(nice2020_ffu, :revenue_recycle=>:transfer))
india_2050_imf = filter(row -> row.time == 2050 && row.country == :IND, getdataframe(nice2020_differenciated_prices, :revenue_recycle=>:transfer))

# ======================================================
# Organisation en tableau Excel
# ======================================================

using CSV

# Fonction pour construire le DataFrame complet
function build_results_csv(bau, capshare, ffu, imf, stoft, global_bau, global_capshare, global_ffu, global_imf, global_stoft, bautemp, capsharetemp, ffutemp, imftemp, stofttemp, india_bau, india_global, india_ffu, india_imf, india_stoft)
    df = DataFrame()
    countries = (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :DEU, :COD, :RUS)
    years = (2030, 2050, 2100)

    # Consommation EDE par pays
    for t in years
        for c in countries
            BAU_val = bau[(bau.time .== t) .& (bau.country .== c), :cons_EDE_country][1]
            CapShare_val = capshare[(capshare.time .== t) .& (capshare.country .== c), :cons_EDE_country][1]
            FFU_val = ffu[(ffu.time .== t) .& (ffu.country .== c), :cons_EDE_country][1]
            IMF_val = imf[(imf.time .== t) .& (imf.country .== c), :cons_EDE_country][1]
            STOFT_val = stoft[(stoft.time .== t) .& (stoft.country .== c), :cons_EDE_country][1]
            Var_Rate_FFU_IMF_val = (IMF_val-FFU_val)/FFU_val * 100
            Var_Rate_CapShare_STOFT_val = (STOFT_val-CapShare_val)/CapShare_val * 100
            push!(df, (
                Indicator = "Consumption EDE (countries)",
                Year = t,
                Country = String(c),
                BAU = BAU_val,
                CapShare = CapShare_val,
                FFU = FFU_val,
                IMF = IMF_val,
                STOFT = STOFT_val,
                Var_Rate_FFU_IMF = Var_Rate_FFU_IMF_val,
                Var_Rate_CapShare_STOFT = Var_Rate_CapShare_STOFT_val
            ))
        end
    end

    # Consommation EDE globale
    for t in years
        BAU_val = global_bau[(global_bau.time .== t), :cons_EDE_global][1]
        CapShare_val = global_capshare[(global_capshare.time .== t), :cons_EDE_global][1]
        FFU_val = global_ffu[(global_ffu.time .== t), :cons_EDE_global][1]
        IMF_val = global_imf[(global_imf.time .== t), :cons_EDE_global][1]
        STOFT_val = global_stoft[(global_stoft.time .== t), :cons_EDE_global][1]
        Var_Rate_FFU_IMF_val = (IMF_val-FFU_val)/FFU_val * 100
        Var_Rate_CapShare_STOFT_val = (STOFT_val-CapShare_val)/CapShare_val * 100
        
        push!(df, (
            Indicator = "Consumption EDE (Global)",
            Year = t,
            Country = "Global",
            BAU = BAU_val,
            CapShare = CapShare_val,
            FFU = FFU_val,
            IMF = IMF_val,
            STOFT = STOFT_val,
            Var_Rate_FFU_IMF = Var_Rate_FFU_IMF_val,
            Var_Rate_CapShare_STOFT = Var_Rate_CapShare_STOFT_val
        ))
    end

    for t in years
        BAU_val = bautemp.T
        CapShare_val = capsharetemp.T
        FFU_val = ffutemp.T
        IMF_val = imftemp.T
        STOFT_val = stofttemp.T
        Var_Rate_FFU_IMF_val = (IMF_val-FFU_val)/FFU_val * 100
        Var_Rate_CapShare_STOFT_val = (STOFT_val-CapShare_val)/CapShare_val * 100
        if t ==2100
            push!(df, (
                Indicator = "Global Temperature (2100)",
                Year = t,
                Country = "Global",
                BAU = BAU_val,
                CapShare = CapShare_val,
                FFU = FFU_val,
                IMF = IMF_val,
                STOFT = STOFT_val,
                Var_Rate_FFU_IMF = Var_Rate_FFU_IMF_val,
                Var_Rate_CapShare_STOFT = Var_Rate_CapShare_STOFT_val
            ))
        end
        
        if t == 2050
            BAU_val = india_bau[(india_bau.time .== t) .& (india_bau.country .== :IND), :transfer][1]
            CapShare_val = india_global[(india_global.time .== t) .& (india_global.country .== :IND), :transfer][1]
            FFU_val = india_ffu[(india_ffu.time .== t) .& (india_ffu.country .== :IND), :transfer][1]
            IMF_val = india_imf[(india_imf.time .== t) .& (india_imf.country .== :IND), :transfer][1]
            STOFT_val = india_stoft[(india_stoft.time .== t) .& (india_stoft.country .== :IND), :transfer][1]
            Var_Rate_FFU_IMF_val = (IMF_val-FFU_val)/FFU_val * 100
            Var_Rate_CapShare_STOFT_val = (STOFT_val-CapShare_val)/CapShare_val * 100
            push!(df, (
                Indicator = "Transfers India (2050)",
                Year = t,
                Country = "IND",
                BAU = BAU_val,
                CapShare = CapShare_val,
                FFU = FFU_val,
                IMF = IMF_val,
                STOFT = STOFT_val,
                Var_Rate_FFU_IMF = Var_Rate_FFU_IMF_val,
                Var_Rate_CapShare_STOFT = Var_Rate_CapShare_STOFT_val
            ))
        end
    end    


    return df
end

# Création du DataFrame complet
results = build_results_csv(bau_cons, 
global_cap_share_cons, 
ffu_cons, 
imf_cons,
filter(row -> row.time in (2030, 2050, 2100) && row.country in (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :DEU, :COD, :RUS), getdataframe(nice2020_stoft, :welfare=>:cons_EDE_country)),
global_cons_bau, 
global_cons_cap_share, 
global_cons_ffu, 
global_cons_imf,
getdataframe(nice2020_stoft, :welfare=>:cons_EDE_global)[[11, 31, 81], :],
bau_temp, 
capshare_temp, 
ffu_temp,
imf_temp,
getdataframe(nice2020_stoft, :temperature=>:T)[81, :],
india_2050_bau, 
india_2050_global, 
india_2050_ffu, 
india_2050_imf,
filter(row -> row.time == 2050 && row.country == :IND, getdataframe(nice2020_stoft, :revenue_recycle=>:transfer)))

# Sauvegarde finale dans le CSV
output_file = "/Users/agathe/Desktop/Cired/NICE2020/cap_and_share/output/comparison_output.csv"
CSV.write(output_file, results)
