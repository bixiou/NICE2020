# #------------------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------------------
# # This file contains functions used for creating and running the Nested Inequalities Climate Economy (NICE) model.
# #------------------------------------------------------------------------------------------------------------------
# #------------------------------------------------------------------------------------------------------------------

#####################################################################################################################
# CALCULATE DAMAGE, CO₂ MITIGATION COST, OR CO₂ TAX BURDEN DISTRIBUTIONS ACROSS A COUNTRY'S QUANTILES.
#####################################################################################################################
# Description: This function will calculate quantile distribution shares for a country based
#              on a provided income elasticity.
#
# Function Arguments:
#
#       elasticity    = Income elasticity of climate damages, CO₂ mitigation costs, CO₂ tax burdens, etc.
#       income_shares = A vector of quantile income shares for a given country.
#--------------------------------------------------------------------------------------------------------------------

function country_quantile_distribution(elasticity, income_shares, nb_quantile)

    # Apply elasticity to quantile income shares.
    scaled_shares = income_shares .^ elasticity

    # Allocate empty array for distribution across quantiles resulting from the elasticity.
    updated_quantile_distribution = zeros(nb_quantile)

    # Loop through each quantile to calculate updated distribution.
    for q in 1:nb_quantile
        updated_quantile_distribution[q] = scaled_shares[q] ./ sum(scaled_shares[:])
    end

    return updated_quantile_distribution
end


#######################################################################################################################
# CALCULATE A LINEAR CARBON TAX TRAJECTORY
########################################################################################################################
# Description: This function computes a linear carbon tax path. It assumes a carbon tax of $0 in period 1.
#
# Function Arguments:
#
#       tax_start_value:    Starting value for the carbon tax, at year_tax_start + 1*year_tax_step (in 2017US$ per tCO2)
#       increase_value:     Step for carbon tax increase
#       year_tax_start:     First year of the tax path. Tax starts at zero in year_tax_start and jumps to tax_start_value at year_tax_start + 1*year_tax_step
#       year_tax_end:       Last year in which to compute the tax
#       year_tax_step:      Step in years between two values (defaults to 1)
#       year_model_end:     End of the model, if lower than year_tax_end the last tax value is repeated (defaults to 2300)
#
# Note:
#
#       All arguments defined as keyword arguments instead of positional arguments
#----------------------------------------------------------------------------------------------------------------------

function linear_tax_trajectory(;tax_start_value::Real, increase_value::Real=tax_start_value, year_tax_start::Int64, year_tax_end::Int64, year_step::Int64=1, year_model_end::Int64=2300)

    #tax_values = [tax_start_value * (1 + rate_tax_increase)^(t-(year_tax_start+1) ) for t in year_tax_start+1:year_step:year_tax_end]
    tax_values = [tax_start_value + increase_value * (t-(year_tax_start+1) ) for t in year_tax_start+1:year_step:year_tax_end]

    full_tax_path = [0; tax_values; fill(tax_values[end], year_model_end- year_tax_end)]

    return full_tax_path
end

#######################################################################################################################
# CALCULATE AN EXPONENTIAL GROWTH TAX TRAJECTORY
########################################################################################################################
# Description: This function computes an exponential growth carbon tax path. 
# It assumes a carbon tax of $0 in period 1.
#
# Function Arguments:
#
#       tax_start_value:    Starting value for the carbon tax, at year_tax_start*(1+g_rate)^t (in 2017US$ per tCO2)
#       g_rate:             Growth rate of the carbon tax
#       year_tax_start:     First year of the tax path. Tax starts at zero in year_tax_start and jumps to tax_start_value at year_tax_start + 1*year_tax_step
#       year_tax_end:       Last year in which to compute the tax
#       year_tax_step:      Step in years between two values (defaults to 1)
#       year_model_end:     End of the model, if lower than year_tax_end the last tax value is repeated (defaults to 2300)
#
# Note:
#
#       All arguments defined as keyword arguments instead of positional arguments
#----------------------------------------------------------------------------------------------------------------------

function exp_tax_trajectory(;tax_start_value::Real, g_rate::Real, year_tax_start::Int64, year_tax_end::Int64, year_step::Int64=1, year_model_end::Int64=2300)

    tax_values = [tax_start_value * (1+g_rate) ^ (t-(year_tax_start+1) )  for t in year_tax_start+1:year_step:year_tax_end]


    full_tax_path = [0; tax_values; fill(tax_values[end], year_model_end- year_tax_end)]

    return full_tax_path
end


#######################################################################################################################
# CREATE RESULT DIRECTORIES AND SAVE SPECIFIC MODEL OUTPUT
#######################################################################################################################
# Description: This function creates a folder directory to store results (dividing model output by global,
#              regional, and quantile levels)
# Function Arguments:
#
#       m_policy:                 An instance of NICE with CO2 policy (type = Mimi model).
#       m_bau:                    An instance of NICE with 0% mitigation (no CO2 policy) for all regions and years (type = Mimi model).
#       output_directory:         The directory path to the results folder where a particular set of model output will be saved.
#       revenue_recycling:        A check for whether or not the results recycle CO2 tax revenue (true = recycle, false = no recycling).
#----------------------------------------------------------------------------------------------------------------------

function save_nice2020_results(m::Model, output_directory::String; revenue_recycling::Bool=true, recycling_type::Int64=0,  result_year_end::Int64= 2100)

    # Make subdirectory folders to store results with and without revenue recycling.
    if revenue_recycling == true

        if recycling_type==1
            recycling_type_label="within_country"
        elseif recycling_type==2
            recycling_type_label= "global_per_capita"

        end

        global_path   = joinpath(output_directory, "revenue_recycling", recycling_type_label, "global_output")
        regional_path = joinpath(output_directory, "revenue_recycling", recycling_type_label, "regional_output")
		country_path = joinpath(output_directory, "revenue_recycling", recycling_type_label, "country_output")
        quantile_path = joinpath(output_directory, "revenue_recycling", recycling_type_label, "quantile_output")

        mkpath(global_path)
        mkpath(regional_path)
		mkpath(country_path)
        mkpath(quantile_path)

    else

        global_path   = joinpath(output_directory, "no_revenue_recycling", "global_output")
		regional_path = joinpath(output_directory, "no_revenue_recycling", "regional_output")
        country_path = joinpath(output_directory, "no_revenue_recycling", "country_output")
        quantile_path = joinpath(output_directory, "no_revenue_recycling", "quantile_output")

        mkpath(global_path)
        mkpath(regional_path)
		mkpath(country_path)
        mkpath(quantile_path)
    end

    # Save Global Output.
    #save(joinpath(global_path, "global_co2_mitigation.csv"), DataFrame(get_global_mitigation(m_policy, m_bau), :auto))
    save(joinpath(global_path, "temperature.csv"),                              getdataframe(m, :temperature => :T))
    save(joinpath(global_path, "global_gross_output.csv"),    			       getdataframe(m, :grosseconomy => :YGROSS_global))
    save(joinpath(global_path, "global_gtco2_emissions.csv"),                   getdataframe(m, :emissions =>:E_Global_gtco2))
    save(joinpath(global_path, "global_consumption_gini.csv"),                  getdataframe(m, :quantile_recycle =>:gini_cons_global))
    save(joinpath(global_path, "global_consumption_EDE.csv"),                   getdataframe(m, :welfare => :cons_EDE_global))
    save(joinpath(global_path, "total_tax_revenue.csv"),                        getdataframe(m, :revenue_recycle => :total_tax_revenue))
    save(joinpath(global_path, "globally_recycled_tax_revenue.csv"),            getdataframe(m, :revenue_recycle => :global_revenue))
    save(joinpath(global_path, "global_CPC_post_recycle.csv"),                  getdataframe(m, :quantile_recycle => :CPC_post_global))
    save(joinpath(global_path, "global_club_gtco2_emissions.csv"),              getdataframe(m, :emissions => :E_gtco2_club))


    # Save Regional Output
    save(joinpath(regional_path, "regional_gtco2_emissions.csv"),               getdataframe(m, :emissions =>:E_gtco2_rwpp))
    save(joinpath(regional_path, "regional_consumption_per_capita.csv"),        getdataframe(m, :neteconomy => :CPC_rwpp))
    save(joinpath(regional_path, "regional_net_output_per_capita.csv"),         getdataframe(m, :neteconomy => :Y_pc_rwpp))
    save(joinpath(regional_path, "regional_consumption_per_capita_post_recycle.csv"), getdataframe(m, :quantile_recycle => :CPC_post_rwpp))
    save(joinpath(regional_path, "regional_consumption_gini.csv"),              getdataframe(m, :quantile_recycle =>:gini_cons_rwpp))
    save(joinpath(regional_path, "regional_consumption_EDE.csv"),               getdataframe(m, :welfare => :cons_EDE_rwpp))

    # Save Country Output.
    save(joinpath(country_path, "gross_output.csv"),                    getdataframe(m, :grosseconomy =>:YGROSS))
    save(joinpath(country_path, "nice_net_output.csv"),                 getdataframe(m, :neteconomy =>:Y))
    save(joinpath(country_path, "consumption.csv"),                     getdataframe(m, :neteconomy =>:C))
    save(joinpath(country_path, "population.csv"),                      getdataframe(m, :neteconomy =>:l))
    save(joinpath(country_path, "consumption_per_capita.csv"),          getdataframe(m, :neteconomy => :CPC))
    save(joinpath(country_path, "net_output_per_capita.csv"),           getdataframe(m, :neteconomy => :Y_pc))
    save(joinpath(country_path, "local_temp_anomaly.csv"),              getdataframe(m, :damages =>:local_temp_anomaly))
    save(joinpath(country_path, "local_damage_cost_share_KW.csv"),      getdataframe(m, :damages =>:LOCAL_DAMFRAC_KW))
    save(joinpath(country_path, "abatement_cost_share.csv"),            getdataframe(m, :abatement =>:ABATEFRAC))
    save(joinpath(country_path, "country_carbon_tax.csv"),              getdataframe(m, :abatement =>:country_carbon_tax))
    save(joinpath(country_path, "co2_emissions.csv"),        getdataframe(m, :emissions =>:E_gtco2))
    save(joinpath(country_path,  "mu.csv"),        getdataframe(m, :abatement  => :μ))
    save(joinpath(country_path, "country_tax_revenue.csv"),             getdataframe(m, :revenue_recycle =>:tax_revenue))
    save(joinpath(country_path, "country_pc_tax_dividend.csv"),          getdataframe(m, :revenue_recycle =>:country_pc_dividend))
    save(joinpath(country_path, "country_pc_dividend_domestic_transfers.csv"), getdataframe(m, :revenue_recycle =>:country_pc_dividend_domestic_transfers))
    save(joinpath(country_path, "country_pc_dividend_global_transfers.csv"), getdataframe(m, :revenue_recycle =>:country_pc_dividend_global_transfers))

    save(joinpath(country_path, "consumption_per_capita_post_recycle.csv"), getdataframe(m, :quantile_recycle => :CPC_post))
    save(joinpath(country_path, "consumption_gini.csv"),                getdataframe(m, :quantile_recycle =>:gini_cons))
    save(joinpath(country_path, "consumption_EDE.csv"),                 getdataframe(m, :welfare => :cons_EDE_country))
    save(joinpath(country_path, "transfer_over_gdp.csv"),                 getdataframe(m, :revenue_recycle => :transfer_over_gdp))
    save(joinpath(country_path, "transfer_pc.csv"),                    getdataframe(m, :revenue_recycle => :transfer_pc))
    save(joinpath(country_path, "transfer.csv"),                       getdataframe(m, :revenue_recycle => :transfer))

    # Save Quantile Output.
    save(joinpath(quantile_path, "co2_tax_distribution.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :tax_burden_distr)))
    save(joinpath(quantile_path, "base_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_base)))
    save(joinpath(quantile_path, "post_damage_abatement_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_damage_abatement)))
    save(joinpath(quantile_path, "post_tax_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_tax)))
    save(joinpath(quantile_path, "post_recycle_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_recycle)))
    save(joinpath(quantile_path, "post_recycle_share_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :qc_share)))

end

function save_nice2020_results_cap_and_share(
    m::Model,
    output_directory::String;
    revenue_recycling::Bool=true,
    recycling_type::Int64=0,
    result_year_end::Int64=2100,
    switch_custom_transfers::Int64=0,
    file_prefix::String=""
)
    # 1) Choice of transfer label
    #    "custom_transfers" when switch_custom_transfers==1, otherwise "equal_pc_transfer"
    transfer_label = switch_custom_transfers == 1 ? "custom_transfers" : "equal_pc_transfer"

    # 1bis) Build directory label: only prefix when custom_transfers
    dir_label = (switch_custom_transfers == 1 && !isempty(file_prefix)) ?
                 "$(file_prefix)_$(transfer_label)" :
                 transfer_label

    # 2) Path construction based on revenue_recycling and recycling_type
    if revenue_recycling
        if recycling_type == 1
            recycling_type_label = "within_country"
        elseif recycling_type == 2
            recycling_type_label = "global_per_capita"
        else
            error("recycling_type must be 1 or 2.")
        end
        base_rr = joinpath(output_directory, "revenue_recycling", recycling_type_label, dir_label)
    else
        base_rr = joinpath(output_directory, "no_revenue_recycling", dir_label)
    end

    # Define subfolders
    global_path   = joinpath(base_rr, "global_output")
    regional_path = joinpath(base_rr, "regional_output")
    country_path  = joinpath(base_rr, "country_output")
    quantile_path = joinpath(base_rr, "quantile_output")
    mkpath.( (global_path, regional_path, country_path, quantile_path) )

    # 3) Save CSVs
    # Global
    save(joinpath(global_path,   "temperature.csv"),            getdataframe(m, :temperature => :T))
    save(joinpath(global_path,   "global_gross_output.csv"),    getdataframe(m, :grosseconomy => :YGROSS_global))
    save(joinpath(global_path,   "global_gtco2_emissions.csv"), getdataframe(m, :emissions     => :E_Global_gtco2))
    save(joinpath(global_path,   "global_consumption_gini.csv"),getdataframe(m, :quantile_recycle => :gini_cons_global))
    save(joinpath(global_path,   "global_consumption_EDE.csv"), getdataframe(m, :welfare         => :cons_EDE_global))
    save(joinpath(global_path,   "total_tax_revenue.csv"),      getdataframe(m, :revenue_recycle  => :total_tax_revenue))
    save(joinpath(global_path,   "globally_recycled_tax_revenue.csv"), getdataframe(m, :revenue_recycle => :global_revenue))
    save(joinpath(global_path,   "global_CPC_post_recycle.csv"),getdataframe(m, :quantile_recycle => :CPC_post_global))
    save(joinpath(global_path,   "global_club_gtco2_emissions.csv"), getdataframe(m, :emissions => :E_gtco2_club))

    # Regional
    save(joinpath(regional_path, "regional_gtco2_emissions.csv"),               getdataframe(m, :emissions => :E_gtco2_rwpp))
    save(joinpath(regional_path, "regional_consumption_per_capita.csv"),        getdataframe(m, :neteconomy => :CPC_rwpp))
    save(joinpath(regional_path, "regional_net_output_per_capita.csv"),         getdataframe(m, :neteconomy => :Y_pc_rwpp))
    save(joinpath(regional_path, "regional_consumption_per_capita_post_recycle.csv"), getdataframe(m, :quantile_recycle => :CPC_post_rwpp))
    save(joinpath(regional_path, "regional_consumption_gini.csv"),              getdataframe(m, :quantile_recycle => :gini_cons_rwpp))
    save(joinpath(regional_path, "regional_consumption_EDE.csv"),               getdataframe(m, :welfare => :cons_EDE_rwpp))

    # Country
    save(joinpath(country_path,  "gross_output.csv"),                    getdataframe(m, :grosseconomy => :YGROSS))
    save(joinpath(country_path,  "nice_net_output.csv"),                 getdataframe(m, :neteconomy  => :Y))
    save(joinpath(country_path,  "consumption.csv"),                     getdataframe(m, :neteconomy  => :C))
    save(joinpath(country_path,  "population.csv"),                      getdataframe(m, :neteconomy  => :l))
    save(joinpath(country_path, "consumption_EDE.csv"),                 getdataframe(m, :welfare => :cons_EDE_country))
    save(joinpath(country_path,  "consumption_per_capita.csv"),          getdataframe(m, :neteconomy  => :CPC))
    save(joinpath(country_path,  "net_output_per_capita.csv"),           getdataframe(m, :neteconomy  => :Y_pc))
    save(joinpath(country_path,  "local_temp_anomaly.csv"),              getdataframe(m, :damages    => :local_temp_anomaly))
    save(joinpath(country_path,  "local_damage_cost_share_KW.csv"),      getdataframe(m, :damages    => :LOCAL_DAMFRAC_KW))
    save(joinpath(country_path,  "abatement_cost_share.csv"),            getdataframe(m, :abatement  => :ABATEFRAC))
    save(joinpath(country_path,  "country_carbon_tax.csv"),              getdataframe(m, :abatement  => :country_carbon_tax))
    save(joinpath(country_path,  "co2_emissions.csv"),        getdataframe(m, :emissions  => :E_gtco2))
    save(joinpath(country_path,  "mu.csv"),        getdataframe(m, :abatement  => :μ))
    save(joinpath(country_path,  "country_tax_revenue.csv"),             getdataframe(m, :revenue_recycle => :tax_revenue))
    save(joinpath(country_path,  "country_pc_tax_dividend.csv"),         getdataframe(m, :revenue_recycle => :country_pc_dividend))
    save(joinpath(country_path,  "country_pc_dividend_domestic_transfers.csv"), getdataframe(m, :revenue_recycle => :country_pc_dividend_domestic_transfers))
    save(joinpath(country_path,  "country_pc_dividend_global_transfers.csv"),  getdataframe(m, :revenue_recycle => :country_pc_dividend_global_transfers))
    save(joinpath(country_path,  "transfer_over_gdp.csv"),               getdataframe(m, :revenue_recycle => :transfer_over_gdp))
    save(joinpath(country_path,  "transfer_pc.csv"),                     getdataframe(m, :revenue_recycle => :transfer_pc))
    save(joinpath(country_path,  "transfer.csv"),                        getdataframe(m, :revenue_recycle => :transfer))

    # Quantile
    save(joinpath(quantile_path, "co2_tax_distribution.csv"), filter!(:time => x -> x < result_year_end, getdataframe(m, :quantile_recycle => :tax_burden_distr)))
    save(joinpath(quantile_path, "base_pc_consumption.csv"), filter!(:time => x -> x < result_year_end, getdataframe(m, :quantile_recycle => :conso_pc_base)))
    save(joinpath(quantile_path, "post_damage_abatement_pc_consumption.csv"), filter!(:time => x -> x < result_year_end, getdataframe(m, :quantile_recycle => :conso_pc_post_damage_abatement)))
    save(joinpath(quantile_path, "post_tax_pc_consumption.csv"), filter!(:time => x -> x < result_year_end, getdataframe(m, :quantile_recycle => :conso_pc_post_tax)))
    save(joinpath(quantile_path, "post_recycle_pc_consumption.csv"), filter!(:time => x -> x < result_year_end, getdataframe(m, :quantile_recycle => :conso_pc_post_recycle)))
    save(joinpath(quantile_path, "post_recycle_share_consumption.csv"), filter!(:time => x -> x < result_year_end, getdataframe(m, :quantile_recycle => :qc_share)))
end

function save_nice2020_output(m::Model, output_directory::String)
    mkpath(output_directory)
    
    global_path   = joinpath(output_directory, "global_output")
    regional_path = joinpath(output_directory, "regional_output")
    country_path = joinpath(output_directory, "country_output")
    quantile_path = joinpath(output_directory, "quantile_output")

    mkpath(global_path)
    mkpath(regional_path)
    mkpath(country_path)
    mkpath(quantile_path)

    # Save Global Output.
    #save(joinpath(global_path, "global_co2_mitigation.csv"), DataFrame(get_global_mitigation(m_policy, m_bau), :auto))
    save(joinpath(global_path, "temperature.csv"),                              getdataframe(m, :temperature => :T))
    save(joinpath(global_path, "global_gross_output.csv"),    			       getdataframe(m, :grosseconomy => :YGROSS_global))
    save(joinpath(global_path, "global_gtco2_emissions.csv"),                   getdataframe(m, :emissions =>:E_Global_gtco2))
    save(joinpath(global_path, "global_consumption_gini.csv"),                  getdataframe(m, :quantile_recycle =>:gini_cons_global))
    save(joinpath(global_path, "global_consumption_EDE.csv"),                   getdataframe(m, :welfare => :cons_EDE_global))
    save(joinpath(global_path, "total_tax_revenue.csv"),                        getdataframe(m, :revenue_recycle => :total_tax_revenue))
    save(joinpath(global_path, "globally_recycled_tax_revenue.csv"),            getdataframe(m, :revenue_recycle => :global_revenue))
    save(joinpath(global_path, "global_CPC_post_recycle.csv"),                  getdataframe(m, :quantile_recycle => :CPC_post_global))
    save(joinpath(global_path, "global_club_gtco2_emissions.csv"),              getdataframe(m, :emissions => :E_gtco2_club))


    # Save Regional Output
    save(joinpath(regional_path, "regional_gtco2_emissions.csv"),               getdataframe(m, :emissions =>:E_gtco2_rwpp))
    save(joinpath(regional_path, "regional_consumption_per_capita.csv"),        getdataframe(m, :neteconomy => :CPC_rwpp))
    save(joinpath(regional_path, "regional_net_output_per_capita.csv"),         getdataframe(m, :neteconomy => :Y_pc_rwpp))
    save(joinpath(regional_path, "regional_consumption_per_capita_post_recycle.csv"), getdataframe(m, :quantile_recycle => :CPC_post_rwpp))
    save(joinpath(regional_path, "regional_consumption_gini.csv"),              getdataframe(m, :quantile_recycle =>:gini_cons_rwpp))
    save(joinpath(regional_path, "regional_consumption_EDE.csv"),               getdataframe(m, :welfare => :cons_EDE_rwpp))

    # Save Country Output.
    save(joinpath(country_path, "gross_output.csv"),                    getdataframe(m, :grosseconomy =>:YGROSS))
    save(joinpath(country_path, "nice_net_output.csv"),                 getdataframe(m, :neteconomy =>:Y))
    save(joinpath(country_path, "consumption.csv"),                     getdataframe(m, :neteconomy =>:C))
    save(joinpath(country_path, "population.csv"),                      getdataframe(m, :neteconomy =>:l))
    save(joinpath(country_path, "consumption_per_capita.csv"),          getdataframe(m, :neteconomy => :CPC))
    save(joinpath(country_path, "net_output_per_capita.csv"),           getdataframe(m, :neteconomy => :Y_pc))
    save(joinpath(country_path, "local_temp_anomaly.csv"),              getdataframe(m, :damages =>:local_temp_anomaly))
    save(joinpath(country_path, "local_damage_cost_share_KW.csv"),      getdataframe(m, :damages =>:LOCAL_DAMFRAC_KW))
    save(joinpath(country_path, "abatement_cost_share.csv"),            getdataframe(m, :abatement =>:ABATEFRAC))
    save(joinpath(country_path, "country_carbon_tax.csv"),              getdataframe(m, :abatement =>:country_carbon_tax))
    save(joinpath(country_path, "co2_emissions.csv"),        getdataframe(m, :emissions =>:E_gtco2))
    save(joinpath(country_path,  "mu.csv"),        getdataframe(m, :abatement  => :μ))
    save(joinpath(country_path, "country_tax_revenue.csv"),             getdataframe(m, :revenue_recycle =>:tax_revenue))
    save(joinpath(country_path, "country_pc_tax_dividend.csv"),          getdataframe(m, :revenue_recycle =>:country_pc_dividend))
    save(joinpath(country_path, "country_pc_dividend_domestic_transfers.csv"), getdataframe(m, :revenue_recycle =>:country_pc_dividend_domestic_transfers))
    save(joinpath(country_path, "country_pc_dividend_global_transfers.csv"), getdataframe(m, :revenue_recycle =>:country_pc_dividend_global_transfers))

    save(joinpath(country_path, "consumption_per_capita_post_recycle.csv"), getdataframe(m, :quantile_recycle => :CPC_post))
    save(joinpath(country_path, "consumption_gini.csv"),                getdataframe(m, :quantile_recycle =>:gini_cons))
    save(joinpath(country_path, "consumption_EDE.csv"),                 getdataframe(m, :welfare => :cons_EDE_country))
    save(joinpath(country_path, "transfer_over_gdp.csv"),                 getdataframe(m, :revenue_recycle => :transfer_over_gdp))
    save(joinpath(country_path, "transfer_pc.csv"),                    getdataframe(m, :revenue_recycle => :transfer_pc))
    save(joinpath(country_path, "transfer.csv"),                       getdataframe(m, :revenue_recycle => :transfer))

    # Save Quantile Output.
    save(joinpath(quantile_path, "co2_tax_distribution.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :tax_burden_distr)))
    save(joinpath(quantile_path, "base_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_base)))
    save(joinpath(quantile_path, "post_damage_abatement_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_damage_abatement)))
    save(joinpath(quantile_path, "post_tax_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_tax)))
    save(joinpath(quantile_path, "post_recycle_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_recycle)))
    save(joinpath(quantile_path, "post_recycle_share_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :qc_share)))

end