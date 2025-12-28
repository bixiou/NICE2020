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
#       ramp_up:            Number of periods the tax is linearly ramped up
#
# Note:
#
#       All arguments defined as keyword arguments instead of positional arguments
#----------------------------------------------------------------------------------------------------------------------

function exp_tax_trajectory(;tax_start_value::Real, g_rate::Real, year_tax_start::Int64, year_tax_end::Int64, year_step::Int64=1, year_model_end::Int64=2300, ramp_up = 0)

    tax_values = [tax_start_value * (1+g_rate) ^ (t-(year_tax_start+1) )  for t in year_tax_start+1:year_step:year_tax_end]
    if ramp_up > 0
        first_years = [tax_start_value * t/ramp_up  for t in 0:year_step:ramp_up]
    else
        first_years = [0]
    end
    pre_tax = zeros(max(0, tax_start_year - ramp_up - 2020))

    full_tax_path = [pre_tax; first_years; tax_values; fill(tax_values[end], year_model_end - year_tax_end)]

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
    save(joinpath(country_path, "net_surplus.csv"),                       getdataframe(m, :quantile_recycle => :net_surplus))
    save(joinpath(country_path, "net_transfer_pib.csv"),                       getdataframe(m, :quantile_recycle => :net_transfer_pib))
    save(joinpath(country_path, "net_surplus_per_pib.csv"),                       getdataframe(m, :quantile_recycle => :net_surplus_per_pib))


    # Save Quantile Output.
    save(joinpath(quantile_path, "co2_tax_distribution.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :tax_burden_distr)))
    save(joinpath(quantile_path, "base_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_base)))
    save(joinpath(quantile_path, "post_damage_abatement_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_damage_abatement)))
    save(joinpath(quantile_path, "post_tax_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_tax)))
    save(joinpath(quantile_path, "post_recycle_pc_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :conso_pc_post_recycle)))
    save(joinpath(quantile_path, "post_recycle_share_consumption.csv"), filter!(:time => x -> x<2121, getdataframe(m, :quantile_recycle => :qc_share)))

end

#########################################################################################################################
# FUNCTION TO RETRIEVE OUTPUT VALUES AND BUILD A SUMMARY CSV
#########################################################################################################################


function build_results_csv(scenarios, names_scenarios, countries, years)
    df = DataFrame()
    #Consumption EDE per country
    for t in years
        for c in countries
            for (i, m) in enumerate(scenarios)
                val = only(filter(row -> row.time == t && row.country == c,
                  getdataframe(m, :welfare=>:cons_EDE_country)).cons_EDE_country)
                push!(df,(
                    Indicator = "Consumption EDE (countries)",
                    Year = t,
                    Country = string(c),
                    Scenario = String(names_scenarios[i]),
                    Value = val
                ))
            end
        end        
    end

    #Consumption EDE global
    for t in years
        for (i,m) in enumerate(scenarios)
            val_global = only(filter(row -> row.time == t,
                  getdataframe(m, :welfare=>:cons_EDE_global)).cons_EDE_global)
            push!(df, (
                Indicator = "Consumption EDE (global)",
                Year = t,
                Country = "Global",
                Scenario = String(names_scenarios[i]),
                Value = val_global
            ))
        end
    end

    #Temperature in 2100
    for (i,m) in enumerate(scenarios)
        var_temp = only(filter(row -> row.time == 2100,
            getdataframe(m, :temperature=>:T)).T)
         push!(df, (
            Indicator = "Global temperature in 2100",
            Year = 2100,
            Country = "Global",
            Scenario = String(names_scenarios[i]),
            Value = var_temp
        ))
    end

    #Transfers in India in 2050
    for (i,m) in enumerate(scenarios)
        var_india = only(filter(row -> row.time == 2050 && row.country == :IND,
            getdataframe(m, :revenue_recycle=>:transfer)).transfer)
        push!(df, (
            Indicator = "Transfers in India in 2050",
            Year = 2050,
            Country = "IND",
            Scenario = String(names_scenarios[i]),
            Value = var_india
        ))
    end

    df = unstack(df, [:Indicator, :Country, :Year], :Scenario, :Value)
    return df

end


#########################################################################################################################
#FUNCTION TO COMPUTE THE YEAR AT WHICH CONSUMPTION EDE PER CAPITA OF THE SCENARIO BECOMES PERMANENTLY HIGHER THAN THE CONSUMPTION EDE PER CAPITA OF THE BAU
#########################################################################################################################

function year_EDE_higher_than_BAU(scenario)
    countries = dim_keys(scenario, :country)
    years = dim_keys(scenario, :time)
    df = DataFrame(country = String[], year = Int[])
    cons_EDE_bau = getdataframe(bau_model, :welfare, :cons_EDE_country)
    cons_EDE_bau_global = getdataframe(bau_model, :welfare, :cons_EDE_global)
    cons_EDE = getdataframe(scenario, :welfare, :cons_EDE_country)
    cons_EDE_global = getdataframe(scenario, :welfare, :cons_EDE_global)
    for c in countries
        years_inf = Vector{Int}()
        for t in years
            cons_EDE_bau_val = cons_EDE_bau[(cons_EDE_bau.time .== t) .& (cons_EDE_bau.country .== c), :cons_EDE_country][1]
            cons_EDE_val = cons_EDE[(cons_EDE.time .== t) .& (cons_EDE.country .== c), :cons_EDE_country][1]
            if cons_EDE_val <= cons_EDE_bau_val
                push!(years_inf, t)
            end
        end
        if isempty(years_inf)
            push!(df, (String(c), 2020))
        else
            last_year = maximum(years_inf)
            first_year = last_year + 1
            if first_year <= maximum(years)
                push!(df, (String(c), first_year))
            else
                push!(df, (String(c), 9999))
            end
        end
    end

    years_inf_global = Vector{Int}()
    for t in years
        cons_EDE_bau_global_val = cons_EDE_bau_global[cons_EDE_bau_global.time .== t, :cons_EDE_global][1]
        cons_EDE_global_val = cons_EDE_global[cons_EDE_global.time .== t, :cons_EDE_global][1]
        if cons_EDE_global_val <= cons_EDE_bau_global_val
            push!(years_inf_global, t)
        end
    end
    if isempty(years_inf_global)
        push!(df, ("Global", 2020))
    else
        last_year_global = maximum(years_inf_global)
        first_year_global = last_year_global + 1
        if first_year_global <= maximum(years)
            push!(df, ("Global", first_year_global))
        else
            push!(df, ("Global", 9999))
        end
    end
    return df
end


#########################################################################################################################
#FUNCTION TO COMPUTE THE NET PRESENT VALUE 
#########################################################################################################################

function net_present_value(value, start_year, end_year, discount_rate, name_value)
    col = Symbol(name_value)  # convertir le nom en symbole
    mask = (value.time .>= start_year) .& (value.time .<= end_year)
    npv = sum(
        value[mask, col] ./ (1 .+ discount_rate) .^ (value[mask, :time] .- start_year)
    )
    return npv
end



#########################################################################################################################
#FUNCTION THAT RETURNS DIFFERENT DECOMPOSITION OF WELFARE GAINS BETWEEN TWO SCENARIOS
#########################################################################################################################

function welfare_gains(scenario1, scenario2, year, c_list::AbstractVector)
    damages1 = 0.0
    damages2 = 0.0
    transfer1 = 0.0
    transfer2 = 0.0
    cons1 = 0.0
    cons2 = 0.0
    abat_cost1 = 0.0
    abat_cost2 = 0.0
    tot_cons_post_1 = 0.0
    tot_cons_post_2 = 0.0
    ede_1 = 0.0
    ede_2 = 0.0
    
    # Retrieve nb_quantile (scalar parameter, same for all countries and years)
    nb_quantile = try
        Mimi.get_param(scenario1, :quantile_recycle, :nb_quantile)
    catch
        10  # Default value if parameter not found
    end
    
    #We create a loop over the list of countries to compute the sum of each component => values kept in million USD2017 per year
    for c in c_list
        #1 Avoided damages => national level (1e6 USD2017 per year)
        damages1 =  damages1 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :damages=>:LOCAL_DAMFRAC_KW)).LOCAL_DAMFRAC_KW))* (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :grosseconomy=>:YGROSS)).YGROSS))
        damages2 = damages2 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :damages=>:LOCAL_DAMFRAC_KW)).LOCAL_DAMFRAC_KW))* (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :grosseconomy=>:YGROSS)).YGROSS))
        #2 Transfers => national level ($) => /1e6 => million USD2017 per year
        transfer1 = transfer1 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :revenue_recycle=>:transfer)).transfer)/1e6)
        #println(transfer1)
        transfer2 = transfer2 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :revenue_recycle=>:transfer)).transfer)/1e6)
        #println(transfer2)
        #3 Growth => comparison of gross consumtion => national level (1e6 USD2017 per year)
        cons1 = cons1 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :grosseconomy=>:YGROSS)).YGROSS) * (1 - only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :abatement=>:s)).s)))
        cons2 = cons2 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :grosseconomy=>:YGROSS)).YGROSS) * (1 - only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :abatement=>:s)).s)))
        #4 Abatement costs (million 2017 USD per year)
        abat_cost1 = abat_cost1 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :abatement=>:ABATECOST)).ABATECOST))
        abat_cost2 = abat_cost2 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :abatement=>:ABATECOST)).ABATECOST))
        #5 Reduction of Inequalities (thousand USD2017) / (thousand people) /!\ sum_conso_pc_post_recycle is the sum over the quantiles, so it has to be divided by the number of deciles
        tot_cons_post_1 = tot_cons_post_1 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :quantile_recycle=>:sum_conso_pc_post_recycle)).sum_conso_pc_post_recycle) / nb_quantile) * only(filter(row -> row.time == year && row.country == c, getdataframe(scenario1, :quantile_recycle=>:l)).l)
        tot_cons_post_2 = tot_cons_post_2 + (only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :quantile_recycle=>:sum_conso_pc_post_recycle)).sum_conso_pc_post_recycle) / nb_quantile) * only(filter(row -> row.time == year && row.country == c, getdataframe(scenario2, :quantile_recycle=>:l)).l)
        #println(tot_cons_post_1,".",tot_cons_post_2)
    end 
    # Consumption EDE agrégée (via EDE_aggregated)
    # 1) Récupérer les DataFrames nécessaires
    df_ede1 = getdataframe(scenario1, :welfare => :cons_EDE_country)
    df_ede2 = getdataframe(scenario2, :welfare => :cons_EDE_country)
    df_pop1 = getdataframe(scenario1, :neteconomy => :l)
    df_pop2 = getdataframe(scenario2, :neteconomy => :l)
    #normally df_pop1 = df_pop2 because same year same country

    # 2) Construire les vecteurs EDE pays et population, dans l'ordre de c_list
    ede_vec_1 = [only(filter(row -> row.time == year && row.country == c, df_ede1)).cons_EDE_country for c in c_list]
    ede_vec_2 = [only(filter(row -> row.time == year && row.country == c, df_ede2)).cons_EDE_country for c in c_list]
    #println(ede_vec_1,".",ede_vec_2)
    pop_vec_1 = [only(filter(row -> row.time == year && row.country == c, df_pop1)).l for c in c_list]
    pop_vec_2 = [only(filter(row -> row.time == year && row.country == c, df_pop2)).l for c in c_list]

    # 3) Récupérer η (préférence pour get_param; fallback sur shared param; sinon valeur par défaut)
    η = try
        Mimi.get_param(scenario1, :welfare, :η)
    catch
        try
            Mimi.get_shared_param(scenario1, :η)
        catch
            1.5
        end
    end
    # 4) EDE agrégée pour chaque scénario
    #    Evite l'erreur de world-age en appelant via invokelatest,
    #    We convert in 1e6 USD2017 - EDE_aggregated gives values in thousand USD2017 per capita
    ede_1 = Base.invokelatest(MimiNICE2020.EDE_aggregated, ede_vec_1, pop_vec_1, η) * sum(pop_vec_1)
    ede_2 = Base.invokelatest(MimiNICE2020.EDE_aggregated, ede_vec_2, pop_vec_2, η) * sum(pop_vec_2) #1e6
    #print(ede_1,".",ede_2)

    #Now that we have the totals for each component, we can compute the different parts of the decomposition
    #By dividing by the total population (in thousands) we obtain values in thousand USD2017 per capita per year (1e6/1e3)
    damages_avoided = (damages2 - damages1)/ede_2 #we do 2-1 to have the avoided damages => so positive value
    transfer_diff = (transfer1 - transfer2)/ede_2
    growth = (cons1 - cons2)/ede_2
    abat_cost = -(abat_cost1 - abat_cost2)/ede_2
    reduction_inequalities = (((ede_1 - tot_cons_post_1)*(tot_cons_post_2/tot_cons_post_1) - (ede_2 - tot_cons_post_2))/ede_2)
    #reduction_inequalities = (((ede_1 - tot_cons_post_1)* (1+tot_cons_post_2/tot_cons_post_1) - (ede_2 - tot_cons_post_2)*(1+tot_cons_post_1/tot_cons_post_2))/(2*sum(pop_vec_1))
    total_welfare_gains = (ede_1 - ede_2)/ede_2
    residual_tot = total_welfare_gains - (damages_avoided + transfer_diff + growth + abat_cost + reduction_inequalities)
    redis_1 = ede_1 - (damages1 + transfer1 + cons1 - abat_cost1 +(ede_1 - tot_cons_post_1))
    #println("conso brute", cons1, c_list)
    #println("conso_post_recycle", tot_cons_post_1, c_list)
    #println("conso EDE", ede_1, c_list)
    ineq1 = (ede_1 - tot_cons_post_1)

    return (damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, total_welfare_gains, residual_tot)
end


# Build a tidy table of welfare gains components for a list of countries.
function welfare_gains_table(
    scenario1,
    scenario2,
    year::Int,
    countries::AbstractVector;
    scenario1_name::AbstractString = "Scenario1",
    scenario2_name::AbstractString = "Scenario2",
    include_global::Bool = false,
    global_label::AbstractString = "Global",
    eu_label::AbstractString = "European Union (27)"
)
    df = DataFrame(
        Country = String[],
        Year = Int[],
        Scenario1 = String[],
        Scenario2 = String[],
        damages_avoided = Float64[],
        transfer_diff = Float64[],
        growth = Float64[],
        abat_cost = Float64[],
        reduction_inequalities = Float64[],
        total_welfare_gains = Float64[],
        residual_tot = Float64[]
    )

    for c in countries
        damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, total_welfare_gains, residual_tot = welfare_gains(scenario1, scenario2, year, [c])
        push!(df, (
            Country = string(c),
            Year = year,
            Scenario1 = scenario1_name,
            Scenario2 = scenario2_name,
            damages_avoided = damages_avoided,
            transfer_diff = transfer_diff,
            growth = growth,
            abat_cost = abat_cost,
            reduction_inequalities = reduction_inequalities,
            total_welfare_gains = total_welfare_gains,
            residual_tot = residual_tot
        ))
    end

    # Optionally add a global aggregation row over all model countries
    if include_global
        all_countries = collect(dim_keys(scenario1, :country))
        damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, total_welfare_gains, residual_tot = welfare_gains(scenario1, scenario2, year, all_countries)
        push!(df, (
            Country = String(global_label),
            Year = year,
            Scenario1 = scenario1_name,
            Scenario2 = scenario2_name,
            damages_avoided = damages_avoided,
            transfer_diff = transfer_diff,
            growth = growth,
            abat_cost = abat_cost,
            reduction_inequalities = reduction_inequalities,
            total_welfare_gains = total_welfare_gains,
            residual_tot = residual_tot
        ))
    end

    eu27_countries = Symbol.(["AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"])
    damages_avoided, transfer_diff, growth, abat_cost, reduction_inequalities, total_welfare_gains, residual_tot = welfare_gains(scenario1, scenario2, year, eu27_countries)
    push!(df, (
        Country = String(eu_label),
        Year = year,
        Scenario1 = scenario1_name,
        Scenario2 = scenario2_name,
        damages_avoided = damages_avoided,
        transfer_diff = transfer_diff,
        growth = growth,
        abat_cost = abat_cost,
        reduction_inequalities = reduction_inequalities,
        total_welfare_gains = total_welfare_gains,
        residual_tot = residual_tot
        ))

    return df
end

# Convenience wrapper to compute and export welfare gains components to CSV.
function write_welfare_gains_csv(
    scenario1,
    scenario2,
    year::Int,
    countries::AbstractVector,
    filepath::AbstractString;
    scenario1_name::AbstractString = "Scenario1",
    scenario2_name::AbstractString = "Scenario2",
    include_global::Bool = false,
    global_label::AbstractString = "Global",
    eu_label::AbstractString = "European Union (27)"
)
    df = welfare_gains_table(
        scenario1,
        scenario2,
        year,
        countries;
        scenario1_name = scenario1_name,
        scenario2_name = scenario2_name,
        include_global = include_global,
        global_label = global_label,
        eu_label = eu_label
    )
    CSV.write(filepath, df)
    return df
end