# This file creates the NICE2020 model as a module
##################################################

# Set as a Julia module.
module MimiNICE2020

# Load required packages.
using Mimi, MimiFAIRv2, Statistics, Random, Distributions, Inequality

# Load economic data country by country
include("../data/parameters.jl")

# Load helper functions
include("helper_functions.jl")

# Load additional Mimi model components
include(joinpath("components", "gross_economy.jl"))
include(joinpath("components", "abatement.jl"))
include(joinpath("components", "emissions.jl"))

include(joinpath("components", "pattern_scale.jl"))
include(joinpath("components", "damages.jl"))
include(joinpath("components", "net_economy.jl"))
include(joinpath("components", "revenue_recycle.jl"))
include(joinpath("components", "quantile_recycle.jl"))
include(joinpath("components", "welfare.jl"))


# Create a function that couples FAIRv2.0 to the economic model components
function create_nice2020()

	# Get an instance of Mimi-FAIRv2 with SSP2-45 emissions and radiative forcing.
	m = MimiFAIRv2.get_model(emissions_forcing_scenario="ssp245", start_year=2020, end_year=2300, param_type = "Number")

	# Set country dimension
	c = Symbol.(countries)
	set_dimension!(m, :country, c)

	# Set wpp regions dimension (20 wpp regions)
	rwpp = Symbol.(wpp_regions)
	set_dimension!(m, :regionwpp, rwpp )

	# Set scenario dimension (1 to 6)
	set_dimension!(m, :scenario, 1:6)

	# Set quantile dimension
	set_dimension!(m, :quantile, ["First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth", "Ninth", "Tenth"])
	nb_quantile = length(dim_keys(m, :quantile))
	nb_country = length(dim_keys(m, :country))

	# Add emissions and gross economy components before FAIR carbon cycle
	add_comp!(m, emissions, before = :co2_cycle)
	add_comp!(m, abatement, before = :emissions)
	add_comp!(m, grosseconomy, before = :abatement)

	# Couple pattern scaling, regional damages, and neteconomy after FAIR
	add_comp!(m, pattern_scale, after = :temperature)
	add_comp!(m, damages, after = :pattern_scale)
	add_comp!(m, neteconomy, after = :damages)
	add_comp!(m, revenue_recycle, after = :neteconomy)
	add_comp!(m, neteconomy_with_redistrib, after = :revenue_recycle)
	add_comp!(m, quantile_recycle, after = :revenue_recycle)
	add_comp!(m, welfare, after = :quantile_recycle)

	# Set parameters

	# ---------------------------------------------------
	# Shared parameters (used in more than one component)
	# ---------------------------------------------------

	add_shared_param!(m, :switch_recycle, 0) # No revenue recycling by default
	add_shared_param!(m, :l, 			Matrix(pop), dims=[:time, :country])
	add_shared_param!(m, :mapcrwpp,  	map_country_region_wpp, dims = [:country]) # mapping from countries to wpp region
	add_shared_param!(m, :nb_quantile, 	nb_quantile)
	add_shared_param!(m, :η, 	1.5)
	add_shared_param!(m, :σ, 	Matrix(emissionsrate), dims=[:time, :country])
	add_shared_param!(m,  :s, Matrix(srate), dims=[:time, :country])
	add_shared_param!(m, :policy_scenario, 1) # identifies the club of countries participating in the policy
	add_shared_param!(m, :club_countries_binary, club_countries_binary, dims=[:scenario, :country]) # identifies the club of countries participating in the policy

	# --------------------------------
	# FAIR Initial (2020) Conditions
	# --------------------------------

	update_param!(m, :aerosol_plus_cycles, :aerosol_plus_0, init_aerosol[!,:concentration])
	update_param!(m, :aerosol_plus_cycles, :R0_aerosol_plus, Matrix(init_aerosol[!, [:R1, :R2, :R3, :R4]]))
	update_param!(m, :aerosol_plus_cycles, :GU_aerosol_plus_0, init_aerosol[!,:GU])

	update_param!(m, :ch4_cycle, :ch4_0, init_ch4[1,:concentration])
	update_param!(m, :ch4_cycle, :R0_ch4, vec(Matrix(init_ch4[!, [:R1, :R2, :R3, :R4]])))
	update_param!(m, :ch4_cycle, :GU_ch4_0, init_ch4[1,:GU])

	update_param!(m, :co2_cycle, :co2_0, init_co2[1,:concentration])
	update_param!(m, :co2_cycle, :R0_co2, vec(Matrix(init_co2[!, [:R1, :R2, :R3, :R4]])))
	update_param!(m, :co2_cycle, :GU_co2_0, init_co2[1,:GU])

	update_param!(m, :flourinated_cycles, :flourinated_0, init_flourinated[!,:concentration])
	update_param!(m, :flourinated_cycles, :R0_flourinated, Matrix(init_flourinated[!, [:R1, :R2, :R3, :R4]]))
	update_param!(m, :flourinated_cycles, :GU_flourinated_0, init_flourinated[!,:GU])

	update_param!(m, :montreal_cycles, :montreal_0, init_montreal[!,:concentration])
	update_param!(m, :montreal_cycles, :R0_montreal, Matrix(init_montreal[!, [:R1, :R2, :R3, :R4]]))
	update_param!(m, :montreal_cycles, :GU_montreal_0, init_montreal[!,:GU])

	update_param!(m, :n2o_cycle, :n2o_0, init_n2o[1,:concentration])
	update_param!(m, :n2o_cycle, :R0_n2o, vec(Matrix(init_n2o[!, [:R1, :R2, :R3, :R4]])))
	update_param!(m, :n2o_cycle, :GU_n2o_0, init_n2o[1,:GU])

	update_param!(m, :temperature, :Tj_0, init_tj[!,:Tj])
	update_param!(m, :temperature, :T_0, init_temperature[1,:Temperature])


	# --------------------------------
	# Gross Economy
	# --------------------------------

	connect_param!(m, :grosseconomy, :l, :l)
	update_param!(m, :grosseconomy, :tfp, Matrix(productivity))
	update_param!(m, :grosseconomy, :depk, Matrix(depreciation))
	update_param!(m, :grosseconomy, :k0, k0)
	update_param!(m, :grosseconomy, :share, 0.3)

	# --------------------------------
	# Abatement
	# --------------------------------
	
	update_param!(m, :abatement, :control_regime, 3)  #  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
	update_param!(m, :abatement, :global_carbon_tax, zeros(length(dim_keys(m, :time))))
	update_param!(m, :abatement, :reference_carbon_tax, zeros(length(dim_keys(m, :time))))
	update_param!(m, :abatement, :reference_country_index, findfirst(x -> x == "USA", countries))
	update_param!(m, :abatement, :μ_input, zeros(length(dim_keys(m, :time)), length(dim_keys(m, :country))))
	update_param!(m, :abatement, :θ2, 2.6)
	update_param!(m, :abatement, :pbacktime, full_pbacktime)

	connect_param!(m, :abatement, :s, :s)
	connect_param!(m, :abatement, :l, :l)
	connect_param!(m, :abatement, :η, :η)
	connect_param!(m, :abatement, :σ, :σ)
	connect_param!(m, :abatement, :policy_scenario, :policy_scenario)
	connect_param!(m, :abatement, :club_countries_binary, :club_countries_binary)


	# --------------------------------
	# CO2 Emissions
	# --------------------------------

	connect_param!(m, :emissions, :mapcrwpp,  :mapcrwpp) 
	connect_param!(m, :emissions, :σ, :σ)

	# --------------------------------
	# Temperature Pattern Scaling
	# --------------------------------

	update_param!(m, :pattern_scale, :β_temp, cmip_pattern)

	# --------------------------------
	# Damages
	# --------------------------------

	update_param!(m, :damages, :β1, 0.00236)
	update_param!(m, :damages, :β2, 2.0)
	update_param!(m, :damages, :β1_KW, beta1_KW)
	update_param!(m, :damages, :β2_KW, beta2_KW)

	# --------------------------------
	# Net Economy
	# --------------------------------

	connect_param!(m, :neteconomy, :s, :s)
	connect_param!(m, :neteconomy, :l, :l)
	connect_param!(m, :neteconomy, :mapcrwpp,  :mapcrwpp) 
	# --------------------------------
	# Revenue Recycle
	# --------------------------------

	update_param!(m, :revenue_recycle, :switch_scope_recycle, 	0)
	update_param!(m, :revenue_recycle, :switch_global_pc_recycle, 	0)
	update_param!(m, :revenue_recycle, :global_recycle_share, 	zeros(nb_country))
	update_param!(m, :revenue_recycle, :lost_revenue_share, 0.0)

	connect_param!(m, :revenue_recycle, :l, :l)
	connect_param!(m, :revenue_recycle, :switch_recycle, :switch_recycle)
	connect_param!(m, :revenue_recycle, :policy_scenario, :policy_scenario)
	connect_param!(m, :revenue_recycle, :club_countries_binary, :club_countries_binary)

	# --------------------------------
	# Quantile distribution
	# --------------------------------

	update_param!(m, :quantile_recycle, :min_study_gdp, 		meta_min_study_gdp) #minimum(elasticity_studies.pcGDP)
    update_param!(m, :quantile_recycle, :max_study_gdp, 		meta_max_study_gdp)  #maximum(elasticity_studies.pcGDP)
    update_param!(m, :quantile_recycle, :elasticity_intercept, 	meta_intercept)
    update_param!(m, :quantile_recycle, :elasticity_slope, 		meta_slope)
	update_param!(m, :quantile_recycle, :damage_elasticity, 		0.6) #Gilli et al. (2024), estimate based on SSP2 projection
	update_param!(m, :quantile_recycle, :quantile_consumption_shares,  consumption_distribution_2020_2300)
	#update_param!(m, :quantile_recycle, :quantile_consumption_shares, 	consumption_distribution) Static version
	update_param!(m, :quantile_recycle, :recycle_share, 			ones(nb_country, nb_quantile).*1/nb_quantile)

	connect_param!(m, :quantile_recycle, :switch_recycle, :switch_recycle)
	connect_param!(m, :quantile_recycle, :l, 			:l)
	connect_param!(m, :quantile_recycle, :mapcrwpp,  :mapcrwpp) 
	connect_param!(m, :quantile_recycle, :nb_quantile, 	:nb_quantile)

	# --------------------------------
	# Welfare
	# --------------------------------

	connect_param!(m, :welfare, :η, :η)
	connect_param!(m, :welfare, :nb_quantile, :nb_quantile)
	connect_param!(m, :welfare, :l, :l)
	connect_param!(m, :welfare, :mapcrwpp,  :mapcrwpp) 

	# --------------------------------
	# Create Component Connections
	# --------------------------------

	# Syntax is connect_param!(model_name, :component_requiring_value => :name_of_required_value, :component_calculating_value => :name_of_calculated_value)
	connect_param!(m, :grosseconomy    	=> :I, 					:neteconomy 		=> :I )
	connect_param!(m, :abatement 	   	=> :YGROSS, 			:grosseconomy 		=> :YGROSS)
	connect_param!(m, :emissions 	   	=> :YGROSS, 			:grosseconomy 		=> :YGROSS)
	connect_param!(m, :emissions 	   	=> :μ, 					:abatement 			=> :μ)
	connect_param!(m, :co2_cycle 	  	=> :E_co2, 				:emissions 			=> :E_Global_gtc)
	connect_param!(m, :pattern_scale   	=> :global_temperature,	:temperature 		=> :T)
	connect_param!(m, :damages 		   	=> :temp_anomaly, 		:temperature 		=> :T)
	connect_param!(m, :damages 	 	   	=> :local_temp_anomaly, :pattern_scale 		=> :local_temperature)
	connect_param!(m, :neteconomy 	   	=> :ABATEFRAC, 			:abatement 			=> :ABATEFRAC)
	connect_param!(m, :neteconomy 	  	=> :LOCAL_DAMFRAC_KW, 	:damages 			=> :LOCAL_DAMFRAC_KW)
	connect_param!(m, :neteconomy 	   	=> :YGROSS, 			:grosseconomy 		=> :YGROSS )
    connect_param!(m, :revenue_recycle 	=> :E_gtco2, 			:emissions			=> :E_gtco2)
	connect_param!(m, :revenue_recycle 	=> :LOCAL_DAMFRAC_KW,	:damages 			=> :LOCAL_DAMFRAC_KW)
	connect_param!(m, :revenue_recycle 	=> :country_carbon_tax,	:abatement 			=> :country_carbon_tax)
	connect_param!(m, :revenue_recycle  => :Y, 					:neteconomy 		=> :Y)
	connect_param!(m, :quantile_recycle => :Y,					:neteconomy 		=> :Y)
	connect_param!(m, :quantile_recycle => :ABATEFRAC,			:abatement 			=> :ABATEFRAC)
	connect_param!(m, :quantile_recycle => :LOCAL_DAMFRAC_KW,	:damages 			=> :LOCAL_DAMFRAC_KW)
	connect_param!(m, :quantile_recycle => :CPC, 				:neteconomy 		=> :CPC)
	connect_param!(m, :quantile_recycle => :Y,					:neteconomy 		=> :Y)
	connect_param!(m, :quantile_recycle => :Y_pc,				:neteconomy 		=> :Y_pc)
	connect_param!(m, :quantile_recycle => :country_pc_dividend,:revenue_recycle	=> :country_pc_dividend)
	connect_param!(m, :quantile_recycle => :tax_pc_revenue,		:revenue_recycle	=> :tax_pc_revenue)
	connect_param!(m, :welfare 			=> :qcpc_post_recycle, 	:quantile_recycle	=> :qcpc_post_recycle)

	# Return model.
	return m
end
end #module
