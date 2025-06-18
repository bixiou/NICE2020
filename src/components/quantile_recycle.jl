@defcomp quantile_recycle begin

    country          = Index()
    regionwpp       = Index()
	quantile		 = Index()

    ABATEFRAC               	= Parameter(index=[time, country])      		# Cost of CO2 emission reductions (share of gross economic output)
    LOCAL_DAMFRAC_KW            = Parameter(index=[time, country])              # Country-level damages based on local temperatures and on Kalkuhl & Wenz (share of net output)
    CPC                     	= Parameter(index=[time, country])      		# Country level consumption per capita (thousand USD2017 per person per year)
	l           				= Parameter(index=[time, country])  			# Country population (thousands)
    s           				= Parameter(index=[time, country])  			# Savings rate
	mapcrwpp   				    = Parameter(index=[country])        			# Map from country index to wpp region index
	Y           				= Parameter(index=[time, country]) 				# Output net of damages and abatement costs (1e6 USD2017 per year)
    Y_pc                        = Parameter(index=[time, country]) 	            # Per capita output net of abatement and damages (USD2017 per person per year)
    YGROSS                      = Parameter(index=[time, country])              # Gross output (1e6 USD2017 per year)
    country_pc_dividend 	    = Parameter(index=[time, country]) 				# Total per capita carbon tax revenue, including any international transfers (thousand USD2017 per person per year)
    tax_pc_revenue              = Parameter(index=[time, country])              # Carbon tax revenue per capita from country emissions  (thousand USD2017 per person per year)
    switch_recycle              = Parameter()                                   # Switch recycling of tax revenues
    nb_quantile                	= Parameter(default=10)							# Number of quantiles
	min_study_gdp               = Parameter()                          			# Minimum observed per capita GDP value found in elasticity studies (USD2017 per person)
    max_study_gdp            	= Parameter()                          			# Maximum observed per capita GDP value found in elasticity studies (USD2017 per person)
    elasticity_intercept     	= Parameter()                          			# Intercept term to estimate time-varying income elasticity
    elasticity_slope         	= Parameter()                          			# Slope term to estimate time-varying income elasticity
    damage_elasticity        	= Parameter()                          			# Income elasticity of climate damages (1 = proportional to income)
	quantile_consumption_shares = Parameter(index=[time, country, quantile]) 	# Income shares of deciles
    recycle_share           	= Parameter(index=[country, quantile]) 		    # Share of carbon tax revenue recycled back to each quantile
    switch_transfers_affect_growth       = Parameter()                                   # Switch, to choose whether the redistribution macro effects are added
    # --- New International Transfer Parameters ---
    switch_custom_transfers     = Parameter()                          # use equal_pc_transfer (dividend) (=0) or the custom_transfer (using rights proposed) (=1)
    transfer                    = Parameter(index=[time, country])
    transfer_over_gdp           = Parameter(index=[time, country])    # % of GDP
    transfer_pc                 = Parameter(index=[time, country])    # $ per capita
    rights_proposed           = Parameter(index=[time, country])    # droits alloués (GtCO2 par pays/an)
    E_gtco2                 	= Parameter(index=[time, country])
    country_carbon_tax       	= Parameter(index=[time, country])         		# CO2 tax rate (USD2017 per tCO2)


    CO2_income_elasticity    	= Variable(index=[time, country])             	# Elasticity of CO2 price exposure with respect to income
    abatement_cost_dist		 	= Variable(index=[time, country, quantile]) 	# Quantile distribution shares of mitigation costs
    tax_burden_distr		     	= Variable(index=[time, country, quantile]) 	# Quantile distribution shares of CO2 tax burden
    damage_dist					= Variable(index=[time, country, quantile]) 	# Quantile distribution shares of climate damages

	conso_pc_base                  	= Variable(index=[time, country, quantile])  	# Pre-damage, pre-abatement cost, pre-tax quantile consumption per capita (thousand USD2017 per person per year)
    conso_pc_post_damage_abatement 	= Variable(index=[time, country, quantile])  	# Post-damage, post-abatement cost per capita quantile consumption (thousand USD2017 per person per year)
    conso_pc_post_tax              	= Variable(index=[time, country, quantile])  	# Quantile per capita consumption after subtracting out carbon tax (thousand USD2017 per person per year)
    conso_pc_post_recycle          	= Variable(index=[time, country, quantile])  	# Quantile per capita consumption after recycling tax back to quantiles (thousand USD2017 per person per year)
    qc_share				 	= Variable(index=[time, country, quantile])  	# Quantile share of per capita consumption (%)
	sum_conso_pc_post_recycle  	 	= Variable(index=[time, country]) 				# Sum of quantiles consumption per capita after abatement, damages and revenue recycling (thousand USD2017 per (capita per quantile) per year)
    CPC_post                    = Variable(index=[time, country])               # Country consumption per capita post recycle (thousand USD2017 per person per year)
    CPC_post_rwpp               = Variable(index=[time, regionwpp])             # Region consumption per capita post recycle (thousand USD2017 per person per year)
    CPC_post_global             = Variable(index=[time])                        # Global consumption per capita post recycle (thousand USD2017 per person per year)

	gini_cons          		 	= Variable(index=[time, country])				# Country consumption gini (%)
    gini_cons_rwpp              = Variable(index=[time, regionwpp])             # Region consumption gini (%)
    gini_cons_global            = Variable(index=[time])                        # Global consumption gini (%)
	mu_cons				        = Variable(index=[time, country])				# Parameter mu of the lognormal distribution of consumption
	sigma_cons			        = Variable(index=[time, country])				# Parameter sigma of the lognormal distribution of consumption


    function run_timestep(p, v, d, t)

        # temporary variable for global population in t 
        temp_pop_global = sum(p.l[t,:])

        #temporary variable for quantile population in t (country x quantile)
        temp_pop_quantile = repeat( (p.l[t,:] / p.nb_quantile), 1, p.nb_quantile) # all quantiles within a country have same pop, by definition

        for c in d.country

            ########################################################################################################
            ## Calculate time-varying income elasticity of CO2 price exposure (requires  Y_pc units in $/person).
            # Note, hold elasticity constant at boundary value if GDP falls outside the study support range.

            if p.Y_pc[t,c] < p.min_study_gdp
               # GDP below observed study values.
               v.CO2_income_elasticity[t,c] = p.elasticity_intercept + p.elasticity_slope * log(p.min_study_gdp)
           elseif p.Y_pc[t,c] > p.max_study_gdp
               # GDP above observed study values.
               v.CO2_income_elasticity[t,c] = p.elasticity_intercept + p.elasticity_slope * log(p.max_study_gdp)
           else
               #GDP within observed study values.
               v.CO2_income_elasticity[t,c] = p.elasticity_intercept + p.elasticity_slope * log(p.Y_pc[t,c])
           end

            #################################################
            ## Compute consumption distributions for quantiles
            #################################################

            # Calculate quantile distribution shares of CO2 tax burden and mitigation costs (assume both distributions are equal) and climate damages.
			v.abatement_cost_dist[t,c,:] = country_quantile_distribution(v.CO2_income_elasticity[t,c], p.quantile_consumption_shares[t,c,:], p.nb_quantile)
			v.tax_burden_distr[t,c,:]     = country_quantile_distribution(v.CO2_income_elasticity[t,c], p.quantile_consumption_shares[t,c,:], p.nb_quantile)
			v.damage_dist[t,c,:]         = country_quantile_distribution(p.damage_elasticity, p.quantile_consumption_shares[t,c,:], p.nb_quantile)

           
            # Create a temporary variable used to calculate NICE baseline quantile consumption (just for convenience). It is equivalent to some gross consumption
			# This is the former of l.96 : we try and keep the same meaning once transfers can be added in macro variables
            # temp_conso_pc = p.nb_quantile * p.CPC[t,c] * (1.0 + p.LOCAL_DAMFRAC_KW[t,c]) / (1.0 - p.ABATEFRAC[t,c])
            temp_conso_pc = p.nb_quantile * p.YGROSS[t,c] * (1.0 - p.s[t,c])/ p.l[t,c]
			for q in d.quantile

				# Calculate pre-damage, pre-abatement cost quantile consumption.
				v.conso_pc_base[t,c,q] = temp_conso_pc * p.quantile_consumption_shares[t,c,q]

				# Calculate post-damage, post-abatement cost per capita quantile consumption (bounded below to ensure consumptions don't collapse to zero or go negative).
				# Note, this differs from standard NICE equation because quantile CO2 abatement cost and climate damage shares can now vary over time.
				#This is the former version of l.104 : we try and keep the same meaning once transfers can be added in macro variablesAdd commentMore actions
				#v.conso_pc_post_damage_abatement[t,c,q] = max(v.qcpc_base[t,c,q] - (p.nb_quantile* p.CPC[t,c] * p.LOCAL_DAMFRAC_KW[t,c] * v.damage_dist[t,c,q]) - (temp_qcpc * p.ABATEFRAC[t,c] * v.abatement_cost_dist[t,c,q]), 1e-8)
                v.conso_pc_post_damage_abatement[t,c,q] = max(v.conso_pc_base[t,c,q] - (temp_conso_pc* (1.0 - p.ABATEFRAC[t,c]) /(1.0 + p.LOCAL_DAMFRAC_KW[t,c]) * p.LOCAL_DAMFRAC_KW[t,c] * v.damage_dist[t,c,q]) - (temp_conso_pc * p.ABATEFRAC[t,c] * v.abatement_cost_dist[t,c,q]), 1e-8)

				# Subtract tax revenue from each quantile based on quantile CO2 tax burden distributions.
				# Note, per capita tax revenue and consumption should both be in $1000/person.

                v.conso_pc_post_tax[t,c,q] =  v.conso_pc_post_damage_abatement[t,c,q] - (p.nb_quantile * p.tax_pc_revenue[t,c] * v.tax_burden_distr[t,c,q])* ( 1 - p.s[t,c] * p.switch_transfers_affect_growth)				
                                                
                # Recycle tax revenue by adding shares back to all quantiles
				if p.switch_recycle==0 # In the NO revenue recycling case (distributionally neutral), refund tax revenues equal to the initial burden

					v.conso_pc_post_recycle[t,c,q] = v.conso_pc_post_tax[t,c,q]       + (p.nb_quantile * p.tax_pc_revenue[t,c] * v.tax_burden_distr[t,c,q])* ( 1 - p.s[t,c] * p.switch_transfers_affect_growth)

				elseif p.switch_recycle==1 # In the revenue recycling case, distribute divididends from country and/or global tax revenue (assume recycling shares constant over time)

					v.conso_pc_post_recycle[t,c,q] = v.conso_pc_post_tax[t,c,q] + ( p.country_pc_dividend[t,c] * p.nb_quantile * p.recycle_share[c,q]) * ( 1 - p.s[t,c] * p.switch_transfers_affect_growth )
				end

			end # quantile

			#calculate sum of quantile CPC consumption (to calculate consumption shares at later stage)
			v.sum_conso_pc_post_recycle[t,c] = sum(v.conso_pc_post_recycle[t,c,:])

            # CPC = (sum  quantile_pop* quantile_cpc) / population = (population/nb_quantile * (sum quantile_cpc)) / population = (sum quantile_cpc) / nb_quantile
            v.CPC_post[t,c] = v.sum_conso_pc_post_recycle[t,c] / p.nb_quantile

            v.qc_share[t,c,:] .= v.conso_pc_post_recycle[t,c,:]./ v.sum_conso_pc_post_recycle[t,c]*100

            #################
            ## Compute Gini 
            ################

			#gini consumption
            v.gini_cons[t,c] =  gini(convert(Vector{Real}, v.qc_share[t,c,:])) *100

            if !(v.gini_cons[t,c]  > 0 && v.gini_cons[t,c] <100)
                v.gini_cons[t,c] = missing
            end
        end # country loop

        for rwpp in d.regionwpp
            country_indices = findall(x->x==rwpp , p.mapcrwpp) #Country indices for the region

            v.CPC_post_rwpp[t,rwpp] = sum(p.l[t,country_indices] .* v.CPC_post[t, country_indices]) / sum(p.l[t,country_indices])
            v.gini_cons_rwpp[t,rwpp] = gini(convert(Vector{Real},vec(v.conso_pc_post_recycle[t,country_indices,:])), convert(Vector{Real},vec(temp_pop_quantile[country_indices,:])))*100

        end # region loop

        v.CPC_post_global[t] = sum(v.sum_conso_pc_post_recycle[t,:] .* p.l[t,:] ./ p.nb_quantile) / temp_pop_global
        v.gini_cons_global[t] = gini(convert(Vector{Real},vec(v.conso_pc_post_recycle[t,:,:])), convert(Vector{Real},vec(temp_pop_quantile))) *100

    end # timestep
end
