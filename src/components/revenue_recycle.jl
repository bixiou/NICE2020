@defcomp revenue_recycle begin

 # TODOs:
 # - automatically urun calibrate_global_tax_club_emissions when E_gtco2 doesn't coincide with sum of rights_proposed
 # - rename industrial_co2_emissions -> co2_emissions (as NICE2020 covers all CO2 emissions - but not other GHGs)
 # - rename E_gtco2_scenario -> E_gtco2_club 
 # - rename club_countries_binary -> club_country 
 # - make units more consistent (not thousands for one and millions for another), e.g. everything in tCO2, $, etc. Or at least specify the unit where each variable is defined, and in the documentation. For example, why is country_pc_dividend in *thousands* $?
 # - rename redistribution_switch -> switch_transfers_affect_growth
 # - rename switch_scope_recycle -> switch_global_recycling
 # - rename carbon_tax_dist -> tax_burden_distr or emission_share_quantile
 # - rename qcpc_... -> conso_pc_... 
 # - rename Δ -> excess_rights
 # - rename folder names old/new_transfer into equal_pc_transfer / [beginning of name of .csv used to define rights, e.g. "ffu"]_custom_transfers
 # - specify unit in transfer (net_economy.jl)
 # - be more explicit instead of "0=old calculations, 1=new transfers"
 # * replace net_economy.jl:52 by the following (parentheses changed)?  v.Y[t,c] = (1.0 - p.ABATEFRAC[t,c]) ./ (1.0 + p.LOCAL_DAMFRAC_KW[t,c]) * p.YGROSS[t,c] + p.switch_transfers_affect_growth*(p.switch_custom_transfers * p.transfer[t,c] / 1e6 + (1.0 - p.switch_custom_transfers) * (p.country_pc_dividend[t,c] * p.l[t,c] - p.tax_revenue[t,c] / 1e3) )* p.switch_recycle* p.switch_global_recycling 
 # * Aren't cases missing in revenue_recycle.jl:100?

 # Questions:
 # - Why transfer is defined only when switch_custom_transfers==1? Why not defining net_economy.jl:52 as v.Y[t,c] = (1.0 - p.ABATEFRAC[t,c]) ./ (1.0 + p.LOCAL_DAMFRAC_KW[t,c]) * p.YGROSS[t,c] + p.switch_transfers_affect_growth*p.transfer[t,c]* p.switch_recycle* p.switch_global_recycling, country_pc_dividend as rights_proposed*carbon_price/population, and transfer as the thing that was in parentheses in :52?
 # * is "1e6 / p.l[t,c] / 1e3" working as inteneded (quantile_recycle.jl)
 # - in quantile_recycle.jl:107, why tax_pc_revenue isn't valid for both cases of switch_custom_transfers? same question for country_pc_dividend :116
 # * I don't understand the end of quantile_recycle.jl:107, :116, shouldn't it be -s*feedback instead of -(1-s)*feedback? DONE
 # - Why isn't quantile_recycle.jl:112 simply v.qcpc_post_tax[t,c,q] =  v.qcpc_post_damage_abatement[t,c,q] ?
 # * Why lines 56-65 in revenue_recycle.jl?
 # - What are :Union and :Partnership?
 # - Comment ça marche le système des connect! dans nice2020_module?
 
    country          = Index()
    scenario         = Index()

    country_carbon_tax       	= Parameter(index=[time, country])         		# CO2 tax rate (USD2017 per tCO2)
    LOCAL_DAMFRAC_KW            = Parameter(index=[time, country])              #  Country-level damages based on local temperatures and on Kalkuhl & Wenz (share of net output)
    E_gtco2                 	= Parameter(index=[time, country])      		# Country level CO₂ emissions (GtCO2 per year)
    l           				= Parameter(index=[time, country])  			# Country population (thousands)
    lost_revenue_share      	= Parameter()                           		# Share of carbon tax revenue that is lost and cannot be recycled (1 = 100% of revenue lost)
    global_recycle_share    	= Parameter(index=[country])            		# Shares of country revenues that are recycled globally as international transfers (1 = 100%)
    switch_recycle              = Parameter()                                   # Switch, recycling of tax revenues
    switch_global_recycling	   	= Parameter() 									# Switch, carbon tax revenues recycled at country (0) or  global (1) level
    switch_global_pc_recycle    = Parameter()                                   # Switch, carbon tax revenues recycled globally equal per capital (1)
    policy_scenario             = Parameter()                                    # Policy scenario for the country, used to determine which countries are in the club
    club_country       	= Parameter(index=[scenario, country])          # Countries in the club for each scenario (1) or not (0)

     # --- New International Transfer (= custom transfers using rights proposed) Parameters ---
    switch_custom_transfers   = Parameter()                          # use equal_pc_transfer (dividend) (=0) or the custom_transfer (using rights proposed) (=1)
    rights_proposed           = Parameter(index=[time, country])    # droits alloués (GtCO2 par pays/an)
    E_gtco2_club          = Parameter(index=[time])             # émissions du club (GtCO2/an)
    YGROSS                    = Parameter(index=[time, country])    # PIB par pays/an
    
    tax_revenue 				= Variable(index=[time, country]) 				# Country carbon tax revenue (thousand 2017USD per year)
    tax_pc_revenue              = Variable(index=[time, country]) 				# Carbon tax revenue per capita (thousand 2017USD per capita per year)
    total_tax_revenue           = Variable(index=[time]) 		         		# Total carbon tax revenue (thousand 2017USD per year), sum of tax revenue in all countries
    global_revenue 				= Variable(index=[time]) 						# Carbon tax revenue from globally recycled country revenues (thousand 2017USD per year)
    country_pc_dividend 	    = Variable(index=[time, country]) 				# Total per capita carbon tax dividends, including any international transfers (thousand 2017USD per year)
    country_pc_dividend_domestic_transfers = Variable(index=[time, country]) 	# Per capita carbon tax dividends from domestic redistribution (thousand 2017USD per year)
    country_pc_dividend_global_transfers = Variable(index=[time, country]) 		# Per capita carbon tax dividends from international transfers (thousand 2017USD per year)

    # --- New International Transfer Variables ---
    transfer                  = Variable(index=[time, country])    # $ 
    transfer_over_gdp         = Variable(index=[time, country])    # % du PIB
    transfer_pc               = Variable(index=[time, country])    # $ per 1000 inhabitants

    function run_timestep(p, v, d, t)
            
        #######################################
        ## Compute country carbon tax revenues 
        #######################################

       for c in d.country

            # Calculate carbon tax revenue for each country (thousand 2017USD per year)
            # Note, emissions in GtCO2 and tax in 2017 $ per tCO2
            # Convert to tCo2 (Gt to t: *1e9) and to thousand dollars ($ to $1000: /1e3) -> *1e6
            v.tax_revenue[t,c] = (p.E_gtco2[t,c] * p.country_carbon_tax[t,c] * 1e6) * (1.0 - p.lost_revenue_share)

            # Carbon tax revenue per capita for each country (thousand 2017USD per capita per year)
            # population l in thousands, so divide by 1e3
            v.tax_pc_revenue[t,c] =  v.tax_revenue[t,c] / p.l[t,c] / 1e3

        end # country loop

        ##########################################################################
        ## Compute total tax revenue available and revenue recycled at global level 
        ##########################################################################

        # total of all countries carbon tax revenue (thousand 2017USD per year)
        v.total_tax_revenue[t] = sum(v.tax_revenue[t,:])

        # Calculate tax revenue from globally recycled revenue ($1000)
        
        if p.switch_recycle==1 && p.switch_global_recycling==1 && !is_first(t) # if revenues recycled, and if recycled at global level
            
            v.global_revenue[t] = sum(v.tax_revenue[t,:] .* p.global_recycle_share[:])

        else  # no revenues recycled at global level, or first period

            v.global_revenue[t] = 0 

        end

        # Compute endogenous global level revenue recycling share
        # if total_tax_revenue != 0, then the share is global_revenue/total_tax_revenue, else the share is set to 0
        temp_global_recycle_share_endogeneous = (v.total_tax_revenue[t]!=0 ? v.global_revenue[t]/v.total_tax_revenue[t] : 0.0) 

       ###########################################################
       ## Distribute total tax revenue to countries as per capita
       ## dividends, according to recycling scenario
       ###########################################################

        #Calculate total recycled per capita dividend for each country, from domestic and globally recycled revenue.
        # In 1000$ per capita: revenues already in 1000$ and population l in thousands, so for 1000$ per capita, divide by 1e3

        for c in d.country

            if p.switch_recycle==1 # revenue reycling is on

                if p.switch_global_recycling==0 #Revenues recycled only at the country level on per capita basis

                    v.country_pc_dividend_domestic_transfers[t,c] = v.tax_revenue[t,c] / p.l[t,c] / 1e3
                    v.country_pc_dividend_global_transfers[t,c] = 0

                elseif p.switch_global_recycling==1  #Revenues recycled at the global (and at country level depending on global revenue share)
                    
                    # revenues recycled globally with an exogenous share
                    # Recycle a share (1-global_recycle_share) of global revenue within country (in $1000 per capita)
                    v.country_pc_dividend_domestic_transfers[t,c] = (1-p.global_recycle_share[c]) * v.tax_revenue[t,c]  / p.l[t,c] / 1e3

                    # Distribute globally recycled revenues to countries according to scenario
                    ## Globally recycled revenues recycled on a per capita basis =======================
                    if p.switch_global_pc_recycle == 1
                        if p.switch_custom_transfers == 0
                            # Standard per capita transfer
                            v.country_pc_dividend_global_transfers[t,c] = v.global_revenue[t] * p.club_country[p.policy_scenario,c] / ((p.l[t,:]' * p.club_country[p.policy_scenario,:]) * 1e3)
                        elseif p.switch_custom_transfers == 1
                            # Custom transfer based on rights proposed and carbon tax
                            v.country_pc_dividend_global_transfers[t,c] = (p.rights_proposed[t,c] * p.country_carbon_tax[t,c] * 1e6) * p.club_country[p.policy_scenario,c] / (p.l[t,c] * 1e3)
                        end
                    else
                        v.country_pc_dividend_global_transfers[t,c] = 0
                    end

                end # test for scope of recycling (global/local)

            elseif p.switch_recycle==0 # revenue recycling is off
                v.country_pc_dividend_domestic_transfers[t,c] = 0
                v.country_pc_dividend_global_transfers[t,c] = 0

            end # revenue recycling switch

            # Sum per capita dividends from domestic and global redistribution
            v.country_pc_dividend[t,c] = v.country_pc_dividend_domestic_transfers[t,c] + v.country_pc_dividend_global_transfers[t,c]
   
            if p.switch_global_pc_recycle == 1 && p.switch_custom_transfers == 0
                if p.country_carbon_tax[t,c] == 0.
                    excess_rights = 0.
                else
                    excess_rights = 1e6 * p.l[t,c] * v.country_pc_dividend[t,c]/p.country_carbon_tax[t,c] - p.E_gtco2[t,c] * 1e9
                end
            else
                rights_actual = p.E_gtco2_club[t] * p.rights_proposed[t,c]/sum(p.rights_proposed[t,c] * p.club_country[p.policy_scenario,c] for c in d.country)
                excess_rights = (rights_actual - p.E_gtco2[t,c]) * 1e9 * (maximum(p.rights_proposed[t,:]) > 0)
            end
            v.transfer[t,c]          = p.country_carbon_tax[t,c] * excess_rights
            v.transfer_over_gdp[t,c] = v.transfer[t,c] / (p.YGROSS[t,c] * 1e6)
            v.transfer_pc[t,c]       = v.transfer[t,c] / p.l[t,c]
            
        end # country loop
    end # timestep
end
