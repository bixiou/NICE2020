@defcomp revenue_recycle begin

    country          = Index()
    scenario         = Index()

    country_carbon_tax       	= Parameter(index=[time, country])         		# CO2 tax rate (USD2017 per tCO2)
    LOCAL_DAMFRAC_KW            = Parameter(index=[time, country])              #  Country-level damages based on local temperatures and on Kalkuhl & Wenz (share of net output)
    E_gtco2                 	= Parameter(index=[time, country])      		# Country level CO₂ emissions (GtCO2 per year)
    l           				= Parameter(index=[time, country])  			# Country population (thousands)
    lost_revenue_share      	= Parameter()                           		# Share of carbon tax revenue that is lost and cannot be recycled (1 = 100% of revenue lost)
    global_recycle_share    	= Parameter(index=[country])            		# Shares of country revenues that are recycled globally as international transfers (1 = 100%)
    switch_recycle              = Parameter()                                   # Switch, recycling of tax revenues
    switch_scope_recycle	   	= Parameter() 									# Switch, carbon tax revenues recycled at country (0) or  global (1) level
    switch_global_pc_recycle    = Parameter()                                   # Switch, carbon tax revenues recycled globally equal per capital (1)
    policy_scenario             = Parameter()                                    # Policy scenario for the country, used to determine which countries are in the club
    club_countries_binary       	= Parameter(index=[scenario, country])          # Countries in the club for each scenario (1) or not (0)

     # --- New International Transfer Parameters ---
    switch_custom_transfers   = Parameter()                          # 0=anciens calculs, 1=nouveaux transferts
    rights_proposed           = Parameter(index=[time, country])    # droits alloués (GtCO2 par pays/an)
    E_gtco2_scenario          = Parameter(index=[time])             # émissions du club (GtCO2/an)
    YGROSS                    = Parameter(index=[time, country])    # PIB par pays/an
    
    tax_revenue 				= Variable(index=[time, country]) 				# Country carbon tax revenue (thousand 2017USD per year)
    tax_pc_revenue              = Variable(index=[time, country]) 				# Carbon tax revenue per capita (thousand 2017USD per capita per year)
    total_tax_revenue           = Variable(index=[time]) 		         		# Total carbon tax revenue (thousand 2017USD per year), sum of tax revenue in all countries
    global_revenue 				= Variable(index=[time]) 						# Carbon tax revenue from globally recycled country revenues (thousand 2017USD per year)
    country_pc_dividend 	    = Variable(index=[time, country]) 				# Total per capita carbon tax dividends, including any international transfers (thousand 2017USD per year)
    country_pc_dividend_domestic_transfers = Variable(index=[time, country]) 	# Per capita carbon tax dividends from domestic redistribution (thousand 2017USD per year)
    country_pc_dividend_global_transfers = Variable(index=[time, country]) 		# Per capita carbon tax dividends from international transfers (thousand 2017USD per year)

    # --- New International Transfer Variables ---
    transfer                  = Variable(index=[time, country])
    transfer_over_gdp         = Variable(index=[time, country])    # % du PIB
    transfer_pc               = Variable(index=[time, country])    # $ par habitant

    function run_timestep(p, v, d, t)
        # — INIT 
        for c in d.country
            v.transfer[t,c]                    = 0.0
            v.transfer_over_gdp[t,c]           = 0.0
            v.transfer_pc[t,c]                 = 0.0
            
        end

        # — 
        if p.switch_custom_transfers == 1
            # new transfers calculation :
            for c in d.country
                Δ = p.rights_proposed[t,c] - p.E_gtco2[t,c]
                v.transfer[t,c]          = p.country_carbon_tax[t,c] * Δ
                v.transfer_over_gdp[t,c] = p.country_carbon_tax[t,c] * Δ / p.YGROSS[t,c]
                v.transfer_pc[t,c]       = p.country_carbon_tax[t,c] * Δ / p.l[t,c]
            end

            # all* old component variables are neutralized :
            for c in d.country
                v.tax_revenue[t,c]                         = 0.0
                v.tax_pc_revenue[t,c]                      = 0.0
                v.country_pc_dividend_domestic_transfers[t,c] = 0.0
                v.country_pc_dividend_global_transfers[t,c]   = 0.0
                v.country_pc_dividend[t,c]                   = 0.0
            end
            v.total_tax_revenue[t] = 0.0
            v.global_revenue[t]   = 0.0

            return   # exit: you do **not** want to run the “standard” suite
        end

        # — IFELSE (switch_custom_transfers == 0) — execute ALL existing calculations :

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
        
        if p.switch_recycle==1 && p.switch_scope_recycle==1 && !is_first(t) # if revenues recycled, and if recycled at global level
            
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

                if p.switch_scope_recycle==0 #Revenues recycled only at the country level on per capita basis

                    v.country_pc_dividend_domestic_transfers[t,c] = v.tax_revenue[t,c] / p.l[t,c] / 1e3
                    v.country_pc_dividend_global_transfers[t,c] = 0

                elseif p.switch_scope_recycle==1  #Revenues recycled at the global (and at country level depending on global revenue share)
                    
                    # revenues recycled globally with an exogenous share
                    # Recycle a share (1-global_recycle_share) of global revenue within country (in $1000 per capita)
                    v.country_pc_dividend_domestic_transfers[t,c] = (1-p.global_recycle_share[c]) * v.tax_revenue[t,c]  / p.l[t,c] / 1e3

                    # Distribute globally recycled revenues to countries according to scenario
                    ## Globally recycled revenues recycled on a per capita basis =======================
                    if p.switch_global_pc_recycle==1
                        # if country is in the club, it receives a share of global revenue
                        v.country_pc_dividend_global_transfers[t,c] = v.global_revenue[t]*p.club_countries_binary[p.policy_scenario,c] / ((p.l[t,:]' *p.club_countries_binary[p.policy_scenario,:])*1e3)
                    else 
                        v.country_pc_dividend_global_transfers[t,c] = 0
                        end

                end # test for scope of recycling (global/local)

            elseif p.switch_recycle==0 # revenue recycling is off
                v.country_pc_dividend_domestic_transfers[t,c] = 0
                v.country_pc_dividend_global_transfers[t,c] = 0

            end # test if revenue reycling on or off

            # Sum per capita dividends from domestic and global redistribution
            v.country_pc_dividend[t,c] = v.country_pc_dividend_domestic_transfers[t,c] + v.country_pc_dividend_global_transfers[t,c]

        end # country loop
    end # timestep
end
