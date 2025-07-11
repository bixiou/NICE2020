# -----------------------------------------------------------
# Abatement
# -----------------------------------------------------------

@defcomp abatement begin
    country          = Index()
    scenario         = Index()

    σ       = Parameter(index=[time, country])  # Emissions output ratio (GtCO2 per million USD2017)
    emissionsrate_footprint       = Parameter(index=[time, country])  # Emissions output ratio (GtCO2 per million USD2017)
    switch_footprint = Parameter()                       # Switch for emissions footprint (1) or not (0)
    YGROSS  = Parameter(index=[time, country])  # Gross output (1e6 USD2017 per year)
    s       = Parameter(index=[time, country])  # Savings rate
    l       = Parameter(index=[time, country])  #Labor - population (thousands)
    η       = Parameter()                       # Inequality aversion

    θ2                      = Parameter()                       # Exponent of abatement cost function (DICE-2023 value)
    pbacktime               = Parameter(index=[time])           # Backstop price from DICE 2023 (2017USD per tCO2)
    global_carbon_tax       = Parameter(index=[time])           # CO2 tax rate (2017 USD per tCO2)
    reference_carbon_tax    = Parameter(index=[time])           # CO2 tax rate (2017 USD per tCO2)
    reference_country_index = Parameter()                       # Index of the reference country for the differentiated carbon tax case
    control_regime          = Parameter()                       # Switch for emissions control regime  1:"global_carbon_tax", 2:"country_carbon_tax", 3:"country_abatement_rate"
    μ_input                 = Parameter(index=[time, country])  # Input mitigation rate, used with option 3 "country_abatement_rate"
    policy_scenario         = Parameter()                       # Policy scenario for the country, used to determine which countries are in the club
    club_country            = Parameter(index=[scenario, country])  # Countries in the club for each scenario (1) or not (0)
    direct_country_tax      = Parameter(index=[time, country])
    rights_mat         = Parameter(index=[time, country])    # allocated rights (GtCO2)

    θ1                 = Variable(index=[time, country])    # Multiplicative parameter of abatement cost function. Equal to ABATEFRAC at 100% mitigation
    country_carbon_tax = Variable(index=[time, country]) 	# CO2 tax rate (2017 USD per tCO2)
    μ 		           = Variable(index=[time, country]) 	# Emissions control rate
    ABATEFRAC          = Variable(index=[time, country])    # Cost of emissions reductions (share of annual gross output)
    ABATECOST 	       = Variable(index=[time, country]) 	# Cost of emissions reductions  (million 2017 USD per year)

    GLOBAL_ABATEFRAC_full_abatement      = Variable(index=[time]) # Global cost of emissions reductions (share of global annual gross ouput) for 100% mitigation

    function run_timestep(p, v, d, t)
        if (p.switch_footprint==1)
            p.σ[t,:] = p.emissionsrate_footprint[t,:]
        end

        # Define an equation for E
        for c in d.country
            # sigma is in GtCO2 per million 2017USD = 1e9 tCO2 / 1e6 2017 USD = 1e3 tCO2 per 2017USD
            v.θ1[t,c] = p.pbacktime[t] * (p.σ[t,c] * 1e3) / p.θ2

            if (p.control_regime==1)  # global_carbon_tax

                # Set country carbon tax equal to global uniform carbon tax, bounded by the global backstop price, only for countries participating
                v.country_carbon_tax[t,c] = min(p.pbacktime[t], p.global_carbon_tax[t])*p.club_country[p.policy_scenario,c]
                # Find abatement rate from inversion of the expression (tax = marginal abatement cost), bound between 0 and 1
                #v.μ[t,c] = min( max((v.country_carbon_tax[t,c] / p.pbacktime[t,region_index] ) ^ (1 / (p.θ2 - 1.0)), 0.0), 1.0)
                v.μ[t,c] = min( max((v.country_carbon_tax[t,c] / p.pbacktime[t] ) ^ (1 / (p.θ2 - 1.0)), 0.0), 1.0)

                    
            elseif (p.control_regime==2) #country_carbon_tax

                # Expression to compute carbon tax of every country from carbon tax of reference country
                # Follows a simple rule for optimally differentiated taxes (see SI in Young-Brun et al. (in prep.))
                # tax_it = (1-savings_it)/(1-savings_ref,t) * u'(c_ref,t) / u'(c_it) * tax_ref,t
                # Carbon tax is bounded above by the global backstop 
                    v.country_carbon_tax[t,c] = min(p.pbacktime[t], p.reference_carbon_tax[t] *
                                            ((1 - p.s[t,p.reference_country_index])/ (1 - p.s[t,c]) ) *
                                            (p.YGROSS[t,c]/p.YGROSS[t,p.reference_country_index] *
                                            p.l[t,p.reference_country_index]/p.l[t,c] )^p.η )*p.club_country[p.policy_scenario,c]

                # Find abatement rate from inversion of the expression (tax = marginal abatement cost), bound between 0 and 1
                #v.μ[t,c] = min( max((v.country_carbon_tax[t,c] / p.pbacktime[t,region_index] ) ^ (1 / (p.θ2 - 1.0)), 0.0), 1.0)
                v.μ[t,c] = min( max((v.country_carbon_tax[t,c] / p.pbacktime[t] ) ^ (1 / (p.θ2 - 1.0)), 0.0), 1.0)


            elseif (p.control_regime==3) #country_abatement_rate
                v.μ[t,c] = p.μ_input[t,c]
                v.country_carbon_tax[t,c] =  p.pbacktime[t] * v.μ[t,c]^(p.θ2 - 1.0)
            
            elseif (p.control_regime==4) # manually added tax for each country x year
                v.country_carbon_tax[t,c] = p.direct_country_tax[t,c] 
                v.μ[t,c] = min(max(
                  (v.country_carbon_tax[t,c] / p.pbacktime[t])^(1/(p.θ2-1)),
                  0.0), 1.0)    

            elseif (p.control_regime==5) # manually added emissions for each country x year
                v.μ[t,c] = min(max(
                  1 - p.rights_mat[t,c] / (p.σ[t,c] * p.YGROSS[t,c]),
                  0.0), 1.0) * p.club_country[p.policy_scenario,c] * (maximum(p.rights_mat[t,:]) > -1)
                v.country_carbon_tax[t,c] =  p.pbacktime[t] * v.μ[t,c]^(p.θ2 - 1.0)
            end
        end

		for c in d.country
			v.ABATEFRAC[t,c] = v.θ1[t,c] * (v.μ[t,c]^p.θ2)
			v.ABATECOST[t,c] = p.YGROSS[t,c] * v.ABATEFRAC[t,c]
		end

        v.GLOBAL_ABATEFRAC_full_abatement[t] = sum(v.θ1[t,:] .* p.YGROSS[t,:]) / sum(p.YGROSS[t,:])

    end

end
