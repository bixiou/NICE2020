# -----------------------------------------------------------
# Net Economy
# -----------------------------------------------------------


@defcomp neteconomy begin

    country     = Index()   # Index for the modeled countries.
    regionwpp  = Index() # Index for WPP regions

    # Parameters
    YGROSS           = Parameter(index=[time, country]) # Gross output (1e6 USD2017 per year)
    ABATEFRAC        = Parameter(index=[time, country]) # Abatement cost (share of gross output)
    LOCAL_DAMFRAC_KW = Parameter(index=[time, country]) # Country-level damages based on local temperatures and on Kalkuhl & Wenz (share of net output)
    s                = Parameter(index=[time, country]) # Savings rate
    l                = Parameter(index=[time, country]) # Labor - population (thousands)
    mapcrwpp        = Parameter(index=[country])        # Map from country index to WPP region index
    

    # Variables

    Y           = Variable(index=[time, country]) # Output net of damages and abatement costs (1e6 USD2017 per year)
    C           = Variable(index=[time, country]) # Country consumption (1e6 USD2017 per year)
    CPC         = Variable(index=[time, country]) # Country level consumption per capita (thousand USD2017 per person per year)
    Y_pc        = Variable(index=[time, country]) # Per capita output net of abatement and damages (2017 USD per person per year)

    I           = Variable(index=[time, country]) # Investment (1e6 USD2017 per year)
    # WPP region variables
    C_rwpp     = Variable(index=[time, regionwpp]) # Regional consumption (1e6 USD2017 per year)
    l_rwpp     = Variable(index=[time, regionwpp]) # Regional population (thousands)
    CPC_rwpp   = Variable(index=[time, regionwpp]) # Regional CPC (thousand 2017USD per person per year)
    Y_pc_rwpp  = Variable(index=[time, regionwpp]) # Regional per capita output net of abatement and damages (USD2017 per person per year).

    function run_timestep(p, v, d, t)

        v.C_rwpp[t,:] .= 0

        for c in d.country

            # Output net of abatement costs and damages
            v.Y[t,c] = (1.0 - p.ABATEFRAC[t,c]) ./ (1.0 + p.LOCAL_DAMFRAC_KW[t,c]) * p.YGROSS[t,c]  

            # Investment
            v.I[t,c] = p.s[t,c] * v.Y[t,c]

            # Country consumption (No investment in final period).
           if !is_last(t)
               v.C[t,c] = v.Y[t,c] - v.I[t,c]
           else
               v.C[t,c] = v.C[t-1, c]
           end

           # Country per capita consumption.
           v.CPC[t,c] = v.C[t,c] / p.l[t,c]

           # Country per capita net output
           # Note, Y in $million (1e6) and population in thousands (1e3), so scale by 1e3
           v.Y_pc[t,c] = v.Y[t,c] / p.l[t,c] * 1e3

       end # country loop

       for rwpp in d.regionwpp
           country_indices = findall(x->x==rwpp , p.mapcrwpp) #Country indices for the region
           v.C_rwpp[t, rwpp] = sum(v.C[t, country_indices])
           v.l_rwpp[t, rwpp] = sum(p.l[t, country_indices])
           v.CPC_rwpp[t,rwpp]= v.C_rwpp[t,rwpp] / v.l_rwpp[t,rwpp]
           v.Y_pc_rwpp[t,rwpp] = sum(v.Y[t, country_indices]) / sum(p.l[t, country_indices]) * 1e3
       end # region loop

   end # run_timestep


end
