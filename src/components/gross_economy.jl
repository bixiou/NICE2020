# -----------------------------------------------------------
# Gross Economy
# -----------------------------------------------------------


@defcomp grosseconomy begin
    country = Index()                           # Index for countries

    I       = Parameter(index=[time, country]) 	# Investment (1e6 USD2017 per year)
    l       = Parameter(index=[time, country])  # Labor - population (thousands)
    tfp     = Parameter(index=[time, country])  # Total factor productivity
    depk    = Parameter(index=[time, country])  # Depreciation rate on capital
    k0      = Parameter(index=[country])        # Initial level of capital (1e6 USD2017)
    share   = Parameter()                       # Capital share

    YGROSS  = Variable(index=[time, country])   # Gross output (1e6 USD2017 per year)
    YGROSS_global = Variable(index=[time])	    # Global gross output (1e12 USD2017 per year)
    K       = Variable(index=[time, country])   # Capital (1e6 USD2017)

    function run_timestep(p, v, d, t)

        # Define an equation for K
        for c in d.country
            if is_first(t)
                v.K[t,c] = p.k0[c]
            else
                v.K[t,c] = (1 - p.depk[t,c]) * v.K[t-1,c] + p.I[t-1,c] 
            end
        end

        # Define an equation for YGROSS
        for c in d.country
            v.YGROSS[t,c] = p.tfp[t,c] * v.K[t,c]^p.share * p.l[t,c]^(1-p.share)
        end

	# Define an equation for global YGROSS
	# Note: YGROSS in million (1e6) 2017 USD -> divide by 1e6 to get trillion (1e12) dollars
	v.YGROSS_global[t] = sum(v.YGROSS[t,:]) / 1e6

    end
end
