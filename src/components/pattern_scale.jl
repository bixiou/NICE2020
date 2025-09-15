# -----------------------------------------------------------
# Country-level pattern scaled temperatures.
# -----------------------------------------------------------

@defcomp pattern_scale begin

    country           = Index() # Index for the modeled countries

    β_temp             = Parameter(index=[country]) # Pattern scale coefficients that translate global => country-level temperature
    global_temperature = Parameter(index=[time]) # Global average temperature anomaly (°C)

	local_temperature  = Variable(index=[time, country]) # Country-level temperaure anomaly (°C)


    function run_timestep(p, v, d, t)

        # Loop through countries.
        for c in d.country

            # Calculate country-level temperatures.
            v.local_temperature[t,c] = p.β_temp[c] * p.global_temperature[t]
        end
    end
end
