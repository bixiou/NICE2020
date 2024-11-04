@defcomp welfare begin

    country         = Index()
    regionwpp       = Index()
	quantile        = Index()

    qcpc_post_recycle       = Parameter(index=[time, country, quantile])    # Quantile per capita consumption after recycling tax back to quantiles (thousand USD2017 per person per year)
    η                       = Parameter()                                   # Inequality aversion
    nb_quantile             = Parameter()                                   # Number of quantiles
    l                       = Parameter(index=[time, country])              # Population (thousands)
    mapcrwpp                = Parameter(index=[country])                    # Map from country index to wpp region index

    cons_EDE_country        = Variable(index=[time, country])               # Equally distributed welfare equivalent consumption (thousand USD2017 per person per year)
    cons_EDE_rwpp           = Variable(index=[time, regionwpp])             # Regional qually distributed welfare equivalent consumption (thousand USD2017 per person per year)
    cons_EDE_global         = Variable(index=[time])                        # Glibal equally distributed welfare equivalent consumption (thousand USD2017 per person per year)
    welfare_country         = Variable(index=[time, country])               # Country welfare
    welfare_rwpp            = Variable(index=[time, regionwpp])             # WPP region welfare
    welfare_global          = Variable(index=[time])                        # Global welfare

    function run_timestep(p, v, d, t)
        for c in d.country
            v.cons_EDE_country[t,c] = EDE(p.qcpc_post_recycle[t,c,:], p.η, p.nb_quantile)
            v.welfare_country[t,c] = (
                (p.l[t,c] / p.nb_quantile) * sum(utility.(p.qcpc_post_recycle[t,c,:], p.η))
            )
        end # country loop

        for rwpp in d.regionwpp
            country_indices = findall(x -> x == rwpp, p.mapcrwpp)

            v.cons_EDE_rwpp[t,rwpp] = EDE_aggregated(
                v.cons_EDE_country[t,country_indices], p.l[t,country_indices], p.η
            )
            v.welfare_rwpp[t,rwpp] = sum(v.welfare_country[t,country_indices])
        end # region loop

        v.cons_EDE_global[t] = EDE_aggregated(v.cons_EDE_country[t,:], p.l[t,:], p.η)
        v.welfare_global[t] = sum(v.welfare_country[t,:])
    end # timestep
end


function utility(consumption::Real, η::Real)
    if η == 1
        utility = log(consumption)
    else
        utility = consumption^(1 - η) / (1 - η)
    end

    return utility
end


function inverse_utility(utility::Real, η::Real)
    if η == 1
        consumption = exp(utility)
    else
        consumption = (utility * (1 - η))^(1 / (1 - η))
    end

    return consumption
end


function EDE(consumption::AbstractVector, η::Real, nb_quantile::Int)
    average_utility = (1 / nb_quantile) * sum(utility.(consumption, η))
    EDE = inverse_utility(average_utility, η)
    return EDE
end


function EDE_aggregated(
    country_level_EDE::AbstractVector,
    population::AbstractVector,
    η::Real
)
    total_utility = sum(population .* utility.(country_level_EDE, η))
    total_population = sum(population)
    average_utility = total_utility / total_population
    aggregated_EDE = inverse_utility(average_utility, η)
    return aggregated_EDE
end
