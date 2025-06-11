# -----------------------------------------------------------
# Emissions
# -----------------------------------------------------------

@defcomp emissions begin
    country         = Index()       # The regions index must be specified for each component
    regionwpp       = Index()       # Index for WPP regions

    σ          = Parameter(index=[time, country])   # Emissions output ratio (GtCO2 per million USD2017)
    YGROSS     = Parameter(index=[time, country])   # Gross output (million USD2017 per year)
    μ          = Parameter(index=[time, country])   # Emissions control rate
    mapcrwpp  = Parameter(index=[country])          # Map from country index to WPP region index
    policy_scenario         = Parameter()                       # Policy scenario for the country, used to determine which countries are in the club
    club_country       	= Parameter(index=[scenario, country])          # Countries in the club for each scenario (1) or not (0)


    E_gtco2            = Variable(index=[time, country])   # Country level CO₂ emissions (GtCO2 per year)
    E_Global_gtco2     = Variable(index=[time])            # Global CO₂ emissions (sum of all country emissions) (GtCO2 per year)
    E_Global_gtc       = Variable(index=[time])            # Global C emissions (GtC per year)
    E_gtco2_rwpp      = Variable(index=[time, regionwpp])   # Regional CO₂ emissions of WPP regions (GtCO2 per year)
    E_gtco2_club = Variable(index=[time])              # Scenario emissions

    function run_timestep(p, v, d, t)
        # Define an equation for E
        for c in d.country

            v.E_gtco2[t,c] = p.YGROSS[t,c] * p.σ[t,c] * (1-p.μ[t,c])
        end

        # Define an equation for E_Global.
        v.E_Global_gtco2[t] = sum(v.E_gtco2[t,:])

        # somme seulement sur le club de pays
        v.E_gtco2_club[t] = sum(v.E_gtco2[t,c] * p.club_country[p.policy_scenario,c]
                                     for c in d.country)

        # Convert emissions to GtC units (required by FAIR).
        v.E_Global_gtc[t] = v.E_Global_gtco2[t] * 12.01/44.01

        # Regional emissions for WPP regions
        for rwpp in d.regionwpp
            country_indices = findall(x->x==rwpp , p.mapcrwpp) #Country indices for the region
            v.E_gtco2_rwpp[t,rwpp]= sum(v.E_gtco2[t, country_indices])
        end

    end

end
