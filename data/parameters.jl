using Query, JSON, DataFrames, CSVFiles, CSV

nice_inputs = JSON.parsefile("data/nice_inputs.json") # This file contains the economic and emissions calibration and the list of used country codes
# the json file reads in a several nested dictionaries, where in each case the "last" dictionary contains the keys, "units", "dimensions", "notes", and "x". The "x" key always contains the data to be read into a DataFrame.

countries = nice_inputs["country_set_ssp_183"]["x"]["countrycode"] # country set (ISO3 codes in alphabetic order)countries = string.(countries)

# REMOVE SOMALIA, VENEZUELA, NEW CALEDONIA, and TRINIDAD AND TOBAGO
filter!( x-> !(x in ["SOM", "VEN", "NCL", "TTO" ]), countries )

sort!(countries) # Sort country names to be in alphabetical order.

#-----------------------------------------------------------------
# Load mapping of countries to World Population Prospects regions
#----------------------------------------------------------------

mapping_wpp_regions = DataFrame(load("data/WPP_regions_country_list.csv"))

# Filter countries in the country set and sort
filter!(:countrycode => in(countries), mapping_wpp_regions )
sort!(mapping_wpp_regions, :countrycode)

# Extract region index as vector
map_country_region_wpp = mapping_wpp_regions[:, :WPP_region_number]

# Extract vector of regions names in order of region numbers
names_regions_df = unique(mapping_wpp_regions[:, [:WPP_region_name,:WPP_region_number] ])
sort!(names_regions_df, :WPP_region_number)
wpp_regions = names_regions_df[:, :WPP_region_name]

#-----------------------------------------------------------------
# Load mapping of countries participating in each scenario
#----------------------------------------------------------------

# Fonction utilitaire pour créer un vecteur binaire de participation
function participation_vector(participants::Vector{Symbol}, all_countries::Vector{Symbol})
    return [country in participants ? 1 : 0 for country in all_countries]
end

# Scenario labels
scenarios = [:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership, :Union, :JPN, :KOR, :CHN, :WEU]

# Correspondence dictionary name → index
scenario_index = Dict(
    :All_World     => 1,
    :All_Except_Oil_Countries    => 2,
    :Optimistic    => 3,
    :Generous_EU   => 4,
    :Partnership   => 5,
    :Union         => 7,
    :JPN           => 8,
    :KOR           => 9,
    :CHN           => 10,
    :WEU           => 11,
)

rich_oil_countries = ["RUS", "KAZ", "SAU", "QAT", "KWT", "AZE", "OMN", "BHR", "MYS"]

# Scenario 2 : All except rich oil countries
all_except_oil_countries = filter(country -> !(country in rich_oil_countries), countries)

#Scenario 3 : Optimistic scenario: Africa + Latin America + South Asia + South-East Asia + China + EU28 + Norway + Switzerland + Canada + Japan + Korea + NZ
optimistic_regions = [2, 5, 8, 9, 15, 18, 3, 13, 14, 16]
optimistic_countries = filter(row -> row[:WPP_region_number] in optimistic_regions, mapping_wpp_regions)[:, :countrycode]


additional_countries = ["CHN", "NOR", "CHE", "CAN", "JPN", "NZL", "KOR"]
eu28_countries = ["AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", 
"IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE", "GBR"]


optimistic_scenario_countries = unique(vcat(optimistic_countries, additional_countries, eu28_countries))

#Scenario 4: Generous EU: EU27 + China + Africa + Latin America + South Asia + South-East Asia

eu27_countries = ["AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC", "HUN", 
"IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"]

generous_eu_countries = unique(vcat(eu27_countries, optimistic_countries, ["CHN"]))

#Scenario 5 : Africa-EU partnership : Africa + EU_27
africa_regions=[5, 8, 9, 15, 18]
africa_countries = filter(row -> row[:WPP_region_number] in africa_regions, mapping_wpp_regions)[:, :countrycode]

partnership_countries =unique(vcat(eu27_countries, africa_countries))

#Scenario 6 : personalized scenario (add you own countries)
#groups available : eu27_countries, eu28_countries, africa_countries, optimistic Northern countries (additional_countries), latin_america_countries

latin_american_countries = filter(row -> row[:WPP_region_number] in [2, 3, 13], mapping_wpp_regions)[:, :countrycode]

#add the groups of countries that participate in the personalized scenario in here
personalized_countries = unique(vcat(eu27_countries, africa_countries, ["NOR", "CHN", "CHE"]))


#Scenario 7 : Union 
union_countries = ["AFG", "AGO", "ALB", "ARG", "AUT", "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHS", "BIH", "BLZ", "BOL",
"BRA", "BRB", "BTN", "BWA", "CAF", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV",
"CRI", "CUB", "CYP", "CZE", "DEU", "DJI", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "EST", "ETH",
"FIN", "FRA", "GAB", "GBR", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GTM", "GUY", "HND", "HRV", "HTI",
"HUN", "IDN", "IND", "IRL", "IRN", "ISL", "ITA", "JAM", "JPN", "KEN", "KHM", "KOR", "LAO", "LBR", "LBY",
"LKA", "LSO", "LTU", "LUX", "LVA", "MAR", "MDA", "MDG", "MDV", "MEX", "MLI", "MLT", "MMR", "MNG", "MOZ",
"MRT", "MUS", "MWI", "MYS", "NAM", "NER", "NGA", "NIC", "NLD", "NOR", "NPL", "PAK", "PAN", "PER", "PHL",
"PNG", "POL", "PRT", "PRY", "ROU", "RWA", "SDN", "SEN", "SGP", "SLE", "SLV", "SRB", "SUR", "SVK", "SVN",
"SWE", "SWZ", "TCD", "TGO", "THA", "TKM", "TLS", "TUN" ,"TZA", "UGA", "URY", "VNM", "ZAF", "ZMB", 
"ZWE", 
"ARM", "GEO", "IRQ", "JOR", "SYR", "TKM", "TUR", "UKR", "UZB", "YEM" # , "TWN"
]

# Scenarios 8-11
WEU = ["AUT", "BEL", "CHE", "DEU", "DNK", "ESP", "FIN", "FRA", "GBR", "GRC", "HUN", "IRL", "ISL", "ITA", "LTU", "LUX", "LVA", "MDA", "MLT", "NLD", "NOR", "PRT", "SWE"]

JPN = ["JPN"]
KOR = ["KOR"]
CHN = ["CHN"]

club_countries = [
    Symbol.(countries),                            # Scenario 1
    Symbol.(all_except_oil_countries),             # Scenario 2
    Symbol.(optimistic_scenario_countries),        # Scenario 3
    Symbol.(generous_eu_countries),                # Scenario 4
    Symbol.(partnership_countries),                # Scenario 5
    Symbol.(personalized_countries),                # Scenario 6
    Symbol.(union_countries),                       # Scenario 7
    Symbol.(JPN),                       # Scenario 8
    Symbol.(KOR),                       # Scenario 9
    Symbol.(CHN),                       # Scenario 10
    Symbol.(WEU)                       # Scenario 11
]

# Final binary participation matrix per scenario
club_country = transpose(reduce(hcat,[
    fill(1, length(countries)),                                                         # Scenario 1
    participation_vector(Symbol.(all_except_oil_countries), Symbol.(countries)),        # Scenario 2
    participation_vector(Symbol.(optimistic_scenario_countries), Symbol.(countries)),   # Scenario 3
    participation_vector(Symbol.(generous_eu_countries), Symbol.(countries)),           # Scenario 4
    participation_vector(Symbol.(partnership_countries), Symbol.(countries)),           # Scenario 5
    participation_vector(Symbol.(personalized_countries), Symbol.(countries)),           # Scenario 6
    participation_vector(Symbol.(union_countries), Symbol.(countries)), 
    participation_vector(Symbol.(JPN), Symbol.(countries)), 
    participation_vector(Symbol.(KOR), Symbol.(countries)), 
    participation_vector(Symbol.(CHN), Symbol.(countries)), 
    participation_vector(Symbol.(WEU), Symbol.(countries))
]))

#-----------------------------------------
# Load economic and emissions calibration
#----------------------------------------

## Population

pop_raw = DataFrame(nice_inputs["economy"]["pop_projected"]["x"])
filter!(:countrycode => in(countries), pop_raw ) #Filter countries in list

# Unstack the dataframe to have year x country dimensions.
pop_unstack = unstack(pop_raw, :year, :countrycode, :pop_projected, allowduplicates=true)

# Sort the columns (country names) into alphabetical order.
pop = select(pop_unstack, countries)

## Initial capital

init_capital_raw = DataFrame(nice_inputs["economy"]["k0"]["x"])
filter!(:countrycode => in(countries), init_capital_raw)

# Sort the country rows into alphabetical order
initial_capital = sort(init_capital_raw, :countrycode)

# Extract vector of initial capital
k0 = initial_capital[:, :k0]

## Total factor productivity

productivity_raw = DataFrame(nice_inputs["economy"]["tfp"]["x"])
filter!(:countrycode => in(countries), productivity_raw)

# Unstack the dataframe to have year x country dimensions.
productivity_unstack = unstack(productivity_raw, :year, :countrycode, :tfp, allowduplicates=true)

# Sort the columns (country names) into alphabetical order.
productivity = select(productivity_unstack, countries)

## Savings rate

srate_raw = DataFrame(nice_inputs["economy"]["srate"]["x"])
filter!(:countrycode => in(countries), srate_raw)

# Unstack the dataframe to have year x country dimensions.
srate_unstack = unstack(srate_raw, :year, :countrycode, :srate, allowduplicates=true)

# Sort the columns (country names) into alphabetical order.
srate = select(srate_unstack, countries)

## Depreciation

depreciation_raw = DataFrame(nice_inputs["economy"]["depreciation"]["x"])
filter!(:countrycode => in(countries), depreciation_raw)

# Unstack the dataframe to have year x country dimensions.
depreciation_unstack = unstack(depreciation_raw, :year, :countrycode, :depreciation, allowduplicates=true)

# Sort the columns (country names) into alphabetical order.
depreciation = select(depreciation_unstack, countries)

## Emissions intensity

emissionsrate_raw = DataFrame(nice_inputs["emissions"]["bauProjectionV1"]["x"])
filter!(:countrycode => in(countries), emissionsrate_raw)

# Unstack the dataframe to have year x country dimensions.
emissionsrate_unstack = unstack(emissionsrate_raw, :year, :countrycode, :intensity, allowduplicates=true)

# Sort the columns (country names) into alphabetical order.
emissionsrate = select(emissionsrate_unstack, countries)

# Creates a version with consumption-based instead of territorial emissions, using a fixed ratio based on 2022 data from the Global Carbon Project
footprint_over_territorial = CSV.read("data/footprint_over_territorial_2022.csv", DataFrame)
emissionsrate_footprint = Matrix(emissionsrate) .* transpose(footprint_over_territorial[:,2])

#----------------------------------------
# Load inequality calibration
#----------------------------------------

## Income distribution for 2020
deciles = ["d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10"]
consumption_deciles_2020_raw = DataFrame(load("data/consumption_deciles_2020.csv",header_exists=true))
#consumption_deciles_2020_raw = DataFrame(nice_inputs["income_quantile_2020"]["x"])
filter!(:countrycode => in(countries), consumption_deciles_2020_raw)

consumption_distribution = Matrix(select!(consumption_deciles_2020_raw, deciles))

# Consumption distribution varying with time
consumption_deciles_2020_2100_raw = DataFrame(load("data/consumption_deciles_2020_2100.csv",  header_exists=false))
consumption_deciles_2020_2100_countries = filter(:Column1 => x -> x in countries, consumption_deciles_2020_2100_raw )
consumption_deciles_2020_2100_mat = Matrix(consumption_deciles_2020_2100_countries[:,2:end])

consumption_distribution_2020_2300=zeros(Float64, length(2020:2300), length(countries), 10)

for c in 1:length(countries)
    for t in 0:1:80
        for d in 1:10
            consumption_distribution_2020_2300[t+1,c,d] = consumption_deciles_2020_2100_mat[c, t*10 + d ]
        end
    end
    for t in 81:280
        for d in 1:10
            consumption_distribution_2020_2300[t+1,c,d] = consumption_distribution_2020_2300[81, c, d] # repeat last value
        end
    end
end

#--------------------------------------
# Load parameters for revenue recycling
#--------------------------------------

# Results from the  meta-regression based on study results to calculate elasticity vs. ln gdp per capita relationship.
meta_intercept = 3.22
meta_slope =  -0.22
meta_min_study_gdp = 647
meta_max_study_gdp = 48892

#--------------------------------
# Load abatement cost parameters
#-------------------------------

## Global Backstop price from DICE 2023, in 2017USD per tCO2
initial_pback = 670
pback_decrease_rate_2020_2050 = 0.01
pback_decrease_rate_after_2050 = 0.001

pbacktime_2020_2050 = [initial_pback * (1-pback_decrease_rate_2020_2050)^(t-2020) for t in 2020:1:2050 ]
pbacktime_after_2050 = [ pbacktime_2020_2050[end] * (1-pback_decrease_rate_after_2050)^(t-2050) for t in 2051:1:2300 ]

full_pbacktime = [pbacktime_2020_2050; pbacktime_after_2050 ]

#----------------------------------------------
# Load country-level damage function parameters
#----------------------------------------------

## Extract parameters for the country level damage functions based on Kalkuhl and Wenz

country_damage_coeffs = DataFrame(load("data/country_damage_coefficients.csv",header_exists=true))
filter!(:countrycode => in(countries), country_damage_coeffs ) #Filter countries in list
sort!(country_damage_coeffs, :countrycode)

beta1_KW = country_damage_coeffs[!, :beta1_KW]
beta2_KW = country_damage_coeffs[!, :beta2_KW]


#----------------------------------------
# Load FAIR initial conditions for 2020
#----------------------------------------

# This loads FAIR output for the year 2020 saved from a default run started in 1750 (making it possible to initialize FAIR in 2020).
init_aerosol     = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "aerosol.csv")))
init_ch4         = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "ch4.csv")))
init_co2         = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "co2.csv")))
init_flourinated = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "flourinated.csv")))
init_montreal    = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "montreal.csv")))
init_n2o         = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "n2o.csv")))
init_temperature = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "temperature.csv")))
init_tj          = DataFrame(load(joinpath(@__DIR__, "fair_initialize_2020", "tj.csv")))


#-------------------------------------------------------
# Load country-level temperature pattern scaling values
#-------------------------------------------------------

# For now, just use a single CMIP6 model.
cmip6_model = "CESM2"

# Set the SSP scenario.
ssp_scenario = "ssp2"

# Select pattern type (options = "patterns.area", "patterns.gdp.2000", "patterns.pop.2000", "patterns.gdp.2100", "patterns.pop.2100")
pattern_type = Symbol("patterns.pop.2100")

# Load raw pattern file and extract relevant model+scenario coefficients for each country.
raw_patterns = load(joinpath(@__DIR__, "cmip6_patterns_by_country.csv")) |>
               @filter(_.source == cmip6_model && _.scenario == ssp_scenario) |>
               @orderby(_.iso3) |>
               @filter(_.iso3 in countries) |> DataFrame

# Select pattern type from varios options.
cmip_pattern = raw_patterns[!, pattern_type]
