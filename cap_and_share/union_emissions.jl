#############################################
# Building a dataframe for Union emissions from 2020 to 2300
#############################################


using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

using CSV, DataFrames

########## 2030 - 2080 #########

# Proposed rights by country (wide → long)
df_rights = joinpath(@__DIR__, "data", "input", "ffu_rights_proposed_allocation.csv")
df_rights = CSV.read(df_rights, DataFrame)

year_cols = filter(c -> startswith(string(c), "rights_proposed_"), names(df_rights))
df_rights_long = stack(
    df_rights,
    year_cols,
    variable_name = :year_str,
    value_name    = :rights_proposed
)

df_rights_long.rights_proposed .= df_rights_long.rights_proposed ./ 1e9

# year_str = "rights_proposed_2030" → extract year
df_rights_long[!, :time] = parse.(Int, replace.(df_rights_long.year_str, "rights_proposed_"=>""))
select!(df_rights_long, Not(:year_str))
rename!(df_rights_long, Dict("code"=>"country"))


# Définir la liste de pays
pays_autorises = [
    "AFG", "AGO", "ALB", "ARG", "AUT", "BDI", "BEL", "BEN", "BFA", "BGD", "BGR", "BHS", "BIH", "BLZ", "BOL",
    "BRA", "BRB", "BTN", "BWA", "CAF", "CHE", "CHL", "CHN", "CIV", "CMR", "COD", "COG", "COL", "COM", "CPV",
    "CRI", "CUB", "CYP", "CZE", "DEU", "DJI", "DNK", "DOM", "DZA", "ECU", "EGY", "ERI", "ESP", "EST", "ETH",
    "FIN", "FRA", "GAB", "GBR", "GHA", "GIN", "GMB", "GNB", "GNQ", "GRC", "GTM", "GUY", "HND", "HRV", "HTI",
    "HUN", "IDN", "IND", "IRL", "IRN", "ISL", "ITA", "JAM", "JPN", "KEN", "KHM", "KOR", "LAO", "LBR", "LBY", 
    "LKA", "LSO", "LTU", "LUX", "LVA", "MAR", "MDA", "MDG", "MDV", "MEX", "MLI", "MLT", "MMR", "MNG", "MOZ",
    "MRT", "MUS", "MWI", "MYS", "NAM", "NER", "NGA", "NIC", "NLD", "NOR", "NPL", "PAK", "PAN", "PER", "PHL",
    "PNG", "POL", "PRT", "PRY", "ROU", "RWA", "SDN", "SEN", "SGP", "SLE", "SLV", "SRB", "SUR", "SVK", "SVN",
    "SWE", "SWZ", "TCD", "TGO", "THA", "TKM", "TLS", "TUN", "TWN", "TZA", "UGA", "URY", "VNM", "ZAF", "ZMB",
    "ZWE"
]

# Filtrer les pays présents dans la liste
df_true = filter(:country => x -> x in pays_autorises, df_rights_long)

# 1.Filter to keep only countries when participate_union == true
df_true = filter(:participate_union => isequal(true), df_rights_long)

# 2. Group by year (time) and sum rights_proposed
result = combine(groupby(df_true, :time),
                 :rights_proposed => sum => :total_rights_proposed)

result = filter(row -> row.time != 2080, result)


############### 2020 - 2029 and 2081 - 2300 ################

years1 = 2020:2029
values1 = [
    24.22630,
    24.59103,
    24.95112,
    25.30796,
    25.66306,
    26.01797,
    26.30245,
    26.58549,
    26.86846,
    27.15267
]
extra1 = DataFrame(time = years1, total_rights_proposed = values1)

# Création des années 2080–2300 à zéro ---
years2 = 2080:2300
extra2 = DataFrame(time = years2, total_rights_proposed = zeros(Float64, length(years2)))

# Concaténation et tri ---
result_extended = vcat(result, extra1, extra2)

rename!(result_extended, Dict("total_rights_proposed"=>"E_gtco2"))
sort!(result_extended, :time)

# Saving results
CSV.write("cap_and_share/data/input/Final_E_Union_2020_2300.csv", result_extended)


