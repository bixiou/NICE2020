#################################################################################
# Builds annual CO2 trajectories (2020–2300) for each of the 27 EU member states + China,
# based on official climate targets, then writes:
#   data/input/eu_ndc_trajectories.csv  — per-country long format (country, time, E_gtco2)
#   data/input/E_Union_NDC_2020_2300.csv — EU-27 aggregate (time, E_gtco2)
#
# ── Sources ──────────────────────────────────────────────────────────────────
# 1. EU
#   Historical CO2 (1990–2023):
#     emissions_co2_fossil_territorial_pc.csv  (tCO2/person, territorial fossil CO2)
#     Source: Global Carbon Project via Our World in Data
#
#   Population:
#     5530a383-d6a3-4d54-8f7a-5ad1ab4a6ce6_Data.csv  (World Bank, SP.POP.TOTL)
#
#   2030 target: EU NDC (-55% net GHG vs 1990), submitted to UNFCCC
#     Distributed by country using Effort Sharing Regulation (ESR):
#     Regulation (EU) 2023/857, Annex I  — % change in non-ETS GHG vs 2005 by 2030
#     ⚠ Assumption: ESR targets (non-ETS only) are used as proxies for total CO2
#       reduction ambition per country; the EU sum is then rescaled to match the
#       NDC aggregate target.
#
#   2040 target: EU Climate Law amendment (-90% net GHG vs 1990), COM(2025)
#     Distributed proportionally to 2030 country shares.
#
#   2050 target: climate neutrality (net zero CO2)
#
# 2. China
#   Du et al. (2026), "china co2.xlsx" (translated from original) sheet 1
#   Scenario: CO2 neutrality
#   Row: "Total CO2 emissions"
#   Original unit: 亿吨 CO2 (10^8 tCO2) — divide by 10 to convert to GtCO2
#
#   2020–2060 : Du et al. 5-year values, linearly interpolated to annual
#   2061–2070 : linear interpolation from 2060 value to 0
#               (consistent with China's carbon neutrality target by 2060;
#                residual ~0.9 GtCO2 in 2060 reflects hard-to-abate sectors)
#   2071–2300 : zero
#
# ── Interpolation ─────────────────────────────────────────────────────────────
#   2020–2023 : historical data
#   2024–2029 : linear interpolation from 2023 actuals to 2030 NDC target
#   2030–2039 : linear interpolation from 2030 to 2040 target
#   2040–2049 : linear interpolation from 2040 to zero (2050)
#   2050–2300 : zero
################################################################################

using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

using CSV, DataFrames

# ── French name → ISO3 for EU-27 ──────────────────────────────────────────────
const FR_TO_ISO3 = Dict(
    "Allemagne"          => "DEU",
    "Autriche"           => "AUT",
    "Belgique"           => "BEL",
    "Bulgarie"           => "BGR",
    "Chypre"             => "CYP",
    "Croatie"            => "HRV",
    "Danemark"           => "DNK",
    "Espagne"            => "ESP",
    "Estonie"            => "EST",
    "Finlande"           => "FIN",
    "France"             => "FRA",
    "Grèce"              => "GRC",
    "Hongrie"            => "HUN",
    "Irlande"            => "IRL",
    "Italie"             => "ITA",
    "Lettonie"           => "LVA",
    "Lituanie"           => "LTU",
    "Luxembourg"         => "LUX",
    "Malte"              => "MLT",
    "Pays-Bas"           => "NLD",
    "Pologne"            => "POL",
    "Portugal"           => "PRT",
    "Roumanie"           => "ROU",
    "République tchèque" => "CZE",
    "Slovaquie"          => "SVK",
    "Slovénie"           => "SVN",
    "Suède"              => "SWE",
)

const EU27 = sort(collect(values(FR_TO_ISO3)))

# ── Effort Sharing Regulation 2030 targets ────────────────────────────────────
# % change in non-ETS GHG emissions vs 2005 by 2030
# Source: Regulation (EU) 2023/857, Annex I
const ESR_TARGET = Dict(
    "AUT" => -0.480,  "BEL" => -0.470,  "BGR" =>  0.000,
    "HRV" => -0.070,  "CYP" => -0.240,  "CZE" => -0.260,
    "DNK" => -0.500,  "EST" => -0.240,  "FIN" => -0.500,
    "FRA" => -0.475,  "DEU" => -0.500,  "GRC" => -0.225,
    "HUN" => -0.185,  "IRL" => -0.510,  "ITA" => -0.437,
    "LVA" => -0.170,  "LTU" => -0.210,  "LUX" => -0.500,
    "MLT" => -0.190,  "NLD" => -0.480,  "POL" => -0.070,
    "PRT" => -0.287,  "ROU" => -0.020,  "SVK" => -0.225,
    "SVN" => -0.270,  "ESP" => -0.377,  "SWE" => -0.500,
)

# ── Load territorial CO2 per capita (tCO2/person) ─────────────────────────────
# Row 1 is a title; row 2 is headers; data starts at row 3
co2_path = joinpath(@__DIR__, "Modeling_co2_emissions", "Emissions",
                    "emissions_co2_fossil_territorial_pc.csv")

co2_raw = CSV.read(co2_path, DataFrame;
                   header=2, skipto=3, missingstring=["", ".."],
                   normalizenames=false)

rename!(co2_raw, names(co2_raw)[1] => "year")
co2_raw = co2_raw[.!ismissing.(co2_raw.year), :]
co2_raw.year = parse.(Int, strip.(string.(co2_raw.year)))

eu_fr = collect(keys(FR_TO_ISO3))
co2_eu = select(co2_raw, vcat(["year"], eu_fr))
rename!(co2_eu, FR_TO_ISO3)   # rename French columns to ISO3

# Melt to long format
co2_long = stack(co2_eu, EU27; variable_name=:country, value_name=:co2_pc)
co2_long.country = String.(co2_long.country)
co2_long.co2_pc  = coalesce.(co2_long.co2_pc, 0.0)

# ── Load population (World Bank) ──────────────────────────────────────────────
pop_path = joinpath(@__DIR__, "Modeling_co2_emissions", "Population",
                    "5530a383-d6a3-4d54-8f7a-5ad1ab4a6ce6_Data.csv")

pop_raw  = CSV.read(pop_path, DataFrame; missingstring=["", ".."])
pop_eu = filter(row -> coalesce(row["Country Code"] in EU27, false), pop_raw)

# Year columns look like "1960 [YR1960]"
yr_cols  = filter(c -> occursin(r"^\d{4} \[", c), names(pop_raw))
pop_long = stack(pop_eu, yr_cols; variable_name=:yr_str, value_name=:pop)
pop_long[!, :year]    = parse.(Int, first.(split.(pop_long.yr_str, " ")))
pop_long[!, :country] = pop_long[!, "Country Code"]
pop_long = select(pop_long, :country, :year, :pop)
pop_long.pop = Float64.(coalesce.(pop_long.pop, 0.0))

# ── Total CO2 per country per year (GtCO2) ────────────────────────────────────
merged = innerjoin(co2_long, pop_long; on=[:country, :year])
merged[!, :E_gtco2] = merged.co2_pc .* merged.pop ./ 1e9

function co2_by_country(df, yr)
    sub = filter(r -> r.year == yr, df)
    return Dict(r.country => r.E_gtco2 for r in eachrow(sub))
end

co2_1990 = co2_by_country(merged, 1990)
co2_2005 = co2_by_country(merged, 2005)
co2_2023 = co2_by_country(merged, 2023)

eu_1990 = sum(values(co2_1990))
eu_2005 = sum(values(co2_2005))
eu_2023 = sum(values(co2_2023))

println("EU-27 total CO2 (GtCO2):  1990=$(round(eu_1990; digits=3))  2005=$(round(eu_2005; digits=3))  2023=$(round(eu_2023; digits=3))")

# ── EU aggregate NDC milestones ───────────────────────────────────────────────
eu_target_2030 = eu_1990 * (1 - 0.55)   # NDC: -55% vs 1990
eu_target_2040 = eu_1990 * (1 - 0.90)   # Climate Law: -90% vs 1990
eu_target_2050 = 0.0                     # Net zero

println("EU-27 targets (GtCO2):  2030=$(round(eu_target_2030; digits=3))  2040=$(round(eu_target_2040; digits=3))  2050=0")

# ── Per-country 2030 targets ──────────────────────────────────────────────────
# Apply ESR % reductions to 2005 CO2 (note: ESR covers non-ETS only, used as proxy)
raw_2030 = Dict(iso => get(co2_2005, iso, 0.0) * (1 + ESR_TARGET[iso]) for iso in EU27)

# Rescale so EU sum matches NDC aggregate
scale_factor = eu_target_2030 / sum(values(raw_2030))
country_2030 = Dict(iso => raw_2030[iso] * scale_factor for iso in EU27)

println("ESR rescaling factor: $(round(scale_factor; digits=4))  (1.0 = ESR already consistent with NDC)")

# ── Per-country 2040 targets: proportional to 2030 shares ────────────────────
country_2040 = Dict(iso => (country_2030[iso] / eu_target_2030) * eu_target_2040 for iso in EU27)

# ── Build annual trajectories ─────────────────────────────────────────────────
lerp(y0, y1, x0, x1, x) = y0 + (y1 - y0) * (x - x0) / (x1 - x0)

rows = NamedTuple{(:country, :time, :E_gtco2), Tuple{String,Int,Float64}}[]

for iso in EU27
    # 2020–2023: historical
    for yr in 2020:2023
        sub = filter(r -> r.year == yr && r.country == iso, merged)
        val = isempty(sub) ? 0.0 : first(sub.E_gtco2)
        push!(rows, (country=iso, time=yr, E_gtco2=val))
    end

    v23 = get(co2_2023, iso, 0.0)
    v30 = country_2030[iso]
    v40 = country_2040[iso]

    # 2024–2029: linear from 2023 actual → 2030 NDC target
    for yr in 2024:2029
        push!(rows, (country=iso, time=yr, E_gtco2=lerp(v23, v30, 2023, 2030, yr)))
    end
    # 2030–2039: linear from 2030 → 2040 target
    for yr in 2030:2039
        push!(rows, (country=iso, time=yr, E_gtco2=lerp(v30, v40, 2030, 2040, yr)))
    end
    # 2040–2049: linear from 2040 → zero
    for yr in 2040:2049
        push!(rows, (country=iso, time=yr, E_gtco2=lerp(v40, 0.0, 2040, 2050, yr)))
    end
    # 2050–2300: zero
    for yr in 2050:2300
        push!(rows, (country=iso, time=yr, E_gtco2=0.0))
    end
end

df_countries = DataFrame(rows)
sort!(df_countries, [:country, :time])

# ── Save per-country trajectories ─────────────────────────────────────────────
out_countries = joinpath(@__DIR__, "data", "input", "eu_ndc_trajectories.csv")
CSV.write(out_countries, df_countries)
println("Written: $out_countries  ($(nrow(df_countries)) rows)")

# ── Save EU aggregate ─────────────────────────────────────────────────────────
df_agg = combine(groupby(df_countries, :time), :E_gtco2 => sum => :E_gtco2)
sort!(df_agg, :time)

out_agg = joinpath(@__DIR__, "data", "input", "E_Union_NDC_2020_2300.csv")
CSV.write(out_agg, df_agg)
println("Written: $out_agg  ($(nrow(df_agg)) rows)")

# ── Sanity checks ─────────────────────────────────────────────────────────────
agg_2030 = only(filter(r -> r.time == 2030, df_agg)).E_gtco2
agg_2040 = only(filter(r -> r.time == 2040, df_agg)).E_gtco2
println("\nSanity check (aggregate):")
println("  2030: $(round(agg_2030; digits=3)) GtCO2  (target: $(round(eu_target_2030; digits=3)))")
println("  2040: $(round(agg_2040; digits=3)) GtCO2  (target: $(round(eu_target_2040; digits=3)))")

### China's annual CO2 trajectory (2020–2300) from Du et al. (2026)

# ── Du et al. (2026) values — CO2 neutrality scenario, Total CO2 emissions ───
# Original unit: 亿吨 CO2; converted here to GtCO2 (÷ 10)
const DU_YEARS  = [2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060]
const DU_VALUES = [      # 亿吨 CO2 → GtCO2
    111.08802024382278,  # 2020 → 11.109
    128.10430675284121,  # 2025 → 12.810  (peak before 2030)
    127.25737822169319,  # 2030 → 12.726
    108.17644427540304,  # 2035 → 10.818
     82.99977414378408,  # 2040 →  8.300
     49.618955456090781, # 2045 →  4.962
     31.557270544624195, # 2050 →  3.156
     19.249927842725043, # 2055 →  1.925
      8.9705346500544429 # 2060 →  0.897
] ./ 10.0

# ── Linear interpolation helper ───────────────────────────────────────────────
lerp(y0, y1, x0, x1, x) = y0 + (y1 - y0) * (x - x0) / (x1 - x0)

# ── Build annual trajectory ───────────────────────────────────────────────────
rows = NamedTuple{(:country, :time, :E_gtco2), Tuple{String,Int,Float64}}[]

# 2020–2060: interpolate between 5-year Du et al. anchors
for i in 1:(length(DU_YEARS) - 1)
    y0, y1 = DU_YEARS[i], DU_YEARS[i+1]
    v0, v1 = DU_VALUES[i], DU_VALUES[i+1]
    for yr in y0:(y1 - 1)
        push!(rows, (country="CHN", time=yr, E_gtco2=lerp(v0, v1, y0, y1, yr)))
    end
end
push!(rows, (country="CHN", time=2060, E_gtco2=DU_VALUES[end]))

# 2061–2070: linear decay to zero (residual hard-to-abate CO2 eliminated)
v60 = DU_VALUES[end]
for yr in 2061:2070
    push!(rows, (country="CHN", time=yr, E_gtco2=lerp(v60, 0.0, 2060, 2070, yr)))
end

# 2071–2300: zero
for yr in 2071:2300
    push!(rows, (country="CHN", time=yr, E_gtco2=0.0))
end

df_china = DataFrame(rows)

println("China trajectory built: $(nrow(df_china)) rows")
println("  Peak: $(round(maximum(df_china.E_gtco2); digits=3)) GtCO2  in $(df_china.time[argmax(df_china.E_gtco2)])")
println("  2060: $(round(only(filter(r -> r.time==2060, df_china)).E_gtco2; digits=3)) GtCO2")
println("  2070: $(round(only(filter(r -> r.time==2070, df_china)).E_gtco2; digits=3)) GtCO2")

# ── Append to EU trajectories and write combined file ─────────────────────────
eu_path  = joinpath(@__DIR__, "data", "input", "eu_ndc_trajectories.csv")
df_eu    = CSV.read(eu_path, DataFrame)

df_combined = vcat(df_eu, df_china)
sort!(df_combined, [:country, :time])

out_path = joinpath(@__DIR__, "data", "input", "ndc_trajectories.csv")
CSV.write(out_path, df_combined)
println("\nWritten: $out_path  ($(nrow(df_combined)) rows, $(length(unique(df_combined.country))) countries)")
