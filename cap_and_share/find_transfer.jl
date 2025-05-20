#!/usr/bin/env julia
using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

using CSV, DataFrames

# 1) Loading data -----------------------------------------------

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

# year_str = "rights_proposed_2030" → extraire l'année
df_rights_long[!, :time] = parse.(Int, replace.(df_rights_long.year_str, "rights_proposed_"=>""))
select!(df_rights_long, Not(:year_str))
rename!(df_rights_long, Dict("code"=>"country"))


# GDP per country
df_gdp = joinpath(@__DIR__, "..", "results", "uniform_tax_example", "no_revenue_recycling", "country_output", "gross_output.csv")
df_gdp = CSV.read(df_gdp, DataFrame)
rename!(df_gdp, Dict("YGROSS"=>"GDP"))

# Emissions per country
df_emis = joinpath(@__DIR__, "..", "results", "uniform_tax_example", "no_revenue_recycling", "country_output", "industrial_co2_emissions.csv")
df_emis = CSV.read(df_emis, DataFrame)
rename!(df_emis, Dict("E_Global_gtco2"=>"emissions"))

# Cost of carbon per country (same for all Union member)
df_tax = joinpath(@__DIR__, "..", "results", "uniform_tax_example", "no_revenue_recycling", "country_output", "country_carbon_tax.csv")
df_tax = CSV.read(df_tax, DataFrame)
rename!(df_tax, Dict("country_carbon_tax"=>"carbon_price"))

# Population by country
df_pop = joinpath(@__DIR__, "..", "results", "uniform_tax_example", "no_revenue_recycling", "country_output", "population.csv")
df_pop = CSV.read(df_pop, DataFrame)
rename!(df_pop, Dict("l"=>"population"))

# 2) Merge all tables -----------------------------------------

# Start form df_rights_long which contains country, time, rights_proposed
df = df_rights_long

# Successive join by country & time
df = innerjoin(df, df_gdp, on=[:country, :time])
df = innerjoin(df, df_emis, on=[:time])
df = innerjoin(df, df_tax, on=[:country, :time])
df = innerjoin(df, df_pop, on=[:country, :time])

# 3) Calculating transfers ------------------------------------------------

# Transfer as % of GDP
df[!, :transfer_over_gdp] = df.carbon_price .* (df.rights_proposed .- df.emissions) ./ df.GDP

# Transfer pc
df[!, :transfer_pc] = df.carbon_price .* (df.rights_proposed .- df.emissions) ./ df.population

# 4) Savings -----------------------------------------------------------

CSV.write("cap_and_share/data/outputtransfers_by_country.csv", df)

println("✅  Transfers calculated and written in output/transfers_by_country.csv")
