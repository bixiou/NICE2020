################################################################################
# this script builds China's annual CO2 trajectory (2020–2300) from Du et al. (2026),
# appends it to the EU-27 per-country file, and writes a combined output:
#   data/input/ndc_trajectories.csv  — columns: country, time, E_gtco2
#
# /!\ run eu_ndc_emissions.jl first (produces eu_ndc_trajectories.csv)
#
# ── Source ────────────────────────────────────────────────────────────────────
#   Du et al. (2026), "china co2.xlsx" (translated from original) sheet 1
#   Scenario: CO2 neutrality
#   Row: "Total CO2 emissions"
#   Original unit: 亿吨 CO2 (10^8 tCO2) — divide by 10 to convert to GtCO2
#
# ── Trajectory ────────────────────────────────────────────────────────────────
#   2020–2060 : Du et al. 5-year values, linearly interpolated to annual
#   2061–2070 : linear interpolation from 2060 value to 0
#               (consistent with China's carbon neutrality target by 2060;
#                residual ~0.9 GtCO2 in 2060 reflects hard-to-abate sectors)
#   2071–2300 : zero
################################################################################

using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

using CSV, DataFrames

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
