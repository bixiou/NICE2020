# Diagnostic: with NICE_NDC_BASELINES=1, data/parameters.jl overwrites the baseline emission intensity sigma
# of the EU27 member states and China with sigma = E_NDC / GDP_calibrated, where
# E_NDC is the trajectory of cap_and_share/data/input/ndc_trajectories.csv
# (EU: 2030 NDC and ESR, 2040 target, 2050 neutrality; China: Du et al. 2026,
# zero from 2071). Emissions are E = YGROSS * sigma * (1 - mu) and the abatement
# cost coefficient is proportional to sigma, so these paths become the
# *no-policy* baselines of the 28 countries, from which every carbon price then
# abates. This script runs the model with the overwritten and with the original
# sigma, with no carbon price and at the benchmark p*, and compares emissions.
#
#   julia --project=. src/_diag_ndc_baseline.jl
cd(joinpath(@__DIR__, ".."))
# load the model with the replacement switched on (data/parameters.jl), so that
# MimiNICE2020.emissionsrate holds the NDC intensities; the original ones are
# read from data/emission_intensity.csv below
ENV["NICE_NDC_BASELINES"] = "1"
using Mimi, MimiFAIRv2, DataFrames, CSV, CSVFiles, Printf
include(joinpath(@__DIR__, "nice2020_module.jl"))
const N = MimiNICE2020

countries = N.countries
years     = collect(2020:2020 + size(N.emissionsrate, 1) - 1)
yi(y)     = y - 2019

# sigma as shipped with NICE2020, before the overwrite
raw = DataFrame(load("data/emission_intensity.csv", header_exists = true))
filter!(:countrycode => in(countries), raw)
sigma_orig = Matrix(select(unstack(raw, :year, :countrycode, :intensity, allowduplicates = true), countries))
sigma_ndc  = Matrix(N.emissionsrate)
fp         = [get(Dict(string(r[1]) => Float64(r[2]) for r in eachrow(N.footprint_over_territorial)), c, 1.0)
              for c in countries]
changed = [c for (j, c) in enumerate(countries) if any(sigma_orig[:, j] .!= sigma_ndc[:, j])]
println("countries whose sigma is overwritten (", length(changed), "): ", join(changed, " "))
chg_years = [years[t] for t in 1:length(years) if any(sigma_orig[t, :] .!= sigma_ndc[t, :])]
println("years overwritten: ", first(chg_years), "-", last(chg_years))

# p*, the paper's benchmark path (zero before 2025, flat after the file ends)
pdf   = CSV.read(joinpath("cap_and_share", "data", "output", "calibrated_global_exp.csv"), DataFrame)
pd    = Dict(Int(r.time) => Float64(r.global_tax) for r in eachrow(pdf))
pstar = [y < 2025 ? 0.0 : get(pd, y, pd[maximum(keys(pd))]) for y in years]

function run_case(sigma, price)
    m = N.create_nice2020()
    update_param!(m, :switch_footprint, 1)            # as in every run of the paper
    update_param!(m, :switch_recycle, 0)
    update_param!(m, :abatement, :control_regime, 1)  # one global price
    update_param!(m, :σ, sigma)
    update_param!(m, :emissionsrate_footprint, sigma .* transpose(fp))
    update_param!(m, :abatement, :global_carbon_tax, price)
    run(m)
    return Float64.(m[:emissions, :E_gtco2]), Float64.(m[:temperature, :T])
end

eu  = [j for (j, c) in enumerate(countries) if c in N.EU27]
chn = findfirst(==("CHN"), countries)
grp = [("China", [chn]), ("EU27", eu), ("World", collect(1:length(countries)))]
win = yi(2025):yi(2100)
disc = [1 / 1.03^(y - 2025) for y in 2025:2100]

for (lbl, price) in (("no carbon price", zeros(length(years))), ("benchmark p*", pstar))
    E0, T0 = run_case(sigma_orig, price)
    E1, T1 = run_case(sigma_ndc, price)
    println("\n=== ", lbl, ": emissions (GtCO2/yr), original NICE sigma -> sigma overwritten with the NDC paths")
    for (g, idx) in grp
        s0(t) = sum(E0[t, idx]); s1(t) = sum(E1[t, idx])
        @printf("  %-6s", g)
        for y in (2025, 2030, 2040, 2050, 2070, 2100)
            @printf("  %d: %6.2f -> %6.2f", y, s0(yi(y)), s1(yi(y)))
        end
        @printf("\n         cumulative 2025-2100: %7.1f -> %7.1f GtCO2\n",
                sum(s0(t) for t in win), sum(s1(t) for t in win))
    end
    @printf("  warming in 2100: %.2f C -> %.2f C\n", T0[yi(2100)], T1[yi(2100)])
    if lbl == "benchmark p*"
        # the share that drives rho_hat (eq. rhohat_dyn): price-weighted discounted emissions
        w = disc .* pstar[win]
        for (g, idx) in grp[1:2]
            sh(E) = sum(w .* vec(sum(E[win, idx], dims = 2))) / sum(w .* vec(sum(E[win, :], dims = 2)))
            @printf("  %s, share of world price-weighted emissions 2025-2100: %.1f%% -> %.1f%%\n",
                    g, 100 * sh(E0), 100 * sh(E1))
        end
    end
end
