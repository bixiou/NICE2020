################################################################################################
# calibrate_country_tax_dichotomy_with_backstop.jl
#
# Calibrate personalized country‐and‐year carbon taxes in NICE2020 (control_regime = 4),
# so that, for each participating country and each year, emissions match the input targets.
# Si les émissions cibles sont nulles, on utilise le backstop price pour cette année.
################################################################################################

using Pkg
# Activate the NICE2020 project
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

using Mimi, MimiFAIRv2, DataFrames, CSVFiles, CSV

# Load NICE2020 module and helper functions
include(joinpath(@__DIR__, "..", "src", "nice2020_module.jl"))
include(joinpath(@__DIR__, "..", "src", "helper_functions.jl"))

# -------------------------------------------------------------------
# Participation scenario
include(joinpath(@__DIR__, "..", "data", "parameters.jl"))
const scenario_name     = :All_World     # Choice of scenario by name
const policy_scenario   = scenario_index[scenario_name]
const participation_vec = club_country[policy_scenario, :]
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Target file (country × year emissions)
const target_csv = joinpath(@__DIR__, "..", "results", "uniform_tax_example", "no_revenue_recycling", "country_output", "industrial_co2_emissions.csv")
# -------------------------------------------------------------------

####### CSV load and return T×C matrix of target emissions  
function load_target_emissions(path::String, years::Vector, countries::Vector)
    df = CSV.File(path) |> DataFrame
    sort!(df, [:time, :country])
    idx_year    = Dict(y => t for (t, y) in enumerate(years))
    idx_country = Dict(string(c) => i for (i, c) in enumerate(countries))
    T = length(years); C = length(countries)
    M = zeros(T, C)
    for row in eachrow(df)
        t = idx_year[row.time]
        c = idx_country[String(row.country)]
        M[t, c] = row.E_gtco2
    end
    return M
end

#  Prepare a NICE2020 instance in country_carbon_tax mode
function prepare_model(policy_scenario)
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :policy_scenario, policy_scenario)
    # Set control_regime = 4 so that the model will read p.direct_country_tax[t,c]
    update_param!(m, :abatement, :control_regime, 4)
    return m
end

# Simulate and return emissions for one country c in year t,
# given a full T×C matrix of country taxes tax_mat
function emission_country(m::Model, tax_mat::Matrix{Float64}, t::Int, c::Int)
    # pass the entire matrix as direct_country_tax
    update_param!(m, :abatement, :direct_country_tax, tax_mat)
    run(m)
    em = m[:emissions, :E_gtco2]  # T×C matrix of emissions
    return em[t, c]
end

# Dichotomy algorithm for finding tax_{t,c} so that emission_country == target
function find_tax_for_country_year!(
        m::Model, t::Int, c::Int, target_tc::Float64,
        tax_mat::Matrix{Float64};
        tol_em::Float64 = 0.01)  # Emissions gap accepted (0.01 = 0.01 GtCO₂)
    # initial guess
    guess = 100.0
    local_tax = copy(tax_mat)
    local_tax[t, c] = guess
    em0 = emission_country(m, local_tax, t, c)

    lower = guess
    upper = guess
    if em0 > target_tc
        # tax too low → increase until emission < target
        while true
            lower = upper
            upper *= 2
            println("    [bracket ↑] country=", c, ", year=", years[t], " lower=", lower, ", upper=", upper)
            local_tax[t, c] = upper
            if emission_country(m, local_tax, t, c) < target_tc
                break
            end
        end
    else
        # tax too high → decrease until emission > target
        while true
            upper = lower
            lower /= 2
            println("    [bracket ↓] country=", c, ", year=", years[t], " lower=", lower, ", upper=", upper)
            local_tax[t, c] = lower
            if emission_country(m, local_tax, t, c) > target_tc
                break
            end
        end
    end

    # dichotomy
    while true
        mid = 0.5 * (lower + upper)
        local_tax[t, c] = mid
        em_mid = emission_country(m, local_tax, t, c)
        println("    [dicho] country=", c, ", year=", years[t],
                " τ=", round(mid; digits=2),
                " → E=", round(em_mid; digits=4),
                " (target=", round(target_tc; digits=4), ")")
        if abs(em_mid - target_tc) < tol_em
            tax_mat[t, c] = mid
            return tax_mat
        elseif em_mid > target_tc
            lower = mid
        else
            upper = mid
        end
    end
end

# Bracket around tax_{t-1,c} with initial step (for t > 2025)
function find_tax_for_country_year_step!(
        m::Model, t::Int, c::Int, target_tc::Float64,
        tax_mat::Matrix{Float64};
        tol_em::Float64 = 0.01, init_step::Float64 = 10.0)
    guess = tax_mat[t - 1, c]
    step = init_step
    local_tax = copy(tax_mat)

    # initial bracket
    lower = max(0.0, guess - step)
    upper = guess + step

    # adjust lower to ensure emission(lower) > target
    local_tax[t, c] = lower
    while emission_country(m, local_tax, t, c) < target_tc
        println("    [exp-step ↓] country=", c, ", year=", years[t],
                " lower=", lower, ", em=", emission_country(m, local_tax, t, c))
        step *= 2
        lower = max(0.0, lower - step)
        local_tax[t, c] = lower
    end

    # adjust upper to ensure emission(upper) < target
    local_tax[t, c] = upper
    while emission_country(m, local_tax, t, c) > target_tc
        println("    [exp-step ↑] country=", c, ", year=", years[t],
                " upper=", upper, ", em=", emission_country(m, local_tax, t, c))
        step *= 2
        upper += step
        local_tax[t, c] = upper
    end

    # dichotomy
    while true
        mid = 0.5 * (lower + upper)
        local_tax[t, c] = mid
        em_mid = emission_country(m, local_tax, t, c)
        println("    [dicho-step] country=", c, ", year=", years[t],
                " τ=", round(mid; digits=2), " → E=", round(em_mid; digits=4))
        if abs(em_mid - target_tc) < tol_em
            tax_mat[t, c] = mid
            return tax_mat
        elseif em_mid > target_tc
            lower = mid
        else
            upper = mid
        end
    end
end

# Main run
m_ref     = prepare_model(policy_scenario)
years     = collect(dim_keys(m_ref, :time))      # années
countries = dim_keys(m_ref, :country)            # noms des pays
T = length(years)
C = length(countries)

#
target_mat = load_target_emissions(target_csv, years, countries)
# target_mat est T×C : target_mat[t,c] = émission cible pour pays c à l’année t

# Index 2025 et 2100
start_year = 2025
end_year   = 2100
start_idx  = findfirst(==(start_year), years)
end_idx    = findfirst(==(end_year),   years)
if start_idx === nothing
    error("Year $start_year not found in 'years'.")
end
if end_idx === nothing
    error("Year $end_year not found in 'years'.")
end

# Récupérer le backstop price (vectorisé en fonction du temps)
run(m_ref)
pbacktime = m_ref[:abatement, :pbacktime]  # vecteur de longueur T

######## Calibration : tax_mat[t,c] pour chaque pays c participant
tax_mat = zeros(Float64, T, C)

for t in start_idx:end_idx
    println("=== Calibration year ", years[t], " ===")
    for c in 1:C
        if participation_vec[c] == 1
            target_tc = target_mat[t, c]

            # Si l'émission cible est zéro → utiliser le backstop price
            if target_tc == 0.0
                tax_mat[t, c] = pbacktime[t]
                println("  [backstop] country=", countries[c], ", year=", years[t],
                        " → τ=", round(tax_mat[t, c]; digits=4))
                continue
            end

            if t == start_idx
                # première année (2025) : dichotomie « bracket autour de 100 »
                find_tax_for_country_year!(m_ref, t, c, target_tc, tax_mat; tol_em=0.01)
            else
                # années suivantes : « bracket autour de tax_{t-1,c} avec step=20 »
                find_tax_for_country_year_step!(m_ref, t, c, target_tc, tax_mat; tol_em=0.01, init_step=20.0)
            end
            println("  -> country ", countries[c], ", optimal tax = ", round(tax_mat[t, c]; digits=4), " USD/tCO2")
        else
            # Si le pays n'est pas participant, on laisse la taxe à zéro
            tax_mat[t, c] = 0.0
        end
    end
end

# Export des résultats dans un CSV (unique) : colonnes « time, country, country_tax »
num_years = end_idx - start_idx + 1
# Colonne time : pour chaque pays, on répète la liste des années de start_idx à end_idx
time_col = repeat(years[start_idx:end_idx], inner=C)
# Colonne country : on juxtapose [fill(pays1, num_years); fill(pays2, num_years); …]
country_col = vcat([fill(countries[c], num_years) for c in 1:C]...)
# Colonne country_tax : on concatène verticalement tax_mat[start_idx:end_idx, c] pour chaque pays c
country_tax_col = reduce(vcat, [tax_mat[start_idx:end_idx, c] for c in 1:C])

out = DataFrame(
    time        = time_col,
    country     = country_col,
    country_tax = country_tax_col,
)
# On ne garde que les pays participants dans le fichier final
filter!(row -> participation_vec[findfirst(==(row.country), countries)] == 1, out)

CSV.write("calibrated_country_tax_dichotomy_with_backstop.csv", out)
println("Results in 'calibrated_country_tax_dichotomy_with_backstop.csv'.")
