################################################################################################
# calibrate_global_tax_dichotomy.jl
#
# Calibrate a uniform global carbon tax in NICE2020 according to a participation scenario, using :
# - for 2025 (t0) start at 100$ + dichotomy algorithm,
# - for t>2025 a range centered on tax_{t-1} with an initial step of 20 + dichotomy algorithm.
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
const scenario_name     = :All_World     # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership)
const policy_scenario   = scenario_index[scenario_name]
const participation_vec = club_country[policy_scenario, :]


# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Target file
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


#  Prepare a NICE2020 instance in global_carbon_tax mode
function prepare_model(policy_scenario)
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :policy_scenario, policy_scenario)
    update_param!(m, :abatement, :control_regime, 1)
    return m
end

# Simulate and returns aggregated club emissions in year t
function emission_global(m::Model, tax_vec::Vector{Float64}, t::Int, participation::Vector{Int})
    update_param!(m, :abatement, :global_carbon_tax, tax_vec)
    run(m)
    em = m[:emissions, :E_gtco2]
    return sum(em[t, :] .* participation)
end

# Dichotomy algorithm (for t0 = 2025)
function find_tax_for_year!(m::Model, t::Int, target_t::Float64,
                            tax_vec::Vector{Float64}, participation::Vector{Int};
                            tol_em::Float64 = 0.01) # Emissions gap accepted (0.01 means 10 megatone of co2)
    # initial guess
    guess = 100.0
    local_tax = copy(tax_vec)
    local_tax[t] = guess
    em0 = emission_global(m, local_tax, t, participation)

    lower = guess; upper = guess
    if em0 > target_t
        # tax too low → double
        while true
            lower = upper
            upper *= 2
            println("    [bracket ↑] lower=", lower, ", upper=", upper)
            local_tax[t] = upper
            if emission_global(m, local_tax, t, participation) < target_t
                break
            end
        end
    else
        # tax too high → divide
        while true
            upper = lower
            lower /= 2
            println("    [bracket ↓] lower=", lower, ", upper=", upper)
            local_tax[t] = lower
            if emission_global(m, local_tax, t, participation) > target_t
                break
            end
        end
    end

    # dichotomy
    while true
        mid = 0.5*(lower + upper)
        local_tax[t] = mid
        em_mid = emission_global(m, local_tax, t, participation)
        println("    [dicho] τ=", round(mid; digits=2),
                " → E=", round(em_mid; digits=4)," (target=", round(target_t; digits=4),")")
        if abs(em_mid - target_t) < tol_em
            tax_vec[t] = mid; return tax_vec
        elseif em_mid > target_t
            lower = mid
        else
            upper = mid
        end
    end
end

# Bracket around tax_{t-1} with initial step (for t>2025)
function find_tax_for_year_step!(m::Model, t::Int, target_t::Float64,
                                 tax_vec::Vector{Float64}, participation::Vector{Int};
                                 tol_em::Float64=0.01, init_step::Float64=10.0) #  Decrement (with 10 is better)
    guess = tax_vec[t-1]
    step = init_step
    local_tax = copy(tax_vec)

    # initial bracket
    lower = max(0.0, guess - step)
    upper = guess + step

    # adjust lower to guarantee E(lower) > target
    local_tax[t] = lower
    while emission_global(m, local_tax, t, participation) < target_t
        println("    [exp-step ↓] lower=", lower, ", em=", emission_global(m, local_tax, t, participation))
        step *= 2
        lower = max(0.0, lower - step)
        local_tax[t] = lower
    end
    # adjust upper to guarantee E(upper) < target
    local_tax[t] = upper
    while emission_global(m, local_tax, t, participation) > target_t
        println("    [exp-step ↑] upper=", upper, ", em=", emission_global(m, local_tax, t, participation))
        step *= 2
        upper += step
        local_tax[t] = upper
    end

    # Dichotomy
    while true
        mid = 0.5*(lower + upper)
        local_tax[t] = mid
        em_mid = emission_global(m, local_tax, t, participation)
        println("    [dicho-step] τ=", round(mid; digits=2)," → E=", round(em_mid; digits=4))
        if abs(em_mid - target_t) < tol_em
            tax_vec[t] = mid; return tax_vec
        elseif em_mid > target_t
            lower = mid
        else
            upper = mid
        end
    end
end

# Main run
m_ref     = prepare_model(policy_scenario)
years     = collect(dim_keys(m_ref, :time))
countries = dim_keys(m_ref, :country)
T = length(years)

#
target_mat    = load_target_emissions(target_csv, years, countries)
target_global = target_mat * participation_vec

# Index 2025
start_year = 2025
end_year   = 2100
start_idx = findfirst(==(start_year), years)
end_idx    = findfirst(==(end_year),   years)
if start_idx === nothing
    error("Year \$start_year not find in 'years'.")
end
if end_idx === nothing
         error("Year $end_year not find in 'years'.")
end

######## Calibration 
 tax_vec = zeros(Float64, T)
for t in start_idx:end_idx
    println("=== Calibration year ", years[t], " ===")
    if t == start_idx
        find_tax_for_year!(m_ref, t, target_global[t], tax_vec, participation_vec; tol_em=0.01)
    else
        find_tax_for_year_step!(m_ref, t, target_global[t], tax_vec, participation_vec;
                                tol_em=0.01, init_step=20.0)
    end
    println("  -> optimal tax = ", round(tax_vec[t]; digits=4), " USD/tCO2")
end


# export
out = DataFrame(time=years[start_idx:end_idx],
                 global_tax=tax_vec[start_idx:end_idx])
CSV.write("calibrated_global_tax_dichotomy.csv", out)
println("Résults in 'calibrated_global_tax_dichotomy.csv'.")
