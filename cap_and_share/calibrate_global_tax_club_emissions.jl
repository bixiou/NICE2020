################################################################################################
# Calibrates a uniform global carbon tax in NICE2020 for a given participation scenario (with club level emissions),
# using:
#  - for 2025 (t₀): exponentiation + dichotomy bracket,
#  - for t>2025: bracket centered on tax_{t−1} with initial step = 20 + dichotomy.
# Reads a CSV of annual, already‐aggregated club emissions (time,E_gtco2).
# Limits calibration to 2025–2100, handles zero‐emissions years via backstop price.
################################################################################################

using Pkg
# Activate and instantiate the project
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

using Mimi, MimiFAIRv2, DataFrames, CSVFiles, CSV

# Load the NICE2020 module and its helpers
include(joinpath(@__DIR__, "..", "src", "nice2020_module.jl"))
include(joinpath(@__DIR__, "..", "src", "helper_functions.jl"))

# -------------------------------------------------------------------
# Participation scenario
include(joinpath(@__DIR__, "..", "data", "parameters.jl"))
const scenario_name     = :Union
const policy_scenario   = scenario_index[scenario_name]
const participation_vec = club_country[policy_scenario, :]
# -------------------------------------------------------------------

# -------------------------------------------------------------------
# Target file (global aggregated): columns `time` (Int) and `E_gtco2` (Float64)
const target_csv = joinpath(@__DIR__, "data", "input", "E_Union_2020_2300.csv")
# -------------------------------------------------------------------

# Prepare a NICE2020 model instance in global_carbon_tax mode
function prepare_model(policy_scenario)
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :policy_scenario, policy_scenario)
    update_param!(m, :abatement, :control_regime, 1)
    return m
end

# Simulate and return aggregated club emissions in year t
function emission_global(m::Mimi.Model, tax_vec::Vector{Float64}, t::Int)
    update_param!(m, :abatement, :global_carbon_tax, tax_vec)
    run(m)
    em = m[:emissions, :E_gtco2]              # Matrix time×country
    return sum(em[t, :] .* participation_vec) # Sum only over participating countries
end

# Exponentiation + dichotomy for t₀ = 2025
function find_tax_for_year!(m::Mimi.Model, t::Int, target_t::Float64,
                            tax_vec::Vector{Float64}; tol_em::Float64=0.01)
    local_tax = copy(tax_vec)
    local_tax[t] = 100.0
    em0 = emission_global(m, local_tax, t)

    lower = local_tax[t]; upper = lower
    if em0 > target_t
        while true  # double until emissions < target
            lower = upper
            upper *= 2
            println("    [bracket ↑] lower=", lower, ", upper=", upper)
            local_tax[t] = upper
            if emission_global(m, local_tax, t) < target_t
                break
            end
        end
    else
        while true  # halve until emissions > target
            upper = lower
            lower /= 2
            println("    [bracket ↓] lower=", lower, ", upper=", upper)
            local_tax[t] = lower
            if emission_global(m, local_tax, t) > target_t
                break
            end
        end
    end

    while true  # dichotomy
        mid = 0.5*(lower + upper)
        local_tax[t] = mid
        em_mid = emission_global(m, local_tax, t)
        println("    [dicho] τ=", round(mid; digits=2),
                " → E=", round(em_mid; digits=4),
                " (target=", round(target_t; digits=4), ")")
        if abs(em_mid - target_t) < tol_em
            tax_vec[t] = mid
            return tax_vec
        elseif em_mid > target_t
            lower = mid
        else
            upper = mid
        end
    end
end

# Bracket ±init_step around tax_{t−1} + dichotomy for t>2025
function find_tax_for_year_step!(m::Mimi.Model, t::Int, target_t::Float64,
                                 tax_vec::Vector{Float64}; tol_em::Float64=0.01,
                                 init_step::Float64=20.0)
    guess = tax_vec[t-1]
    step  = init_step
    local_tax = copy(tax_vec)

    lower = max(0.0, guess - step)
    upper = guess + step

    while true
        local_tax[t] = lower
        em_l = emission_global(m, local_tax, t)
        if em_l > target_t
            break
        end
        println("    [exp-step ↓] lower=", lower, ", E=", round(em_l; digits=4))
        step *= 2
        lower = max(0.0, guess - step)
    end

    while true
        local_tax[t] = upper
        em_u = emission_global(m, local_tax, t)
        if em_u < target_t
            break
        end
        println("    [exp-step ↑] upper=", upper, ", E=", round(em_u; digits=4))
        step *= 2
        upper = guess + step
    end

    while true  # dichotomy
        mid = 0.5*(lower + upper)
        local_tax[t] = mid
        em_mid = emission_global(m, local_tax, t)
        println("    [dicho-step] τ=", round(mid; digits=2),
                " → E=", round(em_mid; digits=4))
        if abs(em_mid - target_t) < tol_em
            tax_vec[t] = mid
            return tax_vec
        elseif em_mid > target_t
            lower = mid
        else
            upper = mid
        end
    end
end

# Main execution

# 1) prepare model + dimensions
m_ref   = prepare_model(policy_scenario)
years   = collect(dim_keys(m_ref, :time))
T       = length(years)

# 2) allocate tax vector
tax_vec = zeros(Float64, T)

# 3) define 2025–2100 window
start_year = 2030
end_year   = 2100
start_idx  = findfirst(==(start_year), years)
end_idx    = findfirst(==(end_year),   years)
if start_idx===nothing || end_idx===nothing
    error("Years 2025–2100 not present in model time dimension.")
end

# 4) load + parse global CSV
df_glob = CSV.read(target_csv, DataFrame)
df_glob.time = Int.(df_glob.time)
sort!(df_glob, :time)
dict_glob = Dict(row.time => row.E_gtco2 for row in eachrow(df_glob))

# 5) build target_global vector
target_global = zeros(Float64, T)
for t in start_idx:end_idx
    y = years[t]
    if haskey(dict_glob, y)
        target_global[t] = dict_glob[y]
    else
        error("No target for year $y in CSV")
    end
end

# 6) get backstop price vector from the model
run(m_ref)
pbacktime = m_ref[:abatement, :pbacktime]

# 7) calibration loop with zero‐target shortcut
for t in start_idx:end_idx
    println("=== Calibration year ", years[t], " ===")

    if target_global[t] == 0.0
        tax_vec[t] = pbacktime[t]
        println("  target=0 → backstop price=", round(tax_vec[t]; digits=4))
        continue
    end

    if t == start_idx
        find_tax_for_year!(m_ref, t, target_global[t], tax_vec)
    else
        find_tax_for_year_step!(m_ref, t, target_global[t], tax_vec)
    end
    println("  -> optimal tax = ", round(tax_vec[t]; digits=4), " USD/tCO2")
end

# 8) export results
out = DataFrame(
    time       = years[start_idx:end_idx],
    global_tax = tax_vec[start_idx:end_idx]
)
CSV.write("cap_and_share/data/output/calibrated_global_tax_union.csv", out)
