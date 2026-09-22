######################################################################################################################
# This file finds a suitable global carbon tax trajectory for the NICE2020 model
# with an emissions constraint instead of a temperature constraint.
######################################################################################################################

# Activate the project and make sure all packages we need are installed.
using Pkg
Pkg.activate(joinpath(@__DIR__, ".."))
Pkg.instantiate()

# Load required Julia packages.
using Mimi, MimiFAIRv2, DataFrames, CSVFiles

# --- Tax and budget start parameters ---
const tax_start_year        = 2035     # tax start year (2025, 2030…)
const evaluation_end_year   = 2100    # the end of the year for budgeting
const welfare_end_year      = parse(Int, get(ENV, "NICE_WELFARE_END", string(evaluation_end_year)))  # last year of well-being counted
                                      # (set to 2300, the model's horizon, so that the peak-warming ceiling is not
                                      # enforced through years whose welfare the objective ignores)
const emission_budget_limit = parse(Float64, get(ENV, "NICE_BUDGET_LIMIT", "1000"))  # GtCO2 from tax_start_year to evaluation_end_year
const temp_limit            = parse(Float64, get(ENV, "NICE_TEMP_LIMIT", "2.00"))
const use_budget            = get(ENV, "NICE_USE_BUDGET", "0") in ("1", "true")
# With budget_exact the budget is spent exactly (cumulative emissions = the limit)
# rather than being a ceiling: welfare includes damages, so a ceiling need not bind,
# and the search then returns the damage-driven optimum rather than a budget path.
const budget_exact          = get(ENV, "NICE_BUDGET_EXACT", "0") in ("1", "true")
const budget_start_year     = parse(Int, get(ENV, "NICE_BUDGET_START", string(tax_start_year)))  # first year counted in the budget
const ramp_up               = 10    # Number of periods the tax is linearly ramped up
rho = 0.003                      # pure rate of time preference (it discounts welfare_country,
                                 # i.e. CRRA utility, not consumption). Set so that it matches the
                                 # paper's 3% discounting OF CONSUMPTION: 1+r = (1+rho)(1+g)^eta with
                                 # eta = 1.5 and g = 1.78%/yr, the model's own growth of mean
                                 # consumption per capita over 2030-2100, gives rho = 0.31%.
                                 # (Was 0.015; 3% here would be r = 5.7%.) See src/_diag_growth.jl.

# budgets_ndc
#       AFR       AUS       CAN       CHI       CSA       EEU       FSU       IND       JPN       MEA       MEX       ODA       SKO       USA       WEU     World 
#  56.95208   2.61344   3.65085 151.63455  17.76320   4.58928  41.91670  65.30454   7.10390  74.08333  11.91108  71.24156   4.30500  31.41713  14.18151 558.66827 

# ---------------------------------------------------------

# ——————————————————————————————————————————————————————————————
# Choice of Scenario
# ——————————————————————————————————————————————————————————————
include(joinpath(@__DIR__, "..", "data", "parameters.jl"))

const scenario_name     = :All_World     # Choice of scenario by name (:All_World, :All_Except_Oil_Countries, :Optimistic, :Generous_EU, :Partnership, :Union) :KOR
const policy_scenario   = scenario_index[scenario_name]
const participation_vec = club_country[policy_scenario, :]

# ——————————————————————————————————————————————————————————————

# Make directory to optionally save tested pathways outputs
output_directory_test_2deg_global = joinpath(@__DIR__, "..", "budget_ndc_CHN") # test_2deg_global_exp
mkpath(output_directory_test_2deg_global)

println("Test global carbon tax runs")

# Creates a global tax trajectory from starting level and growth rate,
# Runs the model with this carbon tax trajectory and outputs yearly emissions and welfare
function test_global_exp_c_tax(tax_start_value_test, g_rate_test)
    # we pass tax_start_year instead of 2020
    full_co2_tax = exp_tax_trajectory(tax_start_value = tax_start_value_test, g_rate = g_rate_test, year_tax_start = tax_start_year, year_tax_end = 2200, ramp_up = ramp_up)[1:length(dim_keys(nice_v2, :time))]
    # ---  add zeros before tax_start_year to always have nb_steps elements
    # n_pre = tax_start_year - 2020
    # full_co2_tax = vcat(zeros(n_pre), full_co2_tax)[1:length(dim_keys(nice_v2, :time))]
    # ---------------------------------------------------------

    update_param!(nice_v2, :abatement, :global_carbon_tax, full_co2_tax)
    run(nice_v2)

    # — Country-by-country aggregation on selected club only —
    emissions_matrix = nice_v2[:emissions, :E_gtco2]        # (time × country)
    welfare_matrix   = nice_v2[:welfare,   :welfare_country]# (time × country)
    temperature      = nice_v2[:temperature, :T]

    emissions = emissions_matrix * participation_vec
    welfare   = welfare_matrix   * participation_vec

    return emissions, welfare, temperature
end

# Load NICE2020 source code.
include(joinpath(@__DIR__, "..", "src", "nice2020_module.jl"))
include(joinpath(@__DIR__, "..", "src", "helper_functions.jl"))

# Get baseline instance of the model
nice_v2 = MimiNICE2020.create_nice2020()
update_param!(nice_v2, :switch_recycle, 0)             # Switch carbon taxation recycling off 
update_param!(nice_v2, :abatement, :control_regime, 1) # 1 = global_carbon_tax

# We update policy_scenario so that the model knows which country club to apply.
update_param!(nice_v2, :policy_scenario, policy_scenario)

# Get number of time steps in the model
nb_steps   = length(dim_keys(nice_v2, :time))
years_vec  = collect(2020:2020+nb_steps-1)

mask = (years_vec .>= tax_start_year) .& (years_vec .<= evaluation_end_year)
discount = (1 .+ rho) .^ collect(0:(evaluation_end_year - tax_start_year))
bmask     = (years_vec .>= budget_start_year) .& (years_vec .<= evaluation_end_year)   # budget window
wmask     = (years_vec .>= tax_start_year) .& (years_vec .<= welfare_end_year)
wdiscount = (1 .+ rho) .^ collect(0:(welfare_end_year - tax_start_year))

# === Zoom-progressive search with carbon budget constraint ===
const n_zoom    = parse(Int, get(ENV, "NICE_N_ZOOM", "3"))       # number of zoom iterations
const n_points  = 11      # 11×11 grids
# initial bounds 
const start_first  = 0
const start_last   = 2000
const g_rate_first = parse(Float64, get(ENV, "NICE_B_MIN", "0.0"))   # settable, so an interrupted
                                                                     # search can resume from a later level
const g_rate_last  = parse(Float64, get(ENV, "NICE_B_MAX", "0.20"))
start_min, start_max = start_first, start_last
rate_min,  rate_max  = g_rate_first, g_rate_last

best_welfare = -Inf
best_params  = (NaN, NaN)

if use_budget && budget_exact
    # For each growth rate B, the start level A that spends the budget exactly:
    # regula falsi (Illinois variant), cumulative emissions falling monotonically
    # in A. Then zoom on B for the highest welfare. A 2-D grid cannot hit an
    # equality, which is why this mode searches over B alone.
    budget_gap(A, B) = sum(test_global_exp_c_tax(A, B)[1][bmask]) - emission_budget_limit
    function A_for_budget(B; lo = Float64(start_first), hi = Float64(start_last),
                          tol = 0.5, max_iter = 40)          # tol in GtCO2
        f_lo, f_hi = budget_gap(lo, B), budget_gap(hi, B)
        (f_lo > 0 && f_hi < 0) || return NaN                  # not attainable in [lo, hi]
        side = 0
        for it in 1:max_iter
            A = (lo * f_hi - hi * f_lo) / (f_hi - f_lo)
            f = budget_gap(A, B)
            abs(f) < tol && return A
            if f > 0                                         # too many emissions: raise A
                lo, f_lo = A, f
                side == 1 && (f_hi /= 2)
                side = 1
            else
                hi, f_hi = A, f
                side == -1 && (f_lo /= 2)
                side = -1
            end
        end
        return (lo + hi) / 2
    end
    B_min, B_max = g_rate_first, g_rate_last
    for zoom in 1:n_zoom
        global B_min, B_max, best_welfare, best_params
        println("Zoom niveau $zoom (budget exact): B∈[$B_min,$B_max]")
        B_vals = range(B_min, stop = B_max, length = n_points)
        wel = fill(-Inf, n_points); As = fill(NaN, n_points)
        for (j, B) in enumerate(B_vals)
            A = A_for_budget(B)
            isnan(A) && continue
            _, welfare, _ = test_global_exp_c_tax(A, B)
            w = sum(welfare[wmask] ./ wdiscount)
            As[j], wel[j] = A, (isfinite(w) ? w : -Inf)
            println("    B = ", round(B, digits = 5), ": A = ", round(A, digits = 2), ", welfare = ", w)
            flush(stdout)
        end
        j = argmax(wel)
        println("  → meilleur A=$(As[j]), B=$(B_vals[j]), welfare=$(wel[j])")
        flush(stdout)
        if wel[j] > best_welfare
            best_welfare, best_params = wel[j], (As[j], B_vals[j])
        end
        B_min, B_max = B_vals[max(j - 1, 1)], B_vals[min(j + 1, n_points)]
    end
else
for zoom in 1:n_zoom
    # `global`: the loop both reads and rewrites these bounds, and a top-level
    # loop in a script would otherwise make them local (and so undefined here)
    global start_min, start_max, rate_min, rate_max, best_welfare, best_params
    println("Zoom niveau $zoom: domaine A∈[$start_min,$start_max], B∈[$rate_min,$rate_max]")
    A_vals = range(start_min, stop=start_max, length=n_points)
    B_vals = range(rate_min,  stop=rate_max,  length=n_points)

    # grille de scores
    welfare_grid = fill(-Inf, n_points, n_points)

    for (i, A) in enumerate(A_vals), (j, B) in enumerate(B_vals)
        # simulation
        emissions, welfare, temperature = test_global_exp_c_tax(A, B)

        # we restrict to the years [tax_start_year, evaluation_end_year]
        emis_zoom = emissions[bmask]
        wel_zoom  = welfare[wmask] ./ wdiscount

        # carbon budget constraint or temperature ceiling
        if ((use_budget & (sum(emis_zoom) <= emission_budget_limit)) | (!use_budget & (maximum(temperature) < temp_limit))) # temperature[evaluation_end_year - 2020 + 1]
            # a run that breaks down numerically must not win: argmax ranks NaN above every number
            w = sum(wel_zoom)
            welfare_grid[i, j] = isfinite(w) ? w : -Inf
        end
        # sinon reste -Inf
    end

    # detect the max in the grid
    max_idx = argmax(welfare_grid)
    ci = CartesianIndices(welfare_grid)[max_idx]
    i_max, j_max = Tuple(ci)
    best_A, best_B = A_vals[i_max], B_vals[j_max]
    best_val       = welfare_grid[i_max, j_max]
    println("  → meilleur A=$(best_A), B=$(best_B), welfare=$best_val")
    flush(stdout)  # progress visible when the output is redirected to a file

    # memorize if it's the best overall
    if best_val > best_welfare
        best_welfare = best_val
        best_params  = (best_A, best_B)
    end

    # determining the mini-rectangle around max and its neighbors
    i_min = max(i_max-1, 1); i_max_ = min(i_max+1, n_points)
    j_min = max(j_max-1, 1); j_max_ = min(j_max+1, n_points)
    A_neighbors = [A_vals[ii] for ii in i_min:i_max_]
    B_neighbors = [B_vals[jj] for jj in j_min:j_max_]

    start_min, start_max = minimum(A_neighbors), maximum(A_neighbors)
    rate_min,  rate_max  = minimum(B_neighbors), maximum(B_neighbors)
end

end   # use_budget && budget_exact

# --- Checking total emissions for the optimum path ---
emissions_opt, welfare_opt, temperature_opt = test_global_exp_c_tax(best_params[1], best_params[2])

# We keep only the period [tax_start_year, evaluation_end_year]
emis_budget = emissions_opt[bmask]

total_emis_opt = sum(emis_budget)
println("Total emissions of  ", budget_start_year, " to ", evaluation_end_year,
        " for the scenario : ", total_emis_opt, " GtCO2")

println("\n=== Final result ===")
println("Best path: initial tax = $(best_params[1]), growth = $(best_params[2])")
println("Total discounted welfare = $best_welfare")

# Saving the results
save(joinpath("data","uniform_exp_tax_path_params.csv"),
     DataFrame(path=collect(best_params)); header=false)

# The winning path as p*: the very vector the search evaluated for it (ramp-up,
# plateau and all), so that its peak warming is the one checked above. Read by
# read_price_path in src/equivalent_rights_proposals.jl; src/_write_exp_path.jl
# rebuilds the same file from the saved parameters.
let path = exp_tax_trajectory(tax_start_value = best_params[1], g_rate = best_params[2],
                              year_tax_start = tax_start_year, year_tax_end = 2200,
                              ramp_up = ramp_up)
    out = joinpath(@__DIR__, "data", "output", "calibrated_global_exp.csv")
    save(out, DataFrame(time = collect(2020:(2020 + length(path) - 1)), global_tax = path))
    println("Written: ", out)
end

# # Save selected carbon tax pathway to CSV
# tax_path = parse.(Float64, split(tax_path[1], '_'))
# save(joinpath("data","uniform_exp_tax_path_params.csv"),
#      DataFrame(path=tax_path); header=false)

# Save emissions trajectory
# save(joinpath("data","emissions_ndc_CHN.csv"), DataFrame(emissions=emissions_opt, year=collect(2020:2300)); header=false)

# Extract corresponding welfare value
# welfare_value_path = tot_welfare_disc[tot_welfare_disc.value .== maximum(tot_welfare_disc.value), :value]

emissions_matrix = test_global_exp_c_tax(232, .0416)
# save(joinpath("data","emissions_ndc_WEU_all.csv"), DataFrame(emissions_matrix, :auto); header=false)
em, wel, temp = test_global_exp_c_tax(232, .0416)

# World max 1.50°C in 2100, ramp up 2025-30: 408, .0088; carbon budget: 183
# World max 1.80°C in 2100, ramp up 2030-30: 176, .0168; carbon budget: 855
# World max 2.00°C in 2100, ramp up 2030-30: 184, .002 ; carbon budget: 1349
# World max 1.80°C in 2100, ramp up 2025-30: 216, .0128; carbon budget: 600
# World max 2.00°C in 2100, ramp up 2025-30: 184, 0.0088   ; carbon budget:  950
# TODO! display carbon budget when !use_budget and temp_max, temp 2100 when use_budget