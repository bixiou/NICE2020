# Writes p*, the exponential 1.8C path, from the (A, B) that
# cap_and_share/find_global_exp_carbon_tax_buget_zoom.jl saves in
# data/uniform_exp_tax_path_params.csv (A on the first row, B on the second).
#
# The vector is rebuilt with the very call the search evaluated, so that p* is
# exactly the path whose peak warming the search checked -- ramp-up years,
# one-year plateau and post-2200 plateau included -- rather than a re-derived
# exponential.
#
#   julia --project=. src/_write_exp_path.jl
using Mimi, CSV, DataFrames      # helper_functions.jl annotates with Mimi types
include(joinpath(@__DIR__, "helper_functions.jl"))

const ROOT = joinpath(@__DIR__, "..")
params = CSV.read(joinpath(ROOT, "data", "uniform_exp_tax_path_params.csv"), DataFrame;
                  header = false)
A, B = Float64(params[1, 1]), Float64(params[2, 1])

# the same arguments as test_global_exp_c_tax in the search
path  = exp_tax_trajectory(tax_start_value = A, g_rate = B, year_tax_start = 2030,
                           year_tax_end = 2200, ramp_up = 5)
years = collect(2020:(2020 + length(path) - 1))
out   = joinpath(ROOT, "cap_and_share", "data", "output", "calibrated_global_exp.csv")
CSV.write(out, DataFrame(time = years, global_tax = path))

at(y) = path[y - 2019]
println("A = $A (2030), B = $B a year; written ", out)
for y in (2025, 2026, 2030, 2031, 2040, 2050, 2070, 2100)
    println("  ", y, ": ", round(at(y), digits = 1))
end
