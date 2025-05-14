################################################################################################
# calibrate_tax.jl
#
# Calibrates the country-by-country carbon tax (direct_country_tax) in NICE2020
#    pour faire correspondre une trajectoire cible d'émissions par pays et par année
################################################################################################

using Pkg
# Active le projet et installe si nécessaire
auto_dir = joinpath(@__DIR__, "..")
Pkg.activate(auto_dir)
Pkg.instantiate()

include(joinpath(@__DIR__, "..", "src", "nice2020_module.jl"))
include(joinpath(@__DIR__, "..", "src", "helper_functions.jl"))

using Mimi, MimiFAIRv2, DataFrames, CSVFiles, CSV

# --------------------------------------------
# SECTION À MODIFIER :
# Indique le chemin vers ton fichier CSV d'émissions cible
# Format attendu : time,country,E_gtco2 (extraction NICE2020)
# --------------------------------------------
const target_csv = joinpath(@__DIR__, "..", "results", "uniform_tax_example", "no_revenue_recycling", "country_output", "industrial_co2_emissions.csv")
# --------------------------------------------

# Charge la trajectoire cible dans un DataFrame trié
function load_target_emissions(path::String)
    df = CSV.File(path) |> DataFrame
    sort!(df, [:time, :country])
    return df
end

# Calibrage de direct_country_tax pour coller à target_df
function calibrate_tax(
    target_df::DataFrame;
    k::Float64=0.5,       # gain proportionnel de l'algorithme (0<k<=1)
    tol::Float64=1e-3,    # tolérance relative max sur les émissions
    max_iters::Int=50     # itérations max
)
    # 1) Initialise le modèle NICE2020 en régime direct (control_regime = 4)
    m = MimiNICE2020.create_nice2020()
    update_param!(m, :abatement, :control_regime, 4)

    # Dimensions du modèle
    years = dim_keys(m, :time)
    countries = dim_keys(m, :country)
    T = length(years)
    C = length(countries)

    # 2) Prépare la matrice cible (T×C)
    # Dictionnaires de correspondance : String(country) -> index et year -> index
    idx_year    = Dict(y => t for (t, y) in enumerate(years))
    idx_country = Dict(string(c) => i for (i, c) in enumerate(countries))
    target_mat = zeros(T, C)
    for row in eachrow(target_df)
        t = idx_year[row.time]
        c = idx_country[String(row.country)]
        target_mat[t, c] = row.E_gtco2
    end

    # 3) Initialisation de direct_country_tax à zéro
    tax_mat = zeros(T, C)

    # 4) Boucle itérative
    ε0 = 1e-12
    for iter in 1:max_iters
        println("=== Itération \$iter ===")
        # 4.1 Applique direct_country_tax et simule
        update_param!(m, :abatement, :direct_country_tax, tax_mat)
        run(m)
        # 4.2 Récupère les émissions modélisées
        em_mat = m[:emissions, :E_gtco2]
        # 4.3 Calcule l'erreur relative maximale
        rel_err = abs.(em_mat .- target_mat) ./ (target_mat .+ ε0)
        max_err = maximum(rel_err)
        println("  erreur relative maximale = \$(round(max_err, sigdigits=4))")
        # 4.4 Critère d'arrêt
        if max_err < tol
            println("Convergence atteinte (tol = \$tol)")
            break
        end
        # 4.5 Correctif additive de la taxe
        # On ajuste directement la taxe en fonction de l'écart relatif
        tax_mat .+= k .* ((em_mat .- target_mat) ./ (target_mat .+ ε0))
        # On empêche les valeurs négatives
        tax_mat .= max.(tax_mat, 0.0)
    end

    # 5) Résultat final : recalcul de country_carbon_tax via direct_country_tax calibré
    update_param!(m, :abatement, :direct_country_tax, tax_mat)
    run(m)
    final_tax = m[:abatement, :country_carbon_tax]

    return tax_mat, final_tax, years, countries
end

# ========== Exécution principale ==========
# Charge ta trajectoire cible explicitement définie ci-dessus
# Utilise target_csv

df_target = load_target_emissions(target_csv)
# Lance le calibrage
taxtarget_mat, country_tax_mat, years, countries = calibrate_tax(df_target; k=0.5, tol=1e-3, max_iters=50)

# Prépare le DataFrame de sortie (taxe par pays/année)
out = DataFrame(time=Int[], country=String[], tax=Float64[])
for (t, y) in enumerate(years), (c, cn) in enumerate(countries)
    push!(out, (y, String(cn), country_tax_mat[t, c]))
end
# Sauvegarde du résultat
CSV.write("calibrated_tax.csv", out)
println("=> Taxe calibrée enregistrée dans calibrated_tax.csv")
CSV.write("calibrated_tax.csv", out)
println("=> Taxe calibrée enregistrée dans calibrated_tax.csv")
