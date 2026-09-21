# Driver for the Sept 2026 redefinition of the equivalence solves
# (criterion x way x surplus type; see solve_cell! in equivalent_rights_proposals.jl).
#
#   NICE_TARGET=cons|ede  NICE_PROPOSALS=Wolfram,Duflo,EqualRight5,EqualRight \
#   julia --project=. src/run_solves_sept2026.jl
#
# The isolated rho (option A) are unchanged by the redefinition, so they are read
# from disk and only their variants are rerun; the joint solves are redone from
# scratch (warm-started from the previous joint vector).
ENV["NICE_AUTORUN"] = "0"
haskey(ENV, "NICE_WORKERS") || (ENV["NICE_WORKERS"] = "1")   # option A is not solved here
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

const WANT    = String.(split(get(ENV, "NICE_PROPOSALS", "Wolfram,Duflo,EqualRight5,EqualRight"), ","))
const METHODS = String.(split(get(ENV, "NICE_METHODS", "A,B"), ","))

props = [build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate)),
         build_proposal("Duflo",   proposal_tax_matrix(duflo_rate)),
         build_proposal("EqualRight",  equal_right_tax_matrix(escalation = :own)),
         build_proposal("EqualRight5", equal_right_tax_matrix(escalation = :common))]
load_prior_results!(props)

# Cells already solved under the current criterion, one per line as
# "target/method/scenario". With NICE_RESUME=1 they are skipped: their results
# are on disk and `load_prior_results!` has just read them back, so the tables
# written at the end of each cell stay complete. This machine reboots without
# warning, and a run that starts from scratch each time never reaches the end.
const DONE_FILE = get(ENV, "NICE_DONE_FILE",
                      joinpath(@__DIR__, "..", "logs", "tu_done.txt"))
const RESUME    = get(ENV, "NICE_RESUME", "0") in ("1", "true")
done_cells() = isfile(DONE_FILE) ? Set(strip.(readlines(DONE_FILE))) : Set{String}()

for name in WANT, m in METHODS
    cell = "$(TARGET)/$(m)/$(name)"
    if RESUME && cell in done_cells()
        @info "cell already solved under this criterion, skipping" cell
        continue
    end
    P = only(filter(Q -> Q.name == name, props))
    t0 = time()
    @info "solving (Sept 2026 definitions)" scenario = name option = m TARGET
    # NICE_VARIANTS_ONLY=1 reruns the variants from the saved rho of both solves.
    # Option A was rerun from its saved rho because the Sept 2026 redefinition
    # changed only the variants. A change of *criterion* does change the isolated
    # rho itself -- a country's indifference condition is weighted by its
    # population once welfare is total rather than per capita -- so NICE_A_FULL=1
    # solves option A from scratch too.
    vo     = get(ENV, "NICE_VARIANTS_ONLY", "0") in ("1", "true")
    a_full = get(ENV, "NICE_A_FULL", "0") in ("1", "true")
    solve_cell!(P, m, props; variants_only = vo || (m == "A" && !a_full))
    open(DONE_FILE, "a") do io
        println(io, cell)
    end
    @printf("### %s/%s/%s done in %.1f min\n", TARGET, m, name, (time() - t0) / 60)
    flush(stdout)
end
