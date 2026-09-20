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

for name in WANT, m in METHODS
    P = only(filter(Q -> Q.name == name, props))
    t0 = time()
    @info "solving (Sept 2026 definitions)" scenario = name option = m TARGET
    solve_cell!(P, m, props; variants_only = (m == "A"))
    @printf("### %s/%s/%s done in %.1f min\n", TARGET, m, name, (time() - t0) / 60)
    flush(stdout)
end
