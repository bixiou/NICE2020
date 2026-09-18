# one-off: write the three predicted-rho versions of the targets table (paper's Table 4), nothing else
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))
props = [build_proposal("Wolfram",     proposal_tax_matrix(wolfram_rate)),
         build_proposal("Duflo",       proposal_tax_matrix(duflo_rate)),
         build_proposal("EqualRight5", equal_right_tax_matrix(escalation = :common))]
load_prior_results!(props)   # report_order sorts on RESULTS
write_pred_target_tables(props, read_results_for("_ede", props), RESULTS)
