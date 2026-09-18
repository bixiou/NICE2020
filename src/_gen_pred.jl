# one-off: write the three predicted-rho versions of the combined table, nothing else
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))
props = [build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate)),
         build_proposal("Duflo",   proposal_tax_matrix(duflo_rate))]
load_prior_results!(props)
for (kind, stem) in ((:paper, "pred"), (:formula, "pred_formula"), (:now, "pred_now"))
    write_combined_table(joinpath(OUTPUT_BASE, "equivalent_rights_combined_$(stem).tex"), props;
                         label = "tab:equiv_rights", predicted = "Duflo", pred_kind = kind)
end
