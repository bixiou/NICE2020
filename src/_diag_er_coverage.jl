# Check that the Equal Right schedule now prices every country: the six
# economies the report omits, and the resulting club.
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

for c in (:ABW, :PYF, :HKG, :MAC, :TWN, :PSE)
    @printf("  %-4s  2025 charge = %6.1f USD/t\n", c, get(EQUAL_RIGHT_PRICE, c, NaN))
end
for esc in (:own, :common)
    tax = equal_right_tax_matrix(escalation = esc)
    mem = proposal_members(tax)
    @printf("escalation %-7s: %d of %d countries priced\n", esc, length(mem), NB_COUNTRY)
    unpriced = [string(COUNTRIES[c]) for c in 1:NB_COUNTRY if !(c in mem)]
    isempty(unpriced) || println("   still unpriced: ", join(unpriced, ", "))
end
