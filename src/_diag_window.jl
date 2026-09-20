# Diagnostic: is the joint "reduced emissions" solve exploiting the years that
# the welfare window (2030-2100) does not count, while prices start in 2025?
# Takes the saved joint allocation and reports each member's gap to its level
# under the proposal, on the 2030-2100 window (the one solved) and on 2025-2100.
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))
P = build_proposal("Wolfram", proposal_tax_matrix(wolfram_rate))
rho = saved_rho("B", P)
rho === nothing && error("no saved joint rho")
rights = rights_from_rho(rho, P)
cap = vec(sum(rights, dims = 2))
m = make_uniform_model(RECYCLE_SHARE, P.members; fixed_temp = proposal_local_temp(P))
p, err = calibrate_price_to_cap(m, rights, cap; p_init = P.p_ref, tol = 1e-4, label = "diag")
run_uniform!(m, rights, p)
tot  = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
# the proposal run, same configuration
mp = run_autarky!(make_autarky_model(RECYCLE_SHARE), P.tax)
totp = f64(mp[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
for (lbl, yrs) in (("2030-2100", 2030:2100), ("2025-2100", 2025:2100))
    idx = [YEAR_IDX[y] for y in yrs]; disc = [1/(1.03)^(y - first(yrs)) for y in yrs]
    gaps = Float64[]
    for c in P.members
        a = sum(tot[idx, c] .* disc); b = sum(totp[idx, c] .* disc)
        push!(gaps, (a - b) / abs(b) * 100)
    end
    w = [sum(P.pop[idx, c]) for c in P.members]; w ./= sum(w)
    @printf("%s: mean gap %+.4f%%  min %+.4f%%  max %+.4f%%  (%d of %d below -0.005%%)\n",
            lbl, sum(gaps .* w), minimum(gaps), maximum(gaps), count(<(-0.005), gaps), length(gaps))
end
@printf("cumulative club emissions: rights/proposal 2025-2100 = %.4f, 2030-2100 = %.4f\n",
        sum(cap[[YEAR_IDX[y] for y in 2025:2100]]) / sum(P.club_emissions[[YEAR_IDX[y] for y in 2025:2100]]),
        sum(cap[[YEAR_IDX[y] for y in 2030:2100]]) / sum(P.club_emissions[[YEAR_IDX[y] for y in 2030:2100]]))
