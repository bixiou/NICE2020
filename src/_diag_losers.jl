# Diagnostic: why do a few members end below their consumption under the
# proposal, in the "reduced emissions" variant, on the very criterion the solve
# targets?
#
# The solve makes every member indifferent with damages held at the schedule's
# temperature path; the reported gain is then computed with damages endogenous,
# where the tighter cap lowers warming. Countries whose Kalkuhl-Wenz linear
# coefficient is negative gain from warming over the relevant range, so less
# warming costs them. This script evaluates the same allocation both ways and
# prints the two gains next to each country's damage coefficients.
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

const WATCH = ["MNG", "ISL", "FIN", "RUS", "NOR", "CAN", "USA", "CHN", "IND", "NGA"]

function gains_of(P, rho, fixed_temp)
    rights = rights_from_rho(rho, P)
    cap    = vec(sum(rights, dims = 2))
    m = make_uniform_model(RECYCLE_SHARE, P.members; fixed_temp = fixed_temp)
    p, err = calibrate_price_to_cap(m, rights, cap; p_init = P.p_ref, tol = 1e-4,
                                    label = "diag-losers")
    run_uniform!(m, rights, p)
    tot  = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
    pop  = population(m)
    dam  = f64(m[:damages, :LOCAL_DAMFRAC_KW])
    temp = temperature(m)[YEAR_IDX[2100]]
    gain = Dict{String,Float64}()
    damm = Dict{String,Float64}()
    for c in 1:NB_COUNTRY
        e = string(COUNTRIES[c])
        c0 = P.cons[e]
        c0 == 0 && continue
        gain[e] = (npv_pop(@view(tot[:, c]), @view(pop[:, c])) - c0) / abs(c0) * 100
        damm[e] = dam[YEAR_IDX[2100], c] * 100
    end
    return gain, damm, temp, err
end

coeff = DataFrame(load(joinpath(@__DIR__, "..", "data", "country_damage_coefficients.csv")))
β1 = Dict(String(r.countrycode) => Float64(r.beta1_KW) for r in eachrow(coeff))

for name in ("Duflo", "EqualRight5")
    P   = build_proposal(name, name == "Duflo" ? proposal_tax_matrix(duflo_rate) :
                                                 equal_right_tax_matrix(escalation = :common))
    rho = saved_rho("B", P)
    rho === nothing && (@warn "no saved joint rho, skipping" name; continue)
    gp, dp, tp, _ = gains_of(P, rho, proposal_local_temp(P))   # damages pinned (as solved)
    ge, de, te, _ = gains_of(P, rho, nothing)                  # damages endogenous (as reported)
    @printf("\n=== %s: 2100 temperature %.3f C pinned / %.3f C endogenous (proposal %.3f C)\n",
            name, tp, te, P.temp_2100)
    println("  country   beta1_KW   gain pinned   gain endog.   difference   damages 2100 (pinned -> endog.)")
    for e in WATCH
        haskey(gp, e) || continue
        @printf("  %-8s %9.5f %12.4f%% %12.4f%% %12.4f%%   %6.2f%% -> %6.2f%%\n",
                e, get(β1, e, NaN), gp[e], ge[e], ge[e] - gp[e], dp[e], de[e])
    end
    # every member, so that the comparison is not cherry-picked
    mem  = [string(COUNTRIES[c]) for c in P.members]
    lose_e = [e for e in mem if haskey(ge, e) && ge[e] < -0.005]
    lose_p = [e for e in mem if haskey(gp, e) && gp[e] < -0.005]
    println("  members below -0.005%: ", length(lose_p), " with damages pinned (", join(lose_p, ", "),
            "), ", length(lose_e), " with damages endogenous (", join(lose_e, ", "), ")")
    neg = [e for e in mem if get(β1, e, 0.0) < 0]
    @printf("  members with beta1_KW < 0: %d; of which below -0.005%% when endogenous: %d\n",
            length(neg), count(e -> haskey(ge, e) && ge[e] < -0.005, neg))
end
