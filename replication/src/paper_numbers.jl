################################################################################
# In-text numbers of Section 5.3 that no table carries.
#
# 1. "The transfers that differentiation hides" (also the abstract and the
#    conclusion): the implicit transfers of the Banerjee, Duflo & Greenstone
#    schedule, p_t (E^A_it - E*_it), i.e. the net sales of rights at the
#    coalition price under grandfathering on the schedule's own emissions,
#    R_it = E^A_it. The cap is then the coalition's emissions under the
#    schedule, so the coalition price is the proposal's p_ref.
#    Writes cap_and_share/output/implicit_transfers_duflo.csv and prints the
#    gross flows (sum of positive flows) in 2030, 2040 and 2050.
#
# 2. "A schedule with a global dividend": the decomposition of Equal Right's
#    dividend term, pbar/p1 = (pbar/p_ref) x (p_ref/p1), where pbar = D/E is the
#    emission-weighted mean price under the schedule, p_ref the coalition price
#    delivering the schedule's emissions and p1 the coalition price of the
#    reduced-emissions allocation (variant 1). Each year is weighted as in the
#    dividend term of the first-order formula for the DRC:
#    beta_t * (N_DRC,t / N_t) * E_t. Reads price_paths_B_equalright5.csv,
#    written by the joint solve of EqualRight5.
#
# These two results were first computed interactively (28 Sept 2026); this
# script rebuilds them with the code paths of write_transfers and
# write_variant_paths in src/equivalent_rights_proposals.jl.
#
#   NICE_RECYCLING=negishi NICE_TARGET=cons julia --project=. src/paper_numbers.jl
################################################################################
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

# ── 1. implicit transfers of the Banerjee et al. schedule ─────────────────────
let P = build_proposal("Duflo", proposal_tax_matrix(duflo_rate))
    rights = zeros(Float64, NB_STEPS, NB_COUNTRY)
    for c in P.members
        rights[:, c] .= P.emissions[:, c]            # grandfathering on E^A
    end
    cap  = vec(sum(rights, dims = 2))                # = the coalition's emissions under the schedule
    m    = make_uniform_model(RECYCLE_SHARE, P.members)
    p, _ = calibrate_price_to_cap(m, rights, cap; p_init = P.p_ref, tol = 3e-5,
                                  label = "implicit transfers/$(P.name)")
    run_uniform!(m, rights, p)
    tr  = f64(m[:revenue_recycle, :transfer])        # USD2017 per year, negative for net buyers
    gdp = f64(m[:grosseconomy, :YGROSS]) .* 1e6      # USD2017 per year
    df = DataFrame(country = string.(COUNTRIES), member = [c in P.members for c in 1:NB_COUNTRY],
                   npv_transfer = [npv(tr[:, c]) for c in 1:NB_COUNTRY],
                   npv_gdp = [npv(gdp[:, c]) for c in 1:NB_COUNTRY],
                   transfer_2030 = tr[YEAR_IDX[2030], :],
                   transfer_2050 = tr[YEAR_IDX[2050], :])
    out = joinpath(OUTPUT_BASE, "implicit_transfers_duflo$(TAG).csv")
    CSV.write(out, df)
    println("\nImplicit transfers of the Banerjee et al. schedule (USD2017 bn), written to ", out)
    for yr in (2030, 2040, 2050)
        @printf("  %d: gross flows %.1f\n", yr, sum(max.(tr[YEAR_IDX[yr], :], 0.0)) / 1e9)
    end
    for e in ("IND", "USA", "RUS", "NGA")
        c = only(findall(==(Symbol(e)), COUNTRIES))
        @printf("  2050 %s: %+.1f   NPV 2025-2100: %+.2f%% of GDP\n", e,
                tr[YEAR_IDX[2050], c] / 1e9, npv(tr[:, c]) / npv(gdp[:, c]) * 100)
    end
end

# ── 2. Equal Right: the dividend term and its decomposition ───────────────────
let P = build_proposal("EqualRight5", equal_right_tax_matrix(escalation = :common)),
    f = joinpath(OUTPUT_BASE, "price_paths_B_equalright5$(TAG).csv")
    if !isfile(f)
        @warn "no joint solve of EqualRight5 on disk: run it first" f
    else
        pp  = CSV.read(f, DataFrame)
        col(s) = Dict(Int(r.time) => Float64(r[s]) for r in eachrow(pp))
        pref, pbar, p1 = col(:p_ref), col(:pbar), col(:p_v1)
        cod = only(findall(==(:COD), COUNTRIES))
        w   = [NPV_DISC[k] * P.pop[t, cod] / P.club_pop[t] * P.club_emissions[t]
               for (k, t) in enumerate(NPV_IDX)]
        ratio(a, b) = sum(w[k] * a[YEARS[t]] for (k, t) in enumerate(NPV_IDX)) /
                      sum(w[k] * b[YEARS[t]] for (k, t) in enumerate(NPV_IDX))
        println("\nEqual Right (5%/yr), weights beta_t N_DRC,t/N_t E_t over $(NPV_SPAN):")
        @printf("  pbar/p_ref = %.3f   p_ref/p1 = %.3f   pbar/p1 = %.3f\n",
                ratio(pbar, pref), ratio(pref, p1), ratio(pbar, p1))
        for yr in (2030, 2050, 2075, 2100)
            @printf("  %d: p1 = %.0f, p_ref = %.0f, pbar = %.0f USD/tCO2\n", yr, p1[yr], pref[yr], pbar[yr])
        end
    end
end
