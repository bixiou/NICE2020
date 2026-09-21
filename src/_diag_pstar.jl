# Sanity check of p* before the long re-run: the path itself, where it passes the
# backstop, and what the reference run does with it -- world emissions (the
# denominator of every rho is the world average, so it must stay positive over
# the window) and warming.
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

m    = REFERENCE_RUN
E    = vec(sum(country_emissions(m), dims = 2))
T    = temperature(m)
win  = [YEAR_IDX[y] for y in CALIB_YEARS]

println("p* and the backstop (USD/tCO2):")
for y in (2025, 2026, 2030, 2031, 2035, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
    t = YEAR_IDX[y]
    @printf("  %d  p* = %8.1f   backstop = %6.1f   world emissions = %6.2f GtCO2   T = %.3f\n",
            y, P_STAR[t], PBACKTIME[t], E[t], T[t])
end
above = [y for y in CALIB_YEARS if P_STAR[YEAR_IDX[y]] > PBACKTIME[YEAR_IDX[y]]]
neg   = [y for y in CALIB_YEARS if E[YEAR_IDX[y]] < 0]
@printf("p* above the backstop from: %s\n", isempty(above) ? "never (to 2100)" : string(first(above)))
@printf("world emissions negative in: %s\n", isempty(neg) ? "no year to 2100" : string(neg[1], "-", neg[end]))
@printf("peak warming %.3f C in %d; 2100 warming %.3f C\n",
        maximum(T), YEARS[argmax(T)], T[YEAR_IDX[2100]])
@printf("cumulative world emissions 2025-2100: %.0f GtCO2\n", sum(E[win]))

# What actually matters for rho: the price-weighted value of an equal per capita
# share, the denominator of eq. (rhohat_dyn). Negative world emissions are fine
# (an equal share of them is a removal duty); the valued sum must stay clearly
# positive, or every rho becomes ill-conditioned (near zero) or flips meaning.
w    = [NPV_DISC[k] * P_STAR[NPV_IDX[k]] for k in eachindex(NPV_IDX)]
vals = w .* E[NPV_IDX]
vpos, vneg = sum(v for v in vals if v > 0; init = 0.0), sum(v for v in vals if v < 0; init = 0.0)
@printf("price-weighted world budget 2030-2100: %.1f  (positive years %.1f, negative years %.1f: removals offset %.1f%%)\n",
        vpos + vneg, vpos, vneg, -vneg / vpos * 100)
