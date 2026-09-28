# What pure rate of time preference is consistent with the paper's 3% discount
# rate on consumption? Ramsey: 1 + r = (1 + rho)(1 + g)^eta, so rho is pinned by
# the model's own per-capita consumption growth over the NPV window.
ENV["NICE_AUTORUN"] = "0"; ENV["NICE_WORKERS"] = "1"
include(joinpath(@__DIR__, "equivalent_rights_proposals.jl"))

m    = REFERENCE_RUN
tot  = f64(m[:quantile_recycle, :sum_conso_pc_post_recycle]) ./ NB_QUANTILE
pop  = population(m)
ede  = f64(m[:welfare, :cons_EDE_global])

c_pc = vec(sum(tot .* pop, dims = 2)) ./ vec(sum(pop, dims = 2))   # world mean, per capita
i0, i1 = YEAR_IDX[first(YEARS_NPV)], YEAR_IDX[last(YEARS_NPV)]
yrs = last(YEARS_NPV) - first(YEARS_NPV)

g_mean = (c_pc[i1] / c_pc[i0])^(1 / yrs) - 1
g_ede  = (ede[i1]  / ede[i0])^(1 / yrs) - 1
for (lbl, g) in (("mean consumption per capita", g_mean), ("EDE consumption", g_ede))
    rho = (1 + DISCOUNT_RATE) / (1 + g)^ETA - 1
    @printf("%-28s growth %.4f%%/yr over %d-%d  ->  rho = (1+%.2f%%)/(1+g)^%.1f - 1 = %.4f%%\n",
            lbl, g * 100, first(YEARS_NPV), last(YEARS_NPV), DISCOUNT_RATE * 100, ETA, rho * 100)
end
@printf("linearised (r = rho + eta g): rho = %.4f%% with mean consumption growth\n",
        (DISCOUNT_RATE - ETA * g_mean) * 100)
