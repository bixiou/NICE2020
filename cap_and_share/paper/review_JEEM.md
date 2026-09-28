# Referee report — *International Transfers or Differentiated Carbon Prices?*

Submitted to the *Journal of Environmental Economics and Management*. Report written 28 September 2026 on the version in `paper.tex` (main text about 5,700 words, three main-text exhibits, online appendix).

**Recommendation: major revision.**

## Summary

Recent proposals for international carbon pricing differentiate national prices by income, on the grounds that cross-border transfers are politically infeasible: the IMF price floor, Wolfram et al. (2025), Banerjee, Duflo and Greenstone (2025) and Equal Right. The paper argues that differentiation is itself a transfer. It makes four contributions:

1. **Static equivalence (Proposition 1).** Any schedule of national prices leaves each country exactly as well off as a uniform price combined with tradable rights equal to its emissions under the schedule, minus a Harberger (Bregman) term divided by the price. To first order, the equivalent allocation is grandfathering on the emissions the schedule itself induces.
2. **Dominance (Proposition 2).** Granting those emissions as rights makes every country weakly better off under the uniform price, so the uniform price with equivalent rights Pareto-dominates differentiation.
3. **Dynamic version (Proposition 3).** Rights are weighted by the discounted price and, under Hotelling pricing, reduce to a carbon budget.
4. **Quantification in NICE** (179 countries, income deciles, FaIR):
   - indifference curves between autarky prices and rights for seven economies, showing that the equivalence runs one way (a zero price cannot compensate low emitters as an equal per capita share would);
   - equivalent rights for the three proposals. Replacing them with a uniform price and equivalent rights cuts coalition emissions by about 5% (13% for Equal Right) with no member worse off, or raises every member's consumption by about 0.05% at unchanged emissions.

The paper is clearly written, timely and directly relevant to a live policy debate. The formula is simple enough to be used in negotiations, and the numerical work is careful: a joint solve with price recalibration, damages held fixed to isolate the efficiency channel, and two welfare criteria. The one-way nature of the equivalence and the Equal Right result are genuinely interesting. My concerns are about how large the contribution is, and whether the evidence supports the paper's main policy claim.

## Major comments

**1. The theoretical contribution is modest, and the paper should be explicit about where its value lies.**
- Proposition 1 is essentially an accounting identity: equating consumption across the two regimes gives r = e\* + [a(e\*) − a(e^A)]/p\* directly.
- Proposition 2 is Montgomery (1972) applied to a particular starting allocation, as the text acknowledges.
- The dynamic proposition is the same first-order argument applied period by period.

None of this is wrong, and simple results can be valuable. But a JEEM reader will ask what they learn beyond "differentiated prices are grandfathering on counterfactual emissions". I would reframe the contribution around the parts that are not obvious:
- the one-way nature of the equivalence (the bound on what any non-negative price can offer a low emitter);
- the price weighting of the dynamic allocation, and when it departs from a carbon budget (the numerical benchmark price path is not Hotelling, since it grows at 1.7% a year against a 3% discount rate);
- the quantification for actual proposals.

The introduction still leads with the static equivalence as the first result.

**2. The central policy claim is not quantified.**
- The paper's argument is that differentiation "entails implicit transfers of the same first-order size" as explicit ones, so the infeasibility of transfers cannot justify it. Yet the main text never reports the size of these implicit transfers, in dollars or as a share of GDP, for any country or proposal.
- The first-order transfer, p\*(r\*_i − e\*_i), is directly available from the runs. A table of implicit transfers per year for the ten economies of Table 2, compared with, for example, the $100bn climate-finance goal or the $107–169bn equal-cost transfers of Le Moigne et al. (2026), would turn the paper's thesis from an assertion into a result.

**3. The efficiency cost of differentiation is small, which cuts both ways.**
- At unchanged emissions, the uniform price raises members' consumption by about 0.05% (Banerjee et al.) and 0.04% (Wolfram et al.) of NPV consumption.
- A reader sympathetic to differentiation can conclude from this that differentiation buys equity at a negligible efficiency cost, and that avoiding explicit, visible transfers is worth 0.05% of consumption.
- The paper needs to engage with this reading. The proponents' objection to transfers is about their political visibility and enforceability, not their size. Showing that the distributive outcomes are equivalent does not by itself refute an argument about visibility.
- The survey evidence cited in the conclusion is relevant but thin as a rebuttal. It is the authors' own work, and stated support in surveys is not revealed feasibility in negotiations.
- Two points would strengthen the argument:
  - the emissions framing: a 5% cut in coalition emissions, about 0.02°C, is a clearer benefit than 0.05% of consumption;
  - the result that gains grow steeply with price dispersion (13% for Equal Right), which the paper mentions but does not develop in the main text.

**4. The quantitative results depend on assumptions the paper should test.**
- **Price paths.** Holding all tiers at their 2030 levels and raising them by 5% a year indefinitely is the authors' assumption. The proposals themselves foresee graduation into a uniform price. Since the surplus is second order in price dispersion, a convergence scenario could shrink it substantially. This is the most important sensitivity to report.
- **Abatement costs.** In NICE, every country has the same abatement-rate curve and differs only through its emission intensity (the text says so). Differences in marginal abatement costs are the textbook source of gains from uniform pricing. With a common curve, the efficiency gain comes purely from price dispersion, and the implicit transfers scale with emissions only. The paper should discuss how heterogeneous costs would change the equivalent rights and the surplus, or add a variant with heterogeneous costs.
- **NPV window and discounting.** Results are computed over 2025–2100 at 3%, with population weighting (total utilitarianism). Truncating at 2100 matters when prices and rights differ sharply late in the century. A variant with another discount rate and a longer horizon, or at least a statement of how much weight falls after 2070, would help.
- **Emissions basis.** The runs measure emissions on a consumption basis (carbon footprints), but the main text does not say so. Whether rights are defined on territorial or footprint emissions matters for the policy reading and for comparison with the proposals, which price territorial emissions. The paper should state this and justify it.

**5. There are too many variants, and the welfare variant muddies the message.**
- The main text alternates between consumption and welfare criteria, joint and isolated solves, and reduced-emissions and increased-consumption uses of the surplus. The appendix adds two further sharing rules and two price paths for Equal Right.
- The welfare variant yields emissions cuts of 27–35%, which the paper itself says are driven by the value of within-country redistribution through carbon dividends, not by uniform pricing. That is, it measures the absence of any other redistributive instrument in the model.
- I would keep the welfare variant only as a robustness check on the equivalent allocations at π = 1 (where it agrees within 0.03), and drop its surplus figures from the main text.

**6. The Equal Right analysis is interesting but needs care in interpretation.**
- The result that pooling revenue globally makes differentiation detrimental to low emitters (DRC, Nigeria) is striking and deserves more prominence.
- However, reading a negative equivalent ratio (−0.34 for the United States) as "a net duty to remove carbon … in line with historical responsibility" is a normative gloss on what is simply the net contribution the fund requires. I suggest describing it as the net payment into the fund implied by the schedule, and leaving the normative link to the reader.
- The decomposition of the dividend's value into a composition effect (0.75) and the higher price under reduced emissions (0.85) is useful. It should also be given for the increased-consumption allocation, where the second effect disappears.

## Minor comments

1. The introduction and Section 4.2 say "seven diverse economies", but Table 1 lists eight (it includes Turkey, which appears in the welfare-variant figure but not in Figure 1).
2. The footnote on the Wolfram et al. coalition lists 22 members for "47 countries". Explain how the EU and any economies missing from NICE are counted.
3. The abstract cites Parry et al. (2021), but the IMF floor is never run as such: it enters only through the Wolfram et al. tiers, with a different coalition. Say so, or drop the citation from the list of schedules replaced.
4. Section 4 normalises rights by world average emissions per capita, Section 5 by the coalition's under the proposal, and the two sections use different uniform prices (the 1000 GtCO₂ benchmark against the calibrated coalition price). A sentence that contrasts the two designs explicitly would prevent confusion.
5. Corollary 1 (Hotelling case) is not used numerically, since the benchmark price grows more slowly than the discount rate. Say so where the benchmark is introduced, and indicate how far the price weighting moves the equivalent allocations away from cumulative-emission shares (Table 1 suggests little at π = 1).
6. Proposition 3 states an "if and only if" together with "≈ 0". State the approximation order explicitly.
7. Table 2 is solved at damages held at the proposal's temperature path but reports gains with damages endogenous. The note should say so, as the text does.
8. Mongolia, Finland, Iceland and Russia lose from avoided warming because of the Kalkuhl–Wenz damage specification. This is a property of the damage function and could be relegated to a footnote.
9. Equal Right is described as staying "within 1.5°C" in Section 5.1, while the proposal documents refer to 1.6°C. Check the source.
10. Acknowledgements and funding appear both in the title-page footnote and in a separate section. JEEM puts them on the title page only.
11. Several claims rely on the authors' own survey work. That is fine, but in the anonymised version the self-citations should read in the third person.

## Assessment

The paper asks a good question and gives a clean, usable answer. The quantitative work is competent and transparent, with code available. Its weaknesses are:
- a thin theoretical core;
- a central claim, that implicit transfers are as large as explicit ones, that is asserted rather than measured;
- no engagement with the fact that the measured efficiency cost of differentiation is small, which is the proponents' strongest counter-argument;
- results that hinge on untested assumptions: the price paths, common abatement costs, and the emissions basis.

These are fixable within the paper's current scope. Quantifying the implicit transfers (comment 2), adding a price-convergence scenario (comment 4), and reframing the contribution (comments 1 and 3) would make it a solid JEEM paper. Without them, I expect other referees to question whether the contribution clears the bar for a general field journal.

**Recommendation: major revision.**
