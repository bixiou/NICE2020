# The equivalence between differentiated prices and emission rights

**Date**: 09/29/2026
**Domain**: social_sciences/economics
**Taxonomy**: academic/research_paper
**Filter**: Active comments

---

## Overall Feedback

Here are some overall reactions to the document.

**Outline**

A clean idea — mapping any differentiated price schedule into the allocation of tradable rights that reproduces its distribution — carried through a static proposition, a dynamic extension, and a calibrated NICE exercise. The theory is simple and mostly right; the quantitative half is where the paper is fragile, because the criterion used in the simulations is not the one the propositions are stated for, the headline magnitudes ride on a single uncalibrated abatement-cost curvature, and the damage accounting is switched between the solve and the reported gains.

The central identity — that a price wedge is worth, in rights, the autarky emissions minus the Harberger triangle divided by the price — is correct, easy to communicate, and genuinely useful to negotiators. Mapping three live proposals into equivalent per-capita allocations, and showing that the correspondence is one-way (no non-negative price can deliver an equal per-capita share to a low emitter), is the paper's best material. The weaknesses are on the quantitative side: several reported magnitudes are properties of the NICE calibration or of accounting conventions rather than of the economics, and the paper is not consistently transparent about which is which.

**The propositions are stated for a linear-in-consumption objective but implemented with CRRA and inequality aversion**

Section 3.2 writes country $i$'s objective as $\sum_t \beta_t N_{it} c_{it}$ — linear in consumption — and the proof of Proposition 3 uses that linearity twice, once to convert per-capita gains into totals and once to sum period gains into a present value. The quantitative work does not use that objective. The benchmark price path is chosen to maximise discounted world welfare with $\eta = 1.5$ and a pure rate of time preference of $0.3\%$; the welfare variant evaluates countries by population-weighted EDE consumption with $\eta = 1.5$; and the footnote reconciling $3\%$ with $0.3\% + 1.5 \times 1.8\%$ is precisely the CRRA arithmetic that the linear objective rules out. With concave utility the indifference condition is not \eqref{eq:dyn_equiv} but $\sum_t \beta_t u'(c_{it}) N_{it} p^*_t (R_{it} - E^A_{it}) = 0$, so the weights on early versus late rights differ from the ones the paper uses, and $\hat\rho_i$ in \eqref{eq:rhohat_dyn} is the right object only if the marginal-utility path is treated as fixed across regimes. Either state Propositions 1–3 under a general concave $u$ and carry $u'$ through the weighting, or state explicitly that the discounted-price weighting is a Negishi-type linearisation valid because the regimes differ by fractions of a percent of consumption — and then show numerically that the two weightings give the same $\hat\rho_i$. Separately, since abatement costs are time-separable, the exact dynamic condition $\sum_t \beta_t [p^*_t (R_{it} - E^A_{it}) + N_{it} b_{it}] = 0$ follows immediately from \eqref{eq:req_exact} and should replace the first-order "if and only if", which cannot literally be an iff.

**Damages are fixed while solving and endogenous when reporting, so "no member loses" does not hold as stated**

The reduced-emissions solve of Section 5.2 holds each country's damages at the schedule's temperature path, then Section 5.3 and Table 2 report gains with damages endogenous. The Introduction carries the fixed-damage reading ("cuts coalition emissions by 4.8\% ... while leaving every member's consumption as high as under the schedule") but the body reports Mongolia losing $0.02\%$, and Online Appendix B adds Finland, Iceland and Russia under Equal Right. The two statements cannot both stand in the same paper without a qualifier, and the one that reaches the abstract is the stronger one. Compounding this, whether a country counts as a loser depends on a $0.005\%$ threshold set at four times the solver tolerance, while Finland and Iceland sit at $-0.004\%$ and $-0.002\%$ — so the loser count is an artefact of where the cutoff is drawn. Fix by reporting both accountings side by side in Table 2 (indifference gap at fixed damages, and the full gain with damages endogenous), by replacing loser counts with the distribution of members' gains (min, 5th percentile, median), and by qualifying the Introduction's claim to "at the schedule's damages". The decomposition of the reported gain into efficiency, redistribution and avoided damages should also appear in the main text, not only as a remark that the summary rows are computed with damages endogenous.

**The headline magnitudes are a function of one common abatement-cost curvature, with no sensitivity reported**

Section 4.1 confirms that NICE gives every country the same cost function in the abatement rate, $\theta_1 \mu^{\theta_2}$ with $\theta_2 = 2.6$ and a common backstop, so $\mu = (p/p^{\mathrm{back}})^{0.625}$ and the price elasticity $\varepsilon_i$ is identical everywhere. Every quantity the paper reports in levels — the $4.8\%$ and $13.4\%$ emission cuts, the $0.05\%$ and $0.17\%$ consumption gains, the size of $b_i$ — is then a deterministic function of $\theta_2$ and the backstop path, neither of which is defended against empirical marginal-abatement-cost evidence, and neither of which is varied anywhere in the paper. This matters because the platform assumption removes the very heterogeneity in marginal abatement costs that generates gains from permit trade in Montgomery; here all the wedge comes from the schedule itself. There is a way to turn this into a strength: to second order $b_i / |t_i| = \tfrac{1}{2}|1 - \pi_i|$, so the ratio of deadweight loss to implicit transfer is independent of the elasticity level and depends only on price dispersion. That ratio, not the levels, is what survives recalibration, and the "half a dollar per dollar" claim should be presented as the parameter-light result (with the gross-versus-two-sided transfer convention spelled out, since it doubles the number). At minimum, rerun the three proposals with $\theta_2 \in \{2.0, 2.6, 3.2\}$ and two backstop paths, and report ranges.

**The welfare variant measures something other than the inefficiency of differentiation**

Online Appendix D reports emission cuts of $34.5\%$, $27.2\%$ and $29.6\%$ against $4.8\%$, $4.8\%$ and $13.4\%$ in the consumption variant — a sevenfold difference on the same object. The appendix itself explains why: with an equal per-capita dividend and no other domestic redistributive instrument in NICE, raising the coalition price is valued by an inequality-averse criterion as within-country redistribution, so members' common gain keeps rising as the cap is tightened, and the solve only stops at a $27\%$ cut. That is not an efficiency gain from uniform pricing; it is the shadow value of a missing lump-sum instrument. Presenting the two as coequal "variants" throughout Sections 4–5, and reporting the welfare numbers in the main table, invites readers to treat $27\%$ as a plausible upper bound on what the instrument choice is worth. Either add a third run in which domestic redistribution is done by a non-carbon instrument calibrated to deliver the same within-country distribution in both regimes — which isolates the between-country effect under the inequality-averse criterion — or demote the welfare variant to a diagnostic, state in the abstract and Introduction that the consumption variant is the result, and label the appendix numbers as contaminated by the missing instrument.

**The equivalence is distributional; the policy conclusion needs feasibility to depend only on distribution**

Section 2's fourth rationale is that cross-border transfers are politically unavailable, and the paper answers that differentiation "cannot rest on the distributional implications of transfers, since differentiation is equivalent to transfers of the same size". That step needs an assumption the paper never states: that political feasibility is a function of the outcome distribution alone. The differentiation literature's claim is about the institutional form — implicit transfers require no appropriation, no foreign-exchange payment, no annual legislative vote, and no cross-border enforcement, whereas rights purchases require all four. The paper's own Table 2 makes the point sharply: India receives roughly \$185bn and the United States pays roughly \$93bn in a single year of the Banerjee et al. run, flows of a size that has never been agreed internationally. Related mechanisms are absent from the model and from the discussion: market power of a dominant seller such as India, compliance and enforcement, commitment over a 75-year horizon, and renegotiation. NICE also has no international trade, which removes leakage and terms-of-trade channels — yet "no trade" appears to have been dropped from the Conclusion's list of limitations, even though two of the four rationales in Section 2 require trade to operate. Restore it, add a paragraph in Section 6 that engages the institutional-form argument on its merits rather than answering it with survey evidence, and sharpen the positioning relative to Fleurbaey et al. (2025), whose result covers the dominance direction, so that the claimed novelty rests squarely on the closed-form map and its quantification.

**The proposals are reconstructed with consequential choices that are never varied**

Section 5.1 applies a sectorally targeted proposal economy-wide (Wolfram et al.), invents a \$75/t price for high-income countries under Banerjee et al. because the proposal gives no figure, imposes a common \$5\%$-a-year escalation after 2030 on schedules whose authors specify no path, and recasts Equal Right's upstream extraction-licence system with a global fund as a consumption-side price schedule plus population-proportional rebates. Each of these drives the results directly. The invented \$75/t sets the top of the Banerjee et al. dispersion, and since both the surplus and the implicit transfers scale with $(1-\pi_i)$, it is the single most consequential unreported assumption in the paper; the assumed \$5\%$ growth determines how long the schedule's dispersion persists before prices hit the backstop, which is what generates the $13.4\%$ versus $14.4\%$ gap for Equal Right on its two paths. Because the three proposals also differ in coalition membership (47 countries versus 179), in coverage, and in whether revenue crosses borders, the cross-proposal comparison of $4.8\%$, $4.8\%$ and $13.4\%$ mixes the economics with these reconstruction choices. Report a sensitivity table over the high-income price (\$50, \$75, \$100/t), over the escalation rate (0\%, 5\%, 10\%), and over holding coalition membership fixed at the union of the three, so readers can see which differences are the schedules' and which are the authors'.

**Equivalent rights are not invariant to the cap level or to the imposed constant-$\rho$ profile**

The paper offers negotiators an "exchange rate" between price concessions and tonnes, but the exchange rate moves with the policy it is embedded in. Section 4 uses a \$142/t benchmark with allocations expressed against world average per-capita emissions; Section 5 uses coalition prices of \$49–\$140/t with allocations against coalition per-capita emissions. The same country therefore carries several different $\rho$'s across the paper (China 0.95 in Table 1, 1.00 under Banerjee et al.; Nigeria 0.28 at a zero price in Section 4, 0.22 under Banerjee et al.), and the Introduction quotes numbers from both exercises in adjacent sentences. On top of that, rights are constrained to $r_{it} = \rho_i \bar e_t$, a constant multiple, which the paper acknowledges creates temporary price gaps and residual approximation error. Proposition 3 pins down only the discounted-price-weighted present value of rights, so constancy is a normalisation, not an implication — and a different admissible profile would give a different $\rho_i$ with identical welfare. State the normalisation explicitly where $\rho$ is introduced, show how $\rho^*_i$ varies with the uniform price level (a short table at two or three caps would do), and add a sentence warning that the exchange rate must be quoted against a stated cap and price path.

**The joint solve's existence, uniqueness and convergence are not documented in the visible text**

Section 5.2 describes the joint solve in two sentences — the allocation sets the cap, the price is recalibrated, each member's ratio is updated by a secant step, and an outer loop moves the level until the common pre-damage gain is zero — and the only statement of tolerances ($0.002\%$ on each member's gap, $0.001\%$ on the outer loop, $0.01\%$ on the price calibration) appears in an appendix paragraph that is commented out of the manuscript. For a system of 179 indifference conditions plus a cap-level condition, solved through a model with endogenous capital accumulation and an endogenous clearing price, neither existence nor uniqueness of $\tilde\rho$ is obvious, and the maximin sharing in the increased-consumption variant is a separate fixed-point problem. Since the paper's central quantitative claims are that the solve makes every member indifferent to within $0.001\%$ and that the resulting cut is $4.8\%$, the reader needs to see that these are properties of the problem rather than of where the iteration happened to stop. Restore the numerical-accuracy paragraph to the appendix, report convergence diagnostics (iterations, final maximum gap, path of the cap), and add multi-start runs from several initial allocations showing that the solve returns the same $\tilde\rho$ vector.

**No closed-form parametric example of the equivalence**

Section 3 works with a general decreasing convex $a_i$, and every magnitude in the paper then comes out of NICE. Nothing in between: there is no fully specified cost function for which $r^*_i$, $b_i$ and the implicit transfer are written down and compared. The natural candidate is the model's own specification, $a_i(\mu) = \theta^i_1 \mu^{\theta_2}$ with $\theta_2 = 2.6$ and a common backstop, which gives $\mu_i = (p_i/p^{\mathrm{back}})^{1/(\theta_2-1)}$ and hence an isoelastic MAC with $\varepsilon_i$ depending only on $\pi_i$ and $\theta_2$. Half a page would then deliver $b_i$, $r^*_i/e^0_i$ and the ratio $b_i/|t_i|$ as explicit functions of $\pi_i$, and would let the reader check the headline "half a dollar of consumption per dollar of implicit transfer" against the second-order prediction $\tfrac12|1-\pi_i|$ rather than taking it from the solver. A two-country version — one member at $\pi_1 = 0.5$ and one at $\pi_2 = 1.5$, equal populations, equal baselines — would also show concretely who sells, how large the triangle is relative to the transfer, and how the surplus splits. This matters for publication because the paper's quantitative claims are currently unverifiable by hand at any point; a worked case pins the economics separately from the calibration. It would also give the reader the elasticity magnitudes ($\varepsilon$ at $\pi = 0.5$, $1$, $2$) that the second-order formula \eqref{eq:req_second} needs to be usable.

**The one-way correspondence is claimed but never characterized**

That many allocations cannot be replicated by any non-negative price schedule is billed as a contribution in the abstract, the introduction and the conclusion, yet the paper establishes it with two simulated numbers (Nigeria at $0.28$, the DRC at $0.05$) and one sentence. The result deserves a proposition, and one is close at hand: at $p_i = 0$ the country emits its unabated baseline $e^0_i$, so the most generous allocation any non-negative price can match is $\bar r_i \equiv e^0_i - b_i(0)/p^*$, and an allocation $r_i = \rho_i \bar e$ is out of reach exactly when $\rho_i > \bar r_i / \bar e$. With the isoelastic cost of the model this becomes an explicit inequality in the country's baseline emissions per capita relative to the world average, so the paper could say which countries are unreachable rather than naming two. The Equal Right exercise reveals a second, distinct failure of the converse — equivalent rights turn negative for eleven countries once revenue crosses borders, so the schedule is equivalent to rights plus a payment — and that case should be folded into the same statement instead of appearing only as an aside in Section 5.3. Stating and proving the bound, then reporting $\bar r_i/\bar e$ by World Bank income group across the 179 countries, would convert a suggestive pair of numbers into the general claim the abstract already makes.

**No numerical comparison with the transfer magnitudes in prior work**

Section 3 summarizes Chateau et al. (2024), He et al. (2024) and Le Moigne et al. (2026), but not one number from those papers is ever placed beside the paper's own. Le Moigne et al. report the North–South transfers that equalize costs under a uniform tax at \$107--169 bn a year; this paper computes gross implicit transfers rising from roughly \$39 bn in 2030 to \$188 bn in 2050 under the Banerjee et al. schedule. Those are the same currency, the same horizon and the same economic object, and the claim that "differentiation is equivalent to transfers of the same first-order size" would be far more convincing if the paper showed its implicit transfers to be of the order that the literature's explicit transfers require. A second comparison is already sitting in a LaTeX comment and not in the manuscript: the coalition price of \$56/t for the Banerjee et al. schedule matches Chateau et al.'s \$55.8/t equivalent uniform price, an external check on the calibration that the reader should see. Finally, since Fleurbaey et al. (2025) cover the dominance direction at a fixed cap, a paragraph verifying that Proposition~\ref{prop:dominance} reduces to their result when the cap is fixed and the endowment is free would make the boundary of the novelty claim explicit rather than leaving it to a hedge in the introduction.

**No guidance on computing the exchange rate from observable data**

The paper offers negotiators a closed-form "exchange rate" between price concessions and tonnes, but $\hat\rho_i$ in \eqref{eq:rhohat_dyn} requires the full path of a country's autarky emissions to 2100, weighted by the discounted uniform price — that is, a baseline emissions projection, a price path and an elasticity. What a negotiating party would actually need, and how much the answer moves with those inputs, is never discussed. Table~\ref{tab:summary} shows the stakes: China emits $1.7$ times the world average per capita in 2025 yet is equivalent to $\rho_1 = 0.95$, so the projection, not the observed data, is doing the work. A subsection reporting $\hat\rho_i$ for the ten countries of Table~\ref{tab:equiv_rights_main} under two or three alternative baselines (an SSP1- and an SSP3-type growth and emission-intensity path against the model's default) and under a truncated horizon (2025--2050 rather than 2025--2100) would give readers the range within which the exchange rate is pinned down. The paper should also say what happens when realized emissions depart from the projection: a price schedule adjusts automatically, whereas an allocation fixed today on a stale baseline does not, which is a practical asymmetry between the two instruments that the equivalence itself cannot settle.

**Recommendation**: major revision. The core identity in Proposition 1 is correct and the mapping to real proposals is a real contribution, but the criterion used in the simulations is not the one the propositions are proved for, the damage accounting is switched between the solve and the reported gains in a way that contradicts the Introduction's "every member as well off" claim, and every headline magnitude rests on an uncalibrated abatement-cost curvature that is never varied. These are fixable with reruns and reframing rather than new theory, but they touch the numbers in the abstract, so the paper cannot go forward in its present form.

**Key revision targets**:

1. Reconcile the theory with the implementation: either restate Propositions 1–3 under a concave utility with $u'$-weighted discounting, or demonstrate numerically that the discounted-price weighting of \eqref{eq:rhohat_dyn} coincides with the marginal-utility weighting at the magnitudes involved; and state the exact dynamic condition $\sum_t \beta_t [p^*_t(R_{it}-E^A_{it}) + N_{it} b_{it}] = 0$ rather than a first-order "if and only if".
2. Report every headline result under both damage accountings, replace tolerance-dependent loser counts with the distribution of members' gains, and qualify the Introduction's "leaving every member's consumption as high as under the schedule" to hold at the schedule's damages.
3. Add a sensitivity section covering $\theta_2$, the backstop path, $\eta$, the pure rate of time preference, the invented high-income price under Banerjee et al., and the assumed 5\%-a-year escalation, reporting ranges in the main text; and feature the deadweight-loss-to-transfer ratio $\tfrac{1}{2}|1-\pi_i|$, which is nearly free of the abatement-cost calibration, as the headline quantitative claim.
4. Either add a run in which within-country redistribution is delivered by a non-carbon instrument held fixed across regimes, or demote the welfare variant and state plainly that its 27–34.5\% figures reflect the absence of a domestic lump-sum instrument rather than the inefficiency of differentiation.
5. Confront the institutional-form version of the feasibility argument (visible fiscal flows, enforcement, commitment, seller market power, absence of trade and leakage in NICE) in Section 6, and restore "no trade" to the stated limitations given that two of Section 2's four rationales require it.
6. Document the joint solve: restore the tolerances and convergence criteria to the appendix, report iteration diagnostics, and show through multi-start runs that the equivalent allocation vector is unique.

**Status**: [Pending]

---

## Detailed Comments (25)

### 1. Warming figures attributed to proposals, not to the paper's runs

**Status**: [Pending]

**Quote**:
> The proposals yield a 2100 warming of 2.16\textdegree C \citep{wolfram_building_2025}, 1.80\textdegree C \citep{banerjee_grand_2025} and 1.65\textdegree C \citep{equal_right_climate_2023}.

**Feedback**:
On first reading I took these as temperatures reported by the cited authors, then realised they cannot be: the same paragraph states that the first two proposals specify no price path at all, and the paper supplies one (flat to 2030, then 5%/yr, capped at the backstop). So these are outputs of NICE applied to the paper's reconstructions, yet the citation form credits them to the proposals. The Equal Right entry makes the problem visible as an internal contradiction: two paragraphs earlier the schedule is described as rising "by about 16\% a year to stay within 1.5\textdegree C", but $1.65^{\circ}$C is attributed to \citet{equal_right_climate_2023} — the $1.5^{\circ}$C-consistent number is the appendix run of the proposal's own escalation, which the commented-out note puts at $1.48^{\circ}$C. A reader comparing the three numbers would wrongly conclude that Equal Right's own design misses its stated target by $0.15^{\circ}$C, when the shortfall comes from the imposed 5% path. Rewrite the sentence as "Under our common escalation path, the reconstructed schedules yield a 2100 warming of 2.16\textdegree C (\citeauthor{wolfram_building_2025}), 1.80\textdegree C (\citeauthor{banerjee_grand_2025}) and 1.65\textdegree C (Equal Right); under Equal Right's own 16\%-a-year path the figure is 1.48\textdegree C (Online Appendix~\ref{app:ede})." because the temperatures are properties of the paper's runs, not of the cited proposals.

---

### 2. Footnote member list does not add up to 47 countries

**Status**: [Pending]

**Quote**:
> \citet{wolfram_building_2025} envisage a coalition of 47 countries\footnote{The members are Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda and Zambia.} including China, the EU, the UK, India and Brazil, which together emit 48\% of world CO$_2$ in 2030.

**Feedback**:
I tried to reconcile the footnote with the "47 countries" in the text and could not. The list has 22 entries: Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda, Zambia. Twenty-one of these are single economies and the twenty-second is the EU, which at 27 member states gives $21 + 27 = 48$ priced economies, one more than the 47 used repeatedly elsewhere ("24 of 47 Wolfram et al.\ members", "26 of 47"). Either one listed entity is absent from the model's 179 economies (Liechtenstein is the obvious candidate) or the EU enters as fewer than 27 units, and the reader cannot tell which. Since the coalition defines the cap, the calibrated coalition price and every $\rho_i$ reported for this proposal, the membership must be reproducible. Add, immediately after the member list in the footnote, a sentence stating how the 22 listed entries map onto 47 economies in the model (which EU members are included and which listed entities, if any, fall outside the 179-economy set), to close the one-country gap.

---

### 3. Three-tier Wolfram schedule leaves low-income members unpriced

**Status**: [Pending]

**Quote**:
> Members apply \$75, \$50 and \$25/t in high-, upper-middle- and lower-income countries, the tiers of the IMF's international carbon price floor \citep{parry_proposal_2021}. The proposal targets four emissions-intensive industries, whereas NICE has no sectoral detail.

**Feedback**:
The Wolfram schedule is given three tiers keyed to "high-, upper-middle- and lower-income" countries, while the Banerjee schedule in the next paragraph uses four World Bank groups with a distinct low-income tier at \$10/t. The coalition footnote includes several World Bank low-income economies — Mozambique, Togo, Uganda and Zambia — and under the three-tier wording it is undetermined whether they pay \$25/t (i.e.\ "lower-income" pools the low- and lower-middle-income groups) or are unpriced. This is not cosmetic: a member's $\pi_i$ is what the whole exercise maps into rights, and the difference between \$25/t and no price moves these countries between $\pi_i \approx 0.5$ and $\pi_i = 0$, which is exactly the region where the section on unreachable allocations argues the first-order formula breaks down. Rewrite "in high-, upper-middle- and lower-income countries" as "in high-income, upper-middle-income, and low- and lower-middle-income countries" because four coalition members are World Bank low-income economies and the present wording leaves their price undefined.

---

### 4. Equal Right's fund pays grants and a dividend, modelled as dividend only

**Status**: [Pending]

**Quote**:
> The revenues are pooled in a global fund that pays climate grants and a universal dividend. We model Equal Right's between-country transfers by returning global revenues to each country in proportion to its population.

**Feedback**:
The first sentence says the fund has two disbursement channels; the second replaces both with a single population-proportional rebate. Climate grants in the proposal are targeted — they go to vulnerable and low-capacity countries — so a per-capita rule redistributes a different vector than the proposal does, and it does so in a way that is not neutral for the paper's results. The Equal Right exercise is where equivalent rights turn negative for eleven countries, and that sign flip is driven entirely by the gap between what a country pays into the fund and what it draws out, $D_t N_{it}/N_t - p_{it} E^A_{it}$. Shifting the grant share from a vulnerability-weighted to a population-weighted rule changes which countries sit on which side of that difference. Add, after "in proportion to its population", a clause stating what share of the fund the proposal earmarks for grants rather than the dividend and that the grant component is proxied here by the same per-capita rule, so readers can judge how much of the negative-rights finding depends on that substitution.

---

### 5. The $144/t average is inconsistent with an unweighted mean of the tiers

**Status**: [Pending]

**Quote**:
> Each country sells its extraction licences at a price graduated by income group (\$240, \$120, \$60 and \$30/t) and discounted by up to 60\% for climate vulnerability, which gives floors ranging from \$12 to \$240/t in 2025 (\$144/t on average), rising by about 16\% a year to stay within 1.5\textdegree C.

**Feedback**:
The range checks out — the lowest tier discounted by 60% gives $30 \times 0.4 = \$12$, and the top tier undiscounted gives \$240 — but the stated average does not, on the most natural reading. A simple mean of the four base rates is $(240+120+60+30)/4 = \$112.5$, and since every vulnerability discount is non-positive, any unweighted average across countries must be below \$112.5, not \$144. The figure is reconcilable only under emissions or GDP weighting, where high-income countries at \$240 and China at \$120 dominate; that also squares with the coalition price of \$139.4/t quoted for Equal Right elsewhere. As written, a reader checking the arithmetic hits an apparent contradiction with the tier schedule printed in the same parenthesis. Rewrite "(\$144/t on average)" as "(\$144/t on an emissions-weighted average)" because the unweighted mean of the four base rates is \$112.5 before any vulnerability discount and cannot reach \$144.

---

### 6. Warming figures attributed to proposals, not to the paper's runs

**Status**: [Pending]

**Quote**:
> The proposals yield a 2100 warming of 2.16\textdegree C \citep{wolfram_building_2025}, 1.80\textdegree C \citep{banerjee_grand_2025} and 1.65\textdegree C \citep{equal_right_climate_2023}.

**Feedback**:
On first reading I took these as temperatures reported by the cited authors, then realised they cannot be: the same paragraph states that the first two proposals specify no price path at all, and the paper supplies one (flat to 2030, then 5%/yr, capped at the backstop). So these are outputs of NICE applied to the paper's reconstructions, yet the citation form credits them to the proposals. The Equal Right entry makes the problem visible as an internal contradiction: two paragraphs earlier the schedule is described as rising "by about 16\% a year to stay within 1.5\textdegree C", but $1.65^{\circ}$C is attributed to \citet{equal_right_climate_2023} — the $1.5^{\circ}$C-consistent number is the appendix run of the proposal's own escalation, which the commented-out note puts at $1.48^{\circ}$C. A reader comparing the three numbers would wrongly conclude that Equal Right's own design misses its stated target by $0.15^{\circ}$C, when the shortfall comes from the imposed 5% path. Rewrite the sentence as "Under our common escalation path, the reconstructed schedules yield a 2100 warming of 2.16\textdegree C (\citeauthor{wolfram_building_2025}), 1.80\textdegree C (\citeauthor{banerjee_grand_2025}) and 1.65\textdegree C (Equal Right); under Equal Right's own 16\%-a-year path the figure is 1.48\textdegree C (Online Appendix~\ref{app:ede})." because the temperatures are properties of the paper's runs, not of the cited proposals.

---

### 7. Footnote member list does not add up to 47 countries

**Status**: [Pending]

**Quote**:
> \citet{wolfram_building_2025} envisage a coalition of 47 countries\footnote{The members are Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda and Zambia.} including China, the EU, the UK, India and Brazil, which together emit 48\% of world CO$_2$ in 2030.

**Feedback**:
I tried to reconcile the footnote with the "47 countries" in the text and could not. The list has 22 entries: Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda, Zambia. Twenty-one of these are single economies and the twenty-second is the EU, which at 27 member states gives $21 + 27 = 48$ priced economies, one more than the 47 used repeatedly elsewhere ("24 of 47 Wolfram et al.\ members", "26 of 47"). Either one listed entity is absent from the model's 179 economies (Liechtenstein is the obvious candidate) or the EU enters as fewer than 27 units, and the reader cannot tell which. Since the coalition defines the cap, the calibrated coalition price and every $\rho_i$ reported for this proposal, the membership must be reproducible. Add, immediately after the member list in the footnote, a sentence stating how the 22 listed entries map onto 47 economies in the model (which EU members are included and which listed entities, if any, fall outside the 179-economy set), to close the one-country gap.

---

### 8. Three-tier Wolfram schedule leaves low-income members unpriced

**Status**: [Pending]

**Quote**:
> Members apply \$75, \$50 and \$25/t in high-, upper-middle- and lower-income countries, the tiers of the IMF's international carbon price floor \citep{parry_proposal_2021}. The proposal targets four emissions-intensive industries, whereas NICE has no sectoral detail.

**Feedback**:
The Wolfram schedule is given three tiers keyed to "high-, upper-middle- and lower-income" countries, while the Banerjee schedule in the next paragraph uses four World Bank groups with a distinct low-income tier at \$10/t. The coalition footnote includes several World Bank low-income economies — Mozambique, Togo, Uganda and Zambia — and under the three-tier wording it is undetermined whether they pay \$25/t (i.e.\ "lower-income" pools the low- and lower-middle-income groups) or are unpriced. This is not cosmetic: a member's $\pi_i$ is what the whole exercise maps into rights, and the difference between \$25/t and no price moves these countries between $\pi_i \approx 0.5$ and $\pi_i = 0$, which is exactly the region where the section on unreachable allocations argues the first-order formula breaks down. Rewrite "in high-, upper-middle- and lower-income countries" as "in high-income, upper-middle-income, and low- and lower-middle-income countries" because four coalition members are World Bank low-income economies and the present wording leaves their price undefined.

---

### 9. Equal Right's fund pays grants and a dividend, modelled as dividend only

**Status**: [Pending]

**Quote**:
> The revenues are pooled in a global fund that pays climate grants and a universal dividend. We model Equal Right's between-country transfers by returning global revenues to each country in proportion to its population.

**Feedback**:
The first sentence says the fund has two disbursement channels; the second replaces both with a single population-proportional rebate. Climate grants in the proposal are targeted — they go to vulnerable and low-capacity countries — so a per-capita rule redistributes a different vector than the proposal does, and it does so in a way that is not neutral for the paper's results. The Equal Right exercise is where equivalent rights turn negative for eleven countries, and that sign flip is driven entirely by the gap between what a country pays into the fund and what it draws out, $D_t N_{it}/N_t - p_{it} E^A_{it}$. Shifting the grant share from a vulnerability-weighted to a population-weighted rule changes which countries sit on which side of that difference. Add, after "in proportion to its population", a clause stating what share of the fund the proposal earmarks for grants rather than the dividend and that the grant component is proxied here by the same per-capita rule, so readers can judge how much of the negative-rights finding depends on that substitution.

---

### 10. The $144/t average is inconsistent with an unweighted mean of the tiers

**Status**: [Pending]

**Quote**:
> Each country sells its extraction licences at a price graduated by income group (\$240, \$120, \$60 and \$30/t) and discounted by up to 60\% for climate vulnerability, which gives floors ranging from \$12 to \$240/t in 2025 (\$144/t on average), rising by about 16\% a year to stay within 1.5\textdegree C.

**Feedback**:
The range checks out — the lowest tier discounted by 60% gives $30 \times 0.4 = \$12$, and the top tier undiscounted gives \$240 — but the stated average does not, on the most natural reading. A simple mean of the four base rates is $(240+120+60+30)/4 = \$112.5$, and since every vulnerability discount is non-positive, any unweighted average across countries must be below \$112.5, not \$144. The figure is reconcilable only under emissions or GDP weighting, where high-income countries at \$240 and China at \$120 dominate; that also squares with the coalition price of \$139.4/t quoted for Equal Right elsewhere. As written, a reader checking the arithmetic hits an apparent contradiction with the tier schedule printed in the same parenthesis. Rewrite "(\$144/t on average)" as "(\$144/t on an emissions-weighted average)" because the unweighted mean of the four base rates is \$112.5 before any vulnerability discount and cannot reach \$144.

---

### 11. Warming figures attributed to proposals, not to the paper's runs

**Status**: [Pending]

**Quote**:
> The proposals yield a 2100 warming of 2.16\textdegree C \citep{wolfram_building_2025}, 1.80\textdegree C \citep{banerjee_grand_2025} and 1.65\textdegree C \citep{equal_right_climate_2023}.

**Feedback**:
On first reading I took these as temperatures reported by the cited authors, then realised they cannot be: the same paragraph states that the first two proposals specify no price path at all, and the paper supplies one (flat to 2030, then 5%/yr, capped at the backstop). So these are outputs of NICE applied to the paper's reconstructions, yet the citation form credits them to the proposals. The Equal Right entry makes the problem visible as an internal contradiction: two paragraphs earlier the schedule is described as rising "by about 16\% a year to stay within 1.5\textdegree C", but $1.65^{\circ}$C is attributed to \citet{equal_right_climate_2023} — the $1.5^{\circ}$C-consistent number is the appendix run of the proposal's own escalation, which the commented-out note puts at $1.48^{\circ}$C. A reader comparing the three numbers would wrongly conclude that Equal Right's own design misses its stated target by $0.15^{\circ}$C, when the shortfall comes from the imposed 5% path. Rewrite the sentence as "Under our common escalation path, the reconstructed schedules yield a 2100 warming of 2.16\textdegree C (\citeauthor{wolfram_building_2025}), 1.80\textdegree C (\citeauthor{banerjee_grand_2025}) and 1.65\textdegree C (Equal Right); under Equal Right's own 16\%-a-year path the figure is 1.48\textdegree C (Online Appendix~\ref{app:ede})." because the temperatures are properties of the paper's runs, not of the cited proposals.

---

### 12. Footnote member list does not add up to 47 countries

**Status**: [Pending]

**Quote**:
> \citet{wolfram_building_2025} envisage a coalition of 47 countries\footnote{The members are Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda and Zambia.} including China, the EU, the UK, India and Brazil, which together emit 48\% of world CO$_2$ in 2030.

**Feedback**:
I tried to reconcile the footnote with the "47 countries" in the text and could not. The list has 22 entries: Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda, Zambia. Twenty-one of these are single economies and the twenty-second is the EU, which at 27 member states gives $21 + 27 = 48$ priced economies, one more than the 47 used repeatedly elsewhere ("24 of 47 Wolfram et al.\ members", "26 of 47"). Either one listed entity is absent from the model's 179 economies (Liechtenstein is the obvious candidate) or the EU enters as fewer than 27 units, and the reader cannot tell which. Since the coalition defines the cap, the calibrated coalition price and every $\rho_i$ reported for this proposal, the membership must be reproducible. Add, immediately after the member list in the footnote, a sentence stating how the 22 listed entries map onto 47 economies in the model (which EU members are included and which listed entities, if any, fall outside the 179-economy set), to close the one-country gap.

---

### 13. Three-tier Wolfram schedule leaves low-income members unpriced

**Status**: [Pending]

**Quote**:
> Members apply \$75, \$50 and \$25/t in high-, upper-middle- and lower-income countries, the tiers of the IMF's international carbon price floor \citep{parry_proposal_2021}. The proposal targets four emissions-intensive industries, whereas NICE has no sectoral detail.

**Feedback**:
The Wolfram schedule is given three tiers keyed to "high-, upper-middle- and lower-income" countries, while the Banerjee schedule in the next paragraph uses four World Bank groups with a distinct low-income tier at \$10/t. The coalition footnote includes several World Bank low-income economies — Mozambique, Togo, Uganda and Zambia — and under the three-tier wording it is undetermined whether they pay \$25/t (i.e.\ "lower-income" pools the low- and lower-middle-income groups) or are unpriced. This is not cosmetic: a member's $\pi_i$ is what the whole exercise maps into rights, and the difference between \$25/t and no price moves these countries between $\pi_i \approx 0.5$ and $\pi_i = 0$, which is exactly the region where the section on unreachable allocations argues the first-order formula breaks down. Rewrite "in high-, upper-middle- and lower-income countries" as "in high-income, upper-middle-income, and low- and lower-middle-income countries" because four coalition members are World Bank low-income economies and the present wording leaves their price undefined.

---

### 14. Equal Right's fund pays grants and a dividend, modelled as dividend only

**Status**: [Pending]

**Quote**:
> The revenues are pooled in a global fund that pays climate grants and a universal dividend. We model Equal Right's between-country transfers by returning global revenues to each country in proportion to its population.

**Feedback**:
The first sentence says the fund has two disbursement channels; the second replaces both with a single population-proportional rebate. Climate grants in the proposal are targeted — they go to vulnerable and low-capacity countries — so a per-capita rule redistributes a different vector than the proposal does, and it does so in a way that is not neutral for the paper's results. The Equal Right exercise is where equivalent rights turn negative for eleven countries, and that sign flip is driven entirely by the gap between what a country pays into the fund and what it draws out, $D_t N_{it}/N_t - p_{it} E^A_{it}$. Shifting the grant share from a vulnerability-weighted to a population-weighted rule changes which countries sit on which side of that difference. Add, after "in proportion to its population", a clause stating what share of the fund the proposal earmarks for grants rather than the dividend and that the grant component is proxied here by the same per-capita rule, so readers can judge how much of the negative-rights finding depends on that substitution.

---

### 15. Warming figures attributed to proposals, not to the paper's runs

**Status**: [Pending]

**Quote**:
> The proposals yield a 2100 warming of 2.16\textdegree C \citep{wolfram_building_2025}, 1.80\textdegree C \citep{banerjee_grand_2025} and 1.65\textdegree C \citep{equal_right_climate_2023}.

**Feedback**:
On first reading I took these as temperatures reported by the cited authors, then realised they cannot be: the same paragraph states that the first two proposals specify no price path at all, and the paper supplies one (flat to 2030, then 5%/yr, capped at the backstop). So these are outputs of NICE applied to the paper's reconstructions, yet the citation form credits them to the proposals. The Equal Right entry makes the problem visible as an internal contradiction: two paragraphs earlier the schedule is described as rising "by about 16\% a year to stay within 1.5\textdegree C", but $1.65^{\circ}$C is attributed to \citet{equal_right_climate_2023} — the $1.5^{\circ}$C-consistent number is the appendix run of the proposal's own escalation, which the commented-out note puts at $1.48^{\circ}$C. A reader comparing the three numbers would wrongly conclude that Equal Right's own design misses its stated target by $0.15^{\circ}$C, when the shortfall comes from the imposed 5% path. Rewrite the sentence as "Under our common escalation path, the reconstructed schedules yield a 2100 warming of 2.16\textdegree C (\citeauthor{wolfram_building_2025}), 1.80\textdegree C (\citeauthor{banerjee_grand_2025}) and 1.65\textdegree C (Equal Right); under Equal Right's own 16\%-a-year path the figure is 1.48\textdegree C (Online Appendix~\ref{app:ede})." because the temperatures are properties of the paper's runs, not of the cited proposals.

---

### 16. Footnote member list does not add up to 47 countries

**Status**: [Pending]

**Quote**:
> \citet{wolfram_building_2025} envisage a coalition of 47 countries\footnote{The members are Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda and Zambia.} including China, the EU, the UK, India and Brazil, which together emit 48\% of world CO$_2$ in 2030.

**Feedback**:
I tried to reconcile the footnote with the "47 countries" in the text and could not. The list has 22 entries: Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda, Zambia. Twenty-one of these are single economies and the twenty-second is the EU, which at 27 member states gives $21 + 27 = 48$ priced economies, one more than the 47 used repeatedly elsewhere ("24 of 47 Wolfram et al.\ members", "26 of 47"). Either one listed entity is absent from the model's 179 economies (Liechtenstein is the obvious candidate) or the EU enters as fewer than 27 units, and the reader cannot tell which. Since the coalition defines the cap, the calibrated coalition price and every $\rho_i$ reported for this proposal, the membership must be reproducible. Add, immediately after the member list in the footnote, a sentence stating how the 22 listed entries map onto 47 economies in the model (which EU members are included and which listed entities, if any, fall outside the 179-economy set), to close the one-country gap.

---

### 17. Three-tier Wolfram schedule leaves low-income members unpriced

**Status**: [Pending]

**Quote**:
> Members apply \$75, \$50 and \$25/t in high-, upper-middle- and lower-income countries, the tiers of the IMF's international carbon price floor \citep{parry_proposal_2021}. The proposal targets four emissions-intensive industries, whereas NICE has no sectoral detail.

**Feedback**:
The Wolfram schedule is given three tiers keyed to "high-, upper-middle- and lower-income" countries, while the Banerjee schedule in the next paragraph uses four World Bank groups with a distinct low-income tier at \$10/t. The coalition footnote includes several World Bank low-income economies — Mozambique, Togo, Uganda and Zambia — and under the three-tier wording it is undetermined whether they pay \$25/t (i.e.\ "lower-income" pools the low- and lower-middle-income groups) or are unpriced. This is not cosmetic: a member's $\pi_i$ is what the whole exercise maps into rights, and the difference between \$25/t and no price moves these countries between $\pi_i \approx 0.5$ and $\pi_i = 0$, which is exactly the region where the section on unreachable allocations argues the first-order formula breaks down. Rewrite "in high-, upper-middle- and lower-income countries" as "in high-income, upper-middle-income, and low- and lower-middle-income countries" because four coalition members are World Bank low-income economies and the present wording leaves their price undefined.

---

### 18. Equal Right's fund pays grants and a dividend, modelled as dividend only

**Status**: [Pending]

**Quote**:
> The revenues are pooled in a global fund that pays climate grants and a universal dividend. We model Equal Right's between-country transfers by returning global revenues to each country in proportion to its population.

**Feedback**:
The first sentence says the fund has two disbursement channels; the second replaces both with a single population-proportional rebate. Climate grants in the proposal are targeted — they go to vulnerable and low-capacity countries — so a per-capita rule redistributes a different vector than the proposal does, and it does so in a way that is not neutral for the paper's results. The Equal Right exercise is where equivalent rights turn negative for eleven countries, and that sign flip is driven entirely by the gap between what a country pays into the fund and what it draws out, $D_t N_{it}/N_t - p_{it} E^A_{it}$. Shifting the grant share from a vulnerability-weighted to a population-weighted rule changes which countries sit on which side of that difference. Add, after "in proportion to its population", a clause stating what share of the fund the proposal earmarks for grants rather than the dividend and that the grant component is proxied here by the same per-capita rule, so readers can judge how much of the negative-rights finding depends on that substitution.

---

### 19. The $144/t average is inconsistent with an unweighted mean of the tiers

**Status**: [Pending]

**Quote**:
> Each country sells its extraction licences at a price graduated by income group (\$240, \$120, \$60 and \$30/t) and discounted by up to 60\% for climate vulnerability, which gives floors ranging from \$12 to \$240/t in 2025 (\$144/t on average), rising by about 16\% a year to stay within 1.5\textdegree C.

**Feedback**:
The range checks out — the lowest tier discounted by 60% gives $30 \times 0.4 = \$12$, and the top tier undiscounted gives \$240 — but the stated average does not, on the most natural reading. A simple mean of the four base rates is $(240+120+60+30)/4 = \$112.5$, and since every vulnerability discount is non-positive, any unweighted average across countries must be below \$112.5, not \$144. The figure is reconcilable only under emissions or GDP weighting, where high-income countries at \$240 and China at \$120 dominate; that also squares with the coalition price of \$139.4/t quoted for Equal Right elsewhere. As written, a reader checking the arithmetic hits an apparent contradiction with the tier schedule printed in the same parenthesis. Rewrite "(\$144/t on average)" as "(\$144/t on an emissions-weighted average)" because the unweighted mean of the four base rates is \$112.5 before any vulnerability discount and cannot reach \$144.

---

### 20. Warming figures attributed to proposals, not to the paper's runs

**Status**: [Pending]

**Quote**:
> The proposals yield a 2100 warming of 2.16\textdegree C \citep{wolfram_building_2025}, 1.80\textdegree C \citep{banerjee_grand_2025} and 1.65\textdegree C \citep{equal_right_climate_2023}.

**Feedback**:
On first reading I took these as temperatures reported by the cited authors, then realised they cannot be: the same paragraph states that the first two proposals specify no price path at all, and the paper supplies one (flat to 2030, then 5%/yr, capped at the backstop). So these are outputs of NICE applied to the paper's reconstructions, yet the citation form credits them to the proposals. The Equal Right entry makes the problem visible as an internal contradiction: two paragraphs earlier the schedule is described as rising "by about 16\% a year to stay within 1.5\textdegree C", but $1.65^{\circ}$C is attributed to \citet{equal_right_climate_2023} — the $1.5^{\circ}$C-consistent number is the appendix run of the proposal's own escalation, which the commented-out note puts at $1.48^{\circ}$C. A reader comparing the three numbers would wrongly conclude that Equal Right's own design misses its stated target by $0.15^{\circ}$C, when the shortfall comes from the imposed 5% path. Rewrite the sentence as "Under our common escalation path, the reconstructed schedules yield a 2100 warming of 2.16\textdegree C (\citeauthor{wolfram_building_2025}), 1.80\textdegree C (\citeauthor{banerjee_grand_2025}) and 1.65\textdegree C (Equal Right); under Equal Right's own 16\%-a-year path the figure is 1.48\textdegree C (Online Appendix~\ref{app:ede})." because the temperatures are properties of the paper's runs, not of the cited proposals.

---

### 21. Warming figures attributed to proposals, not to the paper's runs

**Status**: [Pending]

**Quote**:
> The proposals yield a 2100 warming of 2.16\textdegree C \citep{wolfram_building_2025}, 1.80\textdegree C \citep{banerjee_grand_2025} and 1.65\textdegree C \citep{equal_right_climate_2023}.

**Feedback**:
On first reading I took these as temperatures reported by the cited authors, then realised they cannot be: the same paragraph states that the first two proposals specify no price path at all, and the paper supplies one (flat to 2030, then 5%/yr, capped at the backstop). So these are outputs of NICE applied to the paper's reconstructions, yet the citation form credits them to the proposals. The Equal Right entry makes the problem visible as an internal contradiction: two paragraphs earlier the schedule is described as rising "by about 16\% a year to stay within 1.5\textdegree C", but $1.65^{\circ}$C is attributed to \citet{equal_right_climate_2023} — the $1.5^{\circ}$C-consistent number is the appendix run of the proposal's own escalation, which the commented-out note puts at $1.48^{\circ}$C. A reader comparing the three numbers would wrongly conclude that Equal Right's own design misses its stated target by $0.15^{\circ}$C, when the shortfall comes from the imposed 5% path. Rewrite the sentence as "Under our common escalation path, the reconstructed schedules yield a 2100 warming of 2.16\textdegree C (\citeauthor{wolfram_building_2025}), 1.80\textdegree C (\citeauthor{banerjee_grand_2025}) and 1.65\textdegree C (Equal Right); under Equal Right's own 16\%-a-year path the figure is 1.48\textdegree C (Online Appendix~\ref{app:ede})." because the temperatures are properties of the paper's runs, not of the cited proposals.

---

### 22. Footnote member list does not add up to 47 countries

**Status**: [Pending]

**Quote**:
> \citet{wolfram_building_2025} envisage a coalition of 47 countries\footnote{The members are Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda and Zambia.} including China, the EU, the UK, India and Brazil, which together emit 48\% of world CO$_2$ in 2030.

**Feedback**:
I tried to reconcile the footnote with the "47 countries" in the text and could not. The list has 22 entries: Algeria, Australia, Brazil, Cameroon, Canada, China, Egypt, the European Union, Ghana, Iceland, India, Indonesia, Kenya, Liechtenstein, Mozambique, Norway, Switzerland, Thailand, Togo, the United Kingdom, Uganda, Zambia. Twenty-one of these are single economies and the twenty-second is the EU, which at 27 member states gives $21 + 27 = 48$ priced economies, one more than the 47 used repeatedly elsewhere ("24 of 47 Wolfram et al.\ members", "26 of 47"). Either one listed entity is absent from the model's 179 economies (Liechtenstein is the obvious candidate) or the EU enters as fewer than 27 units, and the reader cannot tell which. Since the coalition defines the cap, the calibrated coalition price and every $\rho_i$ reported for this proposal, the membership must be reproducible. Add, immediately after the member list in the footnote, a sentence stating how the 22 listed entries map onto 47 economies in the model (which EU members are included and which listed entities, if any, fall outside the 179-economy set), to close the one-country gap.

---

### 23. Three-tier Wolfram schedule leaves low-income members unpriced

**Status**: [Pending]

**Quote**:
> Members apply \$75, \$50 and \$25/t in high-, upper-middle- and lower-income countries, the tiers of the IMF's international carbon price floor \citep{parry_proposal_2021}. The proposal targets four emissions-intensive industries, whereas NICE has no sectoral detail.

**Feedback**:
The Wolfram schedule is given three tiers keyed to "high-, upper-middle- and lower-income" countries, while the Banerjee schedule in the next paragraph uses four World Bank groups with a distinct low-income tier at \$10/t. The coalition footnote includes several World Bank low-income economies — Mozambique, Togo, Uganda and Zambia — and under the three-tier wording it is undetermined whether they pay \$25/t (i.e.\ "lower-income" pools the low- and lower-middle-income groups) or are unpriced. This is not cosmetic: a member's $\pi_i$ is what the whole exercise maps into rights, and the difference between \$25/t and no price moves these countries between $\pi_i \approx 0.5$ and $\pi_i = 0$, which is exactly the region where the section on unreachable allocations argues the first-order formula breaks down. Rewrite "in high-, upper-middle- and lower-income countries" as "in high-income, upper-middle-income, and low- and lower-middle-income countries" because four coalition members are World Bank low-income economies and the present wording leaves their price undefined.

---

### 24. Equal Right's fund pays grants and a dividend, modelled as dividend only

**Status**: [Pending]

**Quote**:
> The revenues are pooled in a global fund that pays climate grants and a universal dividend. We model Equal Right's between-country transfers by returning global revenues to each country in proportion to its population.

**Feedback**:
The first sentence says the fund has two disbursement channels; the second replaces both with a single population-proportional rebate. Climate grants in the proposal are targeted — they go to vulnerable and low-capacity countries — so a per-capita rule redistributes a different vector than the proposal does, and it does so in a way that is not neutral for the paper's results. The Equal Right exercise is where equivalent rights turn negative for eleven countries, and that sign flip is driven entirely by the gap between what a country pays into the fund and what it draws out, $D_t N_{it}/N_t - p_{it} E^A_{it}$. Shifting the grant share from a vulnerability-weighted to a population-weighted rule changes which countries sit on which side of that difference. Add, after "in proportion to its population", a clause stating what share of the fund the proposal earmarks for grants rather than the dividend and that the grant component is proxied here by the same per-capita rule, so readers can judge how much of the negative-rights finding depends on that substitution.

---

### 25. The $144/t average is inconsistent with an unweighted mean of the tiers

**Status**: [Pending]

**Quote**:
> Each country sells its extraction licences at a price graduated by income group (\$240, \$120, \$60 and \$30/t) and discounted by up to 60\% for climate vulnerability, which gives floors ranging from \$12 to \$240/t in 2025 (\$144/t on average), rising by about 16\% a year to stay within 1.5\textdegree C.

**Feedback**:
The range checks out — the lowest tier discounted by 60% gives $30 \times 0.4 = \$12$, and the top tier undiscounted gives \$240 — but the stated average does not, on the most natural reading. A simple mean of the four base rates is $(240+120+60+30)/4 = \$112.5$, and since every vulnerability discount is non-positive, any unweighted average across countries must be below \$112.5, not \$144. The figure is reconcilable only under emissions or GDP weighting, where high-income countries at \$240 and China at \$120 dominate; that also squares with the coalition price of \$139.4/t quoted for Equal Right elsewhere. As written, a reader checking the arithmetic hits an apparent contradiction with the tier schedule printed in the same parenthesis. Rewrite "(\$144/t on average)" as "(\$144/t on an emissions-weighted average)" because the unweighted mean of the four base rates is \$112.5 before any vulnerability discount and cannot reach \$144.

---
