**Referee Report**

**Manuscript:** *International Transfers or Differentiated Carbon Prices?*

### Overall assessment

This paper develops a simple and potentially useful idea: a system of differentiated national carbon prices can, under standard competitive assumptions, be replicated distributionally by a uniform carbon price combined with an appropriate allocation of tradable emission rights. The paper derives an exact static expression for the allocation of rights that makes a country indifferent between a differentiated domestic price and a uniform international price, obtains first- and second-order approximations, extends the argument to a dynamic setting, and quantifies the mapping using NICE for several recently proposed differentiated carbon-price schedules. The quantitative results suggest that the efficiency gains from replacing price differentiation by a uniform price are modest but positive, while the distributional transfers implicit in price differentiation can be large.

I find the central idea interesting and the paper potentially relevant to the international carbon-pricing literature. The numerical implementation is also unusually transparent about its accounting interpretation, and the distinction between the first-order distributional transfer and the second-order efficiency loss is useful.

However, in its current form I do not think the paper meets the standard required for publication in *JEEM*. My concerns are substantive rather than cosmetic. The most important are: (i) the novelty claim is not yet adequately established relative to closely related work, including Fleurbaey, Kornek, and Edenhofer (2025); (ii) Proposition 3 is not correct as stated for the general welfare objective introduced immediately before it; (iii) the transition from the clean static model to the recursive-dynamic NICE exercises is not theoretically characterized sufficiently carefully; and (iv) the main quantitative conclusions rely heavily on modeling restrictions that suppress precisely the heterogeneity that makes differentiated carbon pricing potentially attractive.

My recommendation is therefore **rejection in the present form**, with the view that a substantially reworked paper could be valuable. In particular, I think there is a publishable paper here if the authors narrow and sharpen the theoretical contribution, correct the dynamic result, and make the quantitative exercise a more disciplined test of the mechanism rather than an application of a highly restrictive baseline model.

### 1. The main contribution needs to be more sharply distinguished from the existing literature

The central insight is compelling but, as currently presented, the paper risks overstating its novelty. The manuscript itself correctly notes that the separation between efficiency and distribution under a uniform price plus transfers, and the gains from permit trading, are standard results.

More importantly, the paper does not discuss Fleurbaey, Kornek, and Edenhofer (2025), whose discussion paper appeared in October 2025 and is strikingly close in conceptual scope. They explicitly study the relationship between non-uniform national carbon prices, equity, transfers, and carbon markets, and state that a common carbon price can implement the relevant social objectives when transfers or initial permit allocations can be adjusted, whereas differentiated prices may become preferable when those instruments are constrained.

This does not necessarily eliminate the contribution of the present paper. The present manuscript appears to offer something more specific: a closed-form mapping from a *given price schedule* into an *equivalent permit allocation*, including a second-order expression and an application to concrete proposed schedules. That may well be distinct. But the manuscript currently presents the result as essentially the first mapping of this type without discussing a paper that is sufficiently close that readers will immediately ask whether the contribution is merely a more particular implementation of the same insight.

I therefore recommend substantially rewriting the contribution section around a precise statement of what is new relative to Fleurbaey et al., Chichilnisky and Heal, Montgomery, and the broader literature on burden sharing. The authors should explicitly answer:

> What theorem, quantitative mapping, or policy implication in this paper cannot be obtained directly from the framework of Fleurbaey, Kornek, and Edenhofer (2025)?

A satisfactory answer could be the explicit exchange-rate formula between a national price concession and initial permit endowments, especially the second-order term and the dynamic price-weighted version. But that should become the sharply stated contribution rather than the broader claim that differentiated prices are “equivalent to transfers,” which is closer to existing welfare-theoretic reasoning.

The issue is especially important because *JEEM* currently emphasizes theoretical analyses that are novel and of broad interest, rather than extensions of well-known models.

### 2. Proposition 3 is incorrect as stated unless utility is linear in consumption

This is, in my view, the most important technical issue.

Immediately before Proposition 3, the paper states that under total utilitarianism the country's objective is

$$
\sum_t \beta_t N_{it}u_{it}.
$$

The proposition then states that the first-order welfare-equivalence condition is

$$
\sum_t \beta_t p_t^*(R_{it}-E^A_{it})=0.
$$

But this follows only if the objective is linear in consumption, \(u(c)=c\), or if the \(u'(c)\) terms have otherwise been normalized away. Under a general utility function, a first-order change in consumption enters welfare multiplied by marginal utility. The correct generic first-order condition is of the form

$$
\sum_t \beta_t N_i u'_{it}(c_{it})
p_t^*(r_{it}-e^A_{it})=0,
$$

up to the additional state-variable effects in a genuinely dynamic model. The manuscript's proof of Proposition 3 simply says that the consumption gain is multiplied by population and discounted by \(\beta_t\); it never introduces marginal utility.

This is not just a presentation issue because the paper later introduces an explicitly inequality-averse welfare variant with \(\eta=1.5\), where the distinction is economically material.

The easiest repair is to state Proposition 3 explicitly for **linear intertemporal welfare in aggregate consumption**. If the authors want the more general utilitarian proposition, they should derive the marginal-utility-weighted version and then show that the consumption formulation used in the main quantitative analysis is the special case \(u'(c)=1\).

More interestingly, the generalized proposition could substantially strengthen the paper. It would establish that the relevant “exchange rate” between differentiated prices and rights depends not only on discounted carbon prices, but also on the social marginal value of consumption in each country and period. That would provide a clean bridge between the paper's consumption and welfare variants rather than treating the latter as an appendix robustness check.

### 3. The dynamic result conflates an accounting equivalence with a genuine dynamic welfare equivalence

Relatedly, the dynamic section assumes separable abatement costs and derives equivalence period by period. The manuscript then moves to NICE, where international transfers enter national income and affect investment and subsequent growth.

This creates a conceptual gap.

In the analytical model, the value of an additional permit in period \(t\) is \(p_t^*\). In NICE, however, a transfer in period \(t\) changes national income, investment, future output, and therefore future consumption and emissions. Consequently, the marginal value of a right is not generally just its contemporaneous market price. The authors acknowledge this indirectly by noting that residual gaps between the first-order approximation and the simulation arise partly through the effect of transfers on capital accumulation.

But this means that Proposition 3 does not provide the theoretical foundation for the full dynamic quantitative exercise as currently written. It provides a benchmark accounting identity under restrictive conditions, after which NICE numerically incorporates channels outside the proposition.

I would strongly encourage the authors to make this distinction explicit. There are at least three possible approaches:

1. Restrict Proposition 3 to a model in which international transfers do not alter future state variables, and present NICE as a quantitative extension beyond the theorem.
2. Derive a genuinely dynamic equivalence condition using the envelope theorem, where the value of the transfer includes its effect on the country's shadow value of wealth and future capital.
3. Do both, and show how far the simple discounted-price formula remains an accurate approximation once the capital channel is introduced.

At present the exposition makes the theorem sound more general than it is.

### 4. “Equivalent rights” are not always a feasible cap-and-trade allocation

The paper is particularly interesting when it finds negative equivalent rights under Equal Right. For example, the United States has an equivalent ratio of −0.34 under the 5%-per-year path. The authors correctly explain that matching such a schedule requires the country to buy rights for all of its emissions and then make an additional payment; they therefore interpret the negative allocation as a net payment.

But this creates an important qualification to the headline equivalence.

A standard cap-and-trade allocation consists of a non-negative permit endowment. A negative “initial allocation” is not an initial allocation of permits; it is a liability or a lump-sum transfer obligation. Thus the general statement that *any* differentiated price schedule is equivalent to “a uniform price combined with an allocation of tradable emission rights” is too strong unless the institutional mechanism is allowed to include negative endowments, side payments, or some equivalent fiscal instrument.

This matters particularly because the paper's motivating question is precisely whether international monetary transfers are politically infeasible. A negative-rights equivalent is, economically, very close to the transfer mechanism that the proposal is intended to avoid.

I would therefore distinguish three objects throughout the paper:

* a non-negative permit endowment;
* a permit endowment plus an unrestricted lump-sum transfer;
* a generalized international liability/payment.

The central theorem is strongest under the second interpretation. The paper should be much more explicit about which institutional feasibility set it is considering.

### 5. The quantitative exercise assumes away an important source of the case for differentiated prices

The NICE implementation is elegant in its simplicity, but the restriction that all countries share the same abatement-cost function is a very consequential assumption. The manuscript states that all countries differ only through their baseline emissions, so that a given carbon price induces the same proportional abatement everywhere and the price elasticity is common across countries.

That is precisely the channel through which the paper removes potentially important reasons for differentiated carbon prices.

In the theoretical formula, the equivalent-rights schedule depends on the country's emissions and its price elasticity. In the model, the latter is effectively common across countries. In reality, marginal abatement costs can differ substantially across countries because of energy mixes, industrial composition, technology, pre-existing policies, infrastructure, and opportunities for fuel switching. There is therefore a risk that the quantitative result is being driven by the assumption that the most important efficiency-relevant dimension of heterogeneity is absent.

This concern is reinforced by the literature the paper itself reviews. The second-best rationale for differentiated prices includes pre-existing tax distortions and international market power, while recent work continues to emphasize sectoral heterogeneity and trade. A JEEM paper arguing for a common price should demonstrate robustness to at least some economically plausible heterogeneity in marginal abatement costs.

I would regard the following exercises as important:

* allow country-specific abatement-cost curvature or elasticities;
* introduce systematic heterogeneity correlated with income;
* allow different sectoral carbon intensities and sectoral abatement opportunities;
* examine how the equivalent-rights formula changes when the elasticity differs across countries.

Without such exercises, the quantitative section demonstrates the mechanism within NICE, but it does not establish that the numerical magnitude of the efficiency gain is robust.

### 6. The treatment of the concrete policy proposals needs more sensitivity analysis

The implementation of the three policy proposals involves several discretionary choices that can materially affect the results.

For Wolfram et al., the original proposal targets four emissions-intensive industries, whereas NICE has no sectoral detail. The authors consequently apply the proposed price floors economy-wide and explicitly acknowledge that the resulting scenario is more ambitious than the original proposal.

For Banerjee, Duflo, and Greenstone, the proposal does not specify a numerical price for high-income countries, so the paper assigns them the $75/t tier from Parry et al. and Wolfram et al.

For the first two proposals, the paper imposes a common 5% annual increase after 2030 despite the proposals containing only qualitative language about eventual convergence.

Each of these assumptions is defensible as a benchmark, but the headline quantitative results should not be presented as properties of the underlying proposals without sensitivity analysis. In particular, the 4.8% coalition-emissions reduction is a property of the **paper's implementation** of the proposals, not necessarily of the proposals themselves. Table 2 makes clear that the calculations depend on the chosen schedules and coalition-price calibration.

At minimum, I would like to see results under alternative:

* escalation rates;
* high-income-country price assumptions for Banerjee et al.;
* carbon-budget paths and benchmark price paths;
* treatment of the sectoral scope of the Wolfram proposal.

The current exercise would also benefit from reporting results as a function of the price dispersion rather than only for three particular proposals. That would make the paper's central mechanism much more general.

### 7. The role of the surplus-sharing rule should be presented more carefully

The paper reports a 0.049% increase in coalition consumption when the efficiency surplus is allocated to equalize members' proportional gains. This is a useful illustration, but there is no economically privileged reason for that particular sharing rule.

The appendix demonstrates that the quantitative welfare gain varies substantially with the way the surplus is distributed. In particular, the inequality-averse welfare gain is considerably different under marginal-utility sharing and uniform scaling. This is actually an interesting result and deserves more prominence.

I would recommend treating the surplus allocation as an explicit second-stage bargaining problem rather than presenting one particular allocation as *the* consumption gain from uniform pricing. The paper could perhaps characterize a Pareto frontier between the uniform-price regime and the differentiated-price regime, or at least report a systematic range over plausible distributional weights.

This would fit the paper's underlying message better: the important distinction is between the **efficiency frontier generated by a common price** and the **distributional allocation along that frontier**.

### 8. The welfare interpretation of “leaving every country as well off” needs greater precision

The main quantitative exercise uses NPV of aggregate national consumption as the comparison criterion and deliberately designs the recycling rule to neutralize within-country distributional considerations.

That is reasonable for isolating the instrument-efficiency question. But some of the paper's strongest language is broader than the metric supports. The paper repeatedly speaks of countries being “as well off” and of welfare dominance, while the main exercise actually compares a particular measure of national aggregate consumption. The welfare appendix shows that once within-country distribution is evaluated, the quantitative conclusions can change materially.

I would therefore reserve “welfare” for the welfare variant and use “aggregate consumption” for the benchmark results. The theoretical proposition can still establish a Pareto result in the simplified country-level model, but the empirical implementation should not quietly translate that into a claim about national welfare under arbitrary social preferences.

### 9. The discussion of the political feasibility of transfers is too strong for the evidence presented

The paper's policy motivation rests heavily on the proposition that international transfers are widely regarded as politically infeasible. It then points to survey evidence showing substantial support for international redistribution and concludes that the premise is not solid.

The survey evidence is interesting, but it does not establish institutional or bargaining feasibility. Public support for a hypothetical transfer scheme does not imply that governments can commit to it, that recipient countries would accept the associated conditions, or that international institutions could enforce the required transfers over many decades.

Consequently, I would remove or substantially soften statements suggesting that transfer infeasibility has been empirically disproved. The strongest economic conclusion of the paper does not require that claim. The paper's robust point is instead that **if a common price plus transfers is feasible, differentiation is not a distributionally distinct alternative to transfers; it is one particular way of implementing an implicit transfer.**

That is already an important result.

The final claim that price differentiation “might be justified only in a transition period towards an international cap-and-trade” is not established by the analysis. The paper has not modeled administrative capacity, sovereignty, enforcement, negotiation costs, commitment, or endogenous political constraints sufficiently to support that conclusion. It should be presented as a possible interpretation rather than as an implication of the model.

### 10. The treatment of second-best arguments should be integrated into the core analysis

The paper does a useful job identifying three economic reasons why uniform pricing can fail to be optimal—capital-market distortions, pre-existing fiscal distortions, and international market power—and it explicitly notes that these mechanisms need not have an income-based structure.

But the analysis subsequently becomes almost entirely first-best.

This creates an asymmetry: the paper's main normative conclusion is framed broadly, while its formal result applies to a setting without the very distortions that can justify non-uniform pricing.

I would suggest making the scope condition much sharper. The strongest valid statement is approximately:

> Conditional on a common marginal social cost of emissions, competitive international goods markets, no relevant pre-existing distortions, and a sufficiently flexible transfer/endowment mechanism, differentiated national carbon prices can be represented as a distributional allocation under a common price, with a second-order efficiency loss from the price dispersion.

That is a strong and defensible theorem. The paper should then separately ask how far the equivalence survives under each departure from first best. Even one extension—say, pre-existing domestic tax distortions or heterogeneous abatement costs—would substantially strengthen the paper.

### 11. A few presentation issues

There are several smaller points that should be corrected.

First, the manuscript says that Section 4 traces the result for “seven major economies,” but Figure 1 contains eight country panels and Table 1 contains eight economies.
Second, the terminology “autarky” is somewhat misleading in Section 4, where the country under consideration changes its price while the rest of the world adjusts its price to maintain the global emissions path. This is not autarky in the usual international-economics sense. “Domestic differentiated-price counterfactual” would be more precise.

Third, the first-order approximation is sometimes described as though the mapping itself were approximately linear, whereas the exact static formula in Proposition 1 is particularly attractive and does not require a small price gap. I would emphasize the exact result more strongly and treat the elasticity approximation as an easily interpretable corollary.

Fourth, the paper should consistently distinguish the allocation that makes a country indifferent at a fixed price \(p^*\) from the allocation obtained after jointly changing the cap and therefore the equilibrium price. The manuscript itself notes that these differ because cutting the rights raises the price. This distinction is important enough to deserve explicit notation throughout.

### Conclusion

I believe the paper contains a good core idea, and I particularly like the closed-form expression

$$
r_i^*=e_i^A-\frac{b_i}{p^*},
$$

because it makes transparent the distinction between the first-order distributional effect of price differentiation and its second-order efficiency cost. The subsequent interpretation of differentiated prices as implicit transfers is potentially useful for the international climate-policy literature.

But the paper currently asks the reader to accept too broad a conclusion from too narrow a formal environment. The central dynamic proposition needs correction, the novelty claim needs to be reconsidered in light of closely related work, and the numerical exercise needs to demonstrate that its quantitative magnitudes are not artifacts of common abatement costs, absent trade, the economy-wide application of sectoral proposals, and several ad hoc assumptions about price paths. The paper itself acknowledges some of these limitations, particularly the absence of a capital market and sectoral detail and the omission of trade.

**Recommendation: Reject in present form.**

I would nevertheless encourage the authors to pursue a substantially revised version. The most promising route, in my view, is to make the paper a sharper contribution on the **equivalence between non-uniform carbon prices and international permit endowments**, derive the generalized welfare-weighted dynamic theorem correctly, and then use NICE to quantify how good that approximation is once capital accumulation and other dynamic channels are introduced. That would produce a considerably more precise and potentially stronger contribution than the current broader claim that differentiated prices are simply transfers in disguise.
