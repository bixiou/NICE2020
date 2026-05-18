## Codebook

Ce document détaille les variables de sortie du modèle.

*NB : $ = 2017 US dollars*
### country output

* `ABATEFRAC` (%): part du PIB dédié à la réduction du coût d'émissions
* `E_gtco2` ($10^9$ tCO2/an): émissions totales de GES
* `cons_EDE_country` ($10^3$ $/an/pers) : consommation équivalente à un bien-être équitablement réparti dans un pays donné
*  `gini_cons` (%) : index Gini pour un pays donné
* `CPC_post` ($10^3$ $/an/pers) : consommation par tête après le recyclage du revenu
* `CPC` ($10^3$ $/an/pers) : consommation par tête
* `C` ($10^6$ $/an/pers) : consommation du pays
* `country_carbon_tax` ($/tCO2) : taux d'imposition du CO2
* `country_pc_dividend_domestic_transfers` ($10^3$ $/an/pers) : dividendes fiscaux par personne provenant de la redistribution à l'intérieur du pays
* `country_pc_dividend_global_transfers` ($10^3$ $/an/pers) : dividends fiscaux par personne venant des transferts internationaux
* `country_pc_dividend` ($10^3$ $/an/pers) : dividendes fiscaux totaux, y compris cex des transferts internationaux
* `tax_revenue` ($/an) : revenu de la taxe carbone pour un pays donné
* `YGROSS` ($10^6$ tCO2/an/pers) : production brute
* `LOCAL_ DAMFRAC_KW` (%) : dommages au niveau du pays en pourcentage du PIB net, calculés en fonction des températures locales et selon Kalkuhl et Wenz
* `local_temp_anomaly` : anomalie de température moyenne de surface (par rapport au niveau préindustriel)
* `μ` (%) : taux d'atténuation des émissions de GES (part des émissions évitées)
* `Y_pc` ($/an/pers) : production nette par habitant, après déduction des coûts d'abattement et des dommages climatiques
* `net_surplus` ($10^3$ $/an) : excédent de recettes nettes au niveau national venant de la taxe sur la consommation
* `net_surplus` ($10^3$ $/an) : solde net des recettes fiscales de consommation par pays et par année, après déduction de la contribution au pool et ajout du montant recyclé
* `net_surplus_per_pib` : net_surplus divisé par la production brute du pays (`YGROSS`), donc le surplus net relatif à l’économie du pays
* `net_transfer_pib` : flux net de transfert entre le pays et le mécanisme de partage/recyclage
* `nice_net_output` ($10^6$ $/an): PIB net national (après abattement et dommages)
* `l` ($10^3$ pers) : population totale ou population active
* `transfer` ($) : montant monétaire du transfert national lié aux droits d’émission
* `transfer_pc` ($/$10^3 hab) : transfert ramené à la population
* `transfer_over_gdp` (%) : ratio du transfert sur le PIB du pays


### global output

* `E_gtco2_club` ($10^9$ tCO2/an) : émissions totales de GES sommées pour les pays du club
* `E_Global_gtco2` ($10^9$ tCO2/an) : émissions totales de GES sommées pour tous les pays
* `cons_EDE_global` ($10^3$ $/an/pers) : consommation EDE au niveau global
* `gini_cons_global` (%) : index Gini mondial
* `CPC_post_global` ($10^3$ $/an/pers) : consommation par tête après le recyclage du revenu, au niveau mondial
* `YGROSS_global` ($10^12$ $/an) : production brute, niveau mondial
* `global_revenue` ($10^3$ $/an) : recette carbone mondiale recyclée au niveau international
* `temp` : anomalie de température globale (°C au-dessus du niveau préindustriel), issue du module climatique FAIR
* `total_tax_revenue` ($10^3$ $/an) : revenu de la taxe carbone, niveau mondial


### quantile output

* `conso_pc_post_recycle` ($10^3$ $/an/pers) : consommation par tête du quantile après dommages, coûts d’abattement, taxe carbone et recyclage des recettes vers les quantiles
* `tax_burden_distr` : part de la charge fiscale carbone attribuée à chaque quantile, calculée à partir de l’élasticité de revenu et des parts de consommation
* `conso_pc_base` : consommation par tête du quantile avant dommages climatiques, avant coûts d’abattement et avant taxe carbone
* `conso_pc_post_tax` : consommation par tête du quantile après déduction de la taxe carbone et après ajustement des transferts PIB, mais avant le recyclage final de ces recettes
* `conso_pc_post_damage_abatement` : consommation par tête du quantile après avoir subi les pertes dues aux dommages climatiques et les coûts d’abattement, avant taxation et redistribution
* `qc_share` (%) : part de chaque quantile dans la consommation globale post-recyclage du pays

### regional output

* `E_gtco2_rwpp` ($10^9$ tCO2/an) : somme des émissions de tous les pays de la région WPP
* `CPC_rwpp` ($10^3$ $/an/pers) : consommation régionale par habitant
* `Y_pc_rwpp` ($10^3$ $/an/pers) : production nette régionale par habitant, après déduction des coûts climatiques et des dommages
* `CPC_post_rwpp` ($10^3$ $/an/pers) : consommation régionale par habitant après redistribution des recettes fiscales et du recyclage des transferts
* `gini_cons_rwpp` : indice de Gini de la distribution de consommation au niveau régional WPP
* `cons_EDE_rwpp` ($10^3$ $/an/pers) : consommation équivalente également distribuée (EDE) régionale