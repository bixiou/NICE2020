# Raffiner la modélisation d'un Cap & Share

## Principaux objectifs du projet

0. Installer et faire tourner NICE.
1. Modéliser des scénarios à participation partielle, avec une liste donnée de pays qui met en œuvre le prix du carbone.
2. Créer des cartes pour présenter les effets distributifs des sorties de NICE.
3. Permettre une allocation personnalisée des recettes carbone, à partir d'une répartition du budget carbone entre pays (en modifiant revenue_recycle.jl ou bien loss_damage et calculer prix exponentiel correspondant). => Erwan
4. Faire tourner NICE avec des trajectoires exogènes de PIB et d'émissions. Puis, au contraire, intégrer la rétroaction des transferts sur le PIB.
5. Modéliser les coûts et bénéfices en nominal (à partir des chiffres en PPA).
6. Prédire le ratio de l'empreinte carbone par rapport aux émissions territoriales d'un pays en utilisant son PIB par habitant et sa balance commerciale, à partir de données récentes.
8. Optimiser sous contrainte d'un budget (plutôt que d'un prix) carbone.
9. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 
10. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.


## 0. Installation et exécution

### Étapes
- Installer VS Code, les extensions: Julia
- Créer un compte github
- Cloner dépôt NICE2020

### Liens utiles
- [Documentation Julia](https://docs.julialang.org/en/v1/manual/getting-started/)
- [Documentation Mimi](https://www.mimiframework.org/Mimi.jl/stable/tutorials/tutorial_1/#Tutorial-1:-Install-Mimi-1)
- [NICE repository](https://github.com/bixiou/NICE2020)
- [Documentation git](https://git-scm.com/book/en/v2)

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 1. Participation partielle
Scénarios à tester:
1. All: Whole World
2. All except rich oil countries: World except Russia, Kazakhstan, Saudi Arabia, Qatar, Kuweit, Azerbaijan, Oman, Bahrein, Malaysia
3. Optimistic scenario: Africa + Latin America + South Asia + South-East Asia + China + EU28 + Norway + Switzerland + Canada + Japan + Korea + NZ
central <- all_countries[df$npv_over_gdp_gcs_adj > 0 | df$code %in% c("CHN", EU28_countries, "NOR", "CHE", "JPN", "NZL")]
4. Generous EU: EU27 + China + Africa + Latin America + South Asia + South-East Asia
5. Africa-EU partnership: EU27 + Africa


## 2. Cartes
- Utiliser/adapter le code cap_and_share/map.R pour créer une carte des résultats, notamment (i) les gains/pertes par pays par rapport à la situation sans transferts internationaux et (ii) la variation de bien-être (EDE) par rapport au BAU.


## 3. Allocation personnalisée du budget carbone
- Utiliser le script cap_and_share/find_global_exp_carbon_tax.jl pour calculer la trajectoire exponentielle de prix qui maximise le bien-être pour un budget carbone donné, disons 1000 GtCO2 pour 2025-2100.
- Permettre de définir le budget carbone pays par pays. Tester avec les budgets suivants en GtCO2 pour 2030-2080: 

      rights rights_proposed cumulative_rights_30_future
AFR      166             144                         144
AUS        3               3                           3
CAN        4               4                           4
CHI      120             154                         134
CSA       49              50                          50
EEU        9              10                          10
FSU       25              27                          27
IND      133             135                         135
JPN        9              11                          11
MEA       37              35                          35
MEX       12              12                          12
ODA      115             113                         113
SKO        4               5                           5
USA       32              32                          32
WEU       37              13                          39
World    754             748                         754
union    653             647                         653

Pour info, tout ça vient d'un code que j'ai créé dans https://github.com/bixiou/global_tax_attitudes/blob/main/code_global/map_GCS_incidence.R, starting line 2357

Sachant que l'union est composée de:
regions_union <- c("AFR", "CHI", "IND", "CSA", "MEX", "ODA", "EEU", "WEU", "JPN", "SKO") 
Y a probablement pas besoin de définir les budgets carbone pour les pays hors de l'union.
Cf. map.R pour le mapping pays-région.
Pour désagréger au niveau pays, faire equal pc aussi de chaque région en utilisant la population moyenne de chaque pays sur la période.
Pour la période pre-2030, prendre le BAU pour les émissions (vous obtenez combien d'émissions mondiales pour 2025-2030 ?)

- Quand vous aurez fait ça, on pourra programmer d'autres règles.


## Questions 
- Does NICE model the feedback effect of transfers on GDP? => No.
- "a global and uniform carbon tax leads to the same abatement rate trajectory in every country, but to heterogeneous abatement costs in terms of share of gross output" Why?