# Raffiner la modélisation d'un Cap & Share

## Principaux objectifs du projet

0. Installer et faire tourner NICE.
1. Modéliser des scénarios à participation partielle, avec une liste donnée de pays qui met en œuvre le prix du carbone.
2. Créer des cartes pour présenter les effets distributifs des sorties de NICE.
3. Optimiser sous contrainte d'un budget (plutôt que d'un prix) carbone. => Erwan
4. Permettre une allocation personnalisée des recettes carbone, à partir d'une répartition du budget carbone entre pays (en modifiant revenue_recycle.jl ou bien loss_damage). => Erwan
5. Faire tourner NICE avec des trajectoires exogènes de PIB et d'émissions. Puis, au contraire, intégrer la rétroaction des transferts sur le PIB. => Marius
6. Prédire le ratio de l'empreinte carbone par rapport aux émissions territoriales d'un pays en utilisant son PIB par habitant et sa balance commerciale, à partir de données récentes. => Marius
7. Modéliser les coûts et bénéfices en nominal (à partir des chiffres en PPA).
8. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 
9. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.


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


## 3. Optimiser sous contrainte de budget carbone (Erwan)
1. Utiliser le code envoyé par Marie (cap_and_share/find_global_exp_carbon_tax.jl) pour trouver la trajectoire exponentielle optimale sous contrainte de budget carbone intertemporel, disons 1000 GtCO2 pour 2025-2100.
2. Adapter ce code au cas de participation partielle : on dote les pays participants d'un budget carbone, et on optimise leur bien-être (sans tenir compte du bien-être des pays non participants).
3. Coder un module qui permet de trouver le prix période par période d'un budget carbone défini période par période (qui marche également avec participation partielle).
4. Coder un module qui permet de trouver le prix à partir de budgets carbone définis pour une période donnée dans un pays donné (en entrée on a une table de budgets carbone régions x dates, et en sortie on a une table de prix correspondants). 


## 4. Allocation personnalisée des recettes (Erwan)
1. À partir de (i) budgets carbone intertemporels (en termes de droits d'émissions) par régions et (ii) d'une règle de partage des droits, permettre de désagréger les recettes pays par pays. La règle de partage des droits par défaut est "equal_pc": on divise le budget carbone de la région proportionnellement à la population moyenne de chaque pays sur la période. Note que "budget carbone" est une terminologie abusive, car ce qu'on partage ici ce sont les recettes (avec un prix uniforme dans l'union climatique), mais on les partage proportionnellement aux droits d'émissions.

Tester avec les budgets suivants en GtCO2 pour 2030-2080: 

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

Pour la période pre-2030, prendre le BAU pour les émissions (vous obtenez combien d'émissions mondiales pour 2025-2030 ?)

2. Permettre de définir les budgets carbone (en termes de droits d'émission) au niveau région x date (désagréger temporellement l'étape précédente). Ainsi, pour une date donnée, le budget carbone de l'union égal la somme des budgets carbone des régions, on en déduit le prix international; puis on répartit les recettes entre régions proportionnellement aux budgets carbone régionaux à cette date; enfin on répartit les recettes pays par pays à l'aide de la règle de partage des droits (equal_pc par défaut).

3. Quand vous aurez fait ça, on pourra programmer d'autres règles de partage des droits.


## 5. Trajectoires exogènes et endogènes (Marius)
1. Trajectoires exogènes: À partir d'une table donnant le PIB par pays pour chaque année de simulation, et d'une table équivalente pour les émissions, neutraliser la partie macro de NICE et faire tourner la partie analyse distributive; prenant en entrée ces deux tables et en sortie les sorties habituelles de NICE.
2. Trajectoire endogène: modéliser la rétroaction des transferts sur le PIB. Plus précisément, dans net_economy.jl:40, ajouter les transferts à v.Y[t,c] = (1.0 - p.ABATEFRAC[t,c]) ./ (1.0 + p.LOCAL_DAMFRAC_KW[t,c]) * p.YGROSS[t,c] + transfers[t,c]


## 6. Prédire l'empreinte carbone (Marius)
1. Chercher dans la littérature (Google Scholar) des papiers qui prédisent carbon footprint ou consumption-based emissions ou carbon content of trade à partir d'autres données (GDP, trade balance). Faire la liste des papiers qui s'en rapprochent le plus (avec abstract ou un résumé de votre cru). Si ça n'existe pas, passer aux étapes suivantes :
2. Vérifier (par ex demander à Marie Young-Brun) si NICE concerne seulement le CO2 fossile ou s'il y aussi LULUCF ou d'autres GHG.
3. Télécharger des données récentes pays par pays de consumption-based emissions (CO2 fossil a priori, selon ce que fait NICE), territorial emissions (CO2 fossil), (log) GDP, trade balance, et energy trade balance (in energy units and in $, et si possible distinguer trade balance pétrole, gaz, charbon, autre énergie). Mettre toutes ces variables en per capita terms.
4. Prédire l'empreinte carbone (consumption-based emissions per capita) à partir des autres variables. Par exemple, considérer l'année la plus récente où les données sont dispos, tester des régressions linéaires avec éventuellement termes d'interaction et termes quadratiques. Sélectionner la régression avec le meilleur adjusted R^2.
5. Observer graphiquement l'acuité des prédictions, calculer le mean absolute error. Chercher une façon d'améliorer les prédictions sur les éventueles outliers.
6. Faire tourner les régressions sur les données d'il y a ~20 ans et voir si ça prédit bien les empreinte carbone actuelles, en mettant en entrée les variables actuelles de PIB et émissions, mais en calculant la trade balance à partir de celle d'il y a 20 ans (en % du PIB).
7. À partir des intensités carbone (tCO2/$) des non-energy imported goods (si pas de données là-dessus, chercher des données sur non-energy global GDP), des intensités carbone de chaque énergie, et des données de trade balance par energy goods et non-energy aggregate good, estimer le contenu carbone des importations/exportations par pays. Voir (graphiquement, R^2, mean absolute error) si ça prédit bien l'empreinte carbone.
8. Comparer le mean absolute error de 4 et 6.


## Questions 
- Does NICE model the feedback effect of transfers on GDP? => No.
- "a global and uniform carbon tax leads to the same abatement rate trajectory in every country, but to heterogeneous abatement costs in terms of share of gross output" Why?