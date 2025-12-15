## Tâches pour Adrien
- Mettre à jour rights_proposed
- Expliquer méthodo en annexe

## Tâches à effectuer

0. Installer et faire tourner NICE; prendre en main Julia.
1. Tester prix différenciés du FMI ($25/t LIC & LMIC, $50 UMIC, $75 HIC pour 2025-30, increasing at x% beyond that, where x is chosen to get us to 2+/-.1°C), et d'Equal Right: comparer le welfare avec un equal pc cap-and-trade.
1bis. Model Cramton & Stoft (midway between grandfathering and equal pc)
2. Year at which undiscounted aggregate EDE turns positive
3. Ajouter une redistribution de la conso
4. Réduire la taille des données de sortie.
5. Modéliser une transition entre absence de taxe et taxe optimale pour les premières années.
6. Compute equivalent prices / rights / transfers
7. Décomposer les gains de bien-être
8. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 
9. Estimate welfare of Peskzo, Golub & van der Mensbrugghe (2019)
10. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.
11. Concevoir procédure de décision entre différentes propositions d'écarts à l'allocation de base; rédiger une proposition de traité.
12. IMACLIM?

TODO: différence d'émissions sous cs et ffu

+ voir les commentaires dans revenue_recycle

## NDC.
- Some NDCs are illogical, e.g. Cameroon defines its BAU without LULUCF but its emissions reductions with LULUCF; Angola numbers don't make sense.
- Some countries have less stringent NDC in 2035 compared to 2030, e.g. Angola.

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



## 1. Simuler des prix différenciés

### Étapes
- Rajouter une section dans FFU.jl où simuler des prix différenciés du FMI (sans transferts internationaux, i.e. within-country recycling).
- Trouver dans le code comment le coder.
- Simuler les prix suivants : de 2025 à 2030, $25/tCO2 pour les LICs et LMICs, $50 pour les UMICs, $75 pour HICs. Au-delà de 2030, faire croître le prix de x% par an. Tester plusieurs x et s'arrêter lorsque le x est tel que la température en 2100 est +2 +/- 0.05 °C.
- Rapporter ci-dessous la conso EDE mondiale et dans les principaux pays (disons CHN, USA, DEU, IND, COD, RUS) pour les scénarios suivants: FMI, cap and share, BAU, et pour les dates suivantes: 2030, 2050, 2100, net present value 2030-2100.
- Rajouter une nouvelle section et simuler les prix différenciés proposés par Equal Right (cf. /papers/Equal_Right_prices.xlsx). Pour ce faire, définir les prix en 2025 à partir de la colonne Carbon_charge_rates$I, avec l'augmentation annuelle déterminée par la colonne Global2$D.

- 1bis Stoft: Scénario cap_and_share sauf qu'on met global_recycle_share à 0.1

### Problèmes rencontrés / observations

On crée deux nouveaux scénarios : IMF et IMF_2. 
- IMF : taxe carbone différenciée en fonction du niveau de revenu des pays, qui reste constante de 2025 à 2300
- IMF_2 : même chose mais à partir de 2030, la taxe croit au taux x% par an, avec x calibré de manière à ce que la température en 2100 soit +2°C +/- 0.05.

Dans data/parameters.jl, on crée les différentes catégories en suivant la classification de la Banque Mondiale. Le Venezuela et l'Ethiopie sont exclus de la classification, on décide avec leur PIB/hab de les classer respectivement comme UMIC et LMIC.
Pour le taux de croissance de la taxe, on a testé différentes valeurs, puis fait tourné le modèle pour trouver la température en 2100 jusqu'à atteindre environ 2°C : 2.02°C pour un taux de 3.6%.

+ Stoft 

### Où en est-on ?

Fait

### Résultat

On crée : cap_and_share/output/comparison_output.csv qui permet de comparer la consommation EDE au niveau mondial et de certains pays en 2030, 2050 et 2100 pour l'ensemble des scénarios de FFU. 
On crée également des histogrammes pour comparer les taux de variation de la consommation EDE des scénarios par rapport au scénario BAU, et par rapport au scénario non_losing, en 2030, 2050 et 2100 au niveau mondial et pour certains pays.


## 2. Year at which undiscounted aggregate EDE turns positive

### Étapes
- Créer une fonction qui calcule l'année à partir de laquelle la conso EDE devient supérieure à la valeur BAU (par pays et au niveau mondial), ajouter ça lors de l'export des sorties du modèle.

### Problèmes rencontrés / observations

On crée une section dans FFU (#Year at which consumption_EDE becomes higher than consumption_EDE in the BAU scenario) et une fonction dans helper_functions qui retourne, pour un scénario donné, la première année à laquelle la consommation EDE devient de façon permanente supérieure à la consommation EDE de la même année dans le scénario BAU.
Dans cap_and_share/output, on crée un year_EDE_higher_than_BAU.csv qui contient, pour chaque pays et au niveau mondial et pour plusieurs scénarios (FFU, Global_cap_share, IMF2 et Stoft), l'année où conso_EDE > conso_EDE_BAU. 

### Où en est-on ?

Fait

### Résultat


## 3. Ajouter une redistribution de la conso

### Étapes
- Dans quantile_recycle, ajouter la possibilité d'une redistribution de la conso, prenant comme paramètres la part du PIB net à verser au monde entier (par défaut: 1%), la fraction perdue par inefficience de la redistr, les quantiles concernés et les taux auxquels ils sont taxés (par défaut: 1% pour le top 20%, 5% pour le top 10%). 
- D'abord calculer les recettes totales et les nouvelles consos par quantile une fois la taxe prélevée. 
- Allouer la partie mondiale proportionnellement à la population des pays et calculer la somme dispo à redistribuer par pays "conso_tax_obtained", une fois enlevée la fraction perdue par inefficience (par défaut: 10%). Si la conso_tax_obtained est négative, émettre un Warning. Exporter la liste des conso_tax_obtained/conso_totale par pays x année.
- Pour chaque pays, redistribuer conso_tax_obtained en commençant par les plus pauvres: tant que conso_tax_obtained n'est pas épuisée, remonter les k déciles les plus pauvres au niveau du k+1 ème; puis répartir ce qui reste à part égale aux k plus pauvres. Si conso_tax_obtained était négative, faire le symétrique en partant des déciles les plus hauts (et en les taxant). 
- TODO plus tard: permettre une répartition internationale en fonction du poverty gap

### Problèmes rencontrés / observations

Dans net_economy, on calcule la contribution de chaque pays dans le "pot commun" = % de son PIB, puis la redistribution que chaque pays reçoit (part égale par habitant).
Si consumption_tax = 1, la différence entre ce que le pays reçoit et sa contribution se rajoute au PIB net.

Dans quantile_recycle, on calcule la taxe globale sur le 9ème quantile et celle sur le 10e. 
Puis, on calcule le surplus net = revenue de la taxe sur la consommation*(1-taux d'inefficience) + redistribution du pot - contribution au pot commun.
On restribue le surplus net d'abord au premier quantile jusqu'à ce que sa consommation par habitant = celle du deuxième quantile. Puis on redistribue au premier et deuxième jusqu'à ce que leur consommation par hab = celle du troisième quantile... et ainsi de suite jusqu'à avoir épuisé tout le surplus. 

Problèmes: définir à quelle étape de la construction de CPC_post on rajoute ce scénario.
- La taxe est calculée sur la consommation de base, avant toute transformation, mais on l'enlève à la consommation après la fonction d'abatement
- La fonction d'abatement s'applique sur la consommation de base
- Faire :
v.conso_pc_post_tax[t,c,q] = v.conso_pc_post_tax[t,c,q] + p.switch_consumption_tax*(v.new_conso_pc[t,c,q]-v.conso_pc_base[t,c,q])
pas très logique + vérifier qu'il n'y a pas d'erreur parce que si new_conso_pc pas défini pour les quantiles les plus élevés où le surplus a déjà été épuisé, alors on se retrouve avec 0-conso_pc_base < 0

=> C'est pas logique : 
(1) On a la consommation de départ qui sert à calculer le montant de la taxe à prélever. 
(2)Ensuite la consommation "subit" les dommages climatiques et diminue. 
(3) Puis on enlève la taxe le montant de la taxe. 
(4) Puis on redistribue le revenu généré par la taxe sur la consommation => rajouter à la conso de l'étape 3


### Où en est-on ?

### Résultat


## 4. Réduire la taille des données de sortie.

### Étapes
- Faire un tableur avec la liste des variables, un indicateur disant si elles sont exportées, et si oui quelles années sont exportées.
- Identifier dans une nouvelle colonne des variables inutiles à exporter, ainsi que des années inutiles au sein de variables utiles.
- Une fois la proposition de réduction des exports validée, l'implémenter.
- Potentiellement améliorer le code d'exportation (pour que les chemins de fichiers aient plus de sens).

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 5. Modéliser une transition entre absence de taxe et taxe optimale pour les premières années.

### Étapes
- Modifier le code qui calcule la taxe optimale pour un budget donné, afin d'avoir une trajectoire croissante linéaire les 5 premières années. Pour l'instant, ce code calcule le niveau initial de la taxe t0 et son taux de croissance optimaux pour atteindre un budget donné. Changer le code pour qu'il calcule le niveau de t5 et le taux de croissance optimaux, avec t1, ..., t4 interpolés linéairement entre t0 = 0 et t5.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 6. Compute equivalent prices / rights / transfers

### Étapes
- Compute two types of equivalent prices, rights and transfers:
    - In the autarchy case with differentiated prices:
        - Compute the uniform price trajectory p* that achieves the same emission trajectory as in autarchy, and compute emissions by country according to this price trajectory (and equal per capita rights (even though that's an approximation, ideally we should use the rights defined below)).
        - For each country-year, define/export "welfare-equivalent rights per capita" as r = (abatement+damage cost in cap-and-trade - abatement+damage cost in autarchy + emissions in cap-and-trade)/(p* * population)
        - Also define/export "emission-equivalent rights per capita" as r = emissions in autarchy / population
        - Define the "welfare-equivalent transfer p.c." as t = abatement cost p.c. in cap-and-trade (with welfare-equivalent rights) - abatement cost p.c. in autarchy
        - Define the "emission-equivalent transfer p.c." as t = (emission-equivalent rights per capita - emissions in cap-and-trade) p*
    - In the cap-and-trade case (for these, be careful that negative values are possible):
        - Compute the "welfare-almost-equivalent price" as p = a^(-1)(abatement_cost* + (emission* - rights*)p*) where * denotes cap-and-trade values and a^(-1)(x) = p_backstop * (population * x / (YGROSS * theta_1))^((theta_2 - 1)/theta_2) - It is only "almost" equivalent because it doesn't account for the higher climate damages in autarchy (doing so would require optimizing over all country-year prices, it's too computationally intensive)
        - Compute the "emission-equivalent price" as p = emission^(-1)(right p.c.) where emission^(-1) is given by find_tax_for_country_year! [a faster but less precise alternative is to use p = p_backstop * (1 - right / (YGROSS * sigma))^(theta_2 - 1)]
    

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 7. Décomposer les gains de bien-être

### Étapes
- Écrire une fonction qui prend deux scénarios, par défaut le cap-and-trade with rights_proposed et le BAU.
- Pour le monde et pour chaque pays, pour une année donnée (par défaut 2050), calculer les éléments suivants :
    - dommages évités: différence de dommage évité par personne (-LOCAL_DAMFRAC_KW/population) entre les deux scénarios (1 - 2)
    - transferts directs: différence de transfer_pc
    - croissance: différence de YGROSS*(1-s)
    - coût d'abattement: différence de ABATECOST/population (va donner qq chose de signe contraire à dommages évités)
    - réduction des inégalités: ((conso_EDE_1 - C_1) - (conso_EDE_2 - C_2))*C_2/C_1
    - amélioration totale: différence de conso_EDE
    - résidu: amélioration totale - somme(5 autres)
- Calculer la Net Present Value de chaque variable x_t qui précède de t0 à t_max au tax de R% (t0, t_max, R sont des paramètres de la fonction avec pour défaut 2025, 2100, 3%): sum_t0^tmax(x_t/(1+R)^(t-t0))
- Coder une option de calculer R à partir des sorties du modèles: R = rho + η * g, avec rho un paramètre (= 0 par défaut), η est déjà défini, et g: croissance moyenne de la conso EDE entre t0 et t.
- Exporter un graphique donnant la décomposition au cours du temps pour le monde entier et pour les pays majeurs, avec des stacked barres pour tout sauf amélioration totale, qui est elle en trait plein.

### Problèmes rencontrés / observations

### Où en est-on ?

Calcul de la net present value : FAIT
Fonction créée dans helper_functions qui permet de retourner la valeur présente nette puis dans FFU on calcule la VPN pour tous les scénarios de 2030 à 2100 avec un taux à 3%. 
On a crée cap_and_share/output/net_present_value_cons_EDE.csv + graph_npv.png grâce au fichier R graph_cons_EDE.r

### Résultat


## 8. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 

### Étapes
- Télécharger les données du WID et lire dans leur doc / papiers de recherche la méthode qu'ils utilisent pour avoir la distribution des revenus. Si c'est pas paramétrique (comme NICE fait, i.e. partir d'un Gini pour en déduire une loi lognormale), continuer les étapes suivantes.
- Faire l'hypothèse que l'inégalité va rester constante au cours du siècle (avant redistribution des recettes du prix carbone), et imputer la distribution des revenus du WID.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 9. Estimate welfare of Peskzo, Golub & van der Mensbrugghe (2019)

### Étapes
- Trouver les sources d'émission par pays: pour charbon, pétrole, gaz, trouver les principaux pays producteurs, agréger ça en termes de "production de CO2", et définir des droits d'émission proportionnellement à la production de CO2 du pays
- Créer deux groupes de pays correspondant à ceux de Peskzo, Golub & van der Mensbrugghe (2019): les producteurs de fossiles vs. le club climat
- Faire tourner NICE avec un scénario A: un cap-and-trade et des droits d'émission précédents mis à l'échelle pour que le total des droits corresponde à une trajectoire 2°C. Regarder le bien-être par pays et mondial en 2030.
- Faire tourner NICE avec un scénario B: un prix uniforme avec within-country recycling dans le club, BAU hors du club, avec le prix du club choisi de telle sorte que les émissions en 2030 coïncident avec celles du scénario A.
- Si le bien-être mondial de A est inférieur à celui de B, calculer le transfert que les pays producteurs devraient donner aux pays à bas-revenus pour que les deux bien-être coïncident (en 2030). L'exprimer en proportion des recettes du prix carbone des pays producteurs dans le scénario A.

### Problèmes rencontrés / observations
- Le modèle n'est pas adapté : il ne modélise pas quels pays produisent des fossiles, et ne modélise pas le prix du pétrole. C'est donc un calcul très approximatif.

### Où en est-on ?

### Résultat


## 10. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.

### Étapes
- Réécrire le code du modèle en R.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 11. Concevoir procédure de décision entre différentes propositions d'écarts à l'allocation de base; rédiger une proposition de traité.

### Étapes
- Adrien: TODO

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat