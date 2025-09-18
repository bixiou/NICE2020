## Tâches pour Adrien
- Mettre à jour rights_proposed

## Tâches à effectuer

0. Installer et faire tourner NICE; prendre en main Julia.
1. Tester prix différenciés du FMI ($25/t LIC & LMIC, $50 UMIC, $75 HIC pour 2025-30, increasing at x% beyond that, where x is chosen to get us to 2+/-.1°C), et d'Equal Right: comparer le welfare avec un equal pc cap-and-trade.
1bis. Model Cramton & Stoft (midway between grandfathering and equal pc)
2. Year at which undiscounted aggregate EDE turns positive
3. Réduire la taille des données de sortie.
4. Modéliser une transition entre absence de taxe et taxe optimale pour les premières années.
5. Compute equivalent prices / rights / transfers
6. Décomposer les gains de bien-être
7. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 
8. Estimate welfare of Peskzo, Golub & van der Mensbrugghe (2019)
9. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.
10. Concevoir procédure de décision entre différentes propositions d'écarts à l'allocation de base; rédiger une proposition de traité.
11. IMACLIM?

+ voir les commentaires dans revenue_recycle

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
- Rajouter une nouvelle section et simuler les prix différenciés proposés par Equal Right (cf. /papers/Equal_Right_prices.pdf).

- 1bis Stoft: Scénario cap_and_share sauf qu'on met global_recycle_share à 0.1

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 2. Year at which undiscounted aggregate EDE turns positive

### Étapes
- Créer une fonction qui calcule l'année à partir de laquelle la conso EDE devient supérieure à la valeur BAU (par pays et au niveau mondial), ajouter ça lors de l'export des sorties du modèle.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 3. Réduire la taille des données de sortie.

### Étapes
- Faire un tableur avec la liste des variables, un indicateur disant si elles sont exportées, et si oui quelles années sont exportées.
- Identifier dans une nouvelle colonne des variables inutiles à exporter, ainsi que des années inutiles au sein de variables utiles.
- Une fois la proposition de réduction des exports validée, l'implémenter.
- Potentiellement améliorer le code d'exportation (pour que les chemins de fichiers aient plus de sens).

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 4. Modéliser une transition entre absence de taxe et taxe optimale pour les premières années.

### Étapes
- Modifier le code qui calcule la taxe optimale pour un budget donné, afin d'avoir une trajectoire croissante linéaire les 5 premières années. Pour l'instant, ce code calcule le niveau initial de la taxe t0 et son taux de croissance optimaux pour atteindre un budget donné. Changer le code pour qu'il calcule le niveau de t5 et le taux de croissance optimaux, avec t1, ..., t4 interpolés linéairement entre t0 = 0 et t5.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 5. Compute equivalent prices / rights / transfers

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


## 6. Décomposer les gains de bien-être

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
- Exporter un graphique donnant la décomposition au cours du temps pour le monde entier et pour les pays majeurs, avec des stacked barres pour tout sauf amélioration totale, qui est elle en trait plein.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 7. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 

### Étapes
- Télécharger les données du WID et lire dans leur doc / papiers de recherche la méthode qu'ils utilisent pour avoir la distribution des revenus. Si c'est pas paramétrique (comme NICE fait, i.e. partir d'un Gini pour en déduire une loi lognormale), continuer les étapes suivantes.
- Faire l'hypothèse que l'inégalité va rester constante au cours du siècle (avant redistribution des recettes du prix carbone), et imputer la distribution des revenus du WID.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 8. Estimate welfare of Peskzo, Golub & van der Mensbrugghe (2019)

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


## 9. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.

### Étapes
- Réécrire le code du modèle en R.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 10. Concevoir procédure de décision entre différentes propositions d'écarts à l'allocation de base; rédiger une proposition de traité.

### Étapes
- Adrien: TODO

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat