## Tâches à effectuer

0. Installer et faire tourner NICE; prendre en main Julia.
1. Tester prix différenciés du FMI ($25/t LIC & LMIC, $50 UMIC, $75 HIC pour 2025-30, increasing at x% beyond that, where x is chosen to get us to 2+/-.1°C), et d'Equal Right: comparer le welfare avec un equal pc cap-and-trade.
2. Compare world welfare in a climate club of 
a. global CO2 price recycled to producers
b. club CO2 price recycled equal pc with the same global budget (i.e. lower budget in club to allow BAU emissions outside)
c. if a < b, share of revenue that producers should give to LDCs to make a = b.
4. Réduire la taille des données de sortie.
5. Modéliser une transition entre absence de taxe et taxe optimale pour les premières années.
6. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 
7. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.
8. Étudier l'équivalence entre prix carbone différenciés et droits d'émissions différenciés. / Theoretical/simulation analysis of net gain for a given country in function of price x rights.
9. Concevoir procédure de décision entre différentes propositions d'écarts à l'allocation de base; rédiger une proposition de traité.
10. Décomposer gains en dommages évités, transferts bruts, croissance supplémentaire, etc.
11. IMACLIM?


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
- Rajouter une section dans example_runs.jl où simuler des prix différenciés du FMI (sans transferts internationaux, i.e. within-country recycling).
- Trouver dans le code comment le coder.
- Simuler les prix suivants : de 2025 à 2030, $25/tCO2 pour les LICs et LMICs, $50 pour les UMICs, $75 pour HICs. Au-delà de 2030, faire croître le prix de x% par an. Tester plusieurs x et s'arrêter lorsque le x est tel que la température en 2100 est +2 +/- 0.05 °C.
- Rapporter ci-dessous la conso EDE mondiale et dans les principaux pays (disons CHN, USA, DEU, IND, COD, RUS) pour les scénarios suivants: FMI, cap and share, BAU, et pour les dates suivantes: 2030, 2050, 2100, net present value 2030-2100.
- Rajouter une nouvelle section et simuler les prix différenciés proposés par Equal Right (in /papers/Equal_Right_prices.pdf).

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 2. Year at which undiscounted aggregate EDE turns positive

### Étapes
- Créer une fonction qui calcule l'année à partir de laquelle la conso EDE devient supérieure à la valeur BAU (par pays et au niveau mondial), ajouter ça lors de l'export des sorties du modèle.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat

