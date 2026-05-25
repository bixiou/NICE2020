# Raffiner la modélisation d'un Cap & Share

## Principaux objectifs du projet

1. Pour les principaux pays, tracer la variation d'EDE (par rapport à l'autarcie) en fonction du prix d'autarcie x droits dans le cas uniforme, dans le cas Hotelling, dans différents scénarios de participation
2. Mettre à jour l'allocation de droits (non-losing = proposed) pour l'UE et la Chine.
3. Modéliser une transition entre absence de taxe et taxe optimale pour les premières années.
4. Optimiser le prix carbone sans la contrainte qu'il est exponentiel.
5. Modéliser la Sustainable Union, Global Justice trajectory, Equal Right
6. Améliorer l'algo d'allocation; trouver des réformes telles que welfare gains decomposition soit > 0 pour les principaux pays, notamment avec participation partielle
7. Nettoyer code, documenter changements, vérifier que la version par défaut donne les mêmes résultats que Marie, réduire la taille des données de sortie
8. Étendre les NDCs et les utiliser comme baseline
9. Proposer procédure de vote dans le traité
10. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 

## 1. Tracer l'équivalence prix - droits
### Étapes
- Lire l'Annexe A de uniform_price
On se place dans le cas Hotelling. La situation d'un pays i est définie par le scénario de participation, la trajectoire exponentielle de prix mondial (p\*_t), et soit le prix d'autarcie à t=0 (p_i,0 = pi_i x p\*_0) dans le cas autarcie, soit la part mondiale des droits d'émissions (ou plutôt le rapport rho_i entre cette part et la population du pays à t=0) dans le cas uniforme. 
- Pour la Chine, la RDC, l'Inde, le Nigéria, la Russie, les US et l'UE, tracer un graphe avec en abscisse p_i,0, en ordonnée rho_i, et pour chaque cellule du carré, la variation d'EDE de i dans le cas uniforme par rapport à l'autarcie, colorée du rouge au bleu (en passant par blanc = 0). Considérer que le prix est p\* dans tous les autres pays.
- Écrire un papier à partir de l'Annexe A, de la Section 3.1 de NICE2020/cap_and_share/papers/itmo_rules, et de ces résultats. 

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 2. Mettre à jour les trajectoires UE, Chine
Récupérer/inférer les trajectoires d'émissions de CO2 annuelles de la Chine et de chaque pays de l'UE:
### Étapes
- Pour la Chine, utiliser Du et al 2026 (NICE2020/cap_and_share/papers)
- Pour l'UE, utiliser la dernière NDC, la cible pour 2040, 2050, et l'Effort Sharing Regulation pour la répartition entre pays
- Créer des .csv avec les valeurs. Documenter le code qui permet de les créer (donner les sources, expliquer les hypothèses d'interpolation, etc.)
- Ajouter ces trajectoires dans NICE et refaire tourner le modèle avec elles. Mettre à jour la Table tab:budgets dans global_tax_attitudes/papers/global_climate_policies


### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 3. Modéliser un phase-in
L'idée est de faire démarrer le prix carbone mondial progressivement. Vérifier si cette tâche n'a pas déjà été codée.

### Étapes
- Dans NICE, permettre de faire démarrer le prix optimal à une date donnée (défaut : 2035), et interpoler linéairement 0 à t=0 et le prix optimal à cette date. Attention, le calcul de la trajectoire optimale doit être ajusté pour tenir compte des émissions lors du phase-in.

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 4. Optimiser la trajectoire de prix
### Étapes

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 5. Modéliser propositions existantes
### Étapes

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 6. Algorithme d'allocation et réformes profitant à chaque pays
### Étapes

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 7. Nettoyer, documenter, vérifier
### Étapes

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 8. Étendre les NDCs
### Étapes

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 9. Proposer une procédure de vote dans le traité
### Étapes

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat


## 10. Raffiner la distribution des revenus
### Étapes

### Problèmes rencontrés / observations

### Où en est-on ?

### Résultat

## Idées de recherche
1. Étudier l'équivalence entre prix carbone différenciés et droits d'émissions différenciés. 
2. Compare world welfare in a climate club of 
a. global CO2 price recycled to producers
b. club CO2 price recycled equal pc with the same global budget (i.e. lower budget in club to allow BAU emissions outside)
c. if a < b, share of revenue that producers should give to LDCs to make a = b.
3. Tester prix différenciés de Equal Right.


Pour info, tout ça vient d'un code que j'ai créé dans https://github.com/bixiou/global_tax_attitudes/blob/main/code_global/map_GCS_incidence.R, starting line 2357

L'union est composée de:
regions_union <- c("AFR", "CHI", "IND", "CSA", "MEX", "ODA", "EEU", "WEU", "JPN", "SKO") 



## 7. Modéliser prix nominal
- est-ce que ça a du sens de modéliser empreinte carbone et nominal dans un modèle sans secteurs ? la relation PPP/nominal est-elle stable dans le temps ?
=> Checker si les prix dans KLEM ou IMACLIM v2 reflètent les PPP (KLEM a l'avantage d'être désagrégé et néoclassique/standard, mais ne gère pas le commerce contrairement à IMACLIM).
=> Comment downscale IMACLIM.
=> Comment est calibré la fonction d'abattement dans NICE?


## 8. Inégalités non paramétriques
Francis Dennig avait une modélisation des inégalités non-paramétriques.


## 9. Réduire la taille des données
Actuellement, chaque run produit 60 Mo de données, c'est beaucoup trop.

TODO: faire tourner code de Marie et vérifier qu'on a la même chose que ce qu'ils avaient au départ.

## 10. Clean code

## 11. Update non-losing/NDC by country

## 12. Compute optimal carbon price without recycling (to maximize NPV of EDE)

## 13? Update damage function from Kalkuhl & Wenz to Kotz et al?


## Questions 
- Does NICE model the feedback effect of transfers on GDP? => No, it didn't.
- Is there an important reason for modelling the carbon tax as a tax on consumption rather than production?


## Infos sur NICE
- Calibration exogène: NICE a déjà des trajectoires exogènes (SSP2), la prod totale des facteurs est ajustée; l'intensité carbone sigma est estimée à partir de trajectoires d'émissions d'autres modèles. PIB SSP2 BAU sans dommage, prennent dépréciation et taux d'épargne de Penn World, convergeant vers un taux commun. À chaque période trouvent le TFP nécessaire pour reproduire le PIB. Pour les émissions, ont fit sur les sorties de REMIND. Population exogène. Puis fit sur les émissions pour trouver le sigma = intensité carbone. Les sigma et TFP changent pour chaque période, Marie ne se souvient plus si c'est lissé (genre taux de croissance constant) ou ajusté période par période. 
- certaines calibrations, faites par personnes différentes, sont dans nice_inputs.json; c'est équivalent à une table de données normale en excel.
- Revenus: NICE repasse les revenus en consos pour les pays où les données sont en termes de revenus plutôt que conso
- Gaz: Dans NICE il y a juste le CO2, mais ça inclut tout le CO2 (y.c. LULUCF)