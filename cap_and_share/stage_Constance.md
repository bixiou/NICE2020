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

Pour les grands pays émetteurs comme les USA ou la Chine, imposer des facteurs de prix d'autarcie extrêmes forçait mathématiquement le prix du carbone du reste du monde ($p_{-i}$) à devenir négatif pour maintenir l'identité du budget carbone mondial. Physiquement, le modèle buggait donc j'ai ajouté des fonctions conditionnelles dans le paramétrage du modèle (gross_economy et abatement).

### Où en est-on ?

Code fonctionnel, heatmaps qui font sens, ajoutées dans le .tex. Calcul de l'équivalence pour les propositions de Wolfram et de Duflo, avec comparaison en termes de bien-être et de température. Explication du code :

J'ai commencé par simuler les monde des deux propositions :
1.  **Wolfram (Climate Coalition) :** Les pays d'une coalition paient une taxe carbone, basée sur leur niveau de richesse ($25 pour les pays pauvres, $50 pour les pays intermédiaires, $75 pour les pays riches), augmentant de 5% par an. Les autres pays paient $0.
2.  **Duflo (Grand Bargain) :** Tous les pays du monde paient une taxe selon leur catégorie de revenu ($10, $30, $50), sauf les pays à haut revenu (HIC) qui paient $0 au niveau national (leur taxe correspond à un transfert non modélisé dans le script).

J'ai ensuite calculé le niveau de bien-être NPV de chaque pays.

Ensuite, on bascule vers un régime de prix uniforme mondial (Cap-and-Share), avec distribution de droits d'émission initiaux.

J'utilise une méthode par dichotomie pour trouver le multiplicateur $\rho_i$ pour chaque pays :
*   La formule d'allocation est : $\text{Droits}_i = \rho_i \times \text{Part de la population}_i \times \text{Plafond Mondial}$
*   Si $\rho_i = 1$, le pays reçoit ses droits strictement au prorata de sa population
*   Le code ajuste $\rho_i$ jusqu'à ce que le bien-être du pays sous le régime de prix unique soit exactement égal à son bien-être sous le scénario d'autarcie.

Puisque le code calcule les $\rho_i$ pays par pays (en supposant que le reste du monde se partage le reste des droits au prorata de leurs émissions historiques), on rassemble ensuite tous les $\rho_i$ trouvés dans une même matrice pour une simulation simultanée.
*   On applique tous les $\rho_i$ en même temps. On observe si le budget carbone mondial est respecté et quel impact sur la température globale en 2100
*   On réajuste année par année pour s'assurer que la somme des droits distribués est égale au plafond d'émissions mondiales. On mesure ensuite le gain de bien-être mondial généré par le passage au prix unique.


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

1. Union européenne (UE)

Données historiques (1990–2023) : utilisation du fichier `emissions_co2_fossil_territorial_pc.csv` (exprimé en tCO2/habitant), qui comptabilise les émissions territoriales de CO2 fossile (source : Global Carbon Project via Our World in Data).

Objectif 2030 (NDC de l'UE) : l'objectif est une réduction nette de −55% des gaz à effet de serre (GES) d'ici 2030 par rapport aux niveaux de 1990, déposé auprès de la CCNUCC (UNFCCC). Cet objectif global est distribué par pays en s'appuyant sur la Réglementation sur le partage de l'effort (ESR) :
    - Référence : Règlement (UE) 2023/857, Annexe I — Pourcentage de variation des GES hors système d'échange de quotas d'émission (non-ETS) d'ici 2030 par rapport à 2005
    - Hypothèse sous-jacente : les cibles de l'ESR (qui ne concernent que les secteurs non-ETS) sont utilisées ici comme des proxys de l'ambition globale de réduction du CO2 par pays. La somme des trajectoires nationales obtenues est ensuite réajustée proportionnellement pour correspondre exactement à la cible agrégée de la NDC européenne.

Objectif 2040 : inspiré de l'amendement à la loi européenne sur le climat (réduction nette de −90% des GES par rapport à 1990), basé sur la communication COM(2025). Cet effort est distribué entre les pays proportionnellement à leurs parts d'émissions respectives calculées pour 2030.

Objectif 2050 : neutralité climatique (émissions nettes de CO2 = 0)

Données de population issues du fichier `5530a383-d6a3-4d54-8f7a-5ad1ab4a6ce6_Data.csv` (source : Banque mondiale, indicateur : SP.POP.TOTL).

2. Chine

Source des données : Du et al. (2026), scénario : neutralité CO2, ligne : émissions totales de CO2, unité d'origine : centaines de millions de tonnes de CO2 (10^8 tCO2).

Période 2020–2060 : l'étude de Du et al. fournit des projections par paliers de 5 ans, j'ai donc interpolé linéairement pour obtenir les valeurs annuelles.

Période 2061–2070 : interpolation linéaire pour passer de la valeur de 2060 à 0 (objectif politique de la Chine d'atteindre la neutralité carbone d'ici 2060) ; les émissions résiduelles d'environ 0,9 GtCO2 observées en 2060 reflètent la part des secteurs industriels difficiles à décarboner.

Période 2071–2300 : émissions à zéro

3. Règles d'interpolation temporelle

Les trajectoires annuelles sont construites comme cela :

- 2020–2023 : utilisation des données historiques réelles
- 2024–2029 : interpolation linéaire entre les données réelles de 2023 et la cible NDC de 2030
- 2030–2039 : interpolation linéaire entre la cible de 2030 et la cible de 2040
- 2040–2049 : interpolation linéaire entre la cible de 2040 et l'objectif de zéro émission nette en 2050 (pour l'UE)
- 2050–2300 : émissions maintenues à zéro (à partir de 2060 pour la Chine)

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