# Raffiner la modélisation d'un Cap & Share

## Principaux objectifs du projet

0. Installer et faire tourner NICE.
1. Modéliser des scénarios à participation partielle, avec une liste donnée de pays qui met en œuvre le prix du carbone.
2. Permettre une allocation personnalisée des recettes carbone, à partir d'une répartition du budget carbone entre pays (en modifiant revenue_recycle.jl ou bien loss_damage).
3. Faire tourner NICE avec des trajectoires exogènes de PIB et d'émissions.
4. Créer des cartes pour présenter les effets distributifs des sorties de NICE.
5. Modéliser les coûts et bénéfices en nominal (à partir des chiffres en PPA).
6. Prédire le ratio de l'empreinte carbone par rapport aux émissions territoriales d'un pays en utilisant son PIB par habitant et sa balance commerciale, à partir de données récentes.
7. Raffiner la présentation de la distribution des revenus, en utilisant les données par percentile du WID. 
8. Modéliser en R l'apport de NICE, à savoir la désagrégation en décile-pays et les dégâts par pays.


## 0. Installation et exécution

### Liens utiles
- [Documentation Julia](https://docs.julialang.org/en/v1/manual/getting-started/)
- [NICE repository](https://github.com/bixiou/NICE2020)

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



## 2. Allocation personnalisée


## Questions 
- Does NICE model the feedback effect of transfers on GDP?
- "a global and uniform carbon tax leads to the same abatement rate trajectory in every country, but to heterogeneous abatement costs in terms of share of gross output" Why?