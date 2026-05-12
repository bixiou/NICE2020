### Proposition de sélection des outputs

#### 1. À garder tels quels

Sélection de pays (USA, CHN, EUE, IND, RUS, NGA, COG ?) avec granularité annuelle (2025-2300)

* Variables :
* `consumption_EDE`
* `co2_emissions`
* `country_carbon_tax`
* `gross_output`

Aussi garder `temperature` dans le global output (fichier léger qui prend juste une valeur par an).

#### 2. À réduire

Pour les autres pays, on peut ne garder que les valeurs pour certaines années (2030, 2040, 2050, 2100), pour les mêmes variables qu'au-dessus.

#### 3. À supprimer complètement (non utilisés dans le code et sûrement peu pertinents)

Variables soit redondantes, soit uniquement utiles pour des calculs plus tard :

* On peut supprimer tous les fichiers `regional_...**` , qui sont inutilisés et, au cas où, une analyse régionale peut être refaite à la volée en aggrégant les données pays et la population
* Pareil pour les variables des global et quantile outputs (sauf `temperature` dans global output)
* Variables de redistribution : d'après ce que j'ai compris, ce sont des variables qui servent surtout a calculer l'EDE mais à ajuster éventuellement
    * `transfer`, `transfer_pc`, `transfer_over_gdp`
    * `country_tax_revenue`, `country_pc_tax_dividend`, `country_pc_dividend_domestic_transfers`, `country_pc_dividend_global_transfers`
    * `consumption_per_capita_post_recycle`, `consumption_gini`, `consumption_per_capita`, `consumption`
    * `net_transfer_pib`, `net_surplus`, `net_surplus_per_pib`, `net_output_per_capita`, `nice_net_output`
* Variables techniques : à voir si on pourrait en avoir besoin mais pour l'instant elles ne sont pas utilisées
    * `mu` (taux d'abattement)
    * `abatement_cost_share`, `local_damage_cost_share_KW`
    * `local_temp_anomaly`
* Autres :
    * `population` (on l'a déjà en input, ça ne sert à rien de le réexporter)
