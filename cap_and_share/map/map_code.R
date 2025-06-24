package <- function(p, version = NULL, remove = FALSE, github = '') {
  if (remove) {
    detach(paste0("package:", p), unload = T)
    remove.packages(p)
  }
  if (!is.element(p, installed.packages()[,1])) {
    if (missing(version)) {
      if (github != '') {
        package("devtools")
        install_github(paste0(github, '/', p))
      } else install.packages(p) # , repos='https://cran.rstudio.com/', type = 'source' may help in case of bug
    } else {
      try({detach("package:devtools", unload = T)})
      package("remotes")
      install_version(p, version = version, repos = "http://cran.us.r-project.org", upgrade = "never", dependencies = TRUE)
      package("devtools")
    }
  }
  else { if(!missing(version)) warning(paste("'", p, "' is already installed with a (potentially) newer version. You may want to install the required version (", version, ") to avoid bugs.", sep=""))}
  library(p, character.only = TRUE)
} # loads packages with automatical install if needed

package("dplyr")
package("writexl")
package("rlang")
package("ggplot2")
package("dplyr")
package("readr")
package("sf")
package("rnaturalearth")
package("rnaturalearthdata")

###########################################################
#Please enter the path to your data below and run the code#
###########################################################

root <- "../../NICE2020/cap_and_share/"

# Path of the folder with baseline scenario 
path_baseline_scenario = "cap_and_share/output/bau_no_policy_at_all/no_revenue_recycling/old_transfer"

# Path of the folder with policy scenario of interest

path_policy_interest_new_transfer = "cap_and_share/output/revenue_recycling/global_per_capita/new_transfer"
path_policy_interest_old_transfer = "cap_and_share/output/revenue_recycling/global_per_capita/old_transfer"


# The year you want to represent on maps

year_represent = 2070

#Choose discount rate

discount_rate = 0.03

###########################
#Please run the code below#
###########################


#Can apply a specific transformation to data
heatmap_table_simple <- function(vars, labels = vars, data, along = "country_name", conditions = NULL, alphabetical = TRUE, export_xls = FALSE, 
                                 filename = "heatmap_table.xlsx", folder = ".", remove_na = TRUE, transpose = FALSE) {
  df <- data
  
  # 2. Filtrage conditionnel (ex: "> 0")
  if (!is.null(conditions)) {
    for (i in seq_along(vars)) {
      condition <- conditions[i]
      if (nzchar(condition)) {
        expr <- paste0("df[['", vars[i], "']] ", condition)
        df <- df[eval(parse(text = expr)), ]
      }
    }
  }
  
  # 3. Supprimer les lignes avec NA si demandé
  if (remove_na) {
    df <- df[complete.cases(df[, c(along, vars)]), ]
  }
  
  # 4. Agréger les données
  table <- aggregate(df[, vars, drop = FALSE], by = list(df[[along]]), FUN = mean)
  colnames(table) <- c(along, labels)
  
  # 5. Trier alphabétiquement ?
  if (alphabetical) {
    table <- table[order(table[[along]]), ]
  }
  
  # 6. Transposer ?
  if (transpose) {
    table <- t(table)
  }
  
  # 7. Export Excel ?
  if (export_xls) {
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    path <- file.path(folder, filename)
    suppressMessages(writexl::write_xlsx(as.data.frame(table), path))
    message("✅ Fichier exporté : ", path)
  }
  
  return(table)
}

#Produces the map
plot_map <- function(var, df, along = "country", breaks, labels, colors, legend = "", save = FALSE, format = c("png", "pdf"),
                           folder = "./", filename = NULL, trim = FALSE, zeroindata = FALSE) {
  
  # Vérifie si le dossier de destination existe
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  
  # Données du monde (pays)
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin != "Antarctica")
  
  
  # Merge des données à cartographier avec la carte
  map_data <- world %>%
    left_join(df, by = c("iso_a3_eh" = along))  # Attention ici : à adapter si besoin
  
  # Crée une variable catégorisée avec les breaks
  map_data$var_cat <- cut(map_data[[var]],
                          breaks = breaks,
                          labels = labels,
                          include.lowest = TRUE) 
  
  # Gestion de la catégorie "= 0" si demandé
  if (zeroindata) {
    # Ajoute "= 0" aux niveaux
    map_data$var_cat <- factor(map_data$var_cat, levels = c(labels, "Not participating"))
    
    # Affecte la catégorie "= 0" aux bonnes observations
    map_data$var_cat[map_data[[var]] == 0] <- "Not participating"
  }
  
  #le pack sf a un bug d'affichage de légende quand des valeurs d'un level ne sont pas prises
    fake_row <- map_data[1, ]  # copie une ligne valide pour avoir les bonnes colonnes
    fake_row$geometry <- st_sfc(st_point(c(0,0)), crs = st_crs(map_data))  # point fictif
    fake_row[[var]] <- -1       # une valeur qui tombe bien dans le premier bin
    fake_row$var_cat <- factor("< -5", levels = labels)  # forcer la bonne catégorie
    map_data <- rbind(map_data, fake_row)  # ajoute cette ligne
    
    # Ajouter des fausses lignes pour les niveaux '200 to 300' et '> 300'
    fake_row_200_300 <- map_data[1, ]  # copie une ligne valide pour avoir les bonnes colonnes
    fake_row_200_300$geometry <- st_sfc(st_point(c(0,0)), crs = st_crs(map_data))  # point fictif
    fake_row_200_300[[var]] <- 250     # une valeur qui tombe dans le bin '200 to 300'
    fake_row_200_300$var_cat <- factor("200 to 300", levels = labels)  # forcer la bonne catégorie
    map_data <- rbind(map_data, fake_row_200_300)  # ajoute cette ligne
    
    fake_row_300 <- map_data[1, ]  # copie une ligne valide pour avoir les bonnes colonnes
    fake_row_300$geometry <- st_sfc(st_point(c(0,0)), crs = st_crs(map_data))  # point fictif
    fake_row_300[[var]] <- 350     # une valeur qui tombe dans le bin '> 300'
    fake_row_300$var_cat <- factor("> 300", levels = labels)  # forcer la bonne catégorie
    map_data <- rbind(map_data, fake_row_300)  # ajoute cette ligne
    
    fake_row_10000 <- map_data[1, ]  # copie une ligne valide pour avoir les bonnes colonnes
    fake_row_10000$geometry <- st_sfc(st_point(c(0,0)), crs = st_crs(map_data))  # point fictif
    fake_row_10000[[var]] <- 15000     # une valeur qui tombe dans le bin '> 300'
    fake_row_10000$var_cat <- factor("> 10000", levels = labels)  # forcer la bonne catégorie
    map_data <- rbind(map_data, fake_row_10000)  # ajoute cette ligne
    
    fake_row_5000_10000 <- map_data[1, ]  # copie une ligne valide pour avoir les bonnes colonnes
    fake_row_5000_10000$geometry <- st_sfc(st_point(c(0,0)), crs = st_crs(map_data))  # point fictif
    fake_row_5000_10000[[var]] <- 7000     # une valeur qui tombe dans le bin '> 300'
    fake_row_5000_10000$var_cat <- factor("5000 to 10000", levels = labels)  # forcer la bonne catégorie
    map_data <- rbind(map_data, fake_row_5000_10000)  # ajoute cette ligne

  
  # Carte
  p <- ggplot(map_data) +
    geom_sf(aes(fill = var_cat), color = "gray90", size = 0.1) +
    coord_sf(crs ="+proj=robin") +
    scale_fill_manual(values = colors, drop = FALSE, na.value = "grey80") +
    theme_minimal(base_size = 10) +
    labs(fill = legend)+
    guides(fill = guide_legend(keyheight = 0.7, keywidth = 0.7)) +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      legend.background = element_rect(fill = "white", color= "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      plot.margin = margin(0, 0, 0, 0),
      panel.border = element_blank(),
      legend.position = c(0.05, 0.1),
      legend.justification = c(0,0),
      legend.title = element_text(size = 7 ),
      legend.text = element_text(size = 6),
      legend.key=element_blank(),
      legend.margin = margin (2,2,2,2)
    )
  
  # Affiche la carte
  print(p)
  
  # Sauvegarde si demandé
  if (save) {
    if (is.null(filename)) {
      filename <- paste0(var, "_map_", year_represent)
    }
    for (fmt in format) {
      ggsave(filename = paste0(folder, "/", filename, ".", fmt),
             plot = p, width = 10, height = 6)
    }
  }
}

#Creates the map from the path to data
create_EDE_diff_map <- function(path_baseline_scenario, path_policy_interest, year_represent, save_folder = "cap_and_share/output/revenue_recycling/global_per_capita/new_transfer") {
  # Breaks for consumption
  breaks_EDE <- c(-Inf, -0.05, -0.025, -0.01, -0.0001, 0.0001,
                  0.01, 0.025, 0.05, 0.1, 0.3, Inf)
  
  # Labels for consumption
  labels_EDE <- c("< -5", "-5 to -2.5", "-2.5 to -1","-1 to -0.1", "-0.1 to 0.1", "0.1 to 1", 
                  "1 to 2.5", "2.5 to 5", "5 to 10", "10 to 30", "> 30")
  
  
  colors_EDE <- c(
    "#D05F68",
    "#E08A84",
    "#F7B7A1",
    "#FBE3D9",  
    "#FFFFFF",  
    "#E5F0FB",  
    "#C0E0EF",  
    "#92C5DE",  
    "#6CAAD1",  
    "#2166AC",  
    "#103A71"
  )
  
  EDE_bau <- read.csv(file.path(path_baseline_scenario, "country_output", "consumption_EDE.csv"))
  EDE_int <- read.csv(file.path(path_policy_interest, "country_output", "consumption_EDE.csv"))
  
  EDE_diff <- EDE_int 
  EDE_diff <- EDE_diff %>%
    mutate(cons_diff = (EDE_int$cons_EDE_country - EDE_bau$cons_EDE_country) / EDE_bau$cons_EDE_country)
  
  EDE_diff <- EDE_diff %>%
    dplyr::select(time, country, cons_diff)
  
  EDE_diff_year <- subset(EDE_diff, time == year_represent)
  
  
  plot_map(
    var = "cons_diff",
    df = EDE_diff_year,
    along = "country",
    breaks = breaks_EDE,
    labels = labels_EDE,
    colors = colors_EDE,
    legend = paste0("Change in equally distributed \nequivalent (EDE) consumption \ncompared to baseline (", year_represent, ") \nin %"),
    save = TRUE,
    format = c("png", "pdf"),
    folder = "cap_and_share/output/revenue_recycling/global_per_capita/new_transfer"  # Crée le dossier automatiquement si pas présent
  )
  
}


#Creates two ther maps about the amount of transfers
create_transfer_diff_map <- function(path_baseline_scenario, path_policy_interest, year_represent, save_folder = "cap_and_share/output/revenue_recycling/global_per_capita/old_transfer") {
  
  # Placeholder pour lecture de fichiers
  country_tax_revenue <- read.csv(file.path(path_policy_interest, "country_output", "country_tax_revenue.csv"))
  country_dividend_pc <- read.csv(file.path(path_policy_interest, "country_output", "country_pc_tax_dividend.csv"))
  population <- read.csv(file.path(path_policy_interest, "country_output", "population.csv"))
  
  #create the variable of interest
  transfers_pc <- country_tax_revenue %>%
    left_join(country_dividend_pc, by = c("country", "time")) %>%
    left_join(population, by = c("country", "time")) %>%
    mutate(transfer_pc = country_pc_dividend*1000 - (tax_revenue / l)) %>%
    dplyr::select(time, country, transfer_pc)
  
  transfers_pc_year <- subset(transfers_pc, time == year_represent)
  
  # Calcul de la somme des transferts entre 2020 et 2100
  transfers_pc_2020_2100_discounted <- subset(transfers_pc, time >= 2020 & time <= 2100) %>%
    mutate(discounted_value = transfer_pc/(1+discount_rate)^(time - 2020)) %>%
    group_by(country) %>%
    summarise(total_transfers = sum(discounted_value, na.rm = TRUE)) %>%
    select(country,total_transfers)
  
  #Uncomment if you want to see the tables
  #write.csv(transfers_pc_year, file = "transfers_pc_year.csv", row.names = FALSE)
  #write.csv(transfers_pc_2020_2100_discounted, file = "transfers_pc_2020_2100_discounted.csv", row.names = FALSE)
  
  
  # Breaks et palette personnalisée
  breaks_transfers <- c(-Inf,-1000, -500, -300, -200, -100, -50, 0, 50, 100, 200, 300, Inf)
  labels_transfers <- c(
    "< -1000",
    "-1000 to -500",
    "-500 to -300",
    "-300 to -200",
    "-200 to -100",
    "-100 to -50",
    "-50 to 0",
    "0 to 50",
    "50 to 100",
    "100 to 200",
    "200 to 300",
    "> 300"
  )
  colors_transfers <- c(
    "#67001F",  # < -1000
    "#B2182B",  # -1000 to -500
    "#D6604D",  # -500 to -300
    "#F4A582",  # -300 to -200
    "#FDDBC7",  # -200 to -100
    "#FEE0D2",  # -100 to -50
    "#FEEEF4",  # -50 to 0
    "#F1F8FB",  # 0 to 50
    "#C7E0F4",  # 50 to 100
    "#92C5DE",  # 100 to 200
    "#78B6DC",  # 200 to 300
    "#436EA9",   # > 300
    "#FFFFFF"   #Not participating
  )
  
  plot_map(
    var = "transfer_pc",
    df = transfers_pc_year,
    along="country",
    breaks = breaks_transfers,
    labels = labels_transfers,
    colors = colors_transfers,
    legend = paste("Per capita yearly net transfers \n(USD, 2017) in ", year_represent),
    save = TRUE,
    format = c("png", "pdf"),
    folder = "cap_and_share/output/revenue_recycling/global_per_capita/old_transfer",
    zeroindata=TRUE
  )
  
  breaks_npv <- c(-Inf,-20000, -10000, -5000, -2000, 0, 2000, 5000, 10000, Inf)
  labels_npv <- c(
    "< -20000",
    "-20000 to -10000",
    "-10000 to -5000",
    "-5000 to -2000",
    "-2000 to 0",
    "0 to 2000",
    "2000 to 5000",
    "5000 to 10000",
    "> 10000"
  )
  
  colors_npv <- c(
    "#67001F",  # < -20000
    "#B2182B",  # -20000 to -10000
    "#D6604D",  # -10000 to -5000
    "#F4A582",  # -5000 to -2000
    "#FDDBC7",  # -2000 to 0
    "#C7E0F4",  # 0 to 2000
    "#92C5DE",  # 2000 to 5000
    "#78B6DC",  # 5000 to 10000
    "#436EA9",   # > 10000
    "#FFFFFF"   #Not participating
  )
  
  plot_map(
    var = "total_transfers",
    df = transfers_pc_2020_2100_discounted,
    along="country",
    breaks = breaks_npv,
    labels = labels_npv,
    colors = colors_npv,
    legend = paste("Net present value of transfers \nper capita between 2020 and 100 \nin (USD, 2017),\ndisount rate  =", discount_rate),
    save = TRUE,
    format = c("png", "pdf"),
    folder = "cap_and_share/output/revenue_recycling/global_per_capita/old_transfer",
    zeroindata=TRUE
  )
  
}

#Creates map for transfer_over_gdp
create_transfer_over_gdp_map <- function(path_policy_interest, year_represent, save_folder = path_policy_interest) {
  
  # 1. Lecture du CSV ------------------------------------------------
  #    → attend les colonnes : time | country | transfer_over_gdp
  gdp_df <- read.csv(file.path(path_policy_interest,
                               "country_output",
                               "transfer_over_gdp.csv"))
  
  # 2. Filtrer l'année demandée --------------------------------------
  gdp_year <- subset(gdp_df, time == year_represent)
  
  # 3. Définir breaks / labels / couleurs ----------------------------
  #    Ici, on coupe à ±10 % du PIB, avec pas mal de granularité autour de 0.
  breaks_gdp <- c(-Inf, -0.10, -0.05, -0.02, -0.01, -0.00000000000000001,
                  0,
                  0.01, 0.02, 0.05, 0.10, Inf)
  
  labels_gdp <- c(
    "< -10 %",
    "-10 % à -5 %",
    "-5 % à -2 %",
    "-2 % à -1 %",
    "-1 % à 0 %",
    "0",
    "0 % à 1 %",
    "1 % à 2 %",
    "2 % à 5 %",
    "5 % à 10 %",
    "> 10 %"
  )
  
  # Palette divergente rouge-blanc-bleu
  colors_gdp <- c(
    "#67001F",  # < -10 %
    "#B2182B",  # -10 % à -5 %
    "#D6604D",  # -5 % à -2 %
    "#F4A582",  # -2 % à -1 %
    "#FDDBC7",  # -1 % à 0 %
    "grey80",
    "#D1E5F0",  # 0 % à 1 %
    "#92C5DE",  # 1 % à 2 %
    "#4393C3",  # 2 % à 5 %
    "#2166AC",  # 5 % à 10 %
    "#053061"   # > 10 %
  )
  
  # 4. Appel à la fonction générique ---------------------------------
  plot_map(
    var    = "transfer_over_gdp",
    df     = gdp_year,
    along  = "country",
    breaks = breaks_gdp,
    labels = labels_gdp,
    colors = colors_gdp,
    legend = paste("Transfers as % of GDP –", year_represent),
    save   = TRUE,
    format = c("png", "pdf"),
    folder = "cap_and_share/output/revenue_recycling/global_per_capita/new_transfer",
    zeroindata = FALSE
  )
}


  



###############################
###############################
##Call the following funtions##
###############################
###############################


#Call this function to create the maps about transfers
path <- paste0(root, "output/revenue_recycling/global_per_capita/ffu_custom_transfers/")
create_transfer_diff_map(path_baseline_scenario, path_policy_interest_old_transfer, year_represent, save_folder = "cap_and_share/output/revenue_recycling/global_per_capita/old_transfer")

#Call this function to create the map about changes in EDE consumption
create_EDE_diff_map(path_baseline_scenario, path_policy_interest_new_transfer, year_represent, save_folder = "cap_and_share/output/revenue_recycling/global_per_capita/new_transfer")

create_transfer_over_gdp_map(path, 2040)
