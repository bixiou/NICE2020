library(dplyr)
library(writexl)
library(rlang)
library(ggplot2)
library(dplyr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

regression_data <- read.csv("regression_data.csv")

ratio_data_2022 <- regression_data%>%
  mutate(emissions_balance_percent = ((consumption_emissions/territorial_emissions)-1)*100)%>%
  select(country, year, emissions_balance_percent)%>%
  filter(year==2022)

#Let us realize the map that presents the distribution of emissions_balance_percent across countries
  zeroindata=0
  legend = ""
  
  # Données du monde (pays)
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(admin != "Antarctica")
  
  
  # Merge des données à cartographier avec la carte
  map_data <- world %>%
    left_join(ratio_data_2022, by = c("iso_a3_eh" = "country"))  # Attention ici : à adapter si besoin
  
  # Breaks for consumption
  breaks_ratio <- c(-100, -50, -25, -10, -5,
                  5, 10, 25, 50, 100, Inf)
  
  # Labels for consumption
  labels_ratio <- c("-100% to -50%", "-50% to -25%","-25% to -10%", "-10% to -5%", "-5% to 5%", 
                  "5% to 10%", "10% to 25%", "25% to 50%", "50% to 100%", "100%")
  # Palette divergente rouge-blanc-bleu
  colors_ratio <- c(
    "#B2182B",  
    "#D6604D",  
    "#F4A582",  
    "#FDDBC7",  
    "#FBF4EF",
    "#D1E5F0",  
    "#92C5DE",  
    "#4393C3",  
    "#2166AC",  
    "#053061"   
  )
  
  # Crée une variable catégorisée avec les breaks
  map_data$var_cat <- cut(map_data[["emissions_balance_percent"]],
                          breaks = breaks_ratio,
                          labels = labels_ratio,
                          include.lowest = TRUE) 
  
  # Gestion de la catégorie "= 0" si demandé
  if (zeroindata) {
    # Ajoute "= 0" aux niveaux
    map_data$var_cat <- factor(map_data$var_cat, levels = c(labels, "Not participating"))
    
    # Affecte la catégorie "= 0" aux bonnes observations
    map_data$var_cat[map_data[[var]] == 0] <- "Not participating"
  }
  
  
  # Carte
  p <- ggplot(map_data) +
    geom_sf(aes(fill = var_cat), color = "gray90", size = 0.1) +
    coord_sf(crs ="+proj=robin") +
    scale_fill_manual(values = colors_ratio, drop = FALSE, na.value = "grey80") +
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
      legend.text = element_text(size = 9),
      legend.key=element_blank(),
      legend.margin = margin (2,2,2,2)
    )
  
  # Affiche la carte
  print(p)
  
  ggsave(filename ="map_ratio_emissions.jpg", plot = p, width = 10, height =6)
