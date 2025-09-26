#Attention sur R pour exécuter la ligne il faut faire Command+Enter et pas Control+Enter comme sur Julia

#Creation of graphs(histograms) to compare the consompution EDE per capita (per country and global) in the different scenarios
#Also goal to compare between the years 2030, 2050 and 2100

#First we import the .csv file with consumption EDE for the countries that we need (:IND, :NGA, :CHN, :MNG, :USA, :FRA, :DEU, :COD, :RUS)

options(repos = c(CRAN = "https://cloud.r-project.org/"))

install.packages("ggplot2")
install.packages("readxl")
install.packages("dplyr")
install.packages("readr")
install.packages("tidyr")
install.packages("tibble")
library(tibble)
library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)
library(readr)


conso_EDE <- read_delim("cap_and_share/output/comparison_output.csv", delim = "," )
conso_EDE <- conso_EDE[-c(28, 29),]
View(conso_EDE)

df_long <- conso_EDE %>%
  pivot_longer(
    cols = 4:10,          # colonnes correspondant aux scénarios
    names_to = "Scenario",
    values_to = "Conso"
  )

View(df_long)


#1) Variation compared to the BAU scenario

df_var_bau <- df_long
df_var_bau$Var_Conso <- NA

for (c in unique(df_var_bau$Country)){
  for (t in unique(df_var_bau$Year)){
    for (m in (df_var_bau$Scenario)){
      cons <- df_var_bau$Conso[df_var_bau$Country == c & df_var_bau$Year == t & df_var_bau$Scenario == m]
      cons_bau <- df_var_bau$Conso[df_var_bau$Country == c & df_var_bau$Year == t & df_var_bau$Scenario == "BAU"]
      df_var_bau <- df_var_bau %>%
        mutate(Var_Conso = ifelse(Country == c & Year == t & Scenario == m,
        ((cons - cons_bau)/cons_bau)*100, Var_Conso))
    }
  }
}
View(df_var_bau)

df_var_bau_without_bau <- df_var_bau %>% filter(Scenario != "BAU")

df_var_bau_2030 <- df_var_bau_without_bau %>% filter(Year == "2030")

ggplot(df_var_bau_2030, aes(x = Country, y = Var_Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    labs(title = "Consumption EDE per capita in 2030 by Country and Scenario",
         x = "Country",
         y = "Variation rate of Consumption EDE per capita in 2030 in terms of BAU") +
    scale_y_continuous(limits = c(-5, 25)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 

df_var_bau_2050 <- df_var_bau_without_bau %>% filter(Year == "2050")
View(df_var_bau_2050)
ggplot(df_var_bau_2050, aes(x = Country, y = Var_Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    labs(title = "Consumption EDE per capita in 2030 by Country and Scenario",
         x = "Country",
         y = "Variation rate of Consumption EDE per capita in 2030 in terms of BAU") +
    scale_y_continuous(limits = c(-10, 25)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 

df_var_bau_2100 <- df_var_bau_without_bau %>% filter(Year == "2100")
View(df_var_bau_2100)
ggplot(df_var_bau_2100, aes(x = Country, y = Var_Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    labs(title = "Consumption EDE per capita in 2030 by Country and Scenario",
         x = "Country",
         y = "Variation rate of Consumption EDE per capita in 2030 in terms of BAU") +
    scale_y_continuous(limits = c(-10, 25)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 


#2) Variation compared to the non_losing scenario

df_var_non_losing <- df_long
df_var_non_losing$Var_Conso <- NA

for (c in unique(df_var_non_losing$Country)){
  for (t in unique(df_var_non_losing$Year)){
    for (m in (df_var_non_losing$Scenario)){
      cons <- df_var_non_losing$Conso[df_var_non_losing$Country == c & df_var_non_losing$Year == t & df_var_non_losing$Scenario == m]
      cons_non_losing <- df_var_non_losing$Conso[df_var_non_losing$Country == c & df_var_non_losing$Year == t & df_var_non_losing$Scenario == "Non_losing"]
      df_var_non_losing <- df_var_non_losing %>%
        mutate(Var_Conso = ifelse(Country == c & Year == t & Scenario == m,
        ((cons - cons_non_losing)/cons_non_losing)*100, Var_Conso))
    }
  }
}
View(df_var_non_losing)

df_var_without_non_losing <- df_var_non_losing %>% filter(Scenario != "Non_losing")

df_var_non_losing_2030 <- df_var_without_non_losing %>% filter(Year == "2030")

ggplot(df_var_non_losing_2030, aes(x = Country, y = Var_Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    labs(title = "Consumption EDE per capita in 2030 by Country and Scenario",
         x = "Country",
         y = "Variation rate of Consumption EDE per capita in 2030 in terms of Non_losing") +
    scale_y_continuous(limits = c(-5, 25)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 

df_var_non_losing_2050 <- df_var_without_non_losing %>% filter(Year == "2050")

ggplot(df_var_non_losing_2050, aes(x = Country, y = Var_Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    labs(title = "Consumption EDE per capita in 2050 by Country and Scenario",
         x = "Country",
         y = "Variation rate of Consumption EDE per capita in 2050 in terms of Non_losing") +
    scale_y_continuous(limits = c(-5, 20)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 

df_var_non_losing_2100 <- df_var_without_non_losing %>% filter(Year == "2100")

ggplot(df_var_non_losing_2100, aes(x = Country, y = Var_Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    labs(title = "Consumption EDE per capita in 2100 by Country and Scenario",
         x = "Country",
         y = "Variation rate of Consumption EDE per capita in 2100 in terms of Non_losing") +
    scale_y_continuous(limits = c(-10, 25)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 
