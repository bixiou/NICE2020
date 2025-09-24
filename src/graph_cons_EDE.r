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
conso_EDE <- conso_EDE %>% select(-c(9,10))
conso_EDE <- conso_EDE[-c(31, 32),]

df_long <- conso_EDE %>%
  pivot_longer(
    cols = 4:8,          # colonnes correspondant aux scénarios
    names_to = "Scenario",
    values_to = "Conso"
  )

View(df_long)

df_IND_2030 <- df_long %>% filter(Country == "IND" & Year == 2030)
View(df_IND_2030)

barplot(df_IND_2030$Conso, names.arg = df_IND_2030$Scenario, main = "Consumption EDE per capita in India in 2030", ylab = "Consumption EDE per capita (2015 USD)", xlab = "Scenarios", col = "lightblue")

df_long_2030 <- df_long %>% filter(Year == 2030)
View(df_long_2030)

ggplot(df_IND_2030, aes(x = Scenario, y = Conso, fill = Scenario)) +
  geom_col() +
  labs(title = "Consommation par scénario",
       x = "Scénarios",
       y = "Consommation") +
    scale_y_continuous(limits = c(0, 10)) +
  theme_minimal() +
  theme(legend.position = "none")
end 

ggplot(df_long_2030, aes(x = Country, y = Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    labs(title = "Consumption EDE per capita in 2030 by Country and Scenario",
         x = "Country",
         y = "Consumption EDE per capita (2015 USD)") +
    scale_y_continuous(limits = c(0, 50)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 

df_long$Year <- as.factor(df_long$Year)

ggplot(df_long, aes(x = Country, y = Conso, fill = Scenario)) +
    geom_col(position = "dodge") + 
    facet_grid(. ~ Year) +
    labs(title = "Consumption EDE per capita in 2030 by Country and Scenario",
         x = "Country",
         y = "Consumption EDE per capita (2015 USD)") +
    scale_y_continuous(limits = c(0, 80)) +
    theme_minimal() +
    theme(legend.position = "bottom")
end 

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

df_var_bau_2050 <- df_var_bau %>% filter(Year == "2050")
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

df_var_bau_2100 <- df_var_bau %>% filter(Year == "2100")
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

