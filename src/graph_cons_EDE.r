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


df_long <- conso_EDE %>%
  pivot_longer(
    cols = 4:ncol(conso_EDE),
    names_to = "Scenario",
    values_to = "Conso"
  )
end

df_long$Country <- factor(df_long$Country,levels = c("Global", setdiff(unique(df_long$Country), "Global")))
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


df_var_bau_without_bau <- df_var_bau %>% filter(Scenario != "BAU")

df_var_bau_2030 <- df_var_bau_without_bau %>% filter(Year == "2030")

bau_2030 <- ggplot(df_var_bau_2030, aes(x = Country, y = Var_Conso, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(
    title = "Variation rate of Consumption EDE per capita in 2030 compared to BAU",
    x = "Country",
    y = "Variation rate") +
  scale_y_continuous(limits = c(-5, 25)) +
  theme_minimal() +
  theme(legend.position = "bottom")
end

df_var_bau_2050 <- df_var_bau_without_bau %>% filter(Year == "2050")

bau_2050 <- ggplot(df_var_bau_2050, aes(x = Country, y = Var_Conso, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(
    title = "Variation rate of Consumption EDE per capita in 2050 compared to BAU",
    x = "Country",
    y = "Variation rate"
  ) +
  scale_y_continuous(limits = c(-10, 25)) +
  theme_minimal() +
  theme(legend.position = "bottom")
end

df_var_bau_2100 <- df_var_bau_without_bau %>% filter(Year == "2100")

bau_2100 <- ggplot(df_var_bau_2100, aes(x = Country, y = Var_Conso, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(title = "Variation rate of Consumption EDE per capita in 2100 compared to BAU",
    x = "Country",
    y = "Variation rate") +
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
      cons_non_losing <- df_var_non_losing$Conso[df_var_non_losing$Country == c & df_var_non_losing$Year == t & df_var_non_losing$Scenario == "Non-losing"]
      df_var_non_losing <- df_var_non_losing %>%
        mutate(Var_Conso = ifelse(Country == c & Year == t & Scenario == m,
        ((cons - cons_non_losing)/cons_non_losing)*100, Var_Conso))
    }
  }
}


df_var_non_losing %>%
  group_by(Country, Year) %>%
  summarise(has_non_losing = any(Scenario == "Non-losing")) %>%
  filter(!has_non_losing)

df_var_without_non_losing <- df_var_non_losing %>% filter(Scenario != "Non-losing")

df_var_non_losing_2030 <- df_var_without_non_losing %>% filter(Year == "2030")

nonlosing_2030 <- ggplot(df_var_non_losing_2030, aes(x = Country, y = Var_Conso, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(title = "Variation rate of Consumption EDE per capita in 2030 compared to Non_losing",
    x = "Country",
    y = "Variation rate") +
  scale_y_continuous(limits = c(-5, 25)) +
  theme_minimal() +
  theme(legend.position = "bottom")
end 

df_var_non_losing_2050 <- df_var_without_non_losing %>% filter(Year == "2050")

nonlosing_2050 <- ggplot(df_var_non_losing_2050, aes(x = Country, y = Var_Conso, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(title = "Variation rate of Consumption EDE per capita in 2050 compared to Non_losing",
    x = "Country",
    y = "Variation rate") +
  scale_y_continuous(limits = c(-5, 20)) +
  theme_minimal() +
  theme(legend.position = "bottom")
end 

df_var_non_losing_2100 <- df_var_without_non_losing %>% filter(Year == "2100")

nonlosing_2100 <- ggplot(df_var_non_losing_2100, aes(x = Country, y = Var_Conso, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(title = "Variation rate of Consumption EDE per capita in 2100 compared to Non_losing",
    x = "Country",
    y = "Variation rate") +
  scale_y_continuous(limits = c(-10, 25)) +
  theme_minimal() +
  theme(legend.position = "bottom")
end 

#Export the graphs in a folder

dir <- file.path(getwd(), "cap_and_share", "graphs")
if (!dir.exists(dir)) {
  dir.create(dir, recursive = TRUE)
}

plots <- list(var_rate_cons_EDE_compared_bau_2030 = bau_2030, var_rate_cons_EDE_compared_bau_2050 = bau_2050, var_rate_cons_EDE_compared_bau_2100 = bau_2100, var_rate_cons_EDE_compared_nonlosing_2030 = nonlosing_2030, var_rate_cons_EDE_compared_nonlosing_2050 = nonlosing_2050, var_rate_cons_EDE_compared_nonlosing_2100 = nonlosing_2100)
for (name in names(plots)) {
  ggsave(
    filename = file.path(dir, paste0(name, ".png")),
    plot = plots[[name]],
    width = 8, height = 6
  )
}


###########################################
# Creation of the graph representing net present value of the consumption EDE per capita
###########################################

data_npv <- read_delim("cap_and_share/output/net_present_value_cons_EDE.csv", delim = "," )
df_long_npv <- data_npv %>%
  pivot_longer(
    cols = 2:ncol(data_npv),
    names_to = "Scenario",
    values_to = "NPV_Conso_EDE"
  )
end
View(df_long_npv)

df_long_npv$country <- factor(df_long_npv$country,levels = c("Global", setdiff(unique(df_long_npv$country), "Global")))
View(df_long_npv)

df_var_npv_nonlosing <- df_long_npv
df_var_npv_nonlosing$Var_Conso_NPV <- NA

for (c in unique(df_var_npv_nonlosing$country)){
  for (m in (df_var_npv_nonlosing$Scenario)){
    cons <- df_var_npv_nonlosing$NPV_Conso_EDE[df_var_npv_nonlosing$country == c & df_var_npv_nonlosing$Scenario == m]
    cons_nonlosing <- df_var_npv_nonlosing$NPV_Conso_EDE[df_var_npv_nonlosing$country == c & df_var_npv_nonlosing$Scenario == "Non-losing"]
    df_var_npv_nonlosing <- df_var_npv_nonlosing %>%
      mutate(Var_Conso_NPV = ifelse(country == c & Scenario == m,
      ((cons - cons_nonlosing)/cons_nonlosing)*100, Var_Conso_NPV))
  }
}
View(df_var_npv_nonlosing)


df_var_bau_without_nonlosing <- df_var_npv_nonlosing %>% filter(Scenario != "Non-losing")


graph_npv <- ggplot(df_long_npv, aes(x = country, y = NPV_Conso_EDE, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(title = "Net present value consumption EDE per capita 2030-2100",
    x = "Country",
    y = "NPV (thousand USD2017 per person)") +
  scale_y_continuous(limits = c(0, 1500)) +
  theme_minimal() +
  theme(legend.position = "bottom")
end 

graph_npv


graph_npv_var_nonlosing <- ggplot(df_var_bau_without_nonlosing, aes(x = country, y = Var_Conso_NPV, fill = Scenario)) +
  geom_col(position = "dodge") + 
  labs(title = "Variation rate of net present value consumption EDE per capita 2030-2100 in terms of Non-losing scenario",
    x = "Country",
    y = "Variation rate") +
  scale_y_continuous(limits = c(-5, 11)) +
  theme_minimal() +
  theme(legend.position = "bottom")
end 

graph_npv_var_nonlosing

dir <- file.path(getwd(), "cap_and_share", "graphs")
ggsave(
  filename = file.path(dir, "graph_npv.png"),
  plot = graph_npv,
  width = 8,
  height = 6
)
ggsave(
  filename = file.path(dir, "graph_npv_var_nonlosing.png"),
  plot = graph_npv_var_nonlosing,
  width = 8,
  height = 6
)
