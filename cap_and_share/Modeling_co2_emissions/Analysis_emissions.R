library(dplyr)
library(readr)
library(stats)
library(tidyr)
library(openxlsx)
library(rlang)
library(plm)
library(stats)

#Data about population per country
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives
pop = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Population/5530a383-d6a3-4d54-8f7a-5ad1ab4a6ce6_Data.csv")

#Database of GDP, per capita, in ppp international 2021 USD
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives
GDP = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/GDP/676b275c-ac25-4861-96f2-f03722b33452_Data.csv")

#Balance of payments database, current prices USD
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives
trade_balance_GnS = read.xlsx("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Trade balances/f8effff1-90e9-46f0-8b5a-e74bd04abb53_Data.xlsx",
                        sheet = "Trade balances G&S (2015 USD)")
trade_balance_goods = read.xlsx("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Trade balances/f8effff1-90e9-46f0-8b5a-e74bd04abb53_Data.xlsx",
                                sheet = "Trade balances goods (2015 USD)")

#Data about energy use
#Source : WDI time series (2025)

wdi_energy_data = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Energy data/WDI Data/d9749250-3e13-4307-aa21-4b3198e502a3_Data.csv")

fossil_fuel_share <- wdi_energy_data%>%
  filter(Series.Code=="EG.USE.COMM.FO.ZS")

#Imports and exports as %of GDP per country
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives

imports_exports = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Trade balances/Split exports-imports WDI/2d7238f1-ae7a-4040-aa1c-d879bd5512cb_Data.csv")

#Industry share of GDP
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives

IVA = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Industry share/1a4983dd-00ca-48ef-864b-21eec0deb49b_Data.csv")

for (i in 5:69){
  names(imports_exports)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(imports_exports)[i])
}

imports_GnS_percent <- imports_exports %>%
  filter(Series.Code == "NE.IMP.GNFS.ZS")

exports_GnS_percent <- imports_exports %>%
  filter(Series.Code == "NE.EXP.GNFS.ZS")

#The list of countries which are in the model
countries = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/countries_table.csv")

#Data about emissions
#Source : Global Carbon Atlas, globalcarbonatlas.org
#Data per capita, in tCO2 p.c.

territorial_emissions_raw = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Emissions/emissions_co2_fossil_territorial_pc.csv")
consumption_emissions_raw = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Emissions/emissions_co2_fossil_consumption_based_pc.csv")


#This algorithm passes the dataframes in vertical time series with the same format for all data

territorial_emissions <- territorial_emissions_raw %>%
  slice(2:n()) %>% 
  set_names(c(territorial_emissions_raw[1, ]))

names(territorial_emissions)[1] <- "year"

territorial_emissions <- territorial_emissions %>%
  pivot_longer(
    cols = -year,          # Toutes les colonnes sauf "country"
    names_to = "country",        # Les noms des colonnes deviennent la variable "year"
    values_to = "territorial_emissions"   # Les valeurs deviennent la variable "emissions"
  )

consumption_emissions <- consumption_emissions_raw %>%
  slice(2:n()) %>% 
  set_names(c(consumption_emissions_raw[1, ]))

names(consumption_emissions)[1] <- "year"

consumption_emissions <- consumption_emissions %>%
  pivot_longer(
    cols = -year,          # Toutes les colonnes sauf "country"
    names_to = "country",        # Les noms des colonnes deviennent la variable "year"
    values_to = "consumption_emissions"   # Les valeurs deviennent la variable "emissions"
  )

#Land use emissions are in kgCO2 p.c.
land_use_emissions_raw = read.csv("C:/Users/ZBOOK/Documents/CIRED/Data emissions model/Emissions/emissions_land_use_pc.csv")

land_use_emissions <- land_use_emissions_raw %>%
  slice(2:n()) %>% 
  set_names(c(land_use_emissions_raw[1, ]))

names(land_use_emissions)[1] <- "year"

land_use_emissions <- land_use_emissions %>%
  pivot_longer(
    cols = -year,          # Toutes les colonnes sauf "country"
    names_to = "country",        # Les noms des colonnes deviennent la variable "year"
    values_to = "land_use_emissions"   # Les valeurs deviennent la variable "emissions"
  )

data_emissions_int <- merge(x=consumption_emissions, y=territorial_emissions, by = c("year", "country"))
data_emissions <- merge (x= data_emissions_int, y= land_use_emissions, by = c("year", "country"), all.x=TRUE)

data_emissions_clean <- merge(x=data_emissions, y=countries, by.x = "country", by.y = "LABEL.FR", all.y = TRUE)%>%
  arrange(country, year)

#Transforming the data for GDP, same process

for (i in 5:69){
names(GDP)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(GDP)[i])
}

GDP <- GDP[, -3]
GDP <- GDP[, -3]
GDP <- GDP[, -1]

GDP_clean <- GDP %>%
  pivot_longer(
    cols = -Country.Code,          # Toutes les colonnes sauf "country"
    names_to = "year",        # Les noms des colonnes deviennent la variable "year"
    values_to = "GDP"   # Les valeurs deviennent la variable "emissions"
  ) %>%
  filter (Country.Code %in% countries$country)%>%
  filter(year != 2024)

#Transforming the data for trade balances, same process
#Goods and services
for (i in 4:68){
  names(trade_balance_GnS)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(trade_balance_GnS)[i])
}

trade_balance_GnS <- trade_balance_GnS[,-3]
trade_balance_GnS <- trade_balance_GnS[,-1]

trade_balance_GnS_clean <- trade_balance_GnS %>%
  pivot_longer(
    cols = -Country.Code,          # Toutes les colonnes sauf "country"
    names_to = "year",        # Les noms des colonnes deviennent la variable "year"
    values_to = "trade_balance_GnS"   # Les valeurs deviennent la variable "emissions"
  ) %>%
  filter (Country.Code %in% countries$country)%>%
  filter(year != 2024)

#Goods only
for (i in 4:68){
  names(trade_balance_goods)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(trade_balance_goods)[i])
}

trade_balance_goods <- trade_balance_goods[,-3]
trade_balance_goods <- trade_balance_goods[,-1]

trade_balance_goods_clean <- trade_balance_goods %>%
  pivot_longer(
    cols = -Country.Code,          # Toutes les colonnes sauf "country"
    names_to = "year",        # Les noms des colonnes deviennent la variable "year"
    values_to = "trade_balance_goods"   # Les valeurs deviennent la variable "emissions"
  ) %>%
  filter (Country.Code %in% countries$country)%>%
  filter(year != 2024)

#Trade share data transforming
imports_GnS_percent <- imports_GnS_percent[,-3]
imports_GnS_percent <- imports_GnS_percent[,-3]
imports_GnS_percent <- imports_GnS_percent[,-1]

exports_GnS_percent <- exports_GnS_percent[,-3]
exports_GnS_percent <- exports_GnS_percent[,-3]
exports_GnS_percent <- exports_GnS_percent[,-1]

imports_GnS_percent_clean <- imports_GnS_percent%>%
  pivot_longer(
    cols = -Country.Code,          # Toutes les colonnes sauf "country"
    names_to = "year",        # Les noms des colonnes deviennent la variable "year"
    values_to = "imports_gns_percent"   # Les valeurs deviennent la variable "emissions"
  ) %>%
  filter (Country.Code %in% countries$country)%>%
  filter(year != 2024)

exports_GnS_percent_clean <- exports_GnS_percent%>%
  pivot_longer(
    cols = -Country.Code,          
    names_to = "year",        
    values_to = "exports_gns_percent"  
  ) %>%
  filter (Country.Code %in% countries$country)%>%
  filter(year != 2024)

#Define a function to make the processing of WDI data faster
process_wdi_data <- function(data, extra_columns, name_variable){
  data_copy <- data
  for (i in sort(extra_columns, decreasing=TRUE)){
    data_copy <- data_copy[,-i]
  }
  data_clean <- data_copy%>%
    pivot_longer(
      cols = -Country.Code,          # Toutes les colonnes sauf "country"
      names_to = "year",        # Les noms des colonnes deviennent la variable "year"
      values_to = name_variable   # Les valeurs deviennent la variable "emissions"
    ) %>%
    filter (Country.Code %in% countries$country)%>%
    filter(year != 2024)
  return(data_clean)
}


#Fossil fuel share
for (i in 5:69){
  names(fossil_fuel_share)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(fossil_fuel_share)[i])
}

fossil_fuel_share_clean = process_wdi_data(fossil_fuel_share, c(1,3,4), "fossil_fuel_share")

GDP_clean <- rename(GDP_clean, country= Country.Code )

data_emissions_clean<- data_emissions_clean[,-7]%>%
  relocate(country.y, .before = "year")

data_emissions_clean <- data_emissions_clean[,-1]%>%
  rename(country=country.y)

#population prpcessing
for (i in 5:69){
  names(pop)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(pop)[i])
} 

pop_clean = process_wdi_data(pop, c(1,3,4), "population")

#IVA processing
for (i in 5:69){
  names(IVA)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(IVA)[i])
} 

IVA_clean <- process_wdi_data(IVA, c(1,3,4), "industry_share")

df1 <- merge(x=data_emissions_clean, y=GDP_clean, by=c("country", "year"), all.x = TRUE, all.y = TRUE)

trade_balance_GnS_clean <- rename(trade_balance_GnS_clean, country=Country.Code)
df2 <- merge(x=df1, y=trade_balance_GnS_clean, by=c("country", "year"), all = TRUE)

trade_balance_goods_clean <- rename(trade_balance_goods_clean, country=Country.Code)
df3 <- merge(x=df2, y=trade_balance_goods_clean, by=c("country","year"), all=TRUE)

fossil_fuel_share_clean <- rename(fossil_fuel_share_clean, country=Country.Code)
df4 <- merge(x=df3, y= fossil_fuel_share_clean, by=c("country", "year"), all=TRUE)

imports_GnS_percent_clean <- rename(imports_GnS_percent_clean, country=Country.Code)
df5 <- merge(x=df4, y=imports_GnS_percent_clean ,by=c("country", "year"), all=TRUE)

exports_GnS_percent_clean <- rename(exports_GnS_percent_clean, country=Country.Code)
df6 <- merge(x=df5, y=exports_GnS_percent_clean, by=c("country", "year"), all=TRUE)

pop_clean <- rename(pop_clean, country=Country.Code)
df7 <- merge(x=df6, y=pop_clean, by=c("country", "year"), all=TRUE)

IVA_clean <- rename(IVA_clean, country=Country.Code)
df8 <- merge(x=df7, y=IVA_clean, by=c("country", "year"), all=TRUE)

reg_table <- df8 %>%
  filter(year >= 1990)%>%
  mutate(year = as.numeric(year))%>%
  mutate(consumption_emissions = as.numeric(consumption_emissions))%>%
  mutate(territorial_emissions = as.numeric(territorial_emissions))%>%
  mutate(land_use_emissions = as.numeric(land_use_emissions))%>%
  mutate(GDP = na_if(GDP, ".."), GDP = as.numeric(GDP))%>%
  mutate(fossil_fuel_share = na_if(fossil_fuel_share, ".."), fossil_fuel_share = as.numeric(fossil_fuel_share))%>%
  mutate(imports_gns_percent = na_if(imports_gns_percent, ".."), imports_gns_percent = as.numeric(imports_gns_percent))%>%
  mutate(exports_gns_percent = na_if(exports_gns_percent, ".."), exports_gns_percent = as.numeric(exports_gns_percent))%>%
  mutate(population = na_if(population, ".."), population = as.numeric(population))%>%
  mutate(industry_share = na_if(industry_share, ".."), industry_share = as.numeric(industry_share))%>%
  mutate(trade_share = imports_gns_percent + exports_gns_percent)%>%
  mutate(log_gdp = log(GDP))%>%
  mutate(log_territorial= log(territorial_emissions))%>%
  mutate(log_consumption= log(consumption_emissions))

annoying_countries <- reg_table%>%
  filter(year >= 1997)%>%
  filter(year <=2020)%>%
  filter(is.na(log_gdp) | is.na(log_territorial) | is.na(fossil_fuel_share) | is.na(industry_share)|
           is.na(imports_gns_percent)|is.na(exports_gns_percent) | is.na(industry_share)) %>%
  distinct(country)

annoying_countries_2 <- reg_table%>%
  filter(year >= 1997)%>%
  filter(year <=2020)%>%
  filter(is.na(log_gdp) | is.na(log_consumption) | is.na(fossil_fuel_share) | is.na(industry_share)|
           is.na(imports_gns_percent)|is.na(exports_gns_percent) | is.na(industry_share)) %>%
  distinct(country)

test_table <- reg_table %>%
  filter (!(country %in% annoying_countries$country))%>%
  filter (year >= 1997)%>%
  filter(year <= 2020)

test_table_2 <- reg_table %>%
  filter (!(country %in% annoying_countries_2$country))%>%
  filter (year >= 1997)%>%
  filter(year <= 2020)

test_table_3 <- test_table_2%>%
  mutate(log_gdp_fossil = log(GDP*fossil_fuel_share))

test_table_4 <- test_table_3%>%
  mutate(log_gdp_industrial = log(GDP*industry_share))

regression_3 <- pmg(log_consumption ~ log_gdp_fossil, data = test_table_3, index = c("country", "year"), model = "cmg")
summary(regression_3)

regression_4 <- pmg(log_consumption ~ log_gdp_fossil+ industry_share + imports_gns_percent + exports_gns_percent
                     , data = test_table_3, index = c("country", "year"), model = "cmg")
summary(regression_4)

regression_5 <- pmg(log_territorial ~ log_gdp_fossil, data = test_table_3, index = c("country", "year"), model = "cmg")
summary(regression_5)

regression_6 <- pmg(log_territorial ~ log_gdp_industrial, data = test_table_4, index=c("country", "year"), model="cmg")
summary(regression_6)

regression_7 <- pmg(log_territorial ~ log_gdp_fossil + log_gdp_industrial + log_gdp, data = test_table_4, index=c("country", "year"), model="cmg")
summary(regression_7)

regression_7 <- 
#CCEMG Regression

initial_regression <- pmg(log_territorial ~ log_gdp + fossil_fuel_share + industry_share + imports_gns_percent + exports_gns_percent, 
                   data = test_table, index = c("country", "year"), model = "cmg")
summary(initial_regression)

initial_regression_cons <- pmg(log_consumption ~ log_gdp + fossil_fuel_share + industry_share + imports_gns_percent + exports_gns_percent, 
                                data = test_table_2, index = c("country", "year"), model = "cmg")
summary(initial_regression_cons)

test_regression <- pmg(log_territorial ~ log_gdp_fossil, data=test_table_3,index = c("country", "year"), model = "cmg" )
summary(test_regression)

zz_info_france <- data_emissions_clean %>%
  filter(country=="FRA")%>%
  filter(year >= 1990)

zz_data7_france <- df7 %>%
  filter(country=="FRA")%>%
  filter(year >= 1990)

model <- pmg(log_territorial ~ log_gdp, data = test_table, index=c("country", "year"), model = "mg")
summary(model)

model$stdres

test_model <- lm(territorial_emissions ~ GDP, data=test_table)
summary(test_model)

test_table_5 <- reg_table%>%
  mutate(gdp_fossil = GDP*fossil_fuel_share,
         gdp_industrial = GDP*industry_share)
test_model_2 <- lm(territorial_emissions ~ gdp_fossil, data = test_table_5)
summary(test_model_2)

test_model_3 <- lm(territorial_emissions ~ gdp_fossil + gdp_industrial + GDP, data = test_table_5)
summary(test_model_3)

test_model_4 <- lm(territorial_emissions ~ gdp_industrial + GDP, data = test_table_5)
summary(test_model_4)

test_model_5 <- lm(territorial_emissions ~ gdp_industrial + gdp_fossil, data = test_table_5)
summary(test_model_5)

test_model_6 <- lm(territorial_emissions ~ gdp_fossil + GDP, data = test_table_5)
summary(test_model_6)

regression_3$indcoef
test_regression$indcoef
initial_regression$indcoef

pcdtest(log_territorial ~ log_gdp + fossil_fuel_share + industry_share + imports_gns_percent + exports_gns_percent,
                            data = test_table,
                            index = c("country", "year"))
