####################################################################################################
####################################################################################################
## This code is mostly used to clean raw data, and try out a few regressions                      ##
## But the regression method seems quite imprecise, so another regression is coded in another file##
####################################################################################################
####################################################################################################

library(dplyr)
library(readr)
library(stats)
library(tidyr)
library(openxlsx)
library(rlang)
library(plm)
library(stats)
library(tidyverse)

##############################
##Loading  and cleaning data##
##############################


#Data about population per country
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives
pop = read.csv("Population/5530a383-d6a3-4d54-8f7a-5ad1ab4a6ce6_Data.csv")

#Database of GDP, per capita, in ppp international 2021 USD
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives
GDP = read.csv("GDP/676b275c-ac25-4861-96f2-f03722b33452_Data.csv")

#Balance of payments database, current prices USD
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives
trade_balance_GnS = read.xlsx("Trade balances/f8effff1-90e9-46f0-8b5a-e74bd04abb53_Data.xlsx",
                              sheet = "Trade balances G&S (2015 USD)")
trade_balance_goods = read.xlsx("Trade balances/f8effff1-90e9-46f0-8b5a-e74bd04abb53_Data.xlsx",
                                sheet = "Trade balances goods (2015 USD)")

#Data about energy use
#Source : WDI time series (2025)

wdi_energy_data = read.csv("Energy data/WDI Data/d9749250-3e13-4307-aa21-4b3198e502a3_Data.csv")

fossil_fuel_share <- wdi_energy_data%>%
  filter(Series.Code=="EG.USE.COMM.FO.ZS")

#Imports and exports as %of GDP per country
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives

imports_exports = read.csv("Trade balances/Split exports-imports WDI/2d7238f1-ae7a-4040-aa1c-d879bd5512cb_Data.csv")

#Industry share of GDP
#Source : WDI (2025) https://databank.worldbank.org/source/wdi-database-archives

IVA = read.csv("Industry share/1a4983dd-00ca-48ef-864b-21eec0deb49b_Data.csv")

imports_GnS_percent <- imports_exports %>%
  filter(Series.Code == "NE.IMP.GNFS.ZS")

exports_GnS_percent <- imports_exports %>%
  filter(Series.Code == "NE.EXP.GNFS.ZS")

#The list of countries which are in the model
countries = read.csv("countries_table.csv")

#Upload data from Liddle (2018) database
#It is not used in practice but it could be useful for later
data_liddle = read.xlsx(xlsxFile = "Liddle (2018)/data consumption vs teritory.xlsx", startRow = 4)
data_liddle <- data_liddle[,-3]%>%
  rename(country=X1)%>%
  mutate(country = substr(country, 2, 4))%>%
  filter(year==1990)%>%
  select(country, oecd, low, lowmid, upmid, high)

countries_merge <- merge( x=countries, y=data_liddle, by="country", all.x = TRUE)

#Data about emissions
#Source : Global Carbon Atlas, globalcarbonatlas.org
#Data per capita, in tCO2 p.c.

territorial_emissions_raw = read.csv("Emissions/emissions_co2_fossil_territorial_pc.csv")
consumption_emissions_raw = read.csv("Emissions/emissions_co2_fossil_consumption_based_pc.csv")

#Define a function to make the processing of WDI data faster
process_wdi_data <- function(data, extra_columns, name_variable, first_column){
  for(i in first_column:length(data)){
    names(data)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(data)[i])
  }
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
    filter(year != 2024)%>%
    rename(country=Country.Code)%>%
  return(data_clean)
}

#This algorithm passes the dataframes in vertical time series with the same format for all data
#Not a WDI data so doing it by hand

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
land_use_emissions_raw = read.csv("Emissions/emissions_land_use_pc.csv")

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
  arrange(country, year)%>%
  select(country.y, year, consumption_emissions, territorial_emissions, land_use_emissions)%>%
  rename(country = country.y)

#Processing WDI data using the function
GDP_clean <- process_wdi_data(data = GDP, extra_columns = c(1,3,4), name_variable = "gdp", first_column = 5)
trade_balance_goods_clean <- process_wdi_data(data= trade_balance_goods, extra_columns = c(1,3), name_variable = "trade_balance_goods", first_column = 4)
trade_balance_GnS_clean <- process_wdi_data(data=trade_balance_GnS, extra_columns = c(1,3), name_variable = "trade_balance_gns", first_column = 4)
imports_GnS_percent_clean <- process_wdi_data(data=imports_GnS_percent, extra_columns = c(1,3,4), name_variable = "imports_gns_percent", first_column = 5)
exports_GnS_percent_clean <- process_wdi_data(data= exports_GnS_percent, extra_columns = c(1,3,4), name_variable = "exports_gns_percent", first_column = 5)
fossil_fuel_share_clean <- process_wdi_data(data = fossil_fuel_share, extra_columns = c(1,3,4), name_variable = "fossil_fuel_share", first_column=5)
IVA_clean <- process_wdi_data(data = IVA, extra_columns = c(1,3,4), name_variable = "industry_share", first_column = 5)
pop_clean <- process_wdi_data(data = pop, extra_columns = c(1,3,4), name_variable = "population", first_column = 5)

#Define a function to extract the data about global averaged data
process_world_wdi_data <- function(data, extra_columns, name_variable, first_column){
  for(i in first_column:length(data)){
    names(data)[i] <- gsub(".*?(\\d{4}).*", "\\1", names(data)[i])
  }
  data_copy <- data
  for (i in sort(extra_columns, decreasing=TRUE)){
    data_copy <- data_copy[,-i]
  }
  data_clean <- data_copy%>%
    filter(Country.Code == "WLD")
  data_clean <- as.data.frame(t(data_clean))%>%
    rownames_to_column(var = "year")%>%
    filter(year != "Country.Code")%>%
    mutate(year = as.numeric(year))%>%
    filter(year>=1990)
  names(data_clean)[2]=name_variable
  return(data_clean)
}

#Use the function to extract global averages
fossil_fuel_share_world <- process_world_wdi_data(fossil_fuel_share, c(1,3,4), "fossil_fuel_share.world", 5)
gdp_world <- process_world_wdi_data(GDP, c(1,3,4), "gdp.world", 5)
industry_share_world <- process_world_wdi_data(IVA, c(1,3,4), "industry_share.world", 5)
#By definition, average trade balances are zero over the world
imports_share_world <- process_world_wdi_data(imports_GnS_percent, c(1,3,4), "imports_share.world", 5)
exports_share_world <- process_world_wdi_data(exports_GnS_percent, c(1,3,4), "exports_share.world", 5)
population_world <- process_world_wdi_data(pop, c(1,3,4), "population.world", 5)

global_emissions <- read.csv("Emissions/global_co2_emissions.csv")

global_emissions <- global_emissions %>%
  filter(X!="World")
global_emissions<- global_emissions[,-5]
global_emissions<- global_emissions[,-4]
global_emissions<- global_emissions[,-3]
names(global_emissions)[1]="year"
names(global_emissions)[2]="global_emissions"
global_emissions<-global_emissions[-67,]
global_emissions<-global_emissions[-66,]
global_emissions<-global_emissions[-65,]
global_emissions<-global_emissions%>%
  mutate(year = as.numeric(year))
population_world <- population_world%>%
  mutate(year = as.numeric(year))
sapply(population_world, class)
sapply(global_emissions, class)
global_emissions_merge <- merge(x=global_emissions, y=population_world, by="year", all.x=TRUE)%>%
  filter(year >=1990)%>%
  mutate(emissions_pc.world = as.numeric(global_emissions)/as.numeric(population.world)*1e6)%>%
  select(year, emissions_pc.world)

world_averages <- merge(x = global_emissions_merge, y = exports_share_world, by = "year", all.y = TRUE)
world_averages_2 <- merge(x=world_averages, y = imports_share_world, by = "year", all.x = TRUE)
world_averages_3 <- merge(x=world_averages_2, y= industry_share_world, by = "year", all.x = TRUE)
world_averages_4 <- merge(x=world_averages_3, y= gdp_world, by = "year", all.x = TRUE)
world_averages_5 <- merge(x=world_averages_4, y= fossil_fuel_share_world, all.x = TRUE)%>%
  filter(year != 2024)%>%
  mutate(exports_share.world = as.numeric(exports_share.world))%>%
  mutate(imports_share.world = as.numeric(imports_share.world))%>%
  mutate(industry_share.world = na_if(industry_share.world, ".."), industry_share.world = as.numeric(industry_share.world))%>%
  mutate(gdp.world = as.numeric(gdp.world))%>%
  mutate(fossil_fuel_share.world = na_if(fossil_fuel_share.world, ".."),
         fossil_fuel_share.world = as.numeric(fossil_fuel_share.world))


df1 <- merge(x=data_emissions_clean, y=GDP_clean, by=c("country", "year"), all.x = TRUE, all.y = TRUE)
df2 <- merge(x=df1, y=trade_balance_GnS_clean, by=c("country", "year"), all = TRUE)
df3 <- merge(x=df2, y=trade_balance_goods_clean, by=c("country","year"), all=TRUE)
df4 <- merge(x=df3, y= fossil_fuel_share_clean, by=c("country", "year"), all=TRUE)
df5 <- merge(x=df4, y=imports_GnS_percent_clean ,by=c("country", "year"), all=TRUE)
df6 <- merge(x=df5, y=exports_GnS_percent_clean, by=c("country", "year"), all=TRUE)
df7 <- merge(x=df6, y=pop_clean, by=c("country", "year"), all=TRUE)
df8 <- merge(x=df7, y=IVA_clean, by=c("country", "year"), all=TRUE)

reg_table <- df8 %>%
  filter(year >= 1990)%>%
  mutate(year = as.numeric(year))%>%
  mutate(consumption_emissions = as.numeric(consumption_emissions))%>%
  mutate(territorial_emissions = as.numeric(territorial_emissions))%>%
  mutate(land_use_emissions = as.numeric(land_use_emissions))%>%
  mutate(gdp = na_if(gdp, ".."), gdp = as.numeric(gdp))%>%
  mutate(fossil_fuel_share = na_if(fossil_fuel_share, ".."), fossil_fuel_share = as.numeric(fossil_fuel_share))%>%
  mutate(imports_gns_percent = na_if(imports_gns_percent, ".."), imports_gns_percent = as.numeric(imports_gns_percent))%>%
  mutate(exports_gns_percent = na_if(exports_gns_percent, ".."), exports_gns_percent = as.numeric(exports_gns_percent))%>%
  mutate(population = na_if(population, ".."), population = as.numeric(population))%>%
  mutate(industry_share = na_if(industry_share, ".."), industry_share = as.numeric(industry_share))%>%
  mutate(trade_share = imports_gns_percent + exports_gns_percent)%>%
  mutate(log_gdp = log(gdp))%>%
  mutate(log_territorial= log(territorial_emissions))%>%
  mutate(log_consumption= log(consumption_emissions))

regression_data <- merge(x=reg_table, y= world_averages_5, by="year", all.x=TRUE)
write.csv(x=regression_data, file = "regression_data.csv")

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

countries <- countries%>%
  mutate(has.good.data = !(country %in% annoying_countries_2$country))

#BRN, OMN, SAU have NA as a coefficient for fossil_fuel_share in the main regression
annoying_countries_2 <- rbind(annoying_countries_2, "BRN", "OMN", "SAU", "TKM")
annoying_countries <- rbind(annoying_countries, "BRN", "OMN", "SAU", "TKM")

test_table <- reg_table %>%
  filter (!(country %in% annoying_countries$country))%>%
  filter (year >= 1997)%>%
  filter(year <= 2020)

test_data <- test_table_5 %>%
  filter(country %in% c("BRN", "SAU", "OMN", "TKM"))

test_table_2 <- reg_table %>%
  filter (!(country %in% annoying_countries_2$country))%>%
  filter (year >= 1997)%>%
  filter(year <= 2020)

test_table_3 <- test_table_2%>%
  mutate(log_gdp_fossil = log(gdp*fossil_fuel_share))

test_table_4 <- test_table_3%>%
  mutate(log_gdp_industrial = log(gdp*industry_share))

test_table_5 <- test_table_4 %>%
  mutate(transfer_emissions = consumption_emissions - territorial_emissions,
         log_transfer_emissions = log(transfer_emissions)
  )#%>%
  #group_by(year) %>%
  #mutate(log_gdp_industrial.bar = mean(log_gdp_industrial, na.rm = TRUE)) %>%
  #mutate(log_gdp_fossil.bar = mean(log_gdp_fossil, na.rm = TRUE))%>%
  #mutate(log_gdp.bar = mean(log_gdp, na.rm = TRUE))%>%
  #mutate(log_territorial.bar = mean(log_territorial, na.rm = TRUE))%>%
  #mutate(log_consumption.bar = mean(log_consumption, na.rm = TRUE))%>%
  #mutate(fossil_fuel_share.bar = mean(fossil_fuel_share, na.rm = TRUE))%>%
  #mutate(imports_gns_percent.bar = mean(imports_gns_percent, na.rm = TRUE))%>%
  #mutate(exports_gns_percent.bar = mean(exports_gns_percent, na.rm = TRUE))%>%
  #mutate(industry_share.bar = mean(industry_share, na.rm = TRUE))%>%
  #mutate(trade_share.bar = mean(trade_share, na.rm = TRUE))%>%
  #mutate(log_transfer_emissions.bar = mean(log_transfer_emissions, na.rm = TRUE))%>%
  #mutate(transfer_emissions.bar = mean(transfer_emissions, na.rm = TRUE)) %>%
  #mutate(territorial_emissions.bar = mean(territorial_emissions, na.rm = TRUE)) %>%
  #mutate(consumption_emissions.bar = mean(consumption_emissions, na.rm = TRUE))%>%
  #mutate(gdp.bar = mean(gdp, na.rm = TRUE))%>%
  #mutate(trade_balance_GnS.bar = mean(trade_balance_GnS, na.rm = TRUE))%>%
  #mutate(trade_balance_goods.bar = mean(trade_balance_goods, na.rm = TRUE))%>%
  #ungroup()#%>%
  #select(country, year, fossil_fuel_share, imports_gns_percent, exports_gns_percent, industry_share, trade_share, 
         #log_gdp, log_territorial, log_consumption, log_gdp_fossil, log_gdp_industrial,log_transfer_emissions,
         #transfer_emissions, territorial_emissions, consumption_emissions,
         #fossil_fuel_share.bar, imports_gns_percent.bar, exports_gns_percent.bar, industry_share.bar, trade_share.bar, log_gdp.bar,
         #log_territorial.bar, log_consumption.bar, log_gdp_fossil.bar, log_gdp_industrial.bar, log_transfer_emissions.bar, transfer_emissions.bar,
         #territorial_emissions.bar, consumption_emissions.bar
         #)


#############################
## Testing some regressions##
#############################

  
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

#CCEMG Regressions from the paper
  
initial_regression <- pmg(log_territorial ~ log_gdp + fossil_fuel_share + industry_share + imports_gns_percent + exports_gns_percent , 
                            data = test_table, index = c("country", "year"), model = "cmg")
summary(initial_regression)
initial_regression$indcoef

initial_regression_cons <- pmg(log_consumption ~ log_gdp + fossil_fuel_share + industry_share + imports_gns_percent + exports_gns_percent, 
                               data = test_table_2, index = c("country", "year"), model = "cmg")
summary(initial_regression_cons)

test_regression <- pmg(log_territorial ~ log_gdp_fossil, data=test_table_3,index = c("country", "year"), model = "cmg" )
summary(test_regression)

#############################################################
##Computing absolute errors from predictions of regressions##
#############################################################

# Running the regressions using data between 1997 and 2010
#Then we try and predict 
pred_table <- test_table_5%>%
  filter(year <= 2010)

bad_countries <- pred_table %>%
  filter(is.nan(log_transfer_emissions)) %>%
  distinct(country)

predicted_data <- test_table_5 %>%
  filter(year == 2020)%>%
  mutate(log_gdp_industrial.bar = mean(log_gdp_industrial, na.rm = TRUE)) %>%
  mutate(log_gdp_fossil.bar = mean(log_gdp_fossil, na.rm = TRUE))%>%
  mutate(log_gdp.bar = mean(log_gdp, na.rm = TRUE))%>%
  mutate(log_territorial.bar = mean(log_territorial, na.rm = TRUE))%>%
  mutate(log_consumption.bar = mean(log_consumption, na.rm = TRUE))%>%
  mutate(fossil_fuel_share.bar = mean(fossil_fuel_share, na.rm = TRUE))%>%
  mutate(imports_gns_percent.bar = mean(imports_gns_percent, na.rm = TRUE))%>%
  mutate(exports_gns_percent.bar = mean(exports_gns_percent, na.rm = TRUE))%>%
  mutate(industry_share.bar = mean(industry_share, na.rm = TRUE))%>%
  mutate(trade_share.bar = mean(trade_share, na.rm = TRUE))%>%
  mutate(log_transfer_emissions.bar = mean(log_transfer_emissions, na.rm = TRUE))%>%
  mutate(transfer_emissions.bar = mean(transfer_emissions, na.rm = TRUE)) %>%
  mutate(territorial_emissions.bar = mean(territorial_emissions, na.rm = TRUE)) %>%
  mutate(consumption_emissions.bar = mean(consumption_emissions, na.rm = TRUE))%>%
  mutate(gdp.bar = mean(gdp, na.rm = TRUE))%>%
  mutate(trade_balance_GnS.bar = mean(trade_balance_GnS, na.rm = TRUE))%>%
  mutate(trade_balance_goods.bar = mean(trade_balance_goods, na.rm = TRUE))



#small function to modify the computation of average errors
mean_absolute_error <- function(data = regression_data){
  mea = sum(data$absolute_error * data$territorial_emissions.share, na.rm = TRUE)/sum(data$territorial_emissions.share, na.rm = TRUE)
  return(mea)
}

#Regression : log consumption on log of fossil GDP
regression_8 <- pmg(log_consumption ~ log_gdp_fossil, data = pred_table, index = c("country", "year"), model = "cmg")
summary(regression_8)
pcdtest(regression_8)

coeffs_regression_8 <- as.data.frame(regression_8$indcoef)
coeffs_regression_8_clean <- as.data.frame(t(coeffs_regression))%>%
  rename(beta_gdp_fossil = log_gdp_fossil,
         beta_gdp_fossil.bar = log_gdp_fossil.bar,
         beta_consumption.bar = y.bar,
         intercept = "(Intercept)"
         )%>%
  rownames_to_column("country")

prediction_table <- merge(x=coeffs_regression_8_clean, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + log_consumption.bar*beta_consumption.bar+ log_gdp_fossil.bar*beta_gdp_fossil.bar + beta_gdp_fossil*log_gdp_fossil)%>%
  mutate (absolute_error = abs(log_consumption - fitted_values))

mean_absolute_error_8 = mean(prediction_table$absolute_error)
mean_absolute_error_8

# Regression : the one from the paper

initial_regression <- pmg(log_territorial ~ log_gdp + fossil_fuel_share + industry_share + imports_gns_percent + exports_gns_percent, 
                          data = pred_table, index = c("country", "year"), model = "cmg")
summary(initial_regression)
pcdtest(initial_regression)

coeffs_initial_regression <- as.data.frame(t(as.data.frame(initial_regression$indcoef)))
colnames(coeffs_initial_regression) <- paste0("beta_", colnames(coeffs_initial_regression))
coeffs_initial_regression <- rownames_to_column(coeffs_initial_regression, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_initial_regression, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept +log_gdp*beta_log_gdp + industry_share*beta_industry_share + fossil_fuel_share*beta_fossil_fuel_share+
          imports_gns_percent * beta_imports_gns_percent + exports_gns_percent * beta_exports_gns_percent + log_territorial.bar*beta_y.bar
          + log_gdp.bar*beta_log_gdp.bar + fossil_fuel_share.bar*beta_fossil_fuel_share.bar+ industry_share.bar*beta_industry_share.bar
          + imports_gns_percent.bar * beta_imports_gns_percent.bar + exports_gns_percent.bar * beta_exports_gns_percent.bar
          )%>%
  mutate (absolute_error = abs(log_territorial - fitted_values))

mean_absolute_error_initial = mean(prediction_table$absolute_error)
summary(prediction_table$absolute_error)
mean_absolute_error_initial

#Regression consumption emissions
initial_regression_cons <- pmg(log_consumption ~ log_gdp + fossil_fuel_share + industry_share + imports_gns_percent + exports_gns_percent, 
                               data = pred_table, index = c("country", "year"), model = "cmg")
summary(initial_regression_cons)
pcdtest(initial_regression_cons)


coeffs_initial_regression_cons <- as.data.frame(t(as.data.frame(initial_regression_cons$indcoef)))
colnames(coeffs_initial_regression_cons) <- paste0("beta_", colnames(coeffs_initial_regression_cons))
coeffs_initial_regression_cons <- rownames_to_column(coeffs_initial_regression_cons, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table_cons <- merge(x=coeffs_initial_regression_cons, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + log_gdp*beta_log_gdp + industry_share*beta_industry_share + fossil_fuel_share * beta_fossil_fuel_share +
            imports_gns_percent * beta_imports_gns_percent + exports_gns_percent * beta_exports_gns_percent + log_consumption.bar*beta_y.bar
          + log_gdp.bar*beta_log_gdp.bar + fossil_fuel_share.bar*beta_fossil_fuel_share.bar+ industry_share.bar*beta_industry_share.bar
          + imports_gns_percent.bar * beta_imports_gns_percent.bar + exports_gns_percent.bar * beta_exports_gns_percent.bar
  )%>%
  mutate (absolute_error = abs(log_consumption - fitted_values))

summary(prediction_table_cons$absolute_error)
mean_absolute_error_initial_cons <- mean_absolute_error(prediction_table_cons)
mean_absolute_error_initial_cons
colnames(prediction_table)

#Now I test a regressions with only one significant parameter 

test_regression <- pmg(log_territorial ~ log_gdp_fossil, data=pred_table,index = c("country", "year"), model = "cmg" )
summary(test_regression)
pcdtest(test_regression)

coeffs_test_regression <- as.data.frame(t(as.data.frame(test_regression$indcoef)))
colnames(coeffs_test_regression) <- paste0("beta_", colnames(coeffs_test_regression))
coeffs_test_regression <- rownames_to_column(coeffs_test_regression, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_test_regression, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + log_gdp_fossil*beta_log_gdp_fossil + log_territorial.bar*beta_y.bar 
          + log_gdp_fossil.bar*beta_log_gdp_fossil.bar
  )%>%
  mutate (absolute_error = abs(log_territorial - fitted_values))

mean_absolute_error_initial_test = mean(prediction_table$absolute_error)
mean_absolute_error_initial_test

#Regression with GDP only

regression_9 <- pmg(log_territorial ~ log_gdp, data=pred_table,index = c("country", "year"), model = "cmg" )
summary(regression_9)
pcdtest(regression_9)

coeffs_regression_9 <- as.data.frame(t(as.data.frame(regression_9$indcoef)))
colnames(coeffs_regression_9) <- paste0("beta_", colnames(coeffs_regression_9))
coeffs_regression_9 <- rownames_to_column(coeffs_regression_9, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_9, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + log_gdp*beta_log_gdp + log_territorial.bar*beta_y.bar 
          + log_gdp.bar*beta_log_gdp.bar
  )%>%
  mutate (absolute_error = abs(log_territorial - fitted_values))

mean_absolute_error_initial_9 = mean(prediction_table$absolute_error)
mean_absolute_error_initial_9

# Prediction without the term in y.bar

prediction_table_10 <- prediction_table%>%
  mutate (fitted_values =log_gdp*beta_log_gdp 
          + log_gdp.bar*beta_log_gdp.bar
  )%>%
  mutate (absolute_error = abs(log_territorial - fitted_values))

mean_absolute_error_initial_10 = mean(prediction_table_10$absolute_error)
mean_absolute_error_initial_10 #Not good, must be changed

# Modeling transfer emissions from other variables 

regression_11 <- pmg(transfer_emissions ~ log_gdp + imports_gns_percent + exports_gns_percent, data = pred_table,
                     index = c("country", "year"), model = "cmg")
summary(regression_11)
pcdtest(regression_11)

coeffs_regression_11 <- as.data.frame(t(as.data.frame(regression_11$indcoef)))
colnames(coeffs_regression_11) <- paste0("beta_", colnames(coeffs_regression_11))
coeffs_regression_11 <- rownames_to_column(coeffs_regression_11, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_11, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + log_gdp*beta_log_gdp + log_territorial.bar*beta_y.bar 
          + log_gdp.bar*beta_log_gdp.bar + imports_gns_percent * beta_imports_gns_percent 
          + imports_gns_percent.bar * beta_imports_gns_percent.bar + exports_gns_percent * beta_exports_gns_percent
          + exports_gns_percent.bar*beta_exports_gns_percent.bar
  )%>%
  mutate (absolute_error = abs(transfer_emissions - fitted_values))

mean_absolute_error_initial_11 = mean(prediction_table$absolute_error)
mean_absolute_error_initial_11 #1.094485

mean_transfer_emissions = sqrt(var(prediction_table$transfer_emissions))
mean_transfer_emissions # 2.569771

#Trying a linear regression on this one, this one is not bad

zz_problems <- pred_table %>%
  filter(is.na(trade_balance_GnS) | is.na(trade_balance_goods))%>%
  select(country, year)

pred_table_3 <- pred_table %>%
  filter (!(country %in% zz_problems$country))

regression_12 <- pmg(transfer_emissions ~ gdp + trade_balance_GnS + industry_share, data = pred_table_3, 
                     index = c("country", "year"), model = "cmg")
summary(regression_12)
pcdtest(regression_12)

coeffs_regression_12 <- as.data.frame(t(as.data.frame(regression_12$indcoef)))
colnames(coeffs_regression_12) <- paste0("beta_", colnames(coeffs_regression_12))
coeffs_regression_12 <- rownames_to_column(coeffs_regression_12, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_12, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + gdp*beta_gdp + gdp.bar*beta_gdp.bar +transfer_emissions.bar * beta_y.bar
          + trade_balance_GnS*beta_trade_balance_GnS + trade_balance_GnS.bar * beta_trade_balance_GnS.bar +
            industry_share * beta_industry_share + industry_share.bar*beta_industry_share.bar
  )%>%
  mutate (absolute_error = abs(transfer_emissions - fitted_values))

mean_absolute_error_12 = mean(prediction_table$absolute_error)
mean_absolute_error_12 #1.793


mean_transfer_emissions = sqrt(var(prediction_table$transfer_emissions))
mean_transfer_emissions #2.57

#Linear regression of emissions

regression_13 <- pmg(territorial_emissions ~ gdp + fossil_fuel_share + imports_gns_percent + exports_gns_percent, 
                     data = pred_table, index = c("country", "year"), model = "cmg")
summary(regression_13)  
pcdtest(regression_13)

coeffs_regression_13 <- as.data.frame(t(as.data.frame(regression_13$indcoef)))
colnames(coeffs_regression_13) <- paste0("beta_", colnames(coeffs_regression_13))
coeffs_regression_13 <- rownames_to_column(coeffs_regression_13, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_13, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + gdp*beta_gdp + gdp.bar*beta_gdp.bar +territorial_emissions.bar * beta_y.bar
          + fossil_fuel_share * beta_fossil_fuel_share + fossil_fuel_share.bar * beta_fossil_fuel_share.bar
          + imports_gns_percent * beta_imports_gns_percent + exports_gns_percent * beta_exports_gns_percent
          + imports_gns_percent.bar * beta_imports_gns_percent.bar + exports_gns_percent.bar * beta_exports_gns_percent.bar
  )%>%
  mutate (absolute_error = abs(territorial_emissions - fitted_values))

mean_absolute_error_initial_13 = mean(prediction_table$absolute_error)
mean_absolute_error_initial_13#1.288

mean_transfer_emissions = sqrt(var(prediction_table$transfer_emissions))
mean_transfer_emissions #2.57

# consumption-based-emissions using territorial emissions

regression_14 <- pmg(log_consumption ~ log_territorial + log_gdp + imports_gns_percent 
                      + exports_gns_percent, data = pred_table, index = c("country", "year"), model = "cmg")
summary(regression_14)
pcdtest(regression_14)

coeffs_regression_14 <- as.data.frame(t(as.data.frame(regression_14$indcoef)))
colnames(coeffs_regression_14) <- paste0("beta_", colnames(coeffs_regression_14))
coeffs_regression_14 <- rownames_to_column(coeffs_regression_14, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_14, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept + log_gdp*beta_log_gdp +log_consumption.bar * beta_y.bar
          +log_territorial * beta_log_territorial + log_territorial.bar * beta_log_territorial.bar
          + imports_gns_percent * beta_imports_gns_percent + exports_gns_percent * beta_exports_gns_percent
          + imports_gns_percent.bar * beta_imports_gns_percent.bar + exports_gns_percent.bar * beta_exports_gns_percent.bar
          + log_gdp.bar * beta_log_gdp.bar
  )%>%
  mutate (absolute_error = abs(log_consumption - fitted_values))

mean_absolute_error_initial_14 = mean(prediction_table$absolute_error)
mean_absolute_error_initial_14#0.325

regression_15 <- pmg(log_consumption ~ log_territorial, data = pred_table, index = c("country", "year"), model = "cmg")
summary(regression_15)
pcdtest(regression_15)

coeffs_regression_15 <- as.data.frame(t(as.data.frame(regression_15$indcoef)))
colnames(coeffs_regression_15) <- paste0("beta_", colnames(coeffs_regression_15))
coeffs_regression_15 <- rownames_to_column(coeffs_regression_15, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_15, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept +log_consumption.bar * beta_y.bar
          +log_territorial * beta_log_territorial + log_territorial.bar * beta_log_territorial.bar
  )%>%
  mutate (absolute_error = abs(log_consumption - fitted_values))

mean_absolute_error_initial_15 = mean(prediction_table$absolute_error)
mean_absolute_error_initial_15#0.243

regression_16 <- pmg(consumption_emissions ~ territorial_emissions + gdp + trade_balance_GnS + trade_balance_goods,
                     data = pred_table_3, index = c("country", "year"), model = "cmg")
summary(regression_16)
pcdtest(regression_16)

coeffs_regression_16 <- as.data.frame(t(as.data.frame(regression_16$indcoef)))
colnames(coeffs_regression_16) <- paste0("beta_", colnames(coeffs_regression_16))
coeffs_regression_16 <- rownames_to_column(coeffs_regression_16, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_16, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept +consumption_emissions.bar * beta_y.bar
          +territorial_emissions * beta_territorial_emissions + territorial_emissions.bar * beta_territorial_emissions.bar
          + trade_balance_GnS*beta_trade_balance_GnS + trade_balance_GnS.bar * beta_trade_balance_GnS.bar
          + trade_balance_goods*beta_trade_balance_goods + trade_balance_goods.bar * beta_trade_balance_goods.bar
          +gdp*beta_gdp + gdp.bar*beta_gdp.bar
  )%>%
  mutate (absolute_error = abs(consumption_emissions - fitted_values))

mean_absolute_error_initial_16 = mean(prediction_table$absolute_error)
mean_absolute_error_initial_16#2.01

regression_17 <- pmg(consumption_emissions ~ territorial_emissions, data = pred_table, index = c("country", "year"), model = "cmg")
summary(regression_17)
pcdtest(regression_17)

coeffs_regression_17 <- as.data.frame(t(as.data.frame(regression_17$indcoef)))
colnames(coeffs_regression_17) <- paste0("beta_", colnames(coeffs_regression_17))
coeffs_regression_17 <- rownames_to_column(coeffs_regression_17, "country")%>%
  rename(intercept = "beta_(Intercept)")

prediction_table <- merge(x=coeffs_regression_17, y = predicted_data, by="country")%>%
  mutate (fitted_values = intercept +consumption_emissions.bar * beta_y.bar
          +territorial_emissions * beta_territorial_emissions + territorial_emissions.bar * beta_territorial_emissions.bar
  )%>%
  mutate (absolute_error = abs(consumption_emissions - fitted_values))

mean_absolute_error_initial_17 = mean(prediction_table$absolute_error)
mean_absolute_error_initial_17#1.018


