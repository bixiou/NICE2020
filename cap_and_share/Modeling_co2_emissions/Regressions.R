############################################################################################################
#This code takes cleaned data as input and proceeds to regressions                                         #
#They are linear regressions, that include predefined averages per time-period                             #
#The code runs a regression country using the most data available, and stores the coefficients             #
#Then it predicts data and measures the mean absolute error (weighted-averages by emissions per country)   #
############################################################################################################

library(dplyr)
library(readr)
library(stats)
library(tidyr)
library(openxlsx)
library(rlang)
library(plm)
library(stats)
library(tidyverse)

#############################################################################################################
#Loading data and treating some of it


regression_data <- read.csv("regression_data.csv")
countries <- read.csv("countries_table.csv")

#Adding one column as the share of territorial emissions (not per capita) over all countries
regression_data <- regression_data%>%
  group_by(year)%>%
  mutate( territorial_emissions.country = territorial_emissions * population )%>%
  mutate(territorial_emissions.share =  territorial_emissions.country/sum(territorial_emissions.country, na.rm = TRUE))%>%
  ungroup()

#Adding a few log variables
regression_data <- regression_data%>%
  rename(imports_gns_percent.world = imports_share.world)%>%
  rename(exports_gns_percent.world = exports_share.world)%>%
  mutate(log_ffsh = log(fossil_fuel_share))%>%
  mutate(log_ffsh.world = log(fossil_fuel_share.world))%>%
  mutate(log_imports_gns_percent = log(imports_gns_percent))%>%
  mutate(log_exports_gns_percent = log(exports_gns_percent))%>%
  mutate(log_exports_gns_percent.world = log(exports_gns_percent.world))%>%
  mutate(log_imports_gns_percent.world = log(imports_gns_percent.world))%>%
  mutate(log_industry_share = log(industry_share))%>%
  mutate(log_industry_share.world = log(industry_share.world))%>%
  mutate(log_emissions_pc.world = log(emissions_pc.world))%>%
  mutate(log_gdp.world = log(gdp.world))%>%
  mutate(trade_gns_percent.world = imports_gns_percent.world+exports_gns_percent.world)%>%
  mutate(trade_balance_gns_percent = exports_gns_percent - imports_gns_percent)

test_data <- regression_data%>%
  filter(country=="USA")

test_data <- regression_data%>%
  filter(country %in% c("OMN", "TKM", "BRN"))

#############################################################################################################
#This part defines function to run regressions easily

#This function selects the biggest number of countries and timespan possible depending on the explanatory variables used
regression_emissions <- function(data = regression_data, emissions, explanatory_variables){
  #Firstly the time span must be defined for the data.
  #Define variables
  start_year = 1989
  end_year = 2024
  
  annoying_countries <- regression_data %>%
    filter(year==1990)%>%
    select(country)%>%
    distinct(country)
  
  cols_to_exclude <- c("fossil_fuel_share.world", "industry_share.world")
  cols_to_exclude_present <- intersect(cols_to_exclude, explanatory_variables)
  
  restricted_data <- data%>%
    select(c("year", "country", "territorial_emissions.share" ,emissions, explanatory_variables, "territorial_emissions.share"))
    
  while(dim(annoying_countries)[1] > 80){
    if(end_year > 2020){
      end_year=end_year-1
    }
    start_year = start_year + 1
  restricted_data <- restricted_data %>%
    filter(year >= start_year)%>%
    filter(year <= end_year)
  annoying_countries <- restricted_data %>%
    filter(if_any(-all_of(cols_to_exclude_present), is.na)) %>%
    select(country)%>%
    distinct(country)
  }
  restricted_data <- restricted_data%>%
    filter(!(country %in% annoying_countries$country))
  return(restricted_data)
}

#This function runs the required regressions and returns a tables of predicted data for the asked year
prediction_regression <- function(data = regression_data, emissions, explanatory_variables, predicted_data, 
                                  logged_emissions=FALSE, fixed_effect = FALSE){
  rhs <- paste(explanatory_variables, collapse = " + ")
  formula = paste0(emissions, "~ ", rhs, " - 1")
  if(isTRUE(fixed_effect)){
    formula = paste0(emissions, "~ ", rhs)
  }
  regression_test <- pmg(as.character(formula), data = data, index = c("country","year"), model = "mg")
  print(summary(regression_test))
  print(pcdtest(regression_test))
  
  coeffs_regression_test <- as.data.frame(t(as.data.frame(regression_test$indcoef)))
  colnames(coeffs_regression_test) <- paste0("beta_", colnames(coeffs_regression_test))
  coeffs_regression_test <- rownames_to_column(coeffs_regression_test, "country")
  if(isTRUE(fixed_effect)){
    coeffs_regression_test <- coeffs_regression_test%>%
      rename(intercept = "beta_(Intercept)")
  }
  prediction_table <- merge(x=coeffs_regression_test, y = predicted_data, by="country")
  
  for (var in explanatory_variables) {
    prediction_table[[paste0("term_", var)]] <- prediction_table[[var]] * prediction_table[[paste0("beta_", var)]]
  }
  prediction_table <- prediction_table %>%
    rowwise() %>%
    mutate(fitted_values = sum(c_across(starts_with("term_"))),
    absolute_error = abs(.data[[emissions]] - fitted_values)) %>%
    ungroup()
  if(isTRUE(fixed_effect)){
    prediction_table <- prediction_table%>%
      mutate(fitted_values = fitted_values + intercept,
             absolute_error = abs(.data[[emissions]] - fitted_values)
      )}
  if(isTRUE(logged_emissions)){
    prediction_table<- prediction_table%>%
      mutate(true_emissions = exp(.data[[emissions]]),
             true_fitted_values = exp(fitted_values),
             absolute_error = abs(true_emissions - true_fitted_values))
  }
  return(prediction_table)
}

#This function computes the mean absoluter error from a prediction table
mean_absolute_error <- function(data = regression_data){
  mea = sum(data$absolute_error * data$territorial_emissions.share, na.rm = TRUE)/sum(data$territorial_emissions.share, na.rm = TRUE)
  return(mea)
}

#############################################################################################################
#The rest of the code runs regressions on the data


#Regression 1 : territorial emissions ~ emissions.bar + ffsh + ffsh.bar + gdp + gdp.bar
data_table <- regression_emissions(data = regression_data, "territorial_emissions",  c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world", "fossil_fuel_share.world"))

data_base <- data_table %>%  
  filter(year <= 2012) #select ten years before the end year for prediction

data_pred <- data_table%>%
  filter(year ==2022)

regression_results_1 <- prediction_regression(data = data_base, emissions = "territorial_emissions", 
                                              explanatory_variables = c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world"),
                                              predicted_data = data_pred, fixed_effect = TRUE
                                              )

summary(regression_results_1$absolute_error)

mea_regression_1 <- mean_absolute_error(regression_results_1)
mea_regression_1


#Regression 2
data_table_2 <- regression_emissions(data = regression_data, "log_consumption", 
                                     c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                       "log_imports_gns_percent", "log_imports_gns_percent.world", "log_exports_gns_percent",
                                       "log_exports_gns_percent.world"))

data_base <- data_table_2 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_2%>%
  filter(year ==2020)

regression_results_2 <- prediction_regression(data = data_base, emissions = "log_consumption",
                                              explanatory_variables = c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                                                        "log_imports_gns_percent", "log_imports_gns_percent.world", "log_exports_gns_percent",
                                                                        "log_exports_gns_percent.world"),
                                              predicted_data = data_pred, logged_emissions = TRUE)


summary(regression_results_2$absolute_error)
mea_regression_2 <- mean_absolute_error(regression_results_2)
mea_regression_2

#Regression 3 : Regressions from Liddle(2018)
#I will need to add fossil_fuel_share.world when I have decided what to do with it
data_table_3 <- regression_emissions(data = regression_data, "log_territorial", c("log_gdp", "log_gdp.world", "fossil_fuel_share","fossil_fuel_share.world",
                                                                                  "industry_share", "industry_share.world", "trade_share", "trade_gns_percent.world"))

data_base <- data_table_3 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_3%>%
  filter(year ==2020)

regression_results_3 <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables =c("log_gdp", 
                                              "log_gdp.world", "fossil_fuel_share",
                                              "industry_share", "industry_share.world",
                                              "trade_share", "trade_gns_percent.world"),
                                              predicted_data = data_pred, logged_emissions = TRUE)

summary(regression_results_3$absolute_error)
mea_regression_3 <- mean_absolute_error(regression_results_3)
mea_regression_3

#Regression 4
#I will need to add fossil_fuel_share.world when I have decided what to do with it
data_table_4 <- regression_emissions(data = regression_data, "log_consumption", c("log_gdp", "log_gdp.world", "fossil_fuel_share","fossil_fuel_share.world",
                                                                                  "industry_share", "industry_share.world", "trade_share", "trade_gns_percent.world"))
data_base <- data_table_4 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_4%>%
  filter(year ==2020)

regression_results_4 <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables =c("log_gdp", 
                                              "log_gdp.world", "fossil_fuel_share", "industry_share", "industry_share.world",
                                              "trade_gns_percent.world", "trade_share"),
                                              predicted_data = data_pred, logged_emissions = TRUE)

summary(regression_results_4$absolute_error)
mea_regression_4 <- mean_absolute_error(regression_results_4)
mea_regression_4

#Regression 5
#Add an intercept on this one

data_table_5 <- regression_emissions(data = regression_data, "log_consumption", c("log_gdp", "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                  "log_exports_gns_percent.world", "log_imports_gns_percent.world"))

data_base <- data_table_5 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_5%>%
  filter(year ==2020)

regression_results_5 <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_gdp", 
                                              "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world", 
                                              "log_imports_gns_percent.world"), predicted_data = data_pred, logged_emissions = TRUE)

summary(regression_results_5$absolute_error)
mea_regression_5 <- mean_absolute_error(regression_results_5)
mea_regression_5


#Regression 6
#Add an intercept on this one too
data_table_6 <- regression_emissions(data = regression_data, "log_territorial", c("log_gdp", "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                  "log_exports_gns_percent.world", "log_imports_gns_percent.world"))

data_base <- data_table_6 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_6%>%
  filter(year ==2020)

regression_results_6 <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables = c("log_gdp", 
                                              "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world", 
                                              "log_imports_gns_percent.world"), predicted_data = data_pred, logged_emissions = TRUE,
                                              fixed_effect = TRUE)

summary(regression_results_6$absolute_error)
mea_regression_6 <- mean_absolute_error(regression_results_6)
mea_regression_6

#Regression 7
# test of the relevance of this formula
# log_emissions = log_gdp + log_energy_intensity + log_ffsh + log_emissions_ff_per_ton, 
#where log_energy_intensity + log_emissions_ff_per_ton can be seen as a structural parameter of an economy 
#that can be thought easier to estimate with a "magic formula"

regression_data_7 <- regression_data%>%
  mutate(difference_variable = log_territorial - log_ffsh - log_gdp)

data_table_7 <- regression_emissions(data = regression_data_7, "difference_variable", c("log_gdp", "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", 
                                                                    "log_exports_gns_percent.world", "log_imports_gns_percent.world", "log_gdp.world", "log_industry_share",
                                                                    "log_industry_share.world", "log_emissions_pc.world", "log_territorial", "territorial_emissions"))
data_base <- data_table_7 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_7%>%
  filter(year ==2020)

regression_results_7 <- prediction_regression(data = data_base, emissions = "difference_variable", explanatory_variables = c("log_exports_gns_percent", "log_imports_gns_percent", 
                                              "log_exports_gns_percent.world", "log_imports_gns_percent.world", "log_gdp.world", "log_industry_share",
                                              "log_industry_share.world", "log_emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_7 <- regression_results_7%>%
  mutate(emissions_fitted_values = exp(fitted_values+log_gdp+log_ffsh))%>%
  mutate(absolute_error = abs(emissions_fitted_values - territorial_emissions))

summary(regression_results_7$absolute_error)
mea_regression_7 <- mean_absolute_error(regression_results_7)
mea_regression_7

#Regression 8 : attempt to predict consumption_emisssions with this formula
#cons_emissions = territorial *(1+ (export_percent_emissions - import_emissions)/100)

regression_data_8 <- regression_data%>%
  mutate(emissions_balance_percent = ((consumption_emissions/territorial_emissions)-1)*100)

data_table_8 <- regression_emissions(data = regression_data_8, "emissions_balance_percent", c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                        "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                        "industry_share.world","territorial_emissions", "emissions_pc.world", "consumption_emissions", "trade_balance_gns_percent"))
data_base <- data_table_8 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_8%>%
  filter(year ==2020)

regression_results_8 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                          "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                          "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect=TRUE)

regression_results_8 <- regression_results_8%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_8$absolute_error)
mea_regression_8 <- mean_absolute_error(regression_results_8)
mea_regression_8

#Regression 9 : same idea with less explanatory variables because nothing is significant then
#cons_emissions = territorial *(1+ (export_percent_emissions - import_emissions)/100)
#Please run regression 8 before to set the data tables, prediction tables correctly
#in the explanatory variables, there is not imports_gns_percent_world because it should be equal to exports_gns_percent.world in theory so no sens to keep it

regression_results_9 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", 
                                                                                                                                   "exports_gns_percent.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_9 <- regression_results_9%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_9$absolute_error)
mea_regression_9 <- mean_absolute_error(regression_results_9)
mea_regression_9

#Regression 10 : same idea, with less parameters
#Please run regression 8 before to set the data tables, prediction tables correctly

regression_results_10 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent"), 
                                                predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_10 <- regression_results_10%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_10$absolute_error)
mea_regression_10 <- mean_absolute_error(regression_results_10)
mea_regression_10

#Regression 11 : same idea, different parameters

regression_results_11 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_11 <- regression_results_11%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_11$absolute_error)
mea_regression_11 <- mean_absolute_error(regression_results_11)
mea_regression_11
