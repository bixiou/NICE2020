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
library(broom)
library(lmtest)
library(forecast)
library(tibble)
library(ggplot2)

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
  mutate(trade_balance_gns_percent = exports_gns_percent - imports_gns_percent)%>%
  mutate(emissions_balance_percent = ((consumption_emissions/territorial_emissions)-1)*100)%>%
  mutate(trade_balance_goods_percent = exports_goods_percent - imports_goods_percent)%>%
  mutate(balance_fuel_percent_gdp = fuel_exports_percent_gdp - fuel_imports_percent_gdp)%>%
  mutate(log_gdp_square = log_gdp^2)

test_data <- regression_data%>%
  mutate(transfer_emissions = consumption_emissions - territorial_emissions)

test_data <- regression_data%>%
  filter(country == "QAT")

sigma_transfer = sqrt(var(test_data$transfer_emissions, na.rm = TRUE))
sigma_transfer

#############################################################################################################
#This part defines function to run regressions easily

#This function selects the biggest number of countries and timespan possible depending on the explanatory variables used
regression_emissions <- function(data = regression_data, emissions, explanatory_variables, last_year = 2020, countries_aside = 80){
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
    select(c("year", "country", "territorial_emissions.share" ,"population", emissions, explanatory_variables))
    
  while(dim(annoying_countries)[1] > countries_aside){
    if(end_year > last_year){
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
  prediction_table <- merge(x=coeffs_regression_test, y = predicted_data, by="country", all.y=TRUE)
  
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
             residuals = .data[[emissions]] - fitted_values,
             absolute_error = abs(residuals)
      )}
  if(isTRUE(logged_emissions)){
    prediction_table<- prediction_table%>%
      mutate(true_emissions = exp(.data[[emissions]]),
             true_fitted_values = exp(fitted_values),
             residuals = true_emissions - true_fitted_values,
             absolute_error = abs(residuals))
  }
  return(prediction_table)
}

#This function runs the required regressions and returns a tables of predicted data for the asked year
regression_residuals <- function(data = regression_data, emissions, explanatory_variables, predicted_data, 
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
  regression_table <- merge(x=coeffs_regression_test, y = data, by="country", all.y=TRUE)
  
  for (var in explanatory_variables) {
    regression_table[[paste0("term_", var)]] <- regression_table[[var]] * regression_table[[paste0("beta_", var)]]
  }
  regression_table <- regression_table %>%
    rowwise() %>%
    mutate(fitted_values = sum(c_across(starts_with("term_"))),
           residuals = .data[[emissions]] - fitted_values) %>%
    ungroup()
  if(isTRUE(fixed_effect)){
    regression_table <- regression_table%>%
      mutate(fitted_values = fitted_values + intercept,
             residuals = .data[[emissions]] - fitted_values
      )}
  if(isTRUE(logged_emissions)){
    regression_table<- regression_table%>%
      mutate(true_emissions = exp(.data[[emissions]]),
             true_fitted_values = exp(fitted_values),
             residuals = true_emissions - true_fitted_values)
  }
  return(regression_table)
}

auto_correlation_test <- function(data = regression_data){
  country_code = c("AAA")
  durbin = c(0)
  ljung = c(0)
  results <- data.frame(country = country_code, dw_stat = durbin, ljung_pval = ljung)
  
  for (c in unique(data$country)) {
    data_country <- data %>%
      filter(country == c) %>%
      arrange(year)
    
    resid <- data_country$residuals
    # On continue seulement si au moins 8 valeurs
    if (length(resid) >= 8 && !all(is.na(resid))) {
      
      # Test de Durbin-Watson
      dw <- tryCatch({
        model <- lm(resid ~ seq_along(resid))
        dwtest(model)$statistic
      }, error = function(e) NA)
      
      # Ljung-Box test (jusqu'au lag 3 par ex.)
      lb <- tryCatch({
        Box.test(resid, lag = 3, type = "Ljung-Box")$p.value
      }, error = function(e) NA)
      
      # Ajouter à la table
      results <- rbind(results, c( c, dw, lb))
    }
  }
  # Voir les pays avec autocorrélation significative
  results <- results %>%
    mutate(auto_corr = ifelse(ljung_pval < 0.05, "Yes", "No"))%>%
    filter(country != "AAA")
  data_merge <- merge(x=data, y=results, by = "country", all = TRUE)
  return(data_merge)
}

#This function computes the mean absoluter error from a prediction table
mean_absolute_error <- function(data = regression_data, column = "absolute_error"){
  mea = sum(data[[column]] * data$territorial_emissions.share, na.rm = TRUE)/sum(data$territorial_emissions.share, na.rm = TRUE)
  return(mea)
}

mean_absolute_error_5Y <- function(data=regression_data, last_year = 2020){
  data2<- data%>%
    group_by(country)%>%
    mutate(absolute_error_5Y = mean(absolute_error))%>%
    ungroup()
  return(data2)
}

absolute_error_percent_5Y<- function(data = regression_data){
  data2 <- data%>%
    mutate(absolute_error_percent = absolute_error/consumption_emissions*100)%>%
    group_by(country)%>%
    mutate(percent_error_5Y = mean(absolute_error_percent))%>%
    ungroup()
    return(data2)
}

#############################################################################################################
#The next part of the code runs regressions on the data


#Regression 1 : territorial emissions ~ emissions.bar + ffsh + gdp + gdp.bar
data_table <- regression_emissions(data = regression_data, "territorial_emissions",  c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world", "fossil_fuel_share.world"))

data_base <- data_table %>%  
  filter(year <= 2012) #select ten years before the end year for prediction

data_pred <- data_table%>%
  filter(year ==2022)

data_pred_5Y <- data_table%>%
  filter(year <=2022)%>%
  filter(year >= 2018)


regression_results_1 <- prediction_regression(data = data_base, emissions = "territorial_emissions", 
                                              explanatory_variables = c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world"),
                                              predicted_data = data_pred, fixed_effect = TRUE
                                              )

test_table <- regression_residuals(data = data_base, emissions = "territorial_emissions", 
                                   explanatory_variables = c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world"),
                                   predicted_data = data_pred, fixed_effect = TRUE)

test_table <- auto_correlation_test(test_table)

test_table <- test_table%>%
  select(country, year, residuals, dw_stat, ljung_pval)

summary(regression_results_1$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 0.001127  0.113349  0.617200  2.064845  1.773080 28.312253         2 

mea_regression_1 <- mean_absolute_error(regression_results_1)
mea_regression_1
#1.890395

regression_results_1_5Y <- prediction_regression(data = data_base, emissions = "territorial_emissions", 
                                              explanatory_variables = c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world"),
                                              predicted_data = data_pred_5Y, fixed_effect = TRUE
)

regression_results_1 <- mean_absolute_error_5Y(data = regression_results_1_5Y)%>%
  filter (year==2020)

summary(regression_results_1$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.01265  0.13665  0.56101  1.53213  1.42143 19.21694        2 

regression_results_1 <- regression_results_1%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_1$territorial_emissions*regression_results_1$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_1$residuals*regression_results_1$population, na.rm = TRUE)/1000000
sum_residuals
error_percent = (sum_residuals/sum_emissions)*100
error_percent

mea_regression_1 <- mean_absolute_error(regression_results_1, column = "absolute_error_5Y")
mea_regression_1
# 1.3844


#Regression 2
#log_imports_gns_percent.world and log_exports_gns_percent.world should be the same so one of them goes away
data_table_2 <- regression_emissions(data = regression_data, "log_consumption", 
                                     c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                       "log_imports_gns_percent", "log_imports_gns_percent.world", "log_exports_gns_percent",
                                       "log_exports_gns_percent.world", "consumption_emissions"))

data_base <- data_table_2 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_2%>%
  filter(year ==2020)

data_pred_5Y <- data_table_2%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)

regression_results_2 <- prediction_regression(data = data_base, emissions = "log_consumption",
                                              explanatory_variables = c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                                                        "log_imports_gns_percent", "log_exports_gns_percent.world", "log_exports_gns_percent"
                                                                        ),
                                              predicted_data = data_pred, logged_emissions = TRUE)


summary(regression_results_2$absolute_error)

#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000135  0.132127  0.463264  1.373232  1.166617 22.407738

mea_regression_2 <- mean_absolute_error(regression_results_2)
mea_regression_2
#0.4774391

regression_results_2_5Y <- prediction_regression(data = data_base, emissions = "log_consumption",
                                              explanatory_variables = c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                                                        "log_imports_gns_percent", "log_exports_gns_percent",
                                                                        "log_imports_gns_percent.world"),
                                              predicted_data = data_pred_5Y, logged_emissions = TRUE)

regression_results_2_5Y <- absolute_error_percent_5Y(data = regression_results_2_5Y)
regression_results_2 <- mean_absolute_error_5Y(data = regression_results_2_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_2$consumption_emissions*regression_results_2$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_2$residuals*regression_results_2$population, na.rm = TRUE)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -0.07%

summary(regression_results_2$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.008357  0.171313  0.333318  1.202489  1.015812 26.642325  

mea_regression_2 <- mean_absolute_error(regression_results_2, column = "absolute_error_5Y")
mea_regression_2
# 0.4670147

summary(regression_results_2$percent_error_5Y)


#Regression 3 : Regressions from Liddle(2018)
#I will need to add fossil_fuel_share.world when I have the info
data_table_3 <- regression_emissions(data = regression_data, "log_territorial", c("log_gdp", "log_gdp.world", "fossil_fuel_share","fossil_fuel_share.world",
                                                                                  "industry_share", "industry_share.world", "trade_share", "trade_gns_percent.world"))

data_base <- data_table_3 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_3%>%
  filter(year ==2020)

data_pred_5Y <- data_table_3%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)

regression_results_3 <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables =c("log_gdp", 
                                              "log_gdp.world", "fossil_fuel_share",
                                              "industry_share", "industry_share.world",
                                              "trade_share", "trade_gns_percent.world"),
                                              predicted_data = data_pred, logged_emissions = TRUE)

summary(regression_results_3$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.001045  0.119133  0.383311  1.007138  1.055606 28.945666 

mea_regression_3 <- mean_absolute_error(regression_results_3)
mea_regression_3
#1.555699

regression_results_3_5Y <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables =c("log_gdp", 
                                                                                                                        "log_gdp.world", "fossil_fuel_share",
                                                                                                                        "industry_share", "industry_share.world",
                                                                                                                        "trade_share", "trade_gns_percent.world"),
                                              predicted_data = data_pred_5Y, logged_emissions = TRUE)

regression_results_3 <- mean_absolute_error_5Y(data = regression_results_3_5Y)%>%
  filter (year==2020)

summary(regression_results_3$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.01027  0.16220  0.37621  0.96154  0.80654 28.63058    

mea_regression_3 <- mean_absolute_error(regression_results_3, column = "absolute_error_5Y")
mea_regression_3
# 1.368306


#Regression 4
#I will need to add fossil_fuel_share.world when I have decided what to do with it
data_table_4 <- regression_emissions(data = regression_data, "log_consumption", c("log_gdp", "log_gdp.world", "fossil_fuel_share","fossil_fuel_share.world",
                                                                                  "industry_share", "industry_share.world", "trade_share", "trade_gns_percent.world", "consumption_emissions"))
data_base <- data_table_4 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_4%>%
  filter(year ==2020)

data_pred_5Y <- data_table_4%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)

regression_results_4 <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables =c("log_gdp", 
                                              "log_gdp.world", "fossil_fuel_share", "industry_share", "industry_share.world",
                                              "trade_gns_percent.world", "trade_share"),
                                              predicted_data = data_pred, logged_emissions = TRUE)

summary(regression_results_4$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00297  0.32066  0.81496  2.57351  2.36094 54.58661 

mea_regression_4 <- mean_absolute_error(regression_results_4)
mea_regression_4
# 2.071

regression_results_4_5Y <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables =c("log_gdp", 
                                                                                                                        "log_gdp.world", "fossil_fuel_share", "industry_share", "industry_share.world",
                                                                                                                        "trade_gns_percent.world", "trade_share"),
                                              predicted_data = data_pred_5Y, logged_emissions = TRUE)

regression_results_4 <- mean_absolute_error_5Y(data = regression_results_4_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_4$consumption_emissions*regression_results_4$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_4$residuals*regression_results_4$population, na.rm = TRUE)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -15.63%

summary(regression_results_4$absolute_error_5Y)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.00691  0.33313  0.87941  2.69152  1.95955 37.19393   

mea_regression_4 <- mean_absolute_error(regression_results_4, column = "absolute_error_5Y")
mea_regression_4
# 1.89700

#Regression 5

data_table_5 <- regression_emissions(data = regression_data, "log_consumption", c("log_gdp", "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                  "log_exports_gns_percent.world", "log_imports_gns_percent.world", "consumption_emissions"))

data_base <- data_table_5 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_5%>%
  filter(year ==2020)

data_pred_5Y <- data_table_5%>%
  filter(year <= 2020)%>%
  filter(year >=2016)

regression_results_5 <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_gdp", 
                                              "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world", 
                                              "log_imports_gns_percent.world"), predicted_data = data_pred, logged_emissions = TRUE, fixed_effect = TRUE)

summary(regression_results_5$absolute_error)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#0.0201   0.1830   0.5487   9.2990   1.8748 733.5164        2


mea_regression_5 <- mean_absolute_error(regression_results_5)
mea_regression_5
#1.390

regression_results_5_5Y <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_gdp", 
                                                                                                                         "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world", 
                                                                                                                         "log_imports_gns_percent.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = TRUE, fixed_effect = TRUE)

regression_results_5 <- mean_absolute_error_5Y(data = regression_results_5_5Y)%>%
  filter (year==2020)

regression_results_5 <- regression_results_5%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_5$consumption_emissions*regression_results_5$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_5$residuals*regression_results_5$population, na.rm = TRUE)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -0.37%

summary(regression_results_5$absolute_error_5Y)
#Min.  1st Qu.   Median Mean    3rd Qu. Max.      NA's
#0.03   0.26     0.57   392.55  1.44    37917.66  2   

mea_regression_5 <- mean_absolute_error(regression_results_5, column = "absolute_error_5Y")
mea_regression_5
# 3.031946

#Regression 6
data_table_6 <- regression_emissions(data = regression_data, "log_territorial", c("log_gdp", "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                  "log_exports_gns_percent.world", "log_imports_gns_percent.world"))

data_base <- data_table_6 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_6%>%
  filter(year ==2020)

data_pred_5Y <- data_table_6%>%
  filter(year<=2020)%>%
  filter(year>=2016)

regression_results_6 <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables = c("log_gdp", 
                                              "log_ffsh", "log_exports_gns_percent", "log_exports_gns_percent", 
                                              "log_imports_gns_percent.world"), predicted_data = data_pred, logged_emissions = TRUE,
                                              fixed_effect = FALSE)

summary(regression_results_6$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.001464  0.187781  0.619084  1.397154  1.683666 10.625201

mea_regression_6 <- mean_absolute_error(regression_results_6)
mea_regression_6
# 1.728472

regression_results_6_5Y <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables = c("log_gdp", 
                                                                                                                         "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                                                         "log_exports_gns_percent.world"), 
                                              predicted_data = data_pred_5Y , logged_emissions = TRUE,
                                              fixed_effect = FALSE)

regression_results_6 <- mean_absolute_error_5Y(data = regression_results_6_5Y)%>%
  filter (year==2020)


summary(regression_results_6$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.009868 0.190144 0.534787 1.031816 1.373161 8.118032 

mea_regression_6 <- mean_absolute_error(regression_results_6, column = "absolute_error_5Y")
mea_regression_6
# 0.7995528

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

data_pred_5Y <- data_table_7%>%
  filter(year >= 2016)%>%
  filter(year<=2020)

regression_results_7 <- prediction_regression(data = data_base, emissions = "difference_variable", explanatory_variables = c("log_exports_gns_percent", "log_imports_gns_percent", 
                                              "log_exports_gns_percent.world", "log_gdp.world", "log_industry_share",
                                              "log_industry_share.world", "log_emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_7 <- regression_results_7%>%
  mutate(emissions_fitted_values = exp(fitted_values+log_gdp+log_ffsh))%>%
  mutate(absolute_error = abs(emissions_fitted_values - territorial_emissions))

summary(regression_results_7$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.004959  0.140062  0.411449  0.908262  0.930567 11.504264

mea_regression_7 <- mean_absolute_error(regression_results_7)
mea_regression_7
# 1.022132

regression_results_7_5Y <- prediction_regression(data = data_base, emissions = "difference_variable", explanatory_variables = c("log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                                                                "log_exports_gns_percent.world", "log_gdp.world", "log_industry_share",
                                                                                                                                "log_industry_share.world", "log_emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_7 <- regression_results_7_5Y%>%
  mutate(emissions_fitted_values = exp(fitted_values+log_gdp+log_ffsh))%>%
  mutate(absolute_error = abs(emissions_fitted_values - territorial_emissions))

regression_results_7 <- mean_absolute_error_5Y(data = regression_results_7)%>%
  filter (year==2020)

summary(regression_results_7$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01083 0.13526 0.39478 0.77493 0.84616 7.27365 

mea_regression_7 <- mean_absolute_error(regression_results_7, column = "absolute_error_5Y")
mea_regression_7
# 0.8925552

#Regression 8 : attempt to predict consumption_emisssions with this formula
#cons_emissions = territorial *(1+ (export_percent_emissions - import_emissions)/100)

data_table_8 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                        "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                        "industry_share.world","territorial_emissions", "emissions_pc.world", "consumption_emissions", "trade_balance_gns_percent"))
data_base <- data_table_8 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_8%>%
  filter(year ==2020)

data_pred_5Y <- data_table_8%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_8 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                          "exports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                          "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect=TRUE)

regression_results_8 <- regression_results_8%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_8$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.04887  0.40413  1.43508  3.64426  4.02704 50.27105        3 

mea_regression_8 <- mean_absolute_error(regression_results_8)
mea_regression_8
#1.443498

regression_results_8_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                                                       "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_8 <- regression_results_8_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_8 <- mean_absolute_error_5Y(data = regression_results_8)%>%
  filter (year==2020)

regression_results_8 <- regression_results_8%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_8$consumption_emissions*regression_results_8$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_8$residuals*regression_results_8$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 4.39%

summary(regression_results_8$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.05394  0.36080  1.36176  3.49143  3.23635 63.65582        3   

mea_regression_8 <-mean_absolute_error(regression_results_8, column = "absolute_error_5Y")
mea_regression_8
# 1.231603

#Regression 8bis : the same one without a fixed effect
#It is better because it makes some variables significant

regression_results_8bis <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                   "exports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                                                   "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect=FALSE)

regression_results_8bis <- regression_results_8bis%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_8bis$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01349  0.33015  1.17965  3.47261  3.65821 45.24033 

mea_regression_8bis <- mean_absolute_error(regression_results_8bis)
mea_regression_8bis
# 1.916851

regression_results_8_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                                                       "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_8 <- regression_results_8_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_8 <- mean_absolute_error_5Y(data = regression_results_8)%>%
  filter (year==2020)

regression_results_8 <- regression_results_8%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_8$consumption_emissions*regression_results_8$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_8$residuals*regression_results_8$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 7.65%

summary(regression_results_8$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02817  0.37086  0.98504  2.93163  3.33147 38.49574      

mea_regression_8 <-mean_absolute_error(regression_results_8, column = "absolute_error_5Y")
mea_regression_8
# 1.507348

#Regression 9 : same idea with less explanatory variables because nothing is significant then
#cons_emissions = territorial *(1+ (export_percent_emissions - import_emissions)/100)
#Please run regression 8 before to set the data tables, prediction tables correctly
#in the explanatory variables, there is not imports_gns_percent_world because it should be equal to exports_gns_percent.world in theory so no sense to keep it

regression_results_9 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", 
                                                                                                                                   "exports_gns_percent.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_9 <- regression_results_9%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_9$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000234  0.099830  0.281868  0.895189  1.015069 10.967929
mea_regression_9 <- mean_absolute_error(regression_results_9)
mea_regression_9
#0.307

regression_results_9_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_9 <- regression_results_9_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_9 <- mean_absolute_error_5Y(data = regression_results_9)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_9$consumption_emissions*regression_results_9$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_9$residuals*regression_results_9$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -1.97

summary(regression_results_9$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01719  0.12830  0.32118  0.90150  0.79119 11.67934     

mea_regression_9 <-mean_absolute_error(regression_results_9, column = "absolute_error_5Y")
mea_regression_9
# 0.3416135

#Regression 10 : same idea, with less parameters
#Please run regression 8 before to set the data tables, prediction tables correctly

regression_results_10 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent"), 
                                                predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_10 <- regression_results_10%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_10$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.001498 0.113748 0.292712 0.716670 0.916854 8.167147

mea_regression_10 <- mean_absolute_error(regression_results_10)
mea_regression_10
#0.451

regression_results_10_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("trade_balance_gns_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_10 <- regression_results_10_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_10 <- mean_absolute_error_5Y(data = regression_results_10)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_10$consumption_emissions*regression_results_10$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_10$residuals*regression_results_10$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -2.29%

summary(regression_results_10$absolute_error_5Y)
#Min.      1st Qu.   Median  Mean     3rd Qu.     Max. 
#0.00996  0.11633  0.31109  0.75082  0.84582 10.50518     

mea_regression_10 <-mean_absolute_error(regression_results_10, column = "absolute_error_5Y")
mea_regression_10
# 0.4261112

#Regression 11 : same idea, different parameters
#Please run regression_8 at first

regression_results_11 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_11 <- regression_results_11%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_11$absolute_error)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000571  0.115959  0.338840  0.996417  0.885461 12.185380 

mea_regression_11 <-mean_absolute_error(regression_results_11)
mea_regression_11
#0.429013

regression_results_11_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_11 <- regression_results_11_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_11 <- mean_absolute_error_5Y(data = regression_results_11)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_11$consumption_emissions*regression_results_11$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_11$residuals*regression_results_11$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -2.30%

summary(regression_results_11$absolute_error_5Y)
#Min.      1st Qu.   Median  Mean     3rd Qu.     Max. 
#0.01521  0.12571  0.34823  1.03883  0.85641 14.46472    

mea_regression_11 <-mean_absolute_error(regression_results_11, column = "absolute_error_5Y")
mea_regression_11
# 0.4369344

#Regression 12 

data_table_12 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                              "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                              "industry_share.world","territorial_emissions", "emissions_pc.world", "consumption_emissions", 
                                                                                              "trade_balance_gns_percent", "fuel_imports_percent_gdp", "fuel_exports_percent_gdp"),
                                      last_year = 2019, countries_aside = 90)
data_base <- data_table_12 %>%  
  filter(year <= 2009) #select ten years before the end year for prediction

data_pred <- data_table_12%>%
  filter(year ==2019)

data_pred_5Y <- data_table_12%>%
  filter(year <=2019)%>%
  filter(year >= 2015)

regression_results_12 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                    "fuel_exports_percent_gdp", "exports_gns_percent.world"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_12 <- regression_results_12%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_12$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0027   0.1986   0.5116   6.9557   1.3064 416.0792 
mea_regression_12 <-mean_absolute_error(regression_results_12)
mea_regression_12 
# 5.013065

regression_results_12_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                        "fuel_exports_percent_gdp", "exports_gns_percent.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_12 <- regression_results_12_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_12 <- mean_absolute_error_5Y(data = regression_results_12)%>%
  filter (year==2019)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_12$consumption_emissions*regression_results_12$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_12$residuals*regression_results_12$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 17.95%

summary(regression_results_12$absolute_error_5Y)
#Min.      1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02299   0.17363   0.39370   3.14734   1.18828 118.65266    

mea_regression_12 <-mean_absolute_error(regression_results_12, column = "absolute_error_5Y")
mea_regression_12
# 2.354903

#Regression 13 : trying the same idea with different variables
#Please run regression 12 before

regression_results_13 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                    "fuel_exports_percent_gdp"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_13 <- regression_results_13%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_13$absolute_error)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.00725   0.14678   0.41271   3.17694   1.17782 158.76332

mea_regression_13 <-mean_absolute_error(regression_results_13)
mea_regression_13 
#2.016763

regression_results_13_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                        "fuel_exports_percent_gdp"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_13 <- regression_results_13_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_13 <- mean_absolute_error_5Y(data = regression_results_13)%>%
  filter (year==2019)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_13$consumption_emissions*regression_results_13$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_13$residuals*regression_results_13$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 1.56%%

summary(regression_results_13$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01481  0.17267  0.41082  1.89775  0.95931 52.27609    

mea_regression_13 <-mean_absolute_error(regression_results_13, column = "absolute_error_5Y")
mea_regression_13
# 1.219397

#Regression 14 : including the three balances : energy, goods, and g&s

data_table_14 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("exports_gns_percent", "imports_gns_percent", 
                                                                                                "exports_gns_percent.world", "imports_gns_percent.world",
                                                                                                "territorial_emissions", "consumption_emissions", 
                                                                                                "fuel_imports_percent_gdp", "fuel_exports_percent_gdp",
                                                                                                "imports_goods_percent", "exports_goods_percent"),
                                      last_year = 2020, countries_aside = 90)
data_base <- data_table_14 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_14%>%
  filter(year ==2020)

data_pred_5Y <- data_table_14%>%
  filter(year <= 2020)%>%
  filter(year>=2016)

regression_results_14 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                    "fuel_exports_percent_gdp", "imports_goods_percent", "exports_goods_percent"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_14 <- regression_results_14%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_14$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0044   0.1697   0.5383   8.7960   1.5130 414.9429 
mea_regression_14 <-mean_absolute_error(regression_results_14)
mea_regression_14 
#9.44313

regression_results_14_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                        "fuel_exports_percent_gdp", "imports_goods_percent", "exports_goods_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_14 <- regression_results_14_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_14 <- mean_absolute_error_5Y(data = regression_results_14)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_14$consumption_emissions*regression_results_14$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_14$residuals*regression_results_14$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 48.4%

summary(regression_results_14$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0200   0.2464   0.6040   6.6814   1.6814 335.1440    

mea_regression_14 <-mean_absolute_error(regression_results_14, column = "absolute_error_5Y")
mea_regression_14
# 7.25034

#Regression 15 : let us try the same but with balances instead of imports/exports to reduce the number of variables

data_table_15 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("trade_balance_gns_percent",
                                                                                             "territorial_emissions", "consumption_emissions", 
                                                                                             "balance_fuel_percent_gdp",
                                                                                             "trade_balance_goods_percent"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_15 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_15%>%
  filter(year ==2020)

data_pred_5Y <- data_table_15%>%
  filter(year <= 2020)%>%
  filter(year>=2016)

regression_results_15 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                    "trade_balance_goods_percent"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_15 <- regression_results_15%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_15$absolute_error)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00143  0.09914  0.51165  1.19253  1.38442 11.44760 

mea_regression_15 <-mean_absolute_error(regression_results_15)
mea_regression_15
#1.265895

regression_results_15_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                       "trade_balance_goods_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_15 <- regression_results_15_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_15 <- mean_absolute_error_5Y(data = regression_results_15)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_15$consumption_emissions*regression_results_15$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_15$residuals*regression_results_15$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 7.05%%

summary(regression_results_15$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01282 0.15101 0.40753 1.09394 1.24863 7.21631    

mea_regression_15 <-mean_absolute_error(regression_results_15, column = "absolute_error_5Y")
mea_regression_15
# 1.347694

#Regression 16

data_table_16 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("territorial_emissions", "consumption_emissions", 
                                                                                             "balance_fuel_percent_gdp",
                                                                                             "trade_balance_goods_percent"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_16 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_16%>%
  filter(year ==2020)

data_pred_5Y <- data_table_16%>%
  filter(year <= 2020)%>%
  filter(year>=2016)

regression_results_16 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                                                    "trade_balance_goods_percent"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_16 <- regression_results_16%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_16$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000894  0.136797  0.455350  1.127554  1.223879 10.945408
mea_regression_16 <-mean_absolute_error(regression_results_16)
mea_regression_16
#0.5941903

regression_results_16_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                                                       "trade_balance_goods_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_16 <- regression_results_16_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))

regression_results_16 <- mean_absolute_error_5Y(data = regression_results_16)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_16$consumption_emissions*regression_results_16$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_16$residuals*regression_results_16$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -2.1%

summary(regression_results_16$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02104  0.13525  0.37798  0.99375  0.88055 13.17092    

mea_regression_16 <-mean_absolute_error(regression_results_16, column = "absolute_error_5Y")
mea_regression_16
# 0.5074227

#Regression 17
data_table_17 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("territorial_emissions", "consumption_emissions", 
                                                                                             "balance_fuel_percent_gdp",
                                                                                             "trade_balance_gns_percent"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_17 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_17%>%
  filter(year ==2020)

data_pred_5Y <- data_table_17%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_17 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                                                    "trade_balance_gns_percent"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_17 <- regression_results_17%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_17$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.002706 0.117709 0.379811 1.078145 1.328076 8.735624 
mea_regression_17 <-mean_absolute_error(regression_results_17)
mea_regression_17
#0.5630568

regression_results_17_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                                                       "trade_balance_gns_percent"), 
predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_17 <- regression_results_17_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_17 <- mean_absolute_error_5Y(data = regression_results_17)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_17$consumption_emissions*regression_results_17$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_17$residuals*regression_results_17$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -2.63%

summary(regression_results_17$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.007851 0.142377 0.359326 0.945904 0.958414 8.972502   

mea_regression_17 <-mean_absolute_error(regression_results_17, column = "absolute_error_5Y")
mea_regression_17
# 0.4491961

#Regression 18
regression_results_18 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp"
                                                                                                                                    ), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_18 <- regression_results_18%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_18$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000838  0.108963  0.511789  1.077979  1.014312 10.922235  
mea_regression_18 <-mean_absolute_error(regression_results_18)
mea_regression_18
#0.668645

regression_results_18_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp"
), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_18 <- regression_results_18_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_18 <- mean_absolute_error_5Y(data = regression_results_18)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_18$consumption_emissions*regression_results_18$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_18$residuals*regression_results_18$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -3.5%

summary(regression_results_18$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01126  0.16476  0.43532  0.99801  0.95957 13.24577  

mea_regression_18 <-mean_absolute_error(regression_results_18, column = "absolute_error_5Y")
mea_regression_18
# 0.6198263


#Regression 19 : trying to predict carbon footprints with old trade balances and other significant data
data_table_19 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("trade_balance_gns_percent",
                                                                                             "territorial_emissions", "consumption_emissions", 
                                                                                             "balance_fuel_percent_gdp",
                                                                                             "log_gdp", "log_gdp.world", "emissions_pc.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_19 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_19%>%
  filter(year ==2020)

data_pred_5Y <- data_table_19%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_19 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                     "log_gdp"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_19 <- regression_results_19%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_19$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01135  0.13008  0.45514  1.19858  1.21550 14.05315

mea_regression_19 <-mean_absolute_error(regression_results_19)
mea_regression_19
#0.6778395

regression_results_19_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                       "log_gdp"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_19 <- regression_results_19_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_19 <- mean_absolute_error_5Y(data = regression_results_19)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_19$consumption_emissions*regression_results_19$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_19$residuals*regression_results_19$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 4.61%

summary(regression_results_19$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01392  0.19433  0.48229  1.18934  1.11470 16.20969 

mea_regression_19 <-mean_absolute_error(regression_results_19, column = "absolute_error_5Y")
mea_regression_19
# 0.626079

#Regression 20

data_base <- data_table_19 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_19%>%
  filter(year ==2020)

data_pred_5Y <- data_table_19%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_20 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent",
                                                                                                                                    "log_gdp"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_20 <- regression_results_20%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_20$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.009014 0.143114 0.362848 1.000745 1.015463 8.945577 

mea_regression_20 <-mean_absolute_error(regression_results_20)
mea_regression_20
# 0.5809407

regression_results_20_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent",
                                                                                                                                       "log_gdp"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_20 <- regression_results_20_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_20 <- mean_absolute_error_5Y(data = regression_results_20)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_20$consumption_emissions*regression_results_20$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_20$residuals*regression_results_20$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 0.04%

summary(regression_results_20$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01958 0.21034 0.48602 1.08855 1.09959 7.83040

mea_regression_20 <-mean_absolute_error(regression_results_20, column = "absolute_error_5Y")
mea_regression_20
# 0.5853706

#Regression 21 
data_table_21 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("territorial_emissions", "consumption_emissions", 
                                                                                             "balance_fuel_percent_gdp",
                                                                                             "trade_balance_gns_percent",
                                                                                             "log_gdp","log_ffsh", "fossil_fuel_share"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_21 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_21%>%
  filter(year ==2020)

data_pred_5Y <- data_table_21%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_21 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                           "log_gdp", "fossil_fuel_share"), 
                      predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_21 <- regression_results_21%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_21$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02686  0.19026  0.53002  1.26143  1.41514 13.49528 

mea_regression_21 <-mean_absolute_error(regression_results_21)
mea_regression_21
# 1.044052

regression_results_21_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                    "log_gdp", "fossil_fuel_share"), 
                                               predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_21 <- regression_results_21_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = emissions_fitted_values - consumption_emissions,
         absolute_error = abs(residuals))


regression_results_21 <- mean_absolute_error_5Y(data = regression_results_21)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_21$consumption_emissions*regression_results_21$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_21$residuals*regression_results_21$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 8.45%

summary(regression_results_21$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02145  0.19203  0.45880  1.17538  1.07112 15.89085

mea_regression_21 <-mean_absolute_error(regression_results_21, column = "absolute_error_5Y")
mea_regression_21
# 0.8416661

#Regression 22
data_table_22 <- regression_emissions(data = regression_data, "log_consumption", c("consumption_emissions", "log_gdp", "log_gdp", "log_ffsh", "log_emissions_pc.world", "log_gdp.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_22 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_22%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_22_5Y <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_gdp", "log_ffsh", "log_emissions_pc.world", "log_gdp.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = TRUE, fixed_effect = FALSE)


regression_results_22 <- mean_absolute_error_5Y(data = regression_results_22_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_22$consumption_emissions*regression_results_22$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_22$residuals*regression_results_22$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 13.31%

summary(regression_results_22$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.008395  0.275370  0.683776  1.399820  1.577859 11.469046 

mea_regression_22 <-mean_absolute_error(regression_results_22, column = "absolute_error_5Y")
mea_regression_22
# 1.336118

#Regression 23
data_table_23 <- regression_emissions(data = regression_data, "log_consumption", c("consumption_emissions","log_gdp","log_ffsh", "log_gdp", "log_emissions_pc.world", "log_gdp.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_23 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_23%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_23_5Y <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_gdp", "log_emissions_pc.world", "log_gdp.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = TRUE, fixed_effect = FALSE)


regression_results_23 <- mean_absolute_error_5Y(data = regression_results_23_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_23$consumption_emissions*regression_results_23$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_23$residuals*regression_results_23$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 3.246%

summary(regression_results_23$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02007  0.23088  0.52045  2.18899  1.73223 39.14342  

mea_regression_23 <-mean_absolute_error(regression_results_23, column = "absolute_error_5Y")
mea_regression_23
# 1.160092

#Regression 24
data_table_24 <- regression_emissions(data = regression_data, "log_consumption", c("consumption_emissions","log_territorial", "log_emissions_pc.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_24 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_24%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_24_5Y <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_territorial", "log_emissions_pc.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = TRUE, fixed_effect = TRUE)


regression_results_24 <- mean_absolute_error_5Y(data = regression_results_24_5Y)%>%
  filter (year==2020)

regression_results_8 <- regression_results_8%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_24$consumption_emissions*regression_results_24$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_24$residuals*regression_results_24$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 7.79%

summary(regression_results_24$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01137 0.16788 0.41095 0.85195 0.99913 8.96389  

mea_regression_24 <-mean_absolute_error(regression_results_24, column = "absolute_error_5Y")
mea_regression_24
# 0.869099

#Regression 25

data_table_25 <- regression_emissions(data = regression_data, "consumption_emissions", c("territorial_emissions", "consumption_emissions", "gdp", "emissions_pc.world", "trade_balance_gns", "trade_balance_goods", "gdp.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_25 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_25%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_25_5Y <- prediction_regression(data = data_base, emissions = "consumption_emissions", explanatory_variables = c("territorial_emissions", "gdp", "emissions_pc.world", "trade_balance_gns", "trade_balance_goods", "gdp.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_25 <- mean_absolute_error_5Y(data = regression_results_25_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_25$consumption_emissions*regression_results_25$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_25$residuals*regression_results_25$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -5.20%

summary(regression_results_25$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01607  0.28523  0.65104  2.03520  1.79500 23.16737  

mea_regression_25 <-mean_absolute_error(regression_results_25, column = "absolute_error_5Y")
mea_regression_25
# 0.9782042

#Regression 26

data_table_26 <- regression_emissions(data = regression_data, "consumption_emissions", c("territorial_emissions", "emissions_pc.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_26 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_26%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_26_5Y <- prediction_regression(data = data_base, emissions = "consumption_emissions", explanatory_variables = c("territorial_emissions", "emissions_pc.world"),
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_26 <- mean_absolute_error_5Y(data = regression_results_26_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_26$consumption_emissions*regression_results_26$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_26$residuals*regression_results_26$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 6.14%

summary(regression_results_26$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.009646 0.147919 0.424105 0.921362 1.111834 7.999177  

mea_regression_26 <-mean_absolute_error(regression_results_26, column = "absolute_error_5Y")
mea_regression_26
# 0.8329376

################################################################################################################################
#In this part the table of absolute errors per country and the summary table for regressions are established

absolute_error_table <- function(first_regression = 8, last_regression = 21){
  table =regression_data%>%
    filter(year == 2020)%>%
    select(country, consumption_emissions, territorial_emissions.share)
  for (i in first_regression:last_regression){
    if(!(i %in% c(1,3,6,7))){
    reg_name <- paste0("regression_results_", i)
    regression_table <- get(reg_name)
    table <- merge(x = table, y = regression_table[, c("country", "absolute_error_5Y")], by = "country", all.x = TRUE) %>%
      rename(!!paste0("regression_error_", i) := absolute_error_5Y)
    }
  }
  return(table)
}

error_table <- absolute_error_table(first_regression = 1, last_regression = 26)%>%
  rowwise() %>%
  mutate(min_error = min(c_across(starts_with("regression_error_")), na.rm = TRUE))%>%
  mutate(min_error = na_if(min_error, Inf))%>%
  mutate(best_regression = ifelse(is.na(min_error), NA, names(.)[1+which.min(c_across(starts_with("regression_error_")))])
  ) %>% 
  mutate(best_error_percent = min_error/consumption_emissions*100)
  ungroup()

mean_min_error = mean_absolute_error(data = error_table, column = "min_error")  
mean_min_error
# 0.1282598

summary(error_table$min_error)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00691 0.04783 0.11796 0.28393 0.29395 2.22246      66 

test_data <- error_table%>%
  filter(min_error > 0.5)%>%
  select(country, min_error, best_error_percent, consumption_emissions)

test_data
#There are 15 countries in that case
#It means that the method up to now has identified 98 countries for which consumption-based emissions are correct à 0.5tCO2.pc.py

summary(error_table$best_error_percent)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.5191  1.8179  3.4046  6.9427  7.2084 60.0508      66 

test_data <- error_table%>%
  filter(best_error_percent > 10)%>%
  select(country, best_error_percent, min_error, consumption_emissions)%>%
  arrange(desc(best_error_percent))

write.xlsx(test_data, "annoying_predictions.xlsx")

corrected_error_table <- error_table%>%
  filter(!(country %in% test_data$country))%>%
  filter(!(is.na(min_error)))

corrected_mean_min_error = mean_absolute_error(data = corrected_error_table, column = "min_error")
corrected_mean_min_error
#0.104097

summary(corrected_error_table$min_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.006907 0.041123 0.098143 0.140490 0.214641 0.481683 

predicted_emissions_share = sum(corrected_error_table$territorial_emissions.share)
predicted_emissions_share
#0.9455673

write.xlsx(error_table, "error_table.xlsx")

##############################################################################################
#This is a test to see how many tons of CO2 are unaccounted for in the current computations

added_df <- regression_data%>%
  filter(year==2020)%>%
  select(country, population, territorial_emissions.country)

test_data <- merge(x= error_table, y=added_df, by = "country")%>%
  mutate(consumption_emissions.country = consumption_emissions*population)%>%
  filter(!(is.na(consumption_emissions.country)))

sum_territorial = sum(test_data$territorial_emissions.country)
sum_consumption = sum(test_data$consumption_emissions.country)

sum_territorial
sum_consumption

diff = (sum_consumption/sum_territorial-1)*100
diff

total_emissions = sum_territorial/predicted_emissions_share
total_emissions

sup_data <- regression_data%>%
  filter(year ==2020)
total_emissions = sum(sup_data$territorial_emissions.country, na.rm=TRUE)
total_emissions

missing = 35127000000 - total_emissions
missing

#############################################################################################
#This is a test to see how to model quadratic terms 

p <- ggplot(regression_data, aes(x = log_gdp, y = emissions_balance_percent)) +
  geom_point() +
  theme_minimal()

print(p)

ggsave(filename = "emissions_ratio.png", plot = p, height = 6, width = 10)

cut_data <- regression_data %>%
  filter(year == 2020)

p <- ggplot(regression_data, aes(x = log_gdp, y = emissions_balance_percent)) +
  geom_point() +
  theme_minimal()

print(p)

ggsave(filename = "emissions_ratio_2020.png", plot = p, height = 6, width = 10)

p <- ggplot(regression_data, aes(x = trade_balance_gns_percent, y = emissions_balance_percent)) +
  geom_point() +
  theme_minimal()

print(p)

################################################################################################
#This is the test of a pooled model

data_table_pooled <- regression_data%>%
  select(country, year, emissions_balance_percent, log_gdp, log_gdp_square, trade_balance_gns_percent, 
         trade_balance_goods_percent, balance_fuel_percent_gdp)%>%
  filter(year <= 2010)

regression_pooled <- lm(emissions_balance_percent~ log_gdp + log_gdp_square + trade_balance_gns_percent + trade_balance_goods_percent
                        + balance_fuel_percent_gdp,data = data_table_pooled, na.action = na.omit)

summary(regression_pooled)
coefficients <- regression_pooled$coefficients
coefficients[2]

regression_prediction <- regression_data%>%
  select(country, year, territorial_emissions, consumption_emissions, population, emissions_balance_percent, log_gdp, log_gdp_square, trade_balance_gns_percent, 
         trade_balance_goods_percent, balance_fuel_percent_gdp)%>%
  filter(year <= 2020)%>%
  mutate(estimate_regression = coefficients[1] + coefficients[2]*log_gdp + coefficients[3]*log_gdp_square 
                              + coefficients[4]*trade_balance_gns_percent + coefficients[5]*trade_balance_goods_percent
                              + coefficients[6]*balance_fuel_percent_gdp)%>%
  mutate(residual_percent = emissions_balance_percent - estimate_regression)%>%
  filter(!(is.na(residual_percent)))

regression_evaluation <- regression_prediction%>%
  filter(year == 2020)%>%
  arrange(residual_percent)

summary(regression_evaluation$residual_percent)

balance_emissions = sum(regression_evaluation$residual_percent*regression_evaluation$territorial_emissions*regression_evaluation$population/100)
balance_emissions

sum_territorial = sum(regression_evaluation$territorial_emissions*regression_evaluation$population)
sum_territorial
sum_consumption = sum(regression_evaluation$consumption_emissions*regression_evaluation$population)
sum_consumption


error_percent = balance_emissions/sum_territorial*100
error_percent

#####################################################################################################
#In this part we first make a pooled regression of the ratio on log_gdp and log_gdp_square and the three balances
#to extract the global trend, after we extract the variable ration - log_gdp*beta - log_gdp_square*beta_square
#and then we run a pmg of the residuals on the three trade balances, and evaluate the relevance of the method

data_table_semi <- regression_data%>%
  select(country, year, emissions_balance_percent, log_gdp, log_gdp_square, trade_balance_gns_percent, 
         trade_balance_goods_percent, balance_fuel_percent_gdp)%>%
  filter(year <= 2010)

regression_semi <- lm(emissions_balance_percent~ log_gdp + log_gdp_square ,data = data_table_semi, na.action = na.omit)

summary(regression_semi)
coefficients <- regression_pooled$coefficients

regression_prediction_semi <- regression_data%>%
  select(country, year, territorial_emissions, consumption_emissions, population, emissions_balance_percent, log_gdp, log_gdp_square, trade_balance_gns_percent, 
         trade_balance_goods_percent, balance_fuel_percent_gdp, territorial_emissions.share)%>%
  filter(year <= 2020)%>%
  mutate(new_residuals = emissions_balance_percent - coefficients[1] - coefficients[2]*log_gdp - coefficients[3]*log_gdp_square)

regression_semi2 <- pmg(new_residuals ~ trade_balance_gns_percent + balance_fuel_percent_gdp -1, 
                        data = regression_prediction_semi, index = c("country", "year"), model = "mg")
summary(regression_semi2)
pcdtest(regression_semi2)

coeffs_2 <- as.data.frame(t(regression_semi2$indcoef))
colnames(coeffs_2) <- c("beta_gns_balance", "beta_fuel_balance")
coeffs_2 <- rownames_to_column(coeffs_2, "country")

regression_prediction_semi2 <- regression_prediction_semi%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)

regression_prediction_semi2 <- merge(x=regression_prediction_semi2, y= coeffs_2, by= "country", all.y = TRUE)

regression_prediction_semi2 <- regression_prediction_semi2%>%
  mutate(residuals = (new_residuals - beta_gns_balance*trade_balance_gns_percent - beta_fuel_balance*balance_fuel_percent_gdp)*territorial_emissions/100,
         absolute_error = abs(residuals),
         residuals.country = residuals*population,
         territorial_emissions.country = territorial_emissions*population,
         consumption_emissions.country = consumption_emissions*population)%>%
  filter(!(is.na(residuals)))

regression_prediction_semi2 <- mean_absolute_error_5Y(data = regression_prediction_semi2)%>%
  filter(year == 2020)


summary(regression_prediction_semi2$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.005094 0.112858 0.311313 0.784921 0.924432 9.142964 

mea_semi2 = mean_absolute_error(data = regression_prediction_semi2, column = "absolute_error_5Y")
mea_semi2
#0.7883787

sum_residuals = sum(regression_prediction_semi2$residuals.country)
sum_residuals
# -1013953296
sum_territorial = sum(regression_prediction_semi2$territorial_emissions.country)
sum_territorial
sum_consumption = sum(regression_prediction_semi2$consumption_emissions.country)
sum_consumption

error_percent = sum_residuals/sum_territorial*100
error_percent
# -3.13%

semi_table <- regression_prediction_semi2%>%
  select(country, absolute_error_5Y, residuals.country, consumption_emissions)%>%
  mutate(error_percent = absolute_error_5Y/consumption_emissions*100)%>%
  arrange(desc(error_percent))%>%
  rename(absolute_error_5Y_semi = absolute_error_5Y,
         error_percent_semi = error_percent,
         residuals.country_semi = residuals.country)%>%
  select(-consumption_emissions)


#####################################################################################################
# Now we test the possibility of just taking the mean ratio to predict consumption_emissions

data_table_simple <- regression_data%>%
  select(country, year, emissions_balance_percent)%>%
  filter(year <= 2010)%>%
  group_by(country)%>%
  mutate(mean_ratio = mean(emissions_balance_percent, na.rm = TRUE))%>%
  ungroup()

data_estimate_ratio <- data_table_simple%>%
  filter(year == 2010)%>%
  select(country, mean_ratio)

data_prediction <- regression_data%>%
  select(country, year, territorial_emissions, consumption_emissions, emissions_balance_percent, territorial_emissions.share, population)%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)

data_prediction <- merge( x= data_prediction, y= data_estimate_ratio, by = "country", all.y = TRUE)%>%
  mutate(predicted_emissions = territorial_emissions*(1+mean_ratio/100),
         residuals = consumption_emissions - predicted_emissions,
         absolute_error = abs(residuals),
         residuals.country = residuals*population,
         territorial_emissions.country = territorial_emissions*population)

data_prediction <- mean_absolute_error_5Y(data = data_prediction)%>%
  filter(year == 2020)%>%
  filter(!(is.na(residuals)|is.na(population)))

summary(data_prediction$absolute_error_5Y)

mea_simple = mean_absolute_error(data = data_prediction, column = "absolute_error_5Y")
mea_simple
# 0.5318479

sum_residuals = sum(data_prediction$residuals.country)
sum_residuals
#967360393

sum_territorial = sum(data_prediction$territorial_emissions.country)
sum_territorial
# 32716968919

error_percent = sum_residuals/sum_territorial*100
error_percent
#2.96%

data_compare <- data_prediction%>%
  mutate(error_percent = absolute_error_5Y/consumption_emissions*100)%>%
  arrange(desc(error_percent))%>%
  select(country, territorial_emissions, consumption_emissions, error_percent, residuals.country, absolute_error_5Y)%>%
  rename(error_percent_simple = error_percent,
         absolute_error_5Y_simple = absolute_error_5Y,
         residuals.country_simple=residuals.country )

data_summary <- merge( x= data_compare, y= semi_table, by = "country", all = TRUE)%>%
  mutate(min_error_percent = case_when(
    is.na(error_percent_simple) ~ error_percent_semi,
    is.na(error_percent_semi) ~ error_percent_simple,
    error_percent_simple <= error_percent_semi ~ error_percent_simple,
    TRUE ~ error_percent_semi
  ))%>%
  mutate(best_residuals.country = case_when(
    is.na(error_percent_simple) ~ residuals.country_semi,
    is.na(error_percent_semi) ~ residuals.country_simple,
    error_percent_simple <= error_percent_semi ~ residuals.country_simple,
    TRUE ~ residuals.country_semi
  ))%>%
  arrange(desc(min_error_percent))

sum_residuals = sum(data_summary$best_residuals.country)
sum_residuals
#218367358

total_balance = sum_residuals/sum_consumption*100
total_balance
#0.67%

write.csv(data_summary, "results_two_regressions.csv")
error_table <- read.xlsx("error_table.xlsx")

additional_regression_data <- error_table%>%
  filter(country %in% c("LAO", "PAN", "ZMB", "TGO", "GIN", "CYP", "VNM", "BEN", "RWA", "MOZ", "MLT", "TJK",
                        "BGD", "KAZ", "NPL", "BWA", "BHR", "LVA", "TUR", "GRC", "EST", "DOM", "NAM"))

uncovered_countries <- regression_data%>%
  filter(year == 2020)%>%
  filter(!(country %in% data_summary$country))
