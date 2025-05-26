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
  mutate(trade_balance_gns_percent = exports_gns_percent - imports_gns_percent)%>%
  mutate(emissions_balance_percent = ((consumption_emissions/territorial_emissions)-1)*100)%>%
  mutate(trade_balance_goods_percent = exports_goods_percent - imports_goods_percent)%>%
  mutate(balance_fuel_percent_gdp = fuel_exports_percent_gdp - fuel_imports_percent_gdp)

test_data <- regression_data%>%
  mutate(transfer_emissions = consumption_emissions - territorial_emissions)

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
    select(c("year", "country", "territorial_emissions.share" ,emissions, explanatory_variables, "territorial_emissions.share"))
    
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
  print(pbnftest(regression_test))
  
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
mean_absolute_error <- function(data = regression_data, column = "absolute_error"){
  mea = sum(data[[column]] * data$territorial_emissions.share, na.rm = TRUE)/sum(data$territorial_emissions.share, na.rm = TRUE)
  return(mea)
}

mean_absolute_error_5Y <- function(data=regression_data, last_year = 2020){
  data2<- data%>%
    group_by(country)%>%
    mutate(absolute_error_5Y = mean(absolute_error))%>%
    ungroup()%>%
  return(data2)
}

#############################################################################################################
#The next part of the code runs regressions on the data


#Regression 1 : territorial emissions ~ emissions.bar + ffsh + ffsh.bar + gdp + gdp.bar
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

summary(regression_results_1$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 0.001127  0.113349  0.617200  2.064845  1.773080 28.312253         2

mea_regression_1 <- mean_absolute_error(regression_results_1)
mea_regression_1
#1.890

regression_results_1_5Y <- prediction_regression(data = data_base, emissions = "territorial_emissions", 
                                              explanatory_variables = c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world"),
                                              predicted_data = data_pred_5Y, fixed_effect = TRUE
)

regression_results_1 <- mean_absolute_error_5Y(data = regression_results_1_5Y)%>%
  filter (year==2020)

summary(regression_results_1$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.01265  0.13665  0.56101  1.53213  1.42143 19.21694        2    

mea_regression_1 <- mean_absolute_error(regression_results_1, column = "absolute_error_5Y")
mea_regression_1
# 1.384468


#Regression 2
data_table_2 <- regression_emissions(data = regression_data, "log_consumption", 
                                     c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                       "log_imports_gns_percent", "log_imports_gns_percent.world", "log_exports_gns_percent",
                                       "log_exports_gns_percent.world"))

data_base <- data_table_2 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_2%>%
  filter(year ==2020)

data_pred_5Y <- data_table_2%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)

regression_results_2 <- prediction_regression(data = data_base, emissions = "log_consumption",
                                              explanatory_variables = c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                                                        "log_imports_gns_percent", "log_imports_gns_percent.world", "log_exports_gns_percent",
                                                                        "log_exports_gns_percent.world"),
                                              predicted_data = data_pred, logged_emissions = TRUE)


summary(regression_results_2$absolute_error)

#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01083  0.15678  0.38556  1.34644  1.01906 20.13011 

mea_regression_2 <- mean_absolute_error(regression_results_2)
mea_regression_2
#0.503

regression_results_2_5Y <- prediction_regression(data = data_base, emissions = "log_consumption",
                                              explanatory_variables = c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                                                        "log_imports_gns_percent", "log_imports_gns_percent.world", "log_exports_gns_percent",
                                                                        "log_exports_gns_percent.world"),
                                              predicted_data = data_pred_5Y, logged_emissions = TRUE)

regression_results_2 <- mean_absolute_error_5Y(data = regression_results_2_5Y)%>%
  filter (year==2020)

summary(regression_results_2$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01525  0.15678  0.38523  1.32904  1.07043 24.67282   

mea_regression_2 <- mean_absolute_error(regression_results_2, column = "absolute_error_5Y")
mea_regression_2
# 0.4979511

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
#1.56

regression_results_3_5Y <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables =c("log_gdp", 
                                                                                                                        "log_gdp.world", "fossil_fuel_share",
                                                                                                                        "industry_share", "industry_share.world",
                                                                                                                        "trade_share", "trade_gns_percent.world"),
                                              predicted_data = data_pred_5Y, logged_emissions = TRUE)

regression_results_3 <- mean_absolute_error_5Y(data = regression_results_3_5Y)%>%
  filter (year==2020)

summary(regression_results_3$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01027  0.16220  0.37621  0.96154  0.80654 28.63058   

mea_regression_3 <- mean_absolute_error(regression_results_3, column = "absolute_error_5Y")
mea_regression_3
# 1.368306


#Regression 4
#I will need to add fossil_fuel_share.world when I have decided what to do with it
data_table_4 <- regression_emissions(data = regression_data, "log_consumption", c("log_gdp", "log_gdp.world", "fossil_fuel_share","fossil_fuel_share.world",
                                                                                  "industry_share", "industry_share.world", "trade_share", "trade_gns_percent.world"))
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

summary(regression_results_4$absolute_error_5Y)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#0.00691  0.33313  0.87941  2.69152  1.95955 37.19393   

mea_regression_4 <- mean_absolute_error(regression_results_4, column = "absolute_error_5Y")
mea_regression_4
# 1.89700

#Regression 5

data_table_5 <- regression_emissions(data = regression_data, "log_consumption", c("log_gdp", "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                  "log_exports_gns_percent.world", "log_imports_gns_percent.world"))

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

summary(regression_results_5$absolute_error_5Y)
#Min.  1st Qu.   Median Mean    3rd Qu. Max.      NA's
#0.03   0.26     0.57   392.55  1.44    37917.66  2   

mea_regression_5 <- mean_absolute_error(regression_results_5, column = "absolute_error_5Y")
mea_regression_5
# 3.024377

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
                                              "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world", 
                                              "log_imports_gns_percent.world"), predicted_data = data_pred, logged_emissions = TRUE,
                                              fixed_effect = TRUE)

summary(regression_results_6$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.000967 0.111415 0.462469 0.937628 1.048739 7.102272        2 

mea_regression_6 <- mean_absolute_error(regression_results_6)
mea_regression_6
# 1.895

regression_results_6_5Y <- prediction_regression(data = data_base, emissions = "log_territorial", explanatory_variables = c("log_gdp", 
                                                                                                                         "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world", 
                                                                                                                         "log_imports_gns_percent.world"), 
                                              predicted_data = data_pred_5Y , logged_emissions = TRUE,
                                              fixed_effect = TRUE)

regression_results_6 <- mean_absolute_error_5Y(data = regression_results_6_5Y)%>%
  filter (year==2020)

summary(regression_results_6$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's
#0.007696 0.116420 0.318099 0.832059 0.992751 8.539532        2  

mea_regression_6 <- mean_absolute_error(regression_results_6, column = "absolute_error_5Y")
mea_regression_6
# 1.562676

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
                                              "log_exports_gns_percent.world", "log_imports_gns_percent.world", "log_gdp.world", "log_industry_share",
                                              "log_industry_share.world", "log_emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_7 <- regression_results_7%>%
  mutate(emissions_fitted_values = exp(fitted_values+log_gdp+log_ffsh))%>%
  mutate(absolute_error = abs(emissions_fitted_values - territorial_emissions))

summary(regression_results_7$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00871  0.12188  0.42502  0.89835  0.85330 11.36495

mea_regression_7 <- mean_absolute_error(regression_results_7)
mea_regression_7
# 1.010743

regression_results_7_5Y <- prediction_regression(data = data_base, emissions = "difference_variable", explanatory_variables = c("log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                                                                "log_exports_gns_percent.world", "log_imports_gns_percent.world", "log_gdp.world", "log_industry_share",
                                                                                                                                "log_industry_share.world", "log_emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_7 <- regression_results_7_5Y%>%
  mutate(emissions_fitted_values = exp(fitted_values+log_gdp+log_ffsh))%>%
  mutate(absolute_error = abs(emissions_fitted_values - territorial_emissions))

regression_results_7 <- mean_absolute_error_5Y(data = regression_results_7)%>%
  filter (year==2020)

summary(regression_results_7$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.009496 0.143098 0.388661 0.752737 0.903281 5.732110 

mea_regression_7 <- mean_absolute_error(regression_results_7, column = "absolute_error_5Y")
mea_regression_7
# 0.8449248


regression_results_8 <- mean_absolute_error_5Y(data = regression_results_8)%>%
  filter (year==2020)

summary(regression_results_8$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.0446  0.6597  1.6451  4.1688  3.7144 63.4992       3     

mea_regression_8 <-mean_absolute_error(regression_results_8, column = "absolute_error_5Y")
mea_regression_8
# 1.658866

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
                                                                                                          "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                          "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect=TRUE)

regression_results_8 <- regression_results_8%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_8$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.00063  0.54349  1.71067  4.29618  4.42769 50.17011        3 

mea_regression_8 <- mean_absolute_error(regression_results_8)
mea_regression_8
#1.866836

regression_results_8_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                                                       "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_8 <- regression_results_8_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_8 <- mean_absolute_error_5Y(data = regression_results_8)%>%
  filter (year==2020)

summary(regression_results_8$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.0446  0.6597  1.6451  4.1688  3.7144 63.4992       3     

mea_regression_8 <-mean_absolute_error(regression_results_8, column = "absolute_error_5Y")
mea_regression_8
# 1.658866

#Regression 8bis : the same one without a fixed effect
#It is better because it makes some variables significant

regression_results_8bis <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                   "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                                                   "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect=FALSE)

regression_results_8bis <- regression_results_8bis%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_8bis$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01991  0.33057  1.21740  3.79091  3.88369 51.78078

mea_regression_8bis <- mean_absolute_error(regression_results_8bis)
mea_regression_8bis
# 1.768472

regression_results_8_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world", "imports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                                                       "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_8 <- regression_results_8_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_8 <- mean_absolute_error_5Y(data = regression_results_8)%>%
  filter (year==2020)

summary(regression_results_8$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02873  0.39408  1.09510  3.43863  3.84431 43.23503     

mea_regression_8 <-mean_absolute_error(regression_results_8, column = "absolute_error_5Y")
mea_regression_8
# 1.514736

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_9 <- mean_absolute_error_5Y(data = regression_results_9)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_10 <- mean_absolute_error_5Y(data = regression_results_10)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_11 <- mean_absolute_error_5Y(data = regression_results_11)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_12 <- mean_absolute_error_5Y(data = regression_results_12)%>%
  filter (year==2019)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_13 <- mean_absolute_error_5Y(data = regression_results_13)%>%
  filter (year==2019)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_14 <- mean_absolute_error_5Y(data = regression_results_14)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_15 <- mean_absolute_error_5Y(data = regression_results_15)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_16 <- mean_absolute_error_5Y(data = regression_results_16)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_17 <- mean_absolute_error_5Y(data = regression_results_17)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_18 <- mean_absolute_error_5Y(data = regression_results_18)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_19 <- mean_absolute_error_5Y(data = regression_results_19)%>%
  filter (year==2020)

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
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_20 <- mean_absolute_error_5Y(data = regression_results_20)%>%
  filter (year==2020)

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
                      predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_21 <- regression_results_21%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_21$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#  0.0016   0.1781   0.5331   5.2364   1.4414 334.4257        2 

mea_regression_21 <-mean_absolute_error(regression_results_21)
mea_regression_21
# 3.342385

regression_results_21_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                    "log_gdp", "fossil_fuel_share"), 
                                               predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_21 <- regression_results_21_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))


regression_results_21 <- mean_absolute_error_5Y(data = regression_results_21)%>%
  filter (year==2020)

summary(regression_results_21$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
#  0.00916   0.19862   0.56381   2.76257   1.14993 107.98853         2 

mea_regression_21 <-mean_absolute_error(regression_results_21, column = "absolute_error_5Y")
mea_regression_21
# 1.609775

################################################################################################################################
#In this part the table of absolute errors per country and the summary table for regressions are established

absolute_error_table <- function(first_regression = 8, last_regression = 21){
  table =countries
  for (i in first_regression:last_regression){
    reg_name <- paste0("regression_results_", i)
    regression_table <- get(reg_name)
    table <- merge(x = table, y = regression_table[, c("country", "absolute_error_5Y")], by = "country", all.x = TRUE) %>%
      rename(!!paste0("regression_error_", i) := absolute_error_5Y)
  }
  table <- table%>%
    select(-LABEL.EN, -LABEL.FR)
  return(table)
}

error_table <- absolute_error_table()%>%
  rowwise() %>%
  mutate(min_error = min(c_across(starts_with("regression_error_")), na.rm = TRUE))%>%
  mutate(min_error = na_if(min_error, Inf))%>%
  mutate(best_regression = ifelse(is.na(min_error), NA, names(.)[1+which.min(c_across(starts_with("regression_error_")))])
  ) %>% 
  ungroup()

specific_emissions_data <- regression_data%>%
  filter(year == 2020)%>%
  select(country, territorial_emissions.share)

error_table <- merge(x=error_table, y= specific_emissions_data, by="country")

mean_min_error = mean_absolute_error(data = error_table, column = "min_error")  
mean_min_error
# 0.157716

summary(error_table$min_error)
#Min.    1st Qu. Median    Mean    3rd Qu.    Max.    NA's 
#0.00785 0.06189 0.16892   0.36474 0.33810    3.85329 73

write.xlsx(error_table, "error_table.xlsx")
