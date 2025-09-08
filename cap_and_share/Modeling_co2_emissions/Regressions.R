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
library(stargazer)
#############################################################################################################
#Loading data and treating some of it

regression_data <- read.csv("regression_data.csv")
countries <- read.csv("countries_table.csv")

#Adding one column as the share of territorial emissions (not per capita) over all countries
regression_data <- regression_data%>%
  group_by(year)%>%
  mutate( territorial_emissions.country = territorial_emissions * population )%>%
  mutate(territorial_emissions.share =  territorial_emissions.country/sum(territorial_emissions.country, na.rm = TRUE))%>%
  mutate(population.share = population/sum(population, na.rm = TRUE))%>%
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
  mutate(log_gdp_square = log_gdp^2)%>%
  mutate(transfer_emissions = consumption_emissions - territorial_emissions)
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
    select(c("year", "country", "population.share" ,"population", emissions, explanatory_variables))
    
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
           residuals = .data[[emissions]] - fitted_values,
    absolute_error = abs(residuals)) %>%
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
  mea = sum(data[[column]] * data$population.share, na.rm = TRUE)/sum(data$population.share, na.rm = TRUE)
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

write.csv(test_data, "emissions_summary_2022.csv")
#############################################################################################################
#This part prepares Annex A for the paper
regression_info <- regression_data%>%
  filter(year==2020)%>%
  mutate(predicted_variable = NA)%>%
  select(-X, -year,-country)

regression_info <- as.data.frame(t(regression_info))%>%
  select(columns_to_delete = V1)

regression_info[,"columns_to_delete"]=0
regression_info["fixed_effect","columns_to_delete"]=0

for (i in (1:26)){
  regression_info[,paste("regression ",i)] <- 0
}

fill_regression_info <- function(fixed_effect = FALSE, explanatory_variables, regression_number, emissions){
  data = regression_info
  if (fixed_effect == TRUE){
    data["fixed_effect",paste("regression ",regression_number)]=1
  }
  for (var in explanatory_variables){
    data[var,paste("regression ",regression_number)]=1
  }
  data["predicted_variable",paste("regression ",regression_number)]=emissions
  return(data)
}


#This part can be run after completing the table in order to clean it from unused data
regression_info[,"columns_to_delete"]=1
for (i in (1:26)){
  for (var in row.names(regression_info)){
    if (regression_info[var,paste("regression ", i)]==1){
      regression_info[var,"columns_to_delete"]=0
    }
  }
} 

temp_table <- regression_info["predicted_variable",]%>%
  select(-columns_to_delete)

regression_info <- regression_info%>%
  filter(columns_to_delete == 0)%>%
  select(-columns_to_delete)

regression_info <- rbind(temp_table,regression_info)

regression_info <- rownames_to_column(regression_info, var ="independent variable")

regression_info <- read.xlsx("regression_info.xlsx")
write.xlsx(regression_info, "regression_info.xlsx")
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
regression_info <- fill_regression_info(emissions = "territorial_emissions", 
                     explanatory_variables = c("emissions_pc.world", "fossil_fuel_share", "gdp", "gdp.world"),
                     regression_number = 1, fixed_effect = TRUE)

test_table <- auto_correlation_test(test_table)

test_table <- test_table%>%
  select(country, year, residuals, dw_stat, ljung_pval)

summary(regression_results_1$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max.      NA's 
# 0.001127  0.113349  0.617200  2.064845  1.773080 28.312253         2 

mea_regression_1 <- mean_absolute_error(regression_results_1)
mea_regression_1
#0.910116

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
# 0.7421462

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



regression_info <- fill_regression_info(emissions = "log_consumption", explanatory_variables = c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                                                              "log_imports_gns_percent", "log_exports_gns_percent.world", "log_exports_gns_percent")

                     ,regression_number = 2 , fixed_effect = FALSE)

summary(regression_results_2$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000135  0.132127  0.463264  1.373232  1.166617 22.407738

mea_regression_2 <- mean_absolute_error(regression_results_2)
mea_regression_2
#0.3538679

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
# -0.10%

summary(regression_results_2$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.008357  0.171313  0.333318  1.202489  1.015812 26.642325  

mea_regression_2 <- mean_absolute_error(regression_results_2, column = "absolute_error_5Y")
mea_regression_2
# 0.3397655

#Regression 3 : Regressions from Liddle(2018)
#I would need to add fossil_fuel_share.world when I have the info
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


regression_info <- fill_regression_info(emissions = "log_territorial", explanatory_variables =c("log_gdp", 
                                                                                                "log_gdp.world", "fossil_fuel_share",
                                                                                                "industry_share", "industry_share.world",
                                                                                                "trade_share", "trade_gns_percent.world"),
                                        regression_number = 3, fixed_effect = FALSE)
summary(regression_results_3$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.001045  0.119133  0.383311  1.007138  1.055606 28.945666 

mea_regression_3 <- mean_absolute_error(regression_results_3)
mea_regression_3
#0.9410281

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
# 0.8152554

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
regression_info <- fill_regression_info( emissions = "log_consumption", explanatory_variables =c("log_gdp", 
                                                                                                 "log_gdp.world", "fossil_fuel_share", "industry_share", "industry_share.world",
                                                                                                 "trade_gns_percent.world", "trade_share"),
                                        regression_number = 4, fixed_effect = FALSE)


summary(regression_results_4$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.004024  0.343634  0.906803  2.140988  2.381780 21.143073  

mea_regression_4 <- mean_absolute_error(regression_results_4)
mea_regression_4
# 1.124781

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
# -10.92552%

summary(regression_results_4$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.007366  0.307551  0.849311  2.191833  2.014554 21.740537   

mea_regression_4 <- mean_absolute_error(regression_results_4, column = "absolute_error_5Y")
mea_regression_4
# 0.9097697

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
                                              "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world" 
                                              ), predicted_data = data_pred, logged_emissions = TRUE, fixed_effect = TRUE)
regression_info <- fill_regression_info(emissions = "log_consumption", explanatory_variables = c("log_gdp", 
                                                                                                 "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world" 
                                                                                                 ),
                                        regression_number = 5, fixed_effect = TRUE)


summary(regression_results_5$absolute_error)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.01699   0.25141   0.77317   3.15303   1.79245 132.77760         2 


mea_regression_5 <- mean_absolute_error(regression_results_5)
mea_regression_5
#1.26

regression_results_5_5Y <- prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_gdp", 
                                                                                                                         "log_ffsh", "log_exports_gns_percent", "log_imports_gns_percent", "log_exports_gns_percent.world"
                                                                                                                         ), 
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
# 9.37%

summary(regression_results_5$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0249   0.2336   0.6692   7.7540   1.7014 592.4913

mea_regression_5 <- mean_absolute_error(regression_results_5, column = "absolute_error_5Y")
mea_regression_5
# 1.12

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
                                              "log_ffsh", "log_imports_gns_percent", "log_exports_gns_percent", 
                                              "log_imports_gns_percent.world"), predicted_data = data_pred, logged_emissions = TRUE,
                                              fixed_effect = FALSE)
regression_info <- fill_regression_info(emissions = "log_territorial", explanatory_variables = c("log_gdp", 
                                                                                                 "log_ffsh", "log_exports_gns_percent", "log_exports_gns_percent", 
                                                                                                 "log_imports_gns_percent.world"),
                                        regression_number = 6, fixed_effect = FALSE)


summary(regression_results_6$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.001464  0.187781  0.619084  1.397154  1.683666 10.625201

mea_regression_6 <- mean_absolute_error(regression_results_6)
mea_regression_6
# 0.7587711

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
# 0.540

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

regression_info <- fill_regression_info(emissions = "difference_variable", explanatory_variables = c("log_exports_gns_percent", "log_imports_gns_percent", 
                                                                                                     "log_exports_gns_percent.world", "log_gdp.world", "log_industry_share",
                                                                                                     "log_industry_share.world", "log_emissions_pc.world"),
                                        regression_number = 7, fixed_effect = FALSE)


regression_results_7 <- regression_results_7%>%
  mutate(emissions_fitted_values = exp(fitted_values+log_gdp+log_ffsh))%>%
  mutate(absolute_error = abs(emissions_fitted_values - territorial_emissions))

summary(regression_results_7$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.004959  0.140062  0.411449  0.908262  0.930567 11.504264

mea_regression_7 <- mean_absolute_error(regression_results_7)
mea_regression_7
# 0.6075558

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
# 0.518

#Regression 8 : attempt to predict consumption_emissions with this formula, without a fixed effect
#cons_emissions = territorial *(1+ (export_percent_emissions - import_emissions)/100)
#This regression with a fixed effect is run in regression 30
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
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect=FALSE)
regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                           "exports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                           "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                        regression_number = 8, fixed_effect = FALSE)

regression_results_8 <- regression_results_8%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_8$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01349  0.33015  1.17965  3.47261  3.65821 45.24033 

mea_regression_8 <- mean_absolute_error(regression_results_8)
mea_regression_8
# 1.234752

regression_results_8_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world", "gdp.world", "industry_share",
                                                                                                                                       "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_8 <- regression_results_8_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions-emissions_fitted_values,
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
# -7.65%

summary(regression_results_8$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02817  0.37086  0.98504  2.93163  3.33147 38.49574      

mea_regression_8 <-mean_absolute_error(regression_results_8, column = "absolute_error_5Y")
mea_regression_8
# 0.9674722

#Regression 9 : same idea with less explanatory variables because nothing is significant then
#cons_emissions = territorial *(1+ (export_percent_emissions - import_emissions)/100)
#Please run regression 8 before to set the data tables, prediction tables correctly
#in the explanatory variables, there is not imports_gns_percent_world because it should be equal to exports_gns_percent.world in theory so no sense to keep it

regression_results_9 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", 
                                                                                                                                   "exports_gns_percent.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", 
                                                                                                           "exports_gns_percent.world"), 
                                        regression_number = 9, fixed_effect = TRUE)

regression_results_9 <- regression_results_9%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_9$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000234  0.099830  0.281868  0.895189  1.015069 10.967929
mea_regression_9 <- mean_absolute_error(regression_results_9)
mea_regression_9
#0.2243992

regression_results_9_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_9 <- regression_results_9_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# 1.97%

summary(regression_results_9$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01719  0.12830  0.32118  0.90150  0.79119 11.67934     

mea_regression_9 <-mean_absolute_error(regression_results_9, column = "absolute_error_5Y")
mea_regression_9
# 0.233

#Regression 10 : same idea, with less parameters
#Please run regression 8 before to set the data tables, prediction tables correctly

regression_results_10 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent"), 
                                                predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent"), 
                                        regression_number = 10, fixed_effect = TRUE)

regression_results_10 <- regression_results_10%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_10$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.001498 0.113748 0.292712 0.716670 0.916854 8.167147

mea_regression_10 <- mean_absolute_error(regression_results_10)
mea_regression_10
#0.281

regression_results_10_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("trade_balance_gns_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_10 <- regression_results_10_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# 2.28%

summary(regression_results_10$absolute_error_5Y)
#Min.      1st Qu.   Median  Mean     3rd Qu.     Max. 
#0.00996  0.11633  0.31109  0.75082  0.84582 10.50518     

mea_regression_10 <-mean_absolute_error(regression_results_10, column = "absolute_error_5Y")
mea_regression_10
# 0.2587937

#Regression 11 : same idea, different parameters
#Please run regression_8 at first

regression_results_11 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent"), 
                                        regression_number = 11, fixed_effect = TRUE)

regression_results_11 <- regression_results_11%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_11$absolute_error)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.000571  0.115959  0.338840  0.996417  0.885461 12.185380 

mea_regression_11 <-mean_absolute_error(regression_results_11)
mea_regression_11
#0.283

regression_results_11_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_11 <- regression_results_11_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# 2.30%

summary(regression_results_11$absolute_error_5Y)
#Min.      1st Qu.   Median  Mean     3rd Qu.     Max. 
#0.01521  0.12571  0.34823  1.03883  0.85641 14.46472    

mea_regression_11 <-mean_absolute_error(regression_results_11, column = "absolute_error_5Y")
mea_regression_11
# 0.284

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

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                           "fuel_exports_percent_gdp", "exports_gns_percent.world"), 
                                        regression_number = 12, fixed_effect = FALSE)

regression_results_12 <- regression_results_12%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_12$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0027   0.1986   0.5116   6.9557   1.3064 416.0792 
mea_regression_12 <-mean_absolute_error(regression_results_12)
mea_regression_12 
# 1.460036

regression_results_12_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                        "fuel_exports_percent_gdp", "exports_gns_percent.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_12 <- regression_results_12_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# -17.96%

summary(regression_results_12$absolute_error_5Y)
#Min.      1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02299   0.17363   0.39370   3.14734   1.18828 118.65266    

mea_regression_12 <-mean_absolute_error(regression_results_12, column = "absolute_error_5Y")
mea_regression_12
# 0.794

#Regression 13 : trying the same idea with different variables
#Please run regression 12 before

regression_results_13 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                    "fuel_exports_percent_gdp"), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                           "fuel_exports_percent_gdp"), 
                                        regression_number = 13, fixed_effect = FALSE)

regression_results_13 <- regression_results_13%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_13$absolute_error)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.00725   0.14678   0.41271   3.17694   1.17782 158.76332

mea_regression_13 <-mean_absolute_error(regression_results_13)
mea_regression_13 
#0.722

regression_results_13_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                        "fuel_exports_percent_gdp"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_13 <- regression_results_13_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# -1.66%

summary(regression_results_13$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01481  0.17267  0.41082  1.89775  0.95931 52.27609    

mea_regression_13 <-mean_absolute_error(regression_results_13, column = "absolute_error_5Y")
mea_regression_13
# 0.522

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

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                           "fuel_exports_percent_gdp", "imports_goods_percent", "exports_goods_percent"), 
                                        regression_number = 14, fixed_effect = FALSE)

regression_results_14 <- regression_results_14%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_14$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0044   0.1697   0.5383   8.7960   1.5130 414.9429 
mea_regression_14 <-mean_absolute_error(regression_results_14)
mea_regression_14 
#2.854724

regression_results_14_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent", "fuel_imports_percent_gdp",
                                                                                                                                        "fuel_exports_percent_gdp", "imports_goods_percent", "exports_goods_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_14 <- regression_results_14_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# -48.3%

summary(regression_results_14$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0200   0.2464   0.6040   6.6814   1.6814 335.1440    

mea_regression_14 <-mean_absolute_error(regression_results_14, column = "absolute_error_5Y")
mea_regression_14
# 2.289842

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

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                           "trade_balance_goods_percent"),
                                        regression_number = 15, fixed_effect = FALSE)

regression_results_15 <- regression_results_15%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_15$absolute_error)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.00143  0.09914  0.51165  1.19253  1.38442 11.44760 

mea_regression_15 <-mean_absolute_error(regression_results_15)
mea_regression_15
#0.773

regression_results_15_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                       "trade_balance_goods_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_15 <- regression_results_15_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# -6.94%

summary(regression_results_15$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01282 0.15101 0.40753 1.09394 1.24863 7.21631    

mea_regression_15 <-mean_absolute_error(regression_results_15, column = "absolute_error_5Y")
mea_regression_15
# 0.842

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

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                           "trade_balance_goods_percent"), 
                                        regression_number = 16, fixed_effect = FALSE)

regression_results_16 <- regression_results_16%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_16$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000894  0.136797  0.455350  1.127554  1.223879 10.945408
mea_regression_16 <-mean_absolute_error(regression_results_16)
mea_regression_16
#0.368

regression_results_16_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                                                       "trade_balance_goods_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_16 <- regression_results_16_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# 2.17%

summary(regression_results_16$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02104  0.13525  0.37798  0.99375  0.88055 13.17092    

mea_regression_16 <-mean_absolute_error(regression_results_16, column = "absolute_error_5Y")
mea_regression_16
# 0.322

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
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = TRUE)

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                           "trade_balance_gns_percent"), 
                                        regression_number = 17, fixed_effect = TRUE)

regression_results_17 <- regression_results_17%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_17$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.002706 0.117709 0.379811 1.078145 1.328076 8.735624 
mea_regression_17 <-mean_absolute_error(regression_results_17)
mea_regression_17
#0.352

regression_results_17_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp",
                                                                                                                                       "trade_balance_gns_percent"), 
predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_17 <- regression_results_17_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# 2.71%

summary(regression_results_17$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.007851 0.142377 0.359326 0.945904 0.958414 8.972502   

mea_regression_17 <-mean_absolute_error(regression_results_17, column = "absolute_error_5Y")
mea_regression_17
# 0.288

#Regression 18
regression_results_18 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp"
                                                                                                                                    ), 
                                               predicted_data = data_pred, logged_emissions = FALSE, fixed_effect = FALSE)

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp"), 
                                        regression_number = 18, fixed_effect = FALSE)


regression_results_18 <- regression_results_18%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_18$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.000838  0.108963  0.511789  1.077979  1.014312 10.922235  
mea_regression_18 <-mean_absolute_error(regression_results_18)
mea_regression_18
#0.396


regression_results_18_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("balance_fuel_percent_gdp"
), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_18 <- regression_results_18_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# 3.5%

summary(regression_results_18$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01126  0.16476  0.43532  0.99801  0.95957 13.24577  

mea_regression_18 <-mean_absolute_error(regression_results_18, column = "absolute_error_5Y")
mea_regression_18
# 0.38


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

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                           "log_gdp"), 
                                        regression_number = 19, fixed_effect = TRUE)

regression_results_19 <- regression_results_19%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_19$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01135  0.13008  0.45514  1.19858  1.21550 14.05315

mea_regression_19 <-mean_absolute_error(regression_results_19)
mea_regression_19
#0.398

regression_results_19_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                       "log_gdp"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_19 <- regression_results_19_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# -4.49%

summary(regression_results_19$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01392  0.19433  0.48229  1.18934  1.11470 16.20969 

mea_regression_19 <-mean_absolute_error(regression_results_19, column = "absolute_error_5Y")
mea_regression_19
# 0.377

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

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent",
                                                                                                           "log_gdp"), 
                                        regression_number = 20, fixed_effect = TRUE)

regression_results_20 <- regression_results_20%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_20$absolute_error)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.009014 0.143114 0.362848 1.000745 1.015463 8.945577 

mea_regression_20 <-mean_absolute_error(regression_results_20)
mea_regression_20
# 0.3681164

regression_results_20_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent",
                                                                                                                                       "log_gdp"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_results_20 <- regression_results_20_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# 0.02%

summary(regression_results_20$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01958 0.21034 0.48602 1.08855 1.09959 7.83040

mea_regression_20 <-mean_absolute_error(regression_results_20, column = "absolute_error_5Y")
mea_regression_20
# 0.380

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

regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                           "log_gdp", "fossil_fuel_share"), 
                                        regression_number = 21, fixed_effect = FALSE)

regression_results_21 <- regression_results_21%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_21$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02686  0.19026  0.53002  1.26143  1.41514 13.49528 

mea_regression_21 <-mean_absolute_error(regression_results_21)
mea_regression_21
# 0.644

regression_results_21_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("trade_balance_gns_percent", "balance_fuel_percent_gdp",
                                                                                                                                    "log_gdp", "fossil_fuel_share"), 
                                               predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_results_21 <- regression_results_21_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
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
# -8.27%

summary(regression_results_21$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02145  0.19203  0.45880  1.17538  1.07112 15.89085

mea_regression_21 <-mean_absolute_error(regression_results_21, column = "absolute_error_5Y")
mea_regression_21
# 0.523

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


regression_info <- fill_regression_info(emissions = "log_consumption", explanatory_variables = c("log_gdp", "log_ffsh", "log_emissions_pc.world", "log_gdp.world"), 
                                        regression_number = 22, fixed_effect = FALSE)


regression_results_22 <- mean_absolute_error_5Y(data = regression_results_22_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_22$consumption_emissions*regression_results_22$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_22$residuals*regression_results_22$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 13.33%

summary(regression_results_22$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.008395  0.275370  0.683776  1.399820  1.577859 11.469046 

mea_regression_22 <-mean_absolute_error(regression_results_22, column = "absolute_error_5Y")
mea_regression_22
# 0.863

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

regression_info <- fill_regression_info(emissions = "log_consumption", explanatory_variables = c("log_gdp", "log_emissions_pc.world", "log_gdp.world"), 
                                        regression_number = 23, fixed_effect = TRUE)

regression_results_23 <- mean_absolute_error_5Y(data = regression_results_23_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_23$consumption_emissions*regression_results_23$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_23$residuals*regression_results_23$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 3.13%

summary(regression_results_23$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02007  0.23088  0.52045  2.18899  1.73223 39.14342  

mea_regression_23 <-mean_absolute_error(regression_results_23, column = "absolute_error_5Y")
mea_regression_23
# 0.713

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

regression_info <- fill_regression_info(emissions = "log_consumption", explanatory_variables = c("log_territorial", "log_emissions_pc.world"), 
                                        regression_number = 24, fixed_effect = TRUE)

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
# 7.62%

summary(regression_results_24$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. *9
#0.01137 0.16788 0.41095 0.85195 0.99913 8.96389  

mea_regression_24 <-mean_absolute_error(regression_results_24, column = "absolute_error_5Y")
mea_regression_24
# 0.504

#Regression 25

data_table_25 <- regression_emissions(data = regression_data, "consumption_emissions", c("territorial_emissions", "consumption_emissions", "gdp", "emissions_pc.world", "trade_balance_gns.pc", "trade_balance_goods.pc", "gdp.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_25 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_25%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_25_5Y <- prediction_regression(data = data_base, emissions = "consumption_emissions", explanatory_variables = c("territorial_emissions", "gdp", "emissions_pc.world", "trade_balance_gns.pc", "trade_balance_goods.pc", "gdp.world"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_info <- fill_regression_info(emissions = "consumption_emissions", explanatory_variables = c("territorial_emissions", "gdp", "emissions_pc.world", "trade_balance_gns.pc", "trade_balance_goods.pc", "gdp.world"), 
                                        regression_number = 25, fixed_effect = TRUE)

regression_results_25 <- mean_absolute_error_5Y(data = regression_results_25_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_25$consumption_emissions*regression_results_25$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_25$residuals*regression_results_25$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -5.34%

summary(regression_results_25$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0129  0.2812  0.6892  1.9525  1.8088 23.6485 

mea_regression_25 <-mean_absolute_error(regression_results_25, column = "absolute_error_5Y")
mea_regression_25
# 0.623

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

regression_info <- fill_regression_info(emissions = "consumption_emissions", explanatory_variables = c("territorial_emissions", "emissions_pc.world"), 
                                        regression_number = 26, fixed_effect = TRUE)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_26$consumption_emissions*regression_results_26$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_26$residuals*regression_results_26$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 5.98%

summary(regression_results_26$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.009646 0.147919 0.424105 0.921362 1.111834 7.999177  

mea_regression_26 <-mean_absolute_error(regression_results_26, column = "absolute_error_5Y")
mea_regression_26
# 0.4590796

#Regression 27 : transfer emissions
data_table_27 <- regression_emissions(data = regression_data, "consumption_emissions", c("transfer_emissions", "log_gdp", "trade_balance_gns.pc"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_27 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_27%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_27_5Y <- prediction_regression(data = data_base, emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc"),
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_info <- fill_regression_info(emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc"),
                                        fixed_effect = FALSE, regression_number = 27)

regression_results_27 <- mean_absolute_error_5Y(data = regression_results_27_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_27$consumption_emissions*regression_results_27$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_27$residuals*regression_results_27$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 1.82%

summary(regression_results_27$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.02132  0.09287  0.36702  1.37075  1.16721 33.40080 

mea_regression_27 <-mean_absolute_error(regression_results_27, column = "absolute_error_5Y")
mea_regression_27
# 0.333

#Regression 28 : transfer emissions
data_table_28 <- regression_emissions(data = regression_data, "consumption_emissions", c("transfer_emissions", "log_gdp", "trade_balance_gns.pc"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_28 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_28%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_28_5Y <- prediction_regression(data = data_base, emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc"),
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_info <- fill_regression_info(emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc"),
                                        fixed_effect = TRUE, regression_number = 28)

regression_results_28 <- mean_absolute_error_5Y(data = regression_results_28_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_28$consumption_emissions*regression_results_28$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_28$residuals*regression_results_28$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 1.79%

summary(regression_results_28$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.006077 0.114505 0.270234 0.662607 0.874909 4.896843 

mea_regression_28 <-mean_absolute_error(regression_results_28, column = "absolute_error_5Y")
mea_regression_28
# 0.237

#Regression 29 : transfer emissions
data_table_29 <- regression_emissions(data = regression_data, "consumption_emissions", c("transfer_emissions", "log_gdp", "trade_balance_gns.pc", "log_gdp.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_29 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_29%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_29_5Y <- prediction_regression(data = data_base, emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc","log_gdp", "log_gdp.world"),
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_info <- fill_regression_info(emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc", "log_gdp", "log_gdp.world"),
                                        fixed_effect = TRUE, regression_number = 29)

regression_results_29 <- mean_absolute_error_5Y(data = regression_results_29_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_29$consumption_emissions*regression_results_29$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_29$residuals*regression_results_29$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 0.08%

summary(regression_results_29$absolute_error_5Y)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.01341 0.15834 0.46366 1.25502 1.24321 9.03322  

mea_regression_29 <-mean_absolute_error(regression_results_29, column = "absolute_error_5Y")
mea_regression_29
# 0.459

#Regression 30 : transfer emissions
data_table_30 <- regression_emissions(data = regression_data, "consumption_emissions", c("transfer_emissions", "log_gdp", "trade_balance_gns.pc", "log_gdp.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_30 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred_5Y <- data_table_30%>%
  filter(year <=2020)%>%
  filter(year >= 2016)

regression_results_30_5Y <- prediction_regression(data = data_base, emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc","log_gdp", "log_gdp.world"),
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = FALSE)

regression_info <- fill_regression_info(emissions = "transfer_emissions", explanatory_variables = c("trade_balance_gns.pc", "log_gdp", "log_gdp.world"),
                                        fixed_effect = FALSE, regression_number = 30)

regression_results_30 <- mean_absolute_error_5Y(data = regression_results_30_5Y)%>%
  filter (year==2020)

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_30$consumption_emissions*regression_results_30$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_30$residuals*regression_results_30$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 1.41%

summary(regression_results_30$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.008833 0.155079 0.336710 0.948432 1.136167 9.758801

mea_regression_30 <-mean_absolute_error(regression_results_30, column = "absolute_error_5Y")
mea_regression_30
# 0.360

#Regression 31 : attempt to predict consumption_emissions with this formula
#cons_emissions = territorial *(1+ (export_percent_emissions - import_emissions)/100)
#same as regression 8 but with a fixed effect

data_table_31 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                            "exports_gns_percent.world", "imports_gns_percent.world", "log_gdp.world", "industry_share",
                                                                                            "industry_share.world","territorial_emissions", "emissions_pc.world", "consumption_emissions", "trade_balance_gns_percent"))
data_base <- data_table_31 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_31%>%
  filter(year ==2020)

data_pred_5Y <- data_table_31%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_31 <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                   "exports_gns_percent.world", "log_gdp.world", "industry_share",
                                                                                                                                   "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                              predicted_data = data_pred, logged_emissions = FALSE, fixed_effect=TRUE)


regression_results_31 <- regression_results_31%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(absolute_error = abs(emissions_fitted_values - consumption_emissions))

summary(regression_results_31$absolute_error)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# 0.00886  0.29899  1.08489  3.44169  3.24087 58.16447        3  

mea_regression_31 <- mean_absolute_error(regression_results_31)
mea_regression_31
#0.9340202

regression_results_31_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                                                       "exports_gns_percent.world", "log_gdp.world", "industry_share",
                                                                                                                                       "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)
regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "fossil_fuel_share", "exports_gns_percent", "imports_gns_percent", 
                                                                                                            "exports_gns_percent.world", "log_gdp.world", "industry_share",
                                                                                                            "industry_share.world","territorial_emissions", "emissions_pc.world"), 
                                        regression_number = 31, fixed_effect = TRUE)

regression_results_31 <- regression_results_31_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions -emissions_fitted_values,
         absolute_error = abs(residuals))


regression_results_31 <- mean_absolute_error_5Y(data = regression_results_31)%>%
  filter (year==2020)

regression_results_31 <- regression_results_31%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_31$consumption_emissions*regression_results_31$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_31$residuals*regression_results_31$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -2.65%

summary(regression_results_31$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.05383  0.42391  0.99175  3.29138  2.89820 74.86278     

mea_regression_31 <-mean_absolute_error(regression_results_31, column = "absolute_error_5Y")
mea_regression_31
# 0.7514231

#Regression 32 : now we try a few regressions with pure gdp regressions

data_table_32 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("log_gdp", "gdp.world", "gdp",
                                                                                             "log_gdp.world", "consumption_emissions", "transfer_emissions",
                                                                                             "log_consumption", "log_emissions_pc.world", "emissions_pc.world", "territorial_emissions"))
data_base <- data_table_32 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_32%>%
  filter(year ==2020)

data_pred_5Y <- data_table_32%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_32_5Y <-prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("log_gdp", "log_gdp.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect=TRUE)
regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables =  c("log_gdp", "log_gdp.world"), 
                                        regression_number = 32, fixed_effect = TRUE)

regression_results_32 <- regression_results_32_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions - emissions_fitted_values,
         absolute_error = abs(residuals))


regression_results_32 <- mean_absolute_error_5Y(data = regression_results_32)%>%
  filter (year==2020)

regression_results_32 <- regression_results_32%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_32$consumption_emissions*regression_results_32$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_32$residuals*regression_results_32$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# 3.90%

summary(regression_results_32$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01405  0.20088  0.55366  1.34917  1.53198 12.72087   

mea_regression_32 <-mean_absolute_error(regression_results_32, column = "absolute_error_5Y")
mea_regression_32
# 0.6001412

#Regression 33

data_table_33 <- regression_emissions(data = regression_data, "emissions_balance_percent", c("log_gdp", "gdp.world", "gdp",
                                                                                             "log_gdp.world", "consumption_emissions", "transfer_emissions",
                                                                                             "log_consumption", "log_emissions_pc.world", "emissions_pc.world", "territorial_emissions"))
data_base <- data_table_33 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_33%>%
  filter(year ==2020)

data_pred_5Y <- data_table_33%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_33_5Y <-prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables = c("gdp", "gdp.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect=TRUE)
regression_info <- fill_regression_info(emissions = "emissions_balance_percent", explanatory_variables =  c("gdp", "gdp.world"), 
                                        regression_number = 33, fixed_effect = TRUE)

regression_results_33 <- regression_results_33_5Y%>%
  mutate(emissions_fitted_values = (fitted_values/100+1)*territorial_emissions)%>%
  mutate(residuals = consumption_emissions - emissions_fitted_values,
         absolute_error = abs(residuals))


regression_results_33 <- mean_absolute_error_5Y(data = regression_results_33)%>%
  filter (year==2020)

regression_results_33 <- regression_results_33%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_33$consumption_emissions*regression_results_33$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_33$residuals*regression_results_33$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -1.46%

summary(regression_results_33$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.003669  0.246358  0.651652  1.543453  1.644535 15.006032    

mea_regression_33 <-mean_absolute_error(regression_results_33, column = "absolute_error_5Y")
mea_regression_33
# 0.5103288

#Regression 34

data_table_34 <- regression_emissions(data = regression_data, "consumption_emissions", c("log_gdp", "gdp.world", "gdp",
                                                                                             "log_gdp.world", "consumption_emissions", "transfer_emissions",
                                                                                             "log_consumption", "log_emissions_pc.world", "emissions_pc.world", "territorial_emissions"))
data_base <- data_table_34 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_34%>%
  filter(year ==2020)

data_pred_5Y <- data_table_34%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_34_5Y <-prediction_regression(data = data_base, emissions = "consumption_emissions", explanatory_variables = c("log_gdp", "log_gdp.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect=TRUE)
regression_info <- fill_regression_info(emissions = "consumption_emissions", explanatory_variables =  c("log_gdp", "log_gdp.world"), 
                                        regression_number = 34, fixed_effect = TRUE)


regression_results_34 <- mean_absolute_error_5Y(data = regression_results_34_5Y)%>%
  filter (year==2020)

regression_results_34 <- regression_results_34%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_34$consumption_emissions*regression_results_34$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_34$residuals*regression_results_34$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -4.44%

summary(regression_results_34$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.008439  0.248538  0.883049  1.783635  2.326206 16.219430   

mea_regression_34 <-mean_absolute_error(regression_results_34, column = "absolute_error_5Y")
mea_regression_34
# 0.7848576

#Regression 35

data_table_35 <- regression_emissions(data = regression_data, "consumption_emissions", c("log_gdp", "gdp.world", "gdp",
                                                                                         "log_gdp.world", "consumption_emissions", "transfer_emissions",
                                                                                         "log_consumption", "log_emissions_pc.world", "emissions_pc.world", "territorial_emissions"))
data_base <- data_table_35 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_35%>%
  filter(year ==2020)

data_pred_5Y <- data_table_35%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_35_5Y <-prediction_regression(data = data_base, emissions = "consumption_emissions", explanatory_variables = c("gdp", "gdp.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect=TRUE)
regression_info <- fill_regression_info(emissions = "consumption_emissions", explanatory_variables =  c("gdp", "gdp.world"), 
                                        regression_number = 35, fixed_effect = TRUE)


regression_results_35 <- mean_absolute_error_5Y(data = regression_results_35_5Y)%>%
  filter (year==2020)

regression_results_35 <- regression_results_35%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_35$consumption_emissions*regression_results_35$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_35$residuals*regression_results_35$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -22.52%

summary(regression_results_35$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.008462  0.223190  0.846113  1.902501  1.981958 15.677044    

mea_regression_35 <-mean_absolute_error(regression_results_35, column = "absolute_error_5Y")
mea_regression_35
# 0.9985049

#Regression 36

data_table_36 <- regression_emissions(data = regression_data, "transfer_emissions", c("log_gdp", "gdp.world", "gdp",
                                                                                         "log_gdp.world", "consumption_emissions", "transfer_emissions",
                                                                                         "log_consumption", "log_emissions_pc.world", "emissions_pc.world", "territorial_emissions"))
data_base <- data_table_36 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_36%>%
  filter(year ==2020)

data_pred_5Y <- data_table_36%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_36_5Y <-prediction_regression(data = data_base, emissions = "transfer_emissions", explanatory_variables = c("log_gdp", "log_gdp.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect=TRUE)
regression_info <- fill_regression_info(emissions = "transfer_emissions", explanatory_variables =  c("log_gdp", "log_gdp.world"), 
                                        regression_number = 36, fixed_effect = TRUE)


regression_results_36 <- mean_absolute_error_5Y(data = regression_results_36_5Y)%>%
  filter (year==2020)

regression_results_36 <- regression_results_36%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_36$consumption_emissions*regression_results_36$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_36$residuals*regression_results_36$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -0.61%

summary(regression_results_36$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01208  0.16117  0.52650  1.37978  1.54792 14.71850    

mea_regression_36 <-mean_absolute_error(regression_results_36, column = "absolute_error_5Y")
mea_regression_36
# 0.555979

#Regression 37

data_table_37 <- regression_emissions(data = regression_data, "transfer_emissions", c("log_gdp", "gdp.world", "gdp",
                                                                                         "log_gdp.world", "consumption_emissions", "transfer_emissions",
                                                                                         "log_consumption", "log_emissions_pc.world", "emissions_pc.world", "territorial_emissions"))
data_base <- data_table_37 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_37%>%
  filter(year ==2020)

data_pred_5Y <- data_table_37%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_37_5Y <-prediction_regression(data = data_base, emissions = "transfer_emissions", explanatory_variables = c("gdp", "gdp.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect=TRUE)
regression_info <- fill_regression_info(emissions = "transfer_emissions", explanatory_variables =  c("gdp", "gdp.world"), 
                                        regression_number = 37, fixed_effect = TRUE)


regression_results_37 <- mean_absolute_error_5Y(data = regression_results_37_5Y)%>%
  filter (year==2020)

regression_results_37 <- regression_results_37%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_37$consumption_emissions*regression_results_37$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_37$residuals*regression_results_37$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -1.99%

summary(regression_results_37$absolute_error_5Y)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.01409  0.16942  0.61419  1.56692  1.66117 17.14885    

mea_regression_37 <-mean_absolute_error(regression_results_37, column = "absolute_error_5Y")
mea_regression_37
# 0.5920575

#Regression 38

data_table_38 <- regression_emissions(data = regression_data, "log_consumption", c("log_gdp", "gdp.world", "gdp",
                                                                                         "log_gdp.world", "consumption_emissions", "transfer_emissions",
                                                                                         "log_consumption", "log_emissions_pc.world", "emissions_pc.world", "territorial_emissions"))
data_base <- data_table_38 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction

data_pred <- data_table_38%>%
  filter(year ==2020)

data_pred_5Y <- data_table_38%>%
  filter(year >= 2016)%>%
  filter(year <= 2020)

regression_results_38_5Y <-prediction_regression(data = data_base, emissions = "log_consumption", explanatory_variables = c("log_gdp", "log_gdp.world"), 
                                                 predicted_data = data_pred_5Y, logged_emissions = TRUE, fixed_effect=TRUE)
regression_info <- fill_regression_info(emissions = "log_consumption", explanatory_variables = c("log_gdp", "log_gdp.world"), 
                                        regression_number = 38, fixed_effect = TRUE)


regression_results_38 <- mean_absolute_error_5Y(data = regression_results_38_5Y)%>%
  filter (year==2020)

regression_results_38 <- regression_results_38%>%
  filter(!(is.na(residuals)))

#Testing for the balance of the estimations
sum_emissions = sum(regression_results_38$consumption_emissions*regression_results_38$population)/1000000
sum_emissions
sum_residuals = sum(regression_results_38$residuals*regression_results_38$population)/1000000
sum_residuals

error_percent = (sum_residuals/sum_emissions)*100
error_percent
# -11.54%

summary(regression_results_38$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.009718  0.205923  0.876879  1.745435  2.236887 16.068886     

mea_regression_38 <-mean_absolute_error(regression_results_38, column = "absolute_error_5Y")
mea_regression_38
# 0.6366121

#write.xlsx(results_summary, "summary_regression_results.xlsx")
################################################################################################################################
#This part now prepares Annex B for the report

absolute_error_table <- function(){
  table =regression_data%>%
    filter(year == 2020)%>%
    select(country, consumption_emissions, population.share)
    for(i in c(10,11,17,20)){
    reg_name <- paste0("regression_results_", i)
    regression_table <- get(reg_name)
    table <- merge(x = table, y = regression_table[, c("country", "absolute_error_5Y")], by = "country", all.x = TRUE) %>%
      rename(!!paste0("regression_error_", i) := absolute_error_5Y)
  }
  return(table)
}

error_table <- absolute_error_table()

data_simple <- data_compare%>%
  select(country, regression_error_simple = absolute_error_5Y_simple)

error_table <- merge(x=error_table, y=data_simple, by ="country" , all = TRUE )

regression_list = c("regression_10", "regression_11", "regression_17", "regression_20", "fixed_ratio")

error_table <- error_table%>%
  rowwise%>%
  mutate(min_error = min(c_across(starts_with("regression_error_")), na.rm = TRUE))%>%
  mutate(best_regression = NA)%>%
  mutate(min_error = na_if(min_error, Inf))

write.xlsx(error_table, "Annex_B.xlsx")
annex_b <- read.xlsx("Annex_B.xlsx")

mean_min_error = mean_absolute_error(data = error_table, column = "min_error")  
mean_min_error
# 0.14391

summary(error_table$min_error)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00785 0.06962 0.19712 0.48609 0.44271 7.10272      64  

##############################################################################################
#This is a test to see how many tons of CO2 are unaccounted for in the current computations
error_table <- read.xlsx("error_table.xlsx")

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

# Créer une séquence de x pour tracer la courbe
x_curve <- seq(6, 12, length.out = 300)
y_curve <- 3.35 * x_curve^2 - 59.11 * x_curve + 230
curve_df <- data.frame(x = x_curve, y = y_curve)
print(p + geom_line(data = curve_df, aes(x = x, y = y), color = "red", size = 1))

print(p)

ggsave(filename = "emissions_ratio.png", plot = p, height = 6, width = 10)

cut_data <- regression_data %>%
  filter(year == 2020)

p <- ggplot(cut_data, aes(x = log_gdp, y = emissions_balance_percent)) +
  geom_point() +
  theme_minimal()

print(p)

ggsave(filename = "emissions_ratio_2020.png", plot = p, height = 6, width = 10)

p <- ggplot(cut_data, aes(x = trade_balance_gns_percent, y = emissions_balance_percent)) +
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

regression_prediction <- regression_data%>%
  select(country, year, territorial_emissions, consumption_emissions, population, emissions_balance_percent, log_gdp, log_gdp_square, trade_balance_gns_percent, 
         trade_balance_goods_percent, balance_fuel_percent_gdp, population.share)%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)%>%
  mutate(estimate_regression = coefficients[1] + coefficients[2]*log_gdp + coefficients[3]*log_gdp_square 
                              + coefficients[4]*trade_balance_gns_percent + coefficients[5]*trade_balance_goods_percent
                              + coefficients[6]*balance_fuel_percent_gdp)%>%
  mutate(residual_percent = emissions_balance_percent - estimate_regression)%>%
  mutate(residuals = residual_percent*territorial_emissions/100)%>%
  mutate(absolute_error = abs(residuals))%>%
  mutate(residuals.country = residuals*population)%>%
  filter(!(is.na(residual_percent)))

regression_prediction <- mean_absolute_error_5Y(data = regression_prediction)%>%
  filter(year == 2020)

pooled_table <- regression_prediction%>%
  mutate(error_percent = abs(residual_percent))%>%
  arrange(desc(error_percent))%>%
  select(country, territorial_emissions, consumption_emissions, error_percent, residuals.country, absolute_error_5Y, population.share)%>%
  rename(error_percent_pooled = error_percent,
         absolute_error_5Y_pooled = absolute_error_5Y,
         residuals.country_pooled=residuals.country )

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
#-17.96%

mea_pooled = mean_absolute_error(data = regression_evaluation, column = "absolute_error_5Y")
mea_pooled
#1.094515

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
         trade_balance_goods_percent, balance_fuel_percent_gdp, population.share)%>%
  filter(year <= 2010)%>%
  mutate(new_residuals = emissions_balance_percent - coefficients[1] - coefficients[2]*log_gdp - coefficients[3]*log_gdp_square)

regression_prediction_semi[,"number_na"] <- NA
regression_prediction_semi <- regression_prediction_semi%>%
  group_by(country)%>%
  mutate(number_na = sum(is.na(trade_balance_gns_percent) | is.na(balance_fuel_percent_gdp)))%>%
  ungroup()%>%
  filter(number_na <= 12)

regression_semi2 <- pmg(new_residuals ~ trade_balance_gns_percent + balance_fuel_percent_gdp -1, 
                        data = regression_prediction_semi, index = c("country", "year"), model = "mg")
summary(regression_semi2)
pcdtest(regression_semi2)

coeffs_2 <- as.data.frame(t(regression_semi2$indcoef))
colnames(coeffs_2) <- c("beta_gns_balance", "beta_fuel_balance")
coeffs_2 <- rownames_to_column(coeffs_2, "country")
coeffs_2[,"intercept"] <- regression_semi$coefficients[1]
coeffs_2[,"beta_log_gdp"] <- regression_semi$coefficients[2]
coeffs_2[,"beta_log_gdp_square"] <- regression_semi$coefficient[3]


#This table is the table of coefficients for the two-stage regression that will be used later
coefficients_regression_semi <- coeffs_2%>%
  select(country, intercept, beta_log_gdp, beta_log_gdp_square, beta_gns_balance, beta_fuel_balance)
row.names(coefficients_regression_semi) = coefficients_regression_semi$country

regression_prediction_semi2 <- regression_data%>%
  select(country, year, territorial_emissions, consumption_emissions, population, emissions_balance_percent, log_gdp, log_gdp_square, trade_balance_gns_percent, 
         trade_balance_goods_percent, balance_fuel_percent_gdp, population.share)%>%
  filter(year <= 2020)%>%
  filter(year >= 2016)

regression_prediction_semi2 <- merge(x=regression_prediction_semi2, y= coeffs_2, by= "country", all.y = TRUE)

regression_prediction_semi2 <- regression_prediction_semi2%>%
  mutate(new_residuals = emissions_balance_percent - coefficients[1] - coefficients[2]*log_gdp - coefficients[3]*log_gdp_square)%>%
  mutate(residuals = (new_residuals - beta_gns_balance*trade_balance_gns_percent - beta_fuel_balance*balance_fuel_percent_gdp)*territorial_emissions/100,
         absolute_error = abs(residuals),
         residuals.country = residuals*population,
         territorial_emissions.country = territorial_emissions*population,
         consumption_emissions.country = consumption_emissions*population)%>%
  filter(!(is.na(residuals)))

regression_prediction_semi2 <- mean_absolute_error_5Y(data = regression_prediction_semi2)%>%
  filter(year == 2020)


summary(regression_prediction_semi2$absolute_error_5Y)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.008828  0.169056  0.486241  1.449215  1.849973 12.698607 

mea_semi2 = mean_absolute_error(data = regression_prediction_semi2, column = "absolute_error_5Y")
mea_semi2
#0.8626621

sum_residuals = sum(regression_prediction_semi2$residuals.country)
sum_residuals
# -3191460834
sum_territorial = sum(regression_prediction_semi2$territorial_emissions.country)
sum_territorial
sum_consumption = sum(regression_prediction_semi2$consumption_emissions.country)
sum_consumption

error_percent = sum_residuals/sum_territorial*100
error_percent
# -9.87%

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

#This table establishes a table of coefficients for this regression
coefficients_regression_simple <- data_estimate_ratio%>%
  rename(intercept = mean_ratio)

coefficients_regression_simple[,"beta_log_gdp"] <- 0
coefficients_regression_simple[,"beta_log_gdp_square"] <- 0
coefficients_regression_simple[,"beta_gns_balance"] <- 0
coefficients_regression_simple[,"beta_fuel_balance"] <- 0
row.names(coefficients_regression_simple) = coefficients_regression_simple$country

data_prediction <- regression_data%>%
  select(country, year, territorial_emissions, consumption_emissions, emissions_balance_percent, population.share, population)%>%
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
# 0.308483

sum_residuals = sum(data_prediction$residuals.country)
sum_residuals
#959651439

sum_territorial = sum(data_prediction$territorial_emissions.country)
sum_territorial
# 32716968919

error_percent = sum_residuals/sum_territorial*100
error_percent
#2.93%

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
  mutate(best_regression = case_when(
    is.na(error_percent_simple) ~ "estimate_semi",
    is.na(error_percent_semi) ~ "estimate_simple",
    error_percent_simple <= error_percent_semi ~ "estimate_simple",
    TRUE ~ "estimate_semi"
  ))%>%
  arrange(desc(min_error_percent))

sum_residuals = sum(data_summary$best_residuals.country)
sum_residuals
#771335955

total_balance = sum_residuals/sum_consumption*100
total_balance
#2.39%

write.csv(data_summary, "results_two_regressions.csv")
error_table <- read.xlsx("error_table.xlsx")

############################################################################################
# This part establishes the table of matching for countries

countries_geo = read.csv("WPP_regions_country_list.csv")
#There are 20 regions numbered 1 to 20

countries_geo <- countries_geo%>%
  filter(countrycode %in% countries$country)%>%
  select(country = countrycode,
         WPP_region_name,
         region = WPP_region_number)

regression_data_match <- merge( x= regression_data, y= countries_geo, by ="country")

matching_table <- regression_data_match%>%
  filter(year == 2020)%>%
  select(country, territorial_emissions, consumption_emissions, gdp, population)

matching_table <- merge( x= countries_geo, y= matching_table, by = "country")

data_summary <- data_summary%>%
  select(-territorial_emissions, -consumption_emissions)

matching_table <- merge(x=matching_table, y= data_summary, by = "country", all.x = TRUE)

row.names(matching_table)=matching_table$country

#This function pairs countries without regressions with similar countries with regressions
match_missing_countries <- function(data = matching_table){
  data2 <- data
  data2[,"closest_country"] <- NA
  for (c in data$country){
    if (is.na(data[c,"best_regression"]) & !(is.na(data[c,"gdp"])|is.na(data[c,"population"]))){
      restricted_data <- data%>%
        filter(!(is_na(gdp)|is_na(population)|is_na(best_regression)))%>%
        filter(region == data[c,"region"])%>%
        filter(population <= 2*data[c,"population"])%>%
        filter(population >= 0.5*data[c,"population"])%>%
        filter(country != c)
    if (dim(restricted_data)[1] > 1){
      restricted_data <- restricted_data%>%
        mutate(gdp_diff = abs(gdp - data[c,"gdp"]))
      data2[c,"closest_country"] <- restricted_data[which.min(restricted_data$gdp_diff), "country"]
    }
    }
    else if (!(is.na(data[c,"best_regression"]))){ data2[c,"closest_country"] <- c}
  }
  return(data2)
}

matched_table <- match_missing_countries(data = matching_table)


########################################################################################
#Let us apply this for annex B

matching_table <- merge(x=matching_table, y=annex_b, by ="country")

row.names(matching_table)=matching_table$country

matched_table <- match_missing_countries(data = matching_table)
#######################################################################################################################
#Once the table matching countries is built and almost complete, let us build the table with regression coefficients

sample_table <- matched_table%>%
  select(country, best_regression, closest_country)%>%
  mutate(intercept = NA,
         beta_log_gdp = NA,
         beta_log_gdp_square = NA,
         beta_gns_balance = NA,
         beta_fuel_balance = NA)

assess_coefficients_regression <- function(data = sample_table){
  for (c in data$country){
    if(!(is.na(data[c,"best_regression"]))& data[c,"best_regression"]=="estimate_semi"){
      data[c,"intercept"] = coefficients_regression_semi[c,"intercept"]
      data[c,"beta_log_gdp"] = coefficients_regression_semi[c,"beta_log_gdp"]
      data[c,"beta_log_gdp_square"] = coefficients_regression_semi[c,"beta_log_gdp_square"]
      data[c,"beta_gns_balance"] = coefficients_regression_semi[c,"beta_gns_balance"]
      data[c,"beta_fuel_balance"] = coefficients_regression_semi[c,"beta_fuel_balance"]
    }
    else if( !(is.na(data[c,"best_regression"]))& data[c,"best_regression"]=="estimate_simple"){
      data[c,"intercept"] = coefficients_regression_simple[c,"intercept"]
      data[c,"beta_log_gdp"] = coefficients_regression_simple[c,"beta_log_gdp"]
      data[c,"beta_log_gdp_square"] = coefficients_regression_simple[c,"beta_log_gdp_square"]
      data[c,"beta_gns_balance"] = coefficients_regression_simple[c,"beta_gns_balance"]
      data[c,"beta_fuel_balance"] = coefficients_regression_simple[c,"beta_fuel_balance"]
    }
    data_to_fill <- data%>%
      filter(is.na(best_regression))%>%
      filter(!(is.na(closest_country)))%>%
      filter(is.na(intercept))
    i=0
    while((sum(is.na(data_to_fill$intercept)) >0)&i<10){
      for(c in data_to_fill$country){
          c2 = data[c,"closest_country"]
          data[c,"intercept"] = data[c2,"intercept"]
          data[c,"beta_log_gdp"] = data[c2,"beta_log_gdp"]
          data[c,"beta_log_gdp_square"] = data[c2,"beta_log_gdp_square"]
          data[c,"beta_gns_balance"] = data[c2,"beta_gns_balance"]
          data[c,"beta_fuel_balance"] = data[c2,"beta_fuel_balance"]
          i=i+1
      }
      data_to_fill <- data%>%
        filter(is.na(best_regression))%>%
        filter(!(is.na(closest_country)))%>%
        filter(is.na(intercept))
    }
  }
  return(data)
}

full_regression_coefficients <- assess_coefficients_regression(data = sample_table)
write.csv(full_regression_coefficients, "full_table_coefficients.csv")

uncovered_countries <- full_regression_coefficients%>%
  filter(is.na(intercept))

matching_table_2 <- regression_data_match%>%
  filter(year == 2020)%>%
  select(country, territorial_emissions, consumption_emissions, gdp, population)

uncovered_countries <- merge(x=matching_table_2, y=uncovered_countries, all.y = TRUE, by = "country")

small_uncovered_countries <- uncovered_countries%>%
  filter(population < 2000000)


###########################################################################################################
# In this part the fully-pooled regression is pushed to the end in order to see if it helps with estimates
#Conclusion : the pooled regression is rarely better than the simple mean ratio

data_summary_pooled <- merge( x= data_compare, y= pooled_table, by = "country", all = TRUE)%>%
  mutate(min_error_percent = case_when(
    is.na(error_percent_simple) ~ error_percent_pooled,
    is.na(error_percent_pooled) ~ error_percent_simple,
    error_percent_simple <= error_percent_pooled ~ error_percent_simple,
    TRUE ~ error_percent_pooled
  ))%>%
  mutate(best_residuals.country = case_when(
    is.na(error_percent_simple) ~ residuals.country_pooled,
    is.na(error_percent_pooled) ~ residuals.country_simple,
    error_percent_simple <= error_percent_pooled ~ residuals.country_simple,
    TRUE ~ residuals.country_pooled
  ))%>%
  mutate(best_regression = case_when(
    is.na(error_percent_simple) ~ "estimate_pooled",
    is.na(error_percent_pooled) ~ "estimate_simple",
    error_percent_simple <= error_percent_pooled ~ "estimate_simple",
    TRUE ~ "estimate_pooled"
  ))%>%
  arrange(desc(min_error_percent))

sum_residuals = sum(data_summary_pooled$best_residuals.country)
sum_residuals
#869838086

total_balance = sum_residuals/sum_consumption*100
total_balance
#2.7%

#######################################################################################
#Let us test the following formula cons__it = gdp_square_it*a + gdp_it*b + c_it
regression_data <- as.data.frame(regression_data)
regression_data <- regression_data%>%
  mutate(ID = paste(country, year, sep = "-"))
rownames(regression_data) <- regression_data$ID



data_treatment <- function(data=regression_data){
  data[,"delta_gdp"]=NA
  data[,"sum_gdp"]=NA
  data[,"delta_ratio"]=NA
  for(c in countries$country){
    for(y in (1991:2023)){
      id = paste(c,y,sep="-")
      id1 = paste(c,y-1, sep="-")
      data[id,"delta_gdp"]=data[id,"log_gdp"]-data[id1,"log_gdp"]
      data[id,"sum_gdp"]=data[id,"log_gdp"]+data[id1,"log_gdp"]
      data[id,"delta_ratio"]=data[id,"emissions_balance_percent"]-data[id1,"emissions_balance_percent"]
    }
  }
  return(data)
}
regression_data[,"delta_gdp"]=NA
regression_data[,"sum_gdp"]=NA
regression_data[,"delta_ratio"]=NA
regression_data[,"delta_gdp_square"]=NA



for(c in countries$country){
  for(y in (1991:2023)){
    id = paste(c,y,sep="-")
    id1 = paste(c,y-1, sep="-")
    regression_data[id,"delta_gdp"]=regression_data[id,"log_gdp"]-regression_data[id1,"log_gdp"]
    regression_data[id,"sum_gdp"]=regression_data[id,"log_gdp"]+regression_data[id1,"log_gdp"]
    regression_data[id,"delta_ratio"]=regression_data[id,"emissions_balance_percent"]-regression_data[id1,"emissions_balance_percent"]
    regression_data[id,"delta_gdp_square"]=regression_data[id,"log_gdp_square"]-regression_data[id1,"log_gdp_square"]
  }
}


p <- ggplot(regression_data, aes(x = growth_rate, y = sum_gdp)) +
  geom_point() +
  theme_minimal()
print(p)

regression_28_data <- regression_data%>%
  filter(year <= 2010)

regression_28 <- lm(delta_ratio~delta_gdp+delta_gdp_square-1, data=regression_28_data)
summary(regression_28)

stargazer(regression_28, type = "html", out = "figure_6.html")

regression_28_data <- regression_28_data%>%
  mutate(mean_fixed_effect=NA)%>%
  mutate(fitted_values = 3.146*log_gdp_square -55.595*log_gdp)%>%
  group_by(country)%>%
  mutate(mean_fixed_effect = mean(emissions_balance_percent - fitted_values))%>%
  ungroup()%>%
  filter(year==2010)

regression_28_data <- as.data.frame(regression_28_data)
rownames(regression_28_data) = regression_28_data$country
summary(regression_28_data$mean_fixed_effect)

regression_28_prediction <- regression_data%>%
  filter(year <= 2020,
         year >= 2016)%>%
  mutate(predicted_ratio = 3.146*log_gdp_square -55.595*log_gdp + regression_28_data[country, "mean_fixed_effect"],
         residuals = (emissions_balance_percent - predicted_ratio)*territorial_emissions/100,
         absolute_error = abs(residuals))
summary(regression_28_prediction$residuals)

regression_28_prediction <- mean_absolute_error_5Y(data = regression_28_prediction)%>%
  filter(year == 2020)

mea = mean_absolute_error(data = regression_28_prediction, column="absolute_error_5Y")
mea

summary(regression_28_prediction$absolute_error_5Y)

regression_28_table <- regression_28_prediction%>%
  mutate(residuals.country_28 = residuals*population,
         error_percent_28 = absolute_error_5Y/consumption_emissions*100
         )%>%
  rename(absolute_error_5Y_28 = absolute_error_5Y)%>%
  select(country, error_percent_28, residuals.country_28, absolute_error_5Y_28)%>%
  filter(!(is.na(absolute_error_5Y_28)))

summary(regression_28_table$residuals.country_28)
sum_residuals = sum(regression_28_table$residuals.country_28)
sum_residuals
#43794226

total_balance = sum_residuals/sum_consumption*100
total_balance
#1.34%

#########################################################################################
#Now let us merge the two regressions : fix ratio and regression_28

data_summary_28 <- merge( x= data_compare, y= regression_28_table, by = "country", all = TRUE)%>%
  mutate(min_error_percent = case_when(
    is.na(error_percent_simple) ~ error_percent_28,
    is.na(error_percent_28) ~ error_percent_simple,
    error_percent_simple <= error_percent_28 ~ error_percent_simple,
    TRUE ~ error_percent_28
  ))%>%
  mutate(best_residuals.country = case_when(
    is.na(error_percent_simple) ~ residuals.country_28,
    is.na(error_percent_28) ~ residuals.country_simple,
    error_percent_simple <= error_percent_28 ~ residuals.country_simple,
    TRUE ~ residuals.country_28
  ))%>%
  mutate(best_regression = case_when(
    is.na(error_percent_simple) ~ "estimate_28",
    is.na(error_percent_28) ~ "estimate_simple",
    error_percent_simple <= error_percent_28 ~ "estimate_simple",
    TRUE ~ "estimate_28"
  ))%>%
  arrange(desc(min_error_percent))

sum_residuals = sum(data_summary_28$best_residuals.country)
sum_residuals
#533291723

total_balance = sum_residuals/sum_consumption*100
total_balance
#1.7%

write.csv(data_summary_28, "results_two_regressions_28.csv")
error_table <- read.xlsx("error_table.xlsx")

##################################################################################################
#Matching fixed ratio and regression 28 with missing countries

matching_table <- regression_data_match%>%
  filter(year == 2020)%>%
  select(country, territorial_emissions, consumption_emissions, gdp, population)

matching_table <- merge( x= countries_geo, y= matching_table, by = "country")

annex_b <- annex_b%>%
  select(-consumption_emissions)

matching_table_annex_b <- merge(x=matching_table, y=annex_b, by="country", all.x=TRUE)

data_summary_28 <- data_summary_28%>%
  select(-territorial_emissions, -consumption_emissions)

matching_table_28 <- merge(x=matching_table, y= data_summary_28, by = "country", all.x = TRUE)

row.names(matching_table_28)=matching_table_28$country
row.names(matching_table_annex_b)=matching_table_annex_b$country

#This function pairs countries without regressions with similar countries with regressions
match_missing_countries <- function(data = matching_table_28){
  data2 <- data
  data2[,"closest_country"] <- NA
  for (c in data$country){
    if (is.na(data[c,"best_regression"]) & !(is.na(data[c,"gdp"]))){
      restricted_data <- data%>%
        filter(!(is.na(gdp)),
               !(is.na(best_regression)),
               region == data[c,"region"],
               country!=c)
      if(data[c,"region"]==12){
        restricted_data <- data%>%
          filter(!(is.na(gdp)),
                 !(is.na(best_regression)),
                 region %in% c(12,14),
                 country!=c)
      }
      if (dim(restricted_data)[1] > 0){
        restricted_data <- restricted_data%>%
          mutate(gdp_diff = abs(gdp - data[c,"gdp"]))
        data2[c,"closest_country"] <- restricted_data[which.min(restricted_data$gdp_diff), "country"]
      }
    }
    else if (!(is.na(data[c,"best_regression"]))){ data2[c,"closest_country"] <- c}
  }
  return(data2)
}
matched_annex_b <- match_missing_countries(data = matching_table_annex_b)
write.xlsx(matched_annex_b, "matched_annex_b.xlsx")
matched_table_28.1 <- match_missing_countries(data = matching_table_28)

sample_table_28 <- matched_table_28.1%>%
  select(country, best_regression, closest_country)%>%
  mutate(fixed_effect = NA,
         beta_log_gdp = NA,
         beta_log_gdp_square = NA,
         )

assess_coefficients_regression <- function(data = sample_table_28){
  data2 <- data
  for (c in data$country){
    if(!(is.na(data[c,"best_regression"]))& data[c,"best_regression"]=="estimate_28"){
      data2[c,"fixed_effect"] = regression_28_data[c,"mean_fixed_effect"]
      data2[c,"beta_log_gdp"] = regression_28$coefficients[1]
      data2[c,"beta_log_gdp_square"] = regression_28$coefficients[2]
    }
    else if( !(is.na(data[c,"best_regression"]))& data[c,"best_regression"]=="estimate_simple"){
      data2[c,"fixed_effect"] = coefficients_regression_simple[c,"intercept"]
      data2[c,"beta_log_gdp"] = coefficients_regression_simple[c,"beta_log_gdp"]
      data2[c,"beta_log_gdp_square"] = coefficients_regression_simple[c,"beta_log_gdp_square"]
    }
    data_to_fill <- data2%>%
      filter(is.na(best_regression))%>%
      filter(!(is.na(closest_country)))%>%
      filter(is.na(fixed_effect))
      for(c in data_to_fill$country){
        c2 = data[c,"closest_country"]
        data2[c,"fixed_effect"] = data2[c2,"fixed_effect"]
        data2[c,"beta_log_gdp"] = data2[c2,"beta_log_gdp"]
        data2[c,"beta_log_gdp_square"] = data2[c2,"beta_log_gdp_square"]
      }
  }
  return(data2)
}

full_regression_coefficients_28 <- assess_coefficients_regression(data = sample_table_28)
write.csv(full_regression_coefficients, "full_table_coefficients.csv")

uncovered_countries_data <- matching_table_28%>%
  filter(country %in% uncovered_countries$country)

matching_table_2 <- regression_data_match%>%
  filter(year == 2020)%>%
  select(country, territorial_emissions, consumption_emissions, gdp, population)

uncovered_countries <- merge(x=matching_table_2, y=uncovered_countries, all.y = TRUE, by = "country")

#################################################################################################
# Ok now let us make the final table of coefficients that would be fed to NICE 2020


regression_29 <- lm(delta_ratio~delta_gdp+delta_gdp_square-1, data=regression_data)
summary(regression_29)

regression_29_data <- regression_data%>%
  mutate(mean_fixed_effect=NA)%>%
  mutate(mean_ratio=NA)%>%
  mutate(fitted_values = regression_29$coefficients[2]*log_gdp_square+ regression_29$coefficients[1]*log_gdp)%>%
  group_by(country)%>%
  mutate(mean_fixed_effect = mean(emissions_balance_percent - fitted_values, na.rm=TRUE))%>%
  mutate(mean_ratio = mean(emissions_balance_percent, na.rm=TRUE))%>%
  ungroup()%>%
  filter(year==2010)

regression_29_data <- as.data.frame(regression_29_data)
rownames(regression_29_data) = regression_29_data$country
summary(regression_29_data$mean_fixed_effect)

full_regression_coefficients_29 <- full_regression_coefficients_28%>%
  mutate(fixed_effect = NA,
         beta_log_gdp = NA,
         beta_log_gdp_square = NA
         )
for (c in full_regression_coefficients_29$country){
  if (!(is.na(full_regression_coefficients_29[c,"best_regression"]))){
    if(full_regression_coefficients_29[c,"best_regression"]== "estimate_28"){
      full_regression_coefficients_29[c,"fixed_effect"]=regression_29_data[c,"mean_fixed_effect"]
      full_regression_coefficients_29[c,"beta_log_gdp"]=regression_29$coefficients[1]
      full_regression_coefficients_29[c,"beta_log_gdp_square"]=regression_29$coefficients[2]
    }
    if(full_regression_coefficients_29[c,"best_regression"]== "estimate_simple"){
      full_regression_coefficients_29[c,"fixed_effect"]=regression_29_data[c,"mean_ratio"]
      full_regression_coefficients_29[c,"beta_log_gdp"]=0
      full_regression_coefficients_29[c,"beta_log_gdp_square"]=0
    }
  }
}
for (c in full_regression_coefficients_29$country){
  if (is.na(full_regression_coefficients_29[c,"best_regression"])){
    if(!(is.na(full_regression_coefficients_29[c,"closest_country"]))){
      c2=full_regression_coefficients_29[c,"closest_country"]
      full_regression_coefficients_29[c,"fixed_effect"] = full_regression_coefficients_29[c2,"fixed_effect"]
      full_regression_coefficients_29[c,"beta_log_gdp"] = full_regression_coefficients_29[c2,"beta_log_gdp"]
      full_regression_coefficients_29[c,"beta_log_gdp_square"] = full_regression_coefficients_29[c2,"beta_log_gdp_square"]
    }
  }
}

for (c in full_regression_coefficients_29$country){
  if (is.na(full_regression_coefficients_29[c,"fixed_effect"])){
    full_regression_coefficients_29[c,"fixed_effect"] = 0
    full_regression_coefficients_29[c,"beta_log_gdp"] = 0
    full_regression_coefficients_29[c,"beta_log_gdp_square"] = 0
  }
}

consumption_emissions_coefficients <- full_regression_coefficients_29%>%
  select(fixed_effect, beta_log_gdp, beta_log_gdp_square)

write.csv(consumption_emissions_coefficients, "consumption_emissions_coefficients.csv")



##############################################################################################
#Test de variation du rapport des émissions

variability_test <- regression_data%>%
  select(country, year, emissions_balance_percent, log_gdp)%>%
  mutate(max_volatility = NA)%>%
  group_by(country)

test_table <- regression_data%>%
  filter(country =="CHN")

test_table <- regression_data%>%
  filter(country %in% c("IDN", "IRN", "EGY", "ZAF", "VNM","MOR"))%>%
  filter(year >= 2020)%>%
  select(country, year, emissions_balance_percent)

#############################################################################################################
#This part plots figures for the paper

#Table 3
data_table_2 <- regression_emissions(data = regression_data, "log_consumption", 
                                     c("log_emissions_pc.world", "log_territorial", "log_gdp", "log_gdp.world", 
                                       "log_imports_gns_percent", "log_imports_gns_percent.world", "log_exports_gns_percent",
                                       "log_exports_gns_percent.world", "consumption_emissions"))

data_base <- data_table_2 %>%  
  filter(year <= 2010) #select ten years before the end year for prediction


regression_2 <- pmg(log_consumption~ log_emissions_pc.world + log_territorial + log_gdp + log_gdp.world + log_imports_gns_percent + log_exports_gns_percent.world + log_exports_gns_percent - 1,
                    data = data_base, index = c("country", "year"), model ="mg")

data_table_25 <- regression_emissions(data = regression_data, "consumption_emissions", c("territorial_emissions", "consumption_emissions", "gdp", "emissions_pc.world", "trade_balance_gns.pc", "trade_balance_goods.pc", "gdp.world"),
                                      last_year = 2020, countries_aside =90)

data_base <- data_table_25%>%
  filter(year <= 2010)

regression_25 <- pmg(consumption_emissions~ territorial_emissions + gdp + emissions_pc.world + trade_balance_gns.pc + trade_balance_goods.pc + gdp.world,
                     data = data_base, index = c("country", "year"), model ="mg")
summary(regression_25)

stargazer(list(regression_2, regression_25), type = "html", title = "Table 3 : Regression 2 and regression 25",
          out = "table_3.html")

#Table 4 for the paper

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

regression_results_10_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("trade_balance_gns_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)
regression_results_11_5Y <- prediction_regression(data = data_base, emissions = "emissions_balance_percent", explanatory_variables =  c("exports_gns_percent", "imports_gns_percent"), 
                                                  predicted_data = data_pred_5Y, logged_emissions = FALSE, fixed_effect = TRUE)

regression_10 <- pmg(emissions_balance_percent~ trade_balance_gns_percent, data = data_base,
                     index = c("country", "year"), model = "mg")
regression_11 <- pmg(emissions_balance_percent~ exports_gns_percent + imports_gns_percent, data = data_base,
                     index = c("country", "year"), model = "mg")

summary(regression_11)

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

regression_12 <- pmg(emissions_balance_percent~ exports_gns_percent + imports_gns_percent + fuel_imports_percent_gdp + fuel_exports_percent_gdp + exports_gns_percent.world - 1, data = data_base,
                     index = c("country", "year"), model = "mg")

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

regression_17 <- pmg(emissions_balance_percent~ balance_fuel_percent_gdp + trade_balance_gns_percent - 1, data = data_base,
                     index = c("country", "year"), model = "mg")

#stargazer(list(regression_10, regression_11, regression_12, regression_17), type = "html", title = "Table 4 : Results of regression 10, 11, 12, and 17",
          out = "table_4.html")
stargazer(list(regression_10, regression_11, regression_12, regression_17), title = "Table 4 : Results of regression 10, 11, 12, and 17")

#############################################################################################################
#Plot for Table 5 in the paper

res_sum <- read.xlsx("summary_regression_results.xlsx")
res_sum_2 <- res_sum%>%
  select(11,12,13,18)%>%
  rename("regression 10" = "10",
         "regression 11" = "11",
         "regression 12" = "12",
         "regression 17" = "17")
row.names(res_sum_2)=c("R-square", "CD test", "MAE", "balance percent")


stargazer(res_sum_2, summary = FALSE, type = "html", out = "figure_5. html" )

test_data <- regression_data%>%
  select(country, year, territorial_emissions.share, emissions_pc.world, territorial_emissions, population, consumption_emissions)

test_plus <- regression_data%>%
  filter(year ==2020)

test_value = sum(test_plus$territorial_emissions*test_plus$territorial_emissions.share, na.rm = TRUE)
test_value
  

#############################################################################################################
#This part plots the figure 2 for the paper

cut_data <- regression_data%>%
  select(country, year, population, transfer_emissions, emissions_balance_percent, trade_balance_gns.pc, consumption_emissions, territorial_emissions)%>%
  filter( country %in% c("CHN", "IND", "GBR", "USA", "MEX"))


p <- ggplot(cut_data, aes(x=year, y = transfer_emissions, colour = country))+
  theme_minimal()+
  geom_line()+
  scale_color_manual(
    name = "Transfer emissions \nper capita (tCO2)",  # Titre de la légende
    values = c("USA" = "red", "GBR"="blue", "CHN"="darkgreen", "IND"="orange", "MEX" = "black"),  # Associe la couleur rouge aux USA
    labels = c("USA" = "USA", "GBR" = "GBR", "CHN" = "CHN", "IND"="IND", "MEX"="MEX")  # Texte à afficher dans la légende
  ) +
  theme(legend.position = "right")  # Positionne la légende à droite

print(p)

test_data <- cut_data%>%
  filter(country %in% c("GBR", "USA"))

ggsave(plot =p, filename = "figure_2.jpg")

p <- ggplot(cut_data, aes(x=year, y = trade_balance_gns.pc, colour = country))+
  theme_minimal()+
  geom_line()+
  scale_color_manual(
    name = "Trade imbalance \nper capita \n(in constant 2015 USD) ",  # Titre de la légende
    values = c("USA" = "red", "GBR"="blue", "CHN"="darkgreen", "IND"="orange", "MEX" = "black"),  # Associe la couleur rouge aux USA
    labels = c("USA" = "USA", "GBR" = "GBR", "CHN" = "CHN", "IND"="IND", "MEX"="MEX")  # Texte à afficher dans la légende
  ) +
  theme(legend.position = "right")  # Positionne la légende à droite

print(p)

ggsave(plot=p, filename="figure_3.jpg")





