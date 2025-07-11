####################################################################################
#This code achieves the last part of the analysis about consumption-based emissions#
#It assesses a forecast formula to each country                                    #
#The dataset is split into three parts                                             #
#Training set : 1997-2003; Selection set : 2004-2011; Validation set:2012-2020     # 
#{Training set:1997--2003 --> build multiple country regressions                   #
# Filter : 2004-2020 --> pairwise correlation tests                                #
# Filter : 2004-2020 --> serial correlation                                        #
# Selection set: 2004--2011 --> MAE-based ranking                                  #
# Validation set: 2012--2020 --> test for overfitting : MAE must not increase      #
####################################################################################


library(dplyr)
library(readr)
library(stats)
library(tidyr)
library(openxlsx)
library(rlang)
library(plm)
library(stats)
library(tidyverse)
library(stargazer)
library(ggplot2)

#############################################################################################################
#Loading data

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

################################################################################################################
#Let us now process tables of coefficients and residuals for each regression : 9, 10, 11, 17, 28, fix

training_data <- regression_data%>%
  filter(year <= 2003)

#############################################################################################
#Coeff table regression 9

data_frame_regression_9 <- data.frame()%>%
  mutate(constant = 0,
         exports_gns_percent.world = 0,
         exports_gns_percent =0,
         imports_gns_percent = 0,
         trade_balance_gns_percent = 0,
         balance_fuel_percent_gdp = 0)

for (c in countries$country){
  temp_training_data <- training_data%>%
    filter(country ==c)
  
  y1 = sum(is.na((temp_training_data$emissions_balance_percent)))
  y2 = sum(is.na((temp_training_data$imports_gns_percent)))
  y3 = sum(is.na((temp_training_data$exports_gns_percent)))
  
  if(max(y1,y2,y3)<8){
    country_regression_9 <- lm(emissions_balance_percent ~ exports_gns_percent.world + imports_gns_percent+ exports_gns_percent, data = temp_training_data)
    data_frame_regression_9[c,"constant"]=country_regression_9$coefficients[1]
    data_frame_regression_9[c,"exports_gns_percent.world"]=country_regression_9$coefficients[2]
    data_frame_regression_9[c,"imports_gns_percent"]=country_regression_9$coefficients[3]
    data_frame_regression_9[c,"exports_gns_percent"]=country_regression_9$coefficients[4]
    data_frame_regression_9[c,"trade_balance_gns_percent"]=0
    data_frame_regression_9[c,"balance_fuel_percent_gdp"]=0
  }
}

#############################################################################################
#Coeff table regression 10

data_frame_regression_10 <- data.frame()%>%
  mutate(constant = 0,
         exports_gns_percent.world = 0,
         exports_gns_percent =0,
         imports_gns_percent = 0,
         trade_balance_gns_percent = 0,
         balance_fuel_percent_gdp = 0)

for (c in countries$country){
  temp_training_data <- training_data%>%
    filter(country ==c)
  
  y1 = sum(is.na((temp_training_data$emissions_balance_percent)))
  y2 = sum(is.na((temp_training_data$trade_balance_gns_percent)))
  
  if(max(y1,y2)<8){
    country_regression_10 <- lm(emissions_balance_percent ~ trade_balance_gns_percent, data = temp_training_data)
    data_frame_regression_10[c,"constant"]=country_regression_10$coefficients[1]
    data_frame_regression_10[c,"exports_gns_percent.world"]=0
    data_frame_regression_10[c,"imports_gns_percent"]=0
    data_frame_regression_10[c,"exports_gns_percent"]=0
    data_frame_regression_10[c,"trade_balance_gns_percent"]=country_regression_10$coefficients[2]
    data_frame_regression_10[c,"balance_fuel_percent_gdp"]=0
  }
}

#############################################################################################
#Coeff table regression 11

data_frame_regression_11 <- data.frame()%>%
  mutate(constant = 0,
         exports_gns_percent.world = 0,
         exports_gns_percent =0,
         imports_gns_percent = 0,
         trade_balance_gns_percent = 0,
         balance_fuel_percent_gdp = 0)

for (c in countries$country){
  temp_training_data <- training_data%>%
    filter(country ==c)
  
  y1 = sum(is.na(temp_training_data$emissions_balance_percent))
  y2 = sum(is.na(temp_training_data$imports_gns_percent))
  y3 = sum(is.na(temp_training_data$exports_gns_percent))
  
  
  if(max(y1,y2,y3)<8){
    country_regression_11 <- lm(emissions_balance_percent ~ imports_gns_percent + exports_gns_percent, data = temp_training_data)
    data_frame_regression_11[c,"constant"]=country_regression_11$coefficients[1]
    data_frame_regression_11[c,"exports_gns_percent.world"]=0
    data_frame_regression_11[c,"imports_gns_percent"]=country_regression_11$coefficients[2]
    data_frame_regression_11[c,"exports_gns_percent"]=country_regression_11$coefficients[3]
    data_frame_regression_11[c,"trade_balance_gns_percent"]=0
    data_frame_regression_11[c,"balance_fuel_percent_gdp"]=0
  }
}

#############################################################################################
#Coeff table regression 17

data_frame_regression_17 <- data.frame()%>%
  mutate(constant = 0,
         exports_gns_percent.world = 0,
         exports_gns_percent =0,
         imports_gns_percent = 0,
         trade_balance_gns_percent = 0,
         balance_fuel_percent_gdp = 0)

for (c in countries$country){
  temp_training_data <- training_data%>%
    filter(country ==c)
  
  y1 = sum(is.na(temp_training_data$emissions_balance_percent))
  y2 = sum(is.na(temp_training_data$trade_balance_gns_percent))
  y3 = sum(is.na(temp_training_data$balance_fuel_percent_gdp))
  
  if(max(y1,y2,y3)<8){
    country_regression_17 <- lm(emissions_balance_percent ~ trade_balance_gns_percent + balance_fuel_percent_gdp, data = temp_training_data)
    data_frame_regression_17[c,"constant"]=country_regression_17$coefficients[1]
    data_frame_regression_17[c,"exports_gns_percent.world"]=0
    data_frame_regression_17[c,"imports_gns_percent"]=0
    data_frame_regression_17[c,"exports_gns_percent"]=0
    data_frame_regression_17[c,"trade_balance_gns_percent"]=country_regression_17$coefficients[2]
    data_frame_regression_17[c,"balance_fuel_percent_gdp"]=country_regression_17$coefficients[3]
  }
}

#############################################################################################
#Coeff table fix ratio

data_frame_fix_ratio <- data.frame()%>%
  mutate(constant = 0,
         exports_gns_percent.world = 0,
         exports_gns_percent =0,
         imports_gns_percent = 0,
         trade_balance_gns_percent = 0,
         balance_fuel_percent_gdp = 0)

for (c in countries$country){
  temp_training_data <- training_data%>%
    filter(country ==c)
  
  y1 = sum(is.na(temp_training_data$emissions_balance_percent))
  
  if(y1<9){
    data_frame_fix_ratio[c,"constant"]=mean(temp_training_data$emissions_balance_percent, na.rm = TRUE)
    data_frame_fix_ratio[c,"exports_gns_percent.world"]=0
    data_frame_fix_ratio[c,"imports_gns_percent"]=0
    data_frame_fix_ratio[c,"exports_gns_percent"]=0
    data_frame_fix_ratio[c,"trade_balance_gns_percent"]=0
    data_frame_fix_ratio[c,"balance_fuel_percent_gdp"]=0
  }
}

#############################################################################################
#Coeff table regression 28

data_frame_regression_28 <- data.frame()%>%
  mutate(constant = 0,
         trade_balance_gns = 0)

for (c in countries$country){
  temp_training_data <- training_data%>%
    filter(country ==c)
  
  y1 = sum(is.na(temp_training_data$transfer_emissions))
  y2 = sum(is.na(temp_training_data$trade_balance_gns))
  
  if(max(y1,y2)<8){
    country_regression_28 <- lm(transfer_emissions ~ trade_balance_gns, data = temp_training_data)
    data_frame_regression_28[c,"constant"]=country_regression_28$coefficients[1]
    data_frame_regression_28[c,"trade_balance_gns"]=country_regression_28$coefficients[2]
  }
}

#############################################################################################
#Residuals table regression 9

residuals_regression_9 <- data.frame(row.names = countries$country)

for (c in countries$country){
  prediction_data <- regression_data%>%
    filter(country == c)
  
  row.names(prediction_data) <- prediction_data$year
  
  for (y in (2004:2022)){
    fitted_value = data_frame_regression_9[c,"constant"] 
                  + data_frame_regression_9[c,"exports_gns_percent.world"]*prediction_data[as.character(y),"exports_gns_percent.world"]
                  + data_frame_regression_9[c,"imports_gns_percent"]*prediction_data[as.character(y),"imports_gns_percent"]
                  + data_frame_regression_9[c,"exports_gns_percent"]*prediction_data[as.character(y),"exports_gns_percent"]
  
    if (!(is.na(fitted_value))&!(is.na(prediction_data[paste0(y),"emissions_balance_percent"]))){
      #I need to be careful about the sign -- Must be the same across the tables
      residuals_regression_9[c,as.character(y)]=(prediction_data[as.character(y),"emissions_balance_percent"] - fitted_value)*prediction_data[as.character(y),"territorial_emissions"]/100
    }
  }
}

residuals_regression_9 <- abs(residuals_regression_9)

#############################################################################################
#Residuals table regression 10

residuals_regression_10 <- data.frame(row.names = countries$country)

for (c in countries$country){
  prediction_data <- regression_data%>%
    filter(country == c)
  
  row.names(prediction_data) <- prediction_data$year
  
  for (y in (2004:2022)){
    fitted_value = data_frame_regression_10[c,"constant"] 
    + data_frame_regression_10[c,"trade_balance_gns_percent"]*prediction_data[as.character(y),"trade_balance_gns_percent"]
    
    if (!(is.na(fitted_value))&!(is.na(prediction_data[paste0(y),"emissions_balance_percent"]))){
      #I need to be careful about the sign -- Must be the same across the tables
      residuals_regression_10[c,as.character(y)]=(prediction_data[as.character(y),"emissions_balance_percent"] - fitted_value)*prediction_data[as.character(y),"territorial_emissions"]/100
    }
  }
}

residuals_regression_10 <- abs(residuals_regression_10)

#############################################################################################
#Residuals table regression 11

residuals_regression_11 <- data.frame(row.names = countries$country)

for (c in countries$country){
  prediction_data <- regression_data%>%
    filter(country == c)
  
  row.names(prediction_data) <- prediction_data$year
  
  for (y in (2004:2022)){
    fitted_value = data_frame_regression_11[c,"constant"] 
    + data_frame_regression_11[c,"imports_gns_percent"]*prediction_data[as.character(y),"imports_gns_percent"]
    + data_frame_regression_11[c,"exports_gns_percent"]*prediction_data[as.character(y),"exports_gns_percent"]
    
    if (!(is.na(fitted_value))&!(is.na(prediction_data[paste0(y),"emissions_balance_percent"]))){
      #I need to be careful about the sign -- Must be the same across the tables
      residuals_regression_11[c,as.character(y)]=(prediction_data[as.character(y),"emissions_balance_percent"] - fitted_value)*prediction_data[as.character(y),"territorial_emissions"]/100
    }
  }
}

residuals_regression_11 <- abs(residuals_regression_11)

#############################################################################################
#Residuals table regression 17

residuals_regression_17 <- data.frame(row.names = countries$country)

for (c in countries$country){
  prediction_data <- regression_data%>%
    filter(country == c)
  
  row.names(prediction_data) <- prediction_data$year
  
  for (y in (2004:2022)){
    fitted_value = data_frame_regression_17[c,"constant"] 
    + data_frame_regression_17[c,"trade_balance_gns_percent"]*prediction_data[as.character(y),"trade_balance_gns_percent"]
    + data_frame_regression_17[c,"balance_fuel_percent_gdp"]*prediction_data[as.character(y),"balance_fuel_percent_gdp"]
    
    if (!(is.na(fitted_value))&!(is.na(prediction_data[paste0(y),"emissions_balance_percent"]))){
      #I need to be careful about the sign -- Must be the same across the tables
      residuals_regression_17[c,as.character(y)]=(prediction_data[as.character(y),"emissions_balance_percent"] - fitted_value)*prediction_data[as.character(y),"territorial_emissions"]/100
    }
  }
}

residuals_regression_17 <- abs(residuals_regression_17)

#############################################################################################
#Residuals table fix ratio

residuals_fix_ratio <- data.frame(row.names = countries$country)

for (c in countries$country){
  prediction_data <- regression_data%>%
    filter(country == c)
  
  row.names(prediction_data) <- prediction_data$year
  
  for (y in (2004:2022)){
    fitted_value = data_frame_fix_ratio[c,"constant"] 
    
    if (!(is.na(fitted_value))&!(is.na(prediction_data[paste0(y),"emissions_balance_percent"]))){
      #I need to be careful about the sign -- Must be the same across the tables
      residuals_fix_ratio[c,as.character(y)]=(prediction_data[as.character(y),"emissions_balance_percent"] - fitted_value)*prediction_data[as.character(y),"territorial_emissions"]/100
    }
  }
}

residuals_fix_ratio <- abs(residuals_fix_ratio)

#############################################################################################
#Residuals table regression 28

residuals_regression_28 <- data.frame(row.names = countries$country)

for (c in countries$country){
  prediction_data <- regression_data%>%
    filter(country == c)
  
  row.names(prediction_data) <- prediction_data$year
  
  for (y in (2004:2022)){
    fitted_value = data_frame_regression_28[c,"constant"] 
    + data_frame_regression_28[c,"trade_balance_gns"]*prediction_data[as.character(y),"trade_balance_gns"]
    
    if (!(is.na(fitted_value))&!(is.na(prediction_data[as.character(y),"transfer_emissions"]))){
      #I need to be careful about the sign -- Must be the same across the tables
      residuals_regression_28[c,as.character(y)]=prediction_data[as.character(y),"transfer_emissions"] - fitted_value
    }
  }
}

residuals_regression_28 <- abs(residuals_regression_28)

#############################################################################################
#Now let us prepare tables in order to filter + rank + Validate regressions regressions

filter_table <- data.frame(row.names = countries$country)%>%
  mutate(regression_9 = 1,
         regression_10 = 1,
         regression_11 = 1,
         regression_17 = 1,
         fix_regression = 1,
         regression_28 = 1)

rank_table <- data.frame(row.names = countries$country)%>%
  mutate(regression_9 = NA,
         regression_10 = NA,
         regression_11 = NA,
         regression_17 = NA,
         fix_regression = NA,
         regression_28 = NA)

rank_mea <- data.frame(row.names = countries$country)%>%
  mutate(regression_9 = NA,
         regression_10 = NA,
         regression_11 = NA,
         regression_17 = NA,
         fix_regression = NA,
         regression_28 = NA)

validation_table <- data.frame(row.names = countries$country)%>%
  mutate(regression_9 = 0,
         regression_10 = 0,
         regression_11 = 0,
         regression_17 = 0,
         fix_regression = 0,
         regression_28 = 0)

validation_mea <- data.frame(row.names = countries$country)%>%
  mutate(regression_9 = NA,
         regression_10 = NA,
         regression_11 = NA,
         regression_17 = NA,
         fix_regression = NA,
         regression_28 = NA)
##########################################################################################
#Later I will modify the filter table after

##########################################################################################
#Now I proceed to the arrangement of the ranking table 

order_regression = c("regression_9", "regression_10", "regression_11", "regression_17", "fix_regression", "regression_28")

ranking_regression_9 <- residuals_regression_9%>%
  select("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")

ranking_regression_10 <- residuals_regression_10%>%
  select("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")

ranking_regression_11 <- residuals_regression_11%>%
  select("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")

ranking_regression_17 <- residuals_regression_17%>%
  select("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")

ranking_fix_ratio <- residuals_fix_ratio%>%
  select("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")

ranking_regression_28 <- residuals_regression_28%>%
  select("2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")

rank(c(NA,NA,NA,NA), ties.method = "first" )
#Be careful rank ranks NA too

for (c in countries$country){
  rank_mea_9 = mean(t(ranking_regression_9[c,]), na.rm = TRUE)
  rank_mea_10 = mean(t(ranking_regression_10[c,]), na.rm = TRUE)
  rank_mea_11 = mean(t(ranking_regression_11[c,]), na.rm = TRUE)
  rank_mea_17 = mean(t(ranking_regression_17[c,]), na.rm = TRUE)
  rank_mea_fix = mean(t(ranking_fix_ratio[c,]), na.rm = TRUE)
  rank_mea_28 = mean(t(ranking_regression_28[c,]), na.rm = TRUE)
  order_regression = as.data.frame(c(rank_mea_9, rank_mea_10, rank_mea_11, rank_mea_17, rank_mea_fix, rank_mea_28))
  rank_mea[c,] = as.data.frame(t(order_regression))
  rank_table[c,] = rank(order_regression, ties.method = "first")
  
  if (is.na(rank_mea_9)){
    rank_table[c,"regression_9"]=NA
  }
  if (is.na(rank_mea_10)){
    rank_table[c,"regression_10"]=NA
  }
  if (is.na(rank_mea_11)){
    rank_table[c,"regression_11"]=NA
  }
  if (is.na(rank_mea_17)){
    rank_table[c,"regression_17"]=NA
  }
  if (is.na(rank_mea_fix)){
    rank_table[c,"fix_regression"]=NA
  }
  if (is.na(rank_mea_28)){
    rank_table[c,"regression_28"]=NA
  }
  
}


##########################################################################################
#Lastly I process the validation table 

validation_regression_9 <- residuals_regression_9%>%
  select(-"2004", -"2005", -"2006", -"2007", -"2008", -"2009", -"2010", -"2011")

validation_regression_10 <- residuals_regression_10%>%
  select(-"2004", -"2005", -"2006", -"2007", -"2008", -"2009", -"2010", -"2011")

validation_regression_11 <- residuals_regression_11%>%
  select(-"2004", -"2005", -"2006", -"2007", -"2008", -"2009", -"2010", -"2011")

validation_regression_17 <- residuals_regression_17%>%
  select(-"2004", -"2005", -"2006", -"2007", -"2008", -"2009", -"2010", -"2011")

validation_fix_ratio <- residuals_fix_ratio%>%
  select(-"2004", -"2005", -"2006", -"2007", -"2008", -"2009", -"2010", -"2011")

validation_regression_28 <- residuals_regression_28%>%
  select(-"2004", -"2005", -"2006", -"2007", -"2008", -"2009", -"2010", -"2011")

increasing_mea <- data.frame(row.names = countries$country)%>%
  mutate(best_mea_change = NA)


for (c in countries$country){
  validation_mea_9 = mean(t(validation_regression_9[c,]), na.rm = TRUE)
  validation_mea_10 = mean(t(validation_regression_10[c,]), na.rm = TRUE)
  validation_mea_11 = mean(t(validation_regression_11[c,]), na.rm = TRUE)
  validation_mea_17 = mean(t(validation_regression_17[c,]), na.rm = TRUE)
  validation_mea_fix = mean(t(validation_fix_ratio[c,]), na.rm = TRUE)
  validation_mea_28 = mean(t(validation_regression_28[c,]), na.rm = TRUE)
  print(c)
  print(order_validation)
  order_validation = as.data.frame(c(validation_mea_9, validation_mea_10, validation_mea_11, validation_mea_17, validation_mea_fix, validation_mea_28))
  validation_mea[c,] = as.data.frame(t(order_validation))
  best_regression = which(as.data.frame(rank_table[c,])==1)
  print(c)
  print(best_regression)
  if (!(sum(is.na(order_validation))==6)){
    first_mea = rank_mea[c,best_regression]
    new_mea = validation_mea[c,best_regression]
    increasing_mea[c,"best_mea_change"] = (new_mea/first_mea-1)*100
  }
}

summary(increasing_mea$best_mea_change)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -81.454   -9.482   37.280  139.649  125.907 3651.898       64 


##########################################################################################
#Now let us draw particular interest to the fix ratio regression 

fix_ratio_rank_error <-rank_mea%>%
  select("rank_fix_mea" = "fix_regression" )

fix_ratio_rank_error <- rownames_to_column(fix_ratio_rank_error, var = "country")

fix_ratio_validation_error <- validation_mea%>%
  select("validation_fix_mea" = "fix_regression")

fix_ratio_validation_error <- rownames_to_column(fix_ratio_validation_error, var = "country")

fix_ratio_errors <- merge(x=fix_ratio_rank_error, y=fix_ratio_validation_error, by = "country")%>%
  mutate(mea_change_percent = (validation_fix_mea/rank_fix_mea-1)*100)%>%
  mutate(mea_improves = (mea_change_percent < 0))

summary(fix_ratio_errors$mea_improves)
#Mode   FALSE    TRUE    NA's 
#logical      73      42      64
summary(fix_ratio_errors$mea_change_percent)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -83.43  -26.65   18.35   69.93   84.88 1129.51      64 

fix_ratio_errors <- fix_ratio_errors%>%
  rename("mae_2004_2011" ="rank_fix_mea",
         "mae_2012_2022" = "validation_fix_mea")

write.csv(fix_ratio_errors, "fix_ratio_errors.csv")

##########################################################################################
#Now let us proceed to the comparison of the stability of the different regressions

absolute_error_long_regression_9 <- rownames_to_column(residuals_regression_9, var = "country")%>%
  pivot_longer(
    cols = - country,
    names_to = "year",
    values_to = "abs_err_reg_9"
  )

absolute_error_long_regression_10 <- rownames_to_column(residuals_regression_10, var = "country")%>%
  pivot_longer(
    cols = - country,
    names_to = "year",
    values_to = "abs_err_reg_10"
  )

absolute_error_long_regression_11 <- rownames_to_column(residuals_regression_11, var = "country")%>%
  pivot_longer(
    cols = - country,
    names_to = "year",
    values_to = "abs_err_reg_11"
  )

absolute_error_long_regression_17 <- rownames_to_column(residuals_regression_17, var = "country")%>%
  pivot_longer(
    cols = - country,
    names_to = "year",
    values_to = "abs_err_reg_17"
  )

absolute_error_long_regression_28 <- rownames_to_column(residuals_regression_28, var = "country")%>%
  pivot_longer(
    cols = - country,
    names_to = "year",
    values_to = "abs_err_reg_28"
  )

absolute_error_long_fix_ratio <- rownames_to_column(residuals_fix_ratio, var = "country")%>%
  pivot_longer(
    cols = - country,
    names_to = "year",
    values_to = "abs_err_fix_ratio"
  )

int_merge <- merge(x=absolute_error_long_regression_9, y=absolute_error_long_regression_10, by = c("country","year"))
int_merge_2 <- merge(x=int_merge, y=absolute_error_long_regression_11, by = c("country","year"))
int_merge_3 <- merge(x=int_merge_2, y=absolute_error_long_regression_17, by = c("country","year"))
int_merge_4 <- merge(x=int_merge_3, y=absolute_error_long_regression_28, by = c("country", "year"))
int_merge_5 <- merge(x=int_merge_4, y=absolute_error_long_fix_ratio, by = c("country", "year"))

cut_data <- regression_data%>%
  select(country, year, population)%>%
  filter(year >= 2004,
         year <= 2022)

absolute_error_long_table <- merge(x=int_merge_5, y=cut_data, by = c("country", "year"))

mae_yearly_regressions <- data.frame(row.names = as.character(2004:2022))%>%
  mutate("mae_reg_9" = NA,
         "mae_reg_10" = NA,
         "mae_reg_11" = NA,
         "mae_reg_17" = NA,
         "mae_reg_28" = NA,
         "mae_fix_ratio" = NA)

for (y in (2004:2022)){
  compute_table <- absolute_error_long_table%>%
    filter(year == y)
  #compute mae for reg 9
  temp_reg_table <- compute_table%>%
    filter(!(is.na(abs_err_reg_9)),
           !(is.na(population)))
  mae_yearly_regressions[as.character(y),"mae_reg_9"] = 
    sum(temp_reg_table$abs_err_reg_9*temp_reg_table$population)/sum(temp_reg_table$population)
  #compute mae for reg 10
  temp_reg_table <- compute_table%>%
    filter(!(is.na(abs_err_reg_10)),
           !(is.na(population)))
  mae_yearly_regressions[as.character(y),"mae_reg_10"] = 
    sum(temp_reg_table$abs_err_reg_10*temp_reg_table$population)/sum(temp_reg_table$population)
  #compute mae for reg 11
  temp_reg_table <- compute_table%>%
    filter(!(is.na(abs_err_reg_11)),
           !(is.na(population)))
  mae_yearly_regressions[as.character(y),"mae_reg_11"] = 
    sum(temp_reg_table$abs_err_reg_11*temp_reg_table$population)/sum(temp_reg_table$population)
  #compute mae for reg 17
  temp_reg_table <- compute_table%>%
    filter(!(is.na(abs_err_reg_17)),
           !(is.na(population)))
  mae_yearly_regressions[as.character(y),"mae_reg_17"] = 
    sum(temp_reg_table$abs_err_reg_17*temp_reg_table$population)/sum(temp_reg_table$population)
  #compute mae for reg 28
  temp_reg_table <- compute_table%>%
    filter(!(is.na(abs_err_reg_28)),
           !(is.na(population)))
  mae_yearly_regressions[as.character(y),"mae_reg_28"] = 
    sum(temp_reg_table$abs_err_reg_28*temp_reg_table$population)/sum(temp_reg_table$population)
  #compute mae for fix ratio
  temp_reg_table <- compute_table%>%
    filter(!(is.na(abs_err_fix_ratio)),
           !(is.na(population)))
  mae_yearly_regressions[as.character(y),"mae_fix_ratio"] = 
    sum(temp_reg_table$abs_err_fix_ratio*temp_reg_table$population)/sum(temp_reg_table$population)
}

mae_yearly_long <- rownames_to_column(mae_yearly_regressions, var = "year") %>%
  pivot_longer( cols = -year,
                names_to = "reg_name",
                values_to = "mae")%>%
  mutate(year = as.numeric(year))

##########################################################################################
#Let us plot this result

p <- ggplot(mae_yearly_long, aes(x=year, y = mae, colour = reg_name, group = reg_name))+
  theme_minimal()+
  geom_line()+
  scale_color_manual(
    name = "Yearly mean absolute\nerror by regression",  # Titre de la légende
    values = c("mae_reg_9" = "red", "mae_reg_10"="blue", "mae_reg_11"="darkgreen", "mae_reg_17"="orange", "mae_reg_28" = "black", "mae_fix_ratio" = "purple"),  # Associe la couleur rouge aux USA
    labels = c("reg 9" = "mae_reg_9",     # Mauvais ordre
      "reg 10" = "mae_reg_10",
      "reg_11" = "mae_reg_11",       
      "reg_17" = "mae_reg_17",
      "reg_28" = "mae_reg_28",
      "fix_ratio" = "mae_fix_ratio"
    )  # Texte à afficher dans la légende
  )+
  scale_x_continuous(breaks = c(2004, 2010, 2015, 2020))+
  theme(legend.position = "right") 

print(p)

ggsave(plot =p, filename = "draw_mae.jpg")
