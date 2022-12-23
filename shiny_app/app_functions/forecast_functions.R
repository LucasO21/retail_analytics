# FUNCTIONS FOR FORECASTING ----
# **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Working Dir ----
# setwd(here::here("shiny_app"))
# 
# * Libraries ----
# library(tidyverse)
# library(timetk)
# library(modeltime)

# ******************************************************************************
# DATA IMPORT ----
# ******************************************************************************
forecast_artifacts_list <- read_rds("app_artifacts/forecast_artifacts_list.rds")

future_forecast_tbl <- forecast_artifacts_list$data$future_forecast %>% 
    select(-contains("roll_"), -contains("_K1"), -contains("_lag"))

forecast_accuracy_tbl <- forecast_artifacts_list$data$fit_models_accuracy_tbl

future_forecast_tbl %>% glimpse()
future_forecast_tbl %>% distinct(.model_desc)
future_forecast_tbl %>% distinct(.key)


# ******************************************************************************
# DATA PREP: SUMMARISE BY TIME ----
# ******************************************************************************
# start_date <- as.Date("2011-01-01")
# data       <- future_forecast_tbl

get_forecast_data <- function(data, 
                              .country         = "United Kingdom",
                              .forecast_days   = 90,
                              .lookback_months = 6){
  
  # Look Back Start Date
  start_date <- as.Date("2011-01-01")
  
  # Model
  if(.country == "United Kingdom") forecast_model = "XGBOOST - Tuned"
  if(.country == "All Others")     forecast_model = "ENSEMBLE (MEAN): 4 MODELS"
    
  data_prep <- data %>% 
    filter(country == .country) %>% 
    filter(.model_desc %in% c("ACTUAL", forecast_model)) %>% 
    group_by(.model_id, .model_desc, .key, .index) %>% 
    summarise_by_time(
        .date_var = invoice_date,
        .by       = "day",
        .value    = sum(.value)
    ) %>% 
    ungroup() %>% 
    filter(.index >= start_date) %>% 
    arrange(.index) 
  
  data_prep %>% 
    filter(.model_desc == "prediction")
  
  # Forecast Days 
  if(.forecast_days < 30) .forecast_days = 30
  if(.forecast_days > 90) .forecast_days = 90
  if(.lookback_months < 3) .lookback_months = 3
  if(.lookback_months > 11) .lookback_months = 11
  
  row_count               <- nrow(data_prep)
  forecast_rows_to_remove <- (90 - .forecast_days)
  forecast_days_to_keep   <- row_count - forecast_rows_to_remove
  
  data_prep <- data_prep %>% head(forecast_days_to_keep)
  
  # Look Back Days
  row_count         <- nrow(data_prep)
  lb_rows_to_keep   <- (.lookback_months * 30)
  lb_rows_to_remove <- (row_count - lb_rows_to_keep)
  
  data_prep <- data_prep %>% tail(lb_rows_to_remove)
  
  return(data_prep)
    
}

# future_forecast_tbl %>% 
#   get_forecast_data("United Kingdom", .forecast_days = 30, .lookback_months = 6) %>% 
#   plot_modeltime_forecast()


# ******************************************************************************
# DATA PREP: DATATABLE ----
# ******************************************************************************
get_forecast_data_dt <- function(data){
  
  uk_tbl <- data %>% 
    get_forecast_data(.country = "United Kingdom") %>% 
    filter(.key == "prediction") %>% 
    select(invoice_date, .value) %>% 
    mutate(.value = ifelse(.value < 5, 0, .value)) %>% 
    rename(uk_forecast = .value) 
  
  others_tbl <- data %>% 
    get_forecast_data(.country = "All Others") %>% 
    filter(.key == "prediction") %>% 
    select(invoice_date, .value) %>% 
    mutate(.value = ifelse(.value < 5, 0, .value)) %>% 
    rename(others_forecast = .value) %>% 
    select(-invoice_date)
  
  data_prep <- bind_cols(uk_tbl, others_tbl) %>% 
    mutate(total_forecast = (uk_forecast + others_forecast), .after = invoice_date) %>% 
    mutate(
      uk_forecast_pct     = uk_forecast / total_forecast,
      others_forecast_pct = others_forecast / total_forecast 
    ) %>% 
    mutate(uk_forecast_pct = ifelse(is.nan(uk_forecast_pct), 0, uk_forecast_pct)) %>% 
    mutate(others_forecast_pct = ifelse(is.nan(others_forecast_pct), 0, others_forecast_pct)) %>% 
    mutate(
      total_forecast = total_forecast %>% scales::dollar(accuracy = 1),
      uk_forecast = uk_forecast %>% scales::dollar(accuracy = 1),
      others_forecast = others_forecast %>% scales::dollar(accuracy = 1),
      uk_forecast_pct = uk_forecast_pct %>% scales::percent(accuracy = 0.1),
      others_forecast_pct = others_forecast_pct %>% scales::percent(accuracy = 0.1),
    ) 
  
  
  return(data_prep)
    
}

# future_forecast_tbl %>% get_forecast_data_dt()



  
    
    




