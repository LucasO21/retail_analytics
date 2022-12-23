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
# forecast_artifacts_list <- read_rds("app_artifacts/forecast_artifacts_list.rds")
# 
# future_forecast_tbl <- forecast_artifacts_list$data$future_forecast %>% 
#     select(-contains("roll_"), -contains("_K1"), -contains("_lag"))
# 
# forecast_accuracy_tbl <- forecast_artifacts_list$data$fit_models_accuracy_tbl
# 
# future_forecast_tbl %>% glimpse()
# future_forecast_tbl %>% distinct(.model_desc)
# future_forecast_tbl %>% distinct(.key)

custom_axis_theme <- function(){
  
  theme(
    axis.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 9),
    plot.title = element_text(size = 18, color = "black", face = "bold")
  )
}


# ******************************************************************************
# DATA PREP: SUMMARISE BY TIME ----
# ******************************************************************************
# start_date <- as.Date("2011-01-01")
# data       <- future_forecast_tbl

get_forecast_data <- function(data, 
                                         .country         = "United Kingdom",
                                         .forecast_days   = 90,
                                         .lookback_months = 11){
  
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
  lb_rows_to_keep   <- (.lookback_months * 30) + .forecast_days
  lb_rows_to_remove <- (row_count - lb_rows_to_keep)
  lb_rows_to_keep   <- (row_count - lb_rows_to_remove)
  
  data_prep <- data_prep %>% tail(lb_rows_to_keep)
  
  data_prep <- data_prep %>% 
    mutate(.model_desc = case_when(
      .model_desc == "ACTUAL" ~ paste("Trailing", .lookback_months, "Months", sep = " "),
      TRUE                    ~ paste(.forecast_days, "Day Future Forecast")
    )) %>% 
    mutate(country = .country) %>% 
    mutate(text = str_glue(
      "
    Date: {invoice_date}
    Country: {country}
    Key: {.model_desc}
    Quantity Sold: {scales::comma(.value, accuracy = 1)}
    "
    )) %>% 
    mutate(.model_desc = .model_desc %>% fct_rev()) 
  
  return(data_prep)
    
}

data <-
  future_forecast_tbl %>%
  get_forecast_data("United Kingdom", .forecast_days = 30, .lookback_months = 3) %>%
  select(-text)


# ******************************************************************************
# PLOT TIME SERIES ----
# ******************************************************************************

get_time_series_plot <- function(data){
  
  p <- data %>% 
    ggplot(aes(invoice_date, .value, color = .model_desc))+
    geom_line(size = 0.7)+
    geom_point(aes(text = text), size = 0.01)+
    expand_limits(y = 0)+
    scale_y_continuous(labels = scales::comma_format(accuracy = 1))+
    theme_minimal()+
    custom_axis_theme()+
    theme(
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )+
    scale_color_manual(values = c("#78c2ad", "red"))+
    labs(y = NULL, x = NULL, color = "Legend")
  
  
  p <- plotly::ggplotly(p, tooltip = "text") %>% 
    layout(legend = list(orientation = "h", xanchor = "center", x = 0.5))
  
  return(p)
  
}

# future_forecast_tbl %>% get_forecast_data() %>% get_time_series_plot()

# ******************************************************************************
# DATA PREP: DATATABLE ----
# ******************************************************************************
get_forecast_data_dt <- function(uk_data, others_data){
  
  uk_tbl <- uk_data %>% 
    select(-text) %>% 
    select(invoice_date, .model_desc, .value) %>% 
    mutate(.value = ifelse(.value < 5, 0, .value)) %>% 
    rename(uk_forecast = .value)
  
  others_tbl <- others_data %>% 
    select(-text) %>% 
    select(invoice_date, .model_desc, .value) %>% 
    mutate(.value = ifelse(.value < 5, 0, .value)) %>% 
    rename(others_forecast = .value) %>% 
    select(-invoice_date, -.model_desc)
  
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
    ) %>% 
    select(invoice_date, .model_desc, everything(.)) %>% 
    rename(model_desc = .model_desc) %>% 
    setNames(names(.) %>% str_replace_all("_", " ") %>% str_to_upper()) 
  
  return(data_prep)
    
}

# get_forecast_data_dt(
#   uk_data     = get_forecast_data(data = future_forecast_tbl, .country = "United Kingdom"),
#   others_data = get_forecast_data(data = future_forecast_tbl, .country = "All Others") 
#   
# )

# future_forecast_tbl %>% get_forecast_data_dt()



  
    
    




