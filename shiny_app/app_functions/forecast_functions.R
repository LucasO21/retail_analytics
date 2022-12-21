# FUNCTIONS FOR FORECASTING ----
# **** ----

# # Working Dir ----
# setwd(here::here("shiny_app"))
# 
# # Libraries ----
# library(tidyverse)
# library(timetk)
# library(modeltime)

# ******************************************************************************
# Data Import ----
# ******************************************************************************
# forecast_artifacts_list <- read_rds("app_artifacts/forecast_artifacts_list.rds")
# 
# future_forecast_tbl <- forecast_artifacts_list$data$future_forecast_tbl %>% 
#     select(-contains("roll_"), -contains("_K1"), -contains("_lag"))
# 
# future_forecast_tbl %>% glimpse()
# future_forecast_tbl %>% distinct(.model_desc)
# future_forecast_tbl %>% distinct(.key)


# ******************************************************************************
# Data Prep: Summarize By Time ----
# ******************************************************************************
# start_date <- as.Date("2011-01-01")
# data       <- future_forecast_tbl

get_forecast_data <- function(data, 
                              .country = NULL){
    
    if(is.null(.country)){
        data_prep <-  data 
    }else{
        data_prep <-  data %>% 
            filter(country == .country)
    }
    
    data_prep <- data_prep %>% 
        filter(.model_desc %in% c("ACTUAL", "RANGER")) %>% 
        group_by(.model_id, .model_desc, .key, .index) %>% 
        summarise_by_time(
            .date_var = invoice_date,
            .by = "day",
            .value = sum(.value)
        ) %>% 
        ungroup() %>% 
        filter(.index >= start_date)
    
    return(data_prep)
    
    
}

# future_forecast_tbl %>% get_forecast_data("United Kingdom")


# ******************************************************************************
# Data Prep: Datatable ----
# ******************************************************************************
get_forecast_data_dt <- function(data){
    
    data %>% 
        get_forecast_data() %>% 
        bind_cols(
            get_forecast_data(
                data = data,
                .country = "United Kingdom"
            ) %>% 
                select(.value) %>% 
                rename(forecast_uk = .value)
        ) %>% 
        bind_cols(
            get_forecast_data(
                data = data,
                .country = "All Others"
            ) %>% 
                select(.value) %>% 
                rename(forecast_others = .value)
        ) %>% 
        select(-.model_id, -.index) %>% 
        select(invoice_date, everything()) %>% 
        mutate(
            forecast_uk_pct = forecast_uk / .value,
            forecast_others_pct = forecast_others / .value 
        ) %>% 
        mutate(
            .value = scales::dollar(.value, accuracy = 1),
            forecast_uk = scales::dollar(forecast_uk, accuracy = 1),
            forecast_others = scales::dollar(forecast_others, accuracy = 1),
            forecast_uk_pct = scales::percent(forecast_uk_pct, accuracy = 0.1),
            forecast_others_pct = scales::percent(forecast_others_pct, accuracy = 0.1)
        ) %>%
        setNames(names(.) %>% str_replace_all("_", " ") %>% str_to_title()) %>% 
        rename(
            "Model"        = ".Model Desc",
            "Key"          = ".Key",
            "Forecast All" = ".Value"
        ) %>% 
        mutate_if(is.factor, as.character)
    
}

# future_forecast_tbl %>% get_forecast_data_dt()



  
    
    




