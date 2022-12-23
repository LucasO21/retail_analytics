# RETAIL ANALYTICS PROJECT ----
# FORECAST SCRIPT ----
# **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Set Working Dir ----
setwd(here::here("R"))

# * Libraries ----

# ** Core ----
library(tidyverse)
library(janitor)
library(timetk)
library(lubridate)
library(DBI)

# ** Modeling ----
library(tidymodels)
library(modeltime)
library(rules)

# ** Parallel Processing ----
library(tictoc)
library(doFuture)
library(parallel)


# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")
dbListTables(con)

# * First Purchase Data ----
first_purchase_tbl <- tbl(con, "first_purchase_tbl") %>% collect() 

# * Load Sales Data ----
retail_data_clean_tbl <- tbl(con, "retail_data_clean_tbl") %>% 
    collect() %>% 
    mutate(invoice_date = lubridate::date(invoice_date)) %>% 
    filter(invoice_date <= as.Date("2011-11-30"))


# ******************************************************************************
# DATA PREP ----
# ******************************************************************************

# * Sales by Country ----
retail_data_clean_tbl %>% 
    group_by(country) %>% 
    summarise(
        total_quantity = sum(quantity),
        total_sales    = sum(sales)
    ) %>% 
    ungroup() %>% 
    arrange(desc(total_sales)) %>% 
    mutate(
        pct_total_qty = total_quantity/sum(total_quantity),
        pct_total_sales = total_sales/sum(total_sales)
    )

# * Summarize by Time ----
daily_quantity_tbl <- retail_data_clean_tbl %>% 
    mutate(country = ifelse(country == "United Kingdom", "United Kingdom", "All Others")) %>% 
    group_by(country) %>% 
    summarize_by_time(invoice_date, "day", total_quantity = sum(quantity)) %>% 
    ungroup()

# * Checking Quantity Sold by Weekday ----
daily_quantity_tbl %>% 
    mutate(wday = lubridate::wday(invoice_date, label = TRUE)) %>% 
    group_by(country, wday) %>% 
    summarise(total_quantity = sum(total_quantity)) %>% 
    arrange(desc(total_quantity), .by_group = TRUE) %>% 
    ungroup()


# * Sales Trend ----
daily_quantity_tbl %>% 
    group_by(country) %>% 
    plot_time_series(invoice_date, .value = total_quantity, .facet_ncol = 1)

# * ACF & PACF
daily_quantity_tbl %>% 
    filter(country == "United Kingdom") %>% 
    plot_acf_diagnostics(invoice_date, total_quantity)

# * Params ----
FORECAST_TIMEFRAME <- "day"
FORECAST_HORIZON   <- 90

# * Full Data ----
full_data_tbl <- daily_quantity_tbl %>% 
    mutate_if(is.character, as.factor) %>% 
    
    # Transformations
    mutate(total_quantity = log1p(total_quantity)) %>% 
    
    # Apply Time Series Engineering
    group_by(country) %>% 
    arrange(invoice_date) %>% 
    pad_by_time(invoice_date, .by = FORECAST_TIMEFRAME, .pad_value = 0) %>% 
    
    # Extend Into Future
    future_frame(invoice_date, .length_out = FORECAST_HORIZON, .bind_data = TRUE) %>% 
    
    # Add Time Series Features
    tk_augment_fourier(invoice_date, .periods = c(7, 14, 21, 28)) %>% 
    tk_augment_lags(total_quantity, .lags = FORECAST_HORIZON) %>% 
    tk_augment_slidify(
        .value   = total_quantity_lag90,
        .f       = ~ mean(.x, na.rm = TRUE),
        .period  = c(7, 14, 21, 28),
        .partial = TRUE,
        .align   = "center"
    ) %>% 
    ungroup() %>% 
    rowid_to_column(var = "row_id")

# * Data Prepared ----
data_prepared_tbl <- full_data_tbl %>% 
    filter(! is.na(total_quantity)) %>% 
    drop_na()

# * Future Data ----
future_data_tbl <- full_data_tbl %>% 
    filter(is.na(total_quantity))


# ******************************************************************************
# TIME SPLIT ----
# ******************************************************************************
splits <- data_prepared_tbl %>% 
    time_series_split(invoice_date, assess = FORECAST_HORIZON, cumulative = TRUE)

train_tbl <- training(splits)
test_tbl  <- testing(splits)

# ******************************************************************************
# CLEAN OUTLIERS ----
# ******************************************************************************
train_cleaned_tbl <- train_tbl %>% 
    filter(country == "All Others") %>% 
    mutate(total_quantity_cleaned = ts_clean_vec(total_quantity, period = 7)) %>% 
    mutate(total_quantity = ifelse(invoice_date %>% between_time("2010-03-16", "2010-03-19"),
                                   total_quantity_cleaned, total_quantity)) %>% 
    mutate(total_quantity = ifelse(invoice_date %>% between_time("2010-08-08", "2010-08-10"),
                                   total_quantity_cleaned, total_quantity)) %>% 
    select(-total_quantity_cleaned) %>% 
    bind_rows(
        train_tbl %>% 
            filter(country == "United Kingdom") %>% 
            mutate(total_quantity_cleaned = ts_clean_vec(total_quantity, period = 7)) %>% 
            mutate(total_quantity = ifelse(invoice_date %>% between_time("2010-03-22", "2010-03-24"),
                                           total_quantity_cleaned, total_quantity)) %>% 
            mutate(total_quantity = ifelse(invoice_date %>% between_time("2010-09-26", "2010-09-28"),
                                           total_quantity_cleaned, total_quantity)) %>% 
            mutate(total_quantity = ifelse(invoice_date %>% between_time("2011-01-17", "2011-01-19"),
                                           total_quantity_cleaned, total_quantity)) %>% 
            select(-total_quantity_cleaned)
    ) %>% 
    group_by(country) %>% 
    arrange(invoice_date) %>% 
    ungroup()

# ******************************************************************************
# RECIPE ----
# ******************************************************************************
recipe_spec <- recipe(total_quantity ~., data = train_cleaned_tbl) %>% 
    step_timeseries_signature(invoice_date) %>% 
    step_rm(matches("(.iso)|(.xts)|(hour)|(minute)(second)(am.pm)")) %>% 
    step_dummy(all_nominal(), one_hot = TRUE) %>% 
    step_normalize(invoice_date_index.num, invoice_date_year)

recipe_spec %>% prep() %>% juice() %>% glimpse()

# ******************************************************************************
# MODELING ----
# ******************************************************************************

# * Prophet ----
wflw_fit_prophet <- workflow() %>% 
    add_model(
        spec = prophet_reg() %>% set_engine("prophet")
    ) %>% 
    add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * Prophet Boost ----
wflw_fit_phrophet_boost <- workflow() %>% 
    add_model(
        spec = prophet_boost(
            seasonality_daily = F,
            seasonality_weekly = F,
            seasonality_yearly = F
        ) %>% set_engine("prophet_xgboost")
    ) %>% add_recipe(recipe_spec) %>% 
    fit(train_tbl)

# * Xgboost ----
wflw_fit_xgboost <- workflow() %>% 
    add_model(
        spec = boost_tree() %>% set_mode("regression") %>% set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spec %>% step_rm(invoice_date)) %>% 
    fit(train_tbl)

# *  Random Forest ----
wflw_fit_ranger <- workflow() %>% 
    add_model(
        spec = rand_forest(mode = "regression") %>% set_engine("ranger")
    ) %>% 
    add_recipe(recipe_spec %>% step_rm(invoice_date)) %>% 
    fit(train_tbl)

# * Cubist ----
wflw_fit_cubist <- workflow() %>% 
    add_model(
        spec = cubist_rules() %>% set_engine("Cubist")
    ) %>% 
    add_recipe(recipe_spec %>% step_rm(invoice_date)) %>% 
    fit(train_tbl)

# * Mars ----
wflw_fit_mars <- workflow() %>% 
    add_model(
        spec = mars(mode = "regression") %>% set_engine("earth")
    ) %>% 
    add_recipe(recipe_spec %>% step_rm(invoice_date)) %>% 
    fit(train_tbl)

# * Accuracy Check (Fit Models) ----
fit_models_tbl <- modeltime_table(
    wflw_fit_prophet,
    wflw_fit_xgboost,
    wflw_fit_phrophet_boost,
    wflw_fit_ranger,
    wflw_fit_cubist,
    wflw_fit_mars
)

fit_models_accuracy_tbl <- fit_models_tbl %>% 
    modeltime_accuracy(test_tbl) %>% 
    arrange(rmse) %>% 
    select(-mape, -smape)


# ******************************************************************************
# VISUALIZING FORECASTS ----
# ******************************************************************************

# * Get Test Forecast Data ----
fit_models_calibrate_tbl <- fit_models_tbl %>% 
    modeltime_calibrate(new_data = test_tbl)

test_forecast_tbl <- fit_models_calibrate_tbl %>% 
    modeltime_forecast(
        new_data     = test_tbl,
        actual_data  = data_prepared_tbl,
        keep_data    = TRUE
    )

# * Visualizing Forecasts ----
test_forecast_tbl %>% 
    filter(invoice_date >= as.Date("2011-01-01")) %>% 
    group_by(country) %>% 
    plot_modeltime_forecast(.conf_interval_show = FALSE)


# ******************************************************************************
# HYPER PARAMETER TUNING ----
# ******************************************************************************

# * K FOLD Resamples ----
set.seed(123)
resamples_kfold <- train_cleaned_tbl %>% vfold_cv(v = 5)

# * Parallel Processing ----
registerDoFuture()
n_cores <- 4
plan(strategy = cluster, workers = makeCluster(n_cores))


# ******************************************************************************
# FUTURE FORECASTS ----
# ******************************************************************************

# * Future Forecast Data ----
future_forecast_tbl <- fit_models_calibrate_tbl %>% 
    modeltime_forecast(
        new_data    = future_data_tbl,
        actual_data = data_prepared_tbl, 
        keep_data   = TRUE
    ) %>% 
    mutate(
        .value         = expm1(.value), 
        total_quantity = expm1(total_quantity),
        .conf_lo       = expm1(.conf_lo),
        .conf_hi       = expm1(.conf_hi)
    )


# * Visualize Future Forecast ----
future_forecast_tbl %>% 
    filter(invoice_date >= as.Date("2011-05-01")) %>% 
    group_by(country) %>% 
    plot_modeltime_forecast(
        .facet_ncol     = 1
    )

# ******************************************************************************
# SAVE FORECAST ARTIFACTS ----
# ******************************************************************************

forecast_artifacts_list <- list(
    
    # models
    models = list(fit_models_tbl),
    
    # data
    data = list(
        test_forecast_tbl       = test_forecast_tbl,
        future_forecast_tbl     = future_forecast_tbl,
        fit_models_accuracy_tbl = fit_models_accuracy_tbl
    )
    
)

forecast_artifacts_list %>% write_rds("../artifacts/forecast_artifacts_list.rds")


# ******************************************************************************
# SAVE FORECAST ARTIFACTS ----
# ******************************************************************************




