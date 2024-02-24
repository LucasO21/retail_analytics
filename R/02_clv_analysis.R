# RETAIL ANALYTICS PROJECT ----
# CLV ANALYSIS SCRIPT ----
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
library(data.table)
library(dtplyr)
library(DBI)

# ** Modeling ----
library(tidymodels)
library(xgboost)
library(vip)

# ** Parallel Processing ----
# library(future)
# library(doFuture)
# library(parallel)
# library(tictoc)


# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")
dbListTables(con)

# * First Purchase Data ----
first_purchase_tbl <- tbl(con, "first_purchase_tbl") %>% 
    collect() %>% 
    mutate(across(first_purchase_date:first_purchase_quarter, ~ date(.)))

first_purchase_tbl %>% 
    select(customer_id, first_purchase_cohort) %>% 
    distinct() %>% 
    count(first_purchase_cohort, sort = TRUE)

# * Load Sales Data ----
retail_data_clean_tbl <- tbl(con, "retail_data_clean_tbl") %>% 
    collect() %>% 
    mutate(invoice_date = lubridate::date(invoice_date)) %>% 
    filter(invoice_date <= as.Date("2011-11-30")) %>% 
    mutate(description = gsub("\"", "", description))

#' Filtering retail_data_clean_tbl for invoice_date <= 2011-11-30
#' Purchase cohorts after that date have not reached the 90 day maturity


# ******************************************************************************
# DATA PREP ----
# ******************************************************************************

# * Pick Analysis Cohort ----
# analysis_cohort <- "Q1-2010"

# * Get Analysis Cohort ----
analysis_cohort_tbl <- retail_data_clean_tbl %>% 
    left_join(
        first_purchase_tbl %>% 
            select(customer_id, first_purchase_cohort) 
    ) %>% 
    filter(first_purchase_cohort %in% analysis_cohort)

analysis_cohort_tbl %>% pull(invoice_date) %>% range()


# ******************************************************************************
# MODELING ----
# ******************************************************************************

# - Problem Statement:
# - What will the customers spend in the next 90 days? (Regression)
# - What is the prob of a customer making a purchase in the next 90 days (Classification)

# * Split Data ----
set.seed(123)
ids_train <- analysis_cohort_tbl %>%
    pull(customer_id) %>%
    unique() %>%
    sample(size = round(0.8*length(.))) %>%
    sort()

split_1_train_tbl <- analysis_cohort_tbl %>%
    filter(customer_id %in% ids_train)

split_1_test_tbl  <- analysis_cohort_tbl %>%
    filter(!customer_id %in% ids_train)

splits_2_train <- time_series_split(
    data       = split_1_train_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

splits_2_test <- time_series_split(
    data       = split_1_test_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

# splits_2_train %>%
#     tk_time_series_cv_plan() %>%
#     plot_time_series_cv_plan(invoice_date, sales)


# * Make Target Data ----

# ** Make In-Sample Targets for Training Data ----
targets_train_tbl <- testing(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(sales),
        spend_90_flag  = 1
    )

# ** Make Out-Sample Targets for Testing Data ----
targets_test_tbl <- testing(splits_2_test) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(sales),
        spend_90_flag  = 1
    )


# * Make Training Data ----
#   - RFM Features: Recency, Frequency, Monetary

# ** Max Train Data Invoice Date ----
max_date_train <- training(splits_2_train) %>%
    pull(invoice_date) %>%
    max()

train_tbl <- training(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        recency     = (max(invoice_date) - max_date_train) / ddays(1),
        frequency   = n(),
        sales_sum   = sum(sales, na.rm = TRUE),
        sales_mean  = mean(sales, na.rm = TRUE)
    ) %>%
    left_join(
        targets_train_tbl
    ) %>%
    replace_na(replace = list(
        spend_90_total = 0,
        spend_90_flag  = 0
    )
    ) %>% 
    mutate(spend_90_flag = as.factor(spend_90_flag))
    


# ** Make Testing Data ----
#    - Repeat for testing data
test_tbl <- training(splits_2_test) %>%
    group_by(customer_id) %>%
    summarise(
        recency     = (max(invoice_date) - max_date_train) / ddays(1),
        frequency   = n(),
        sales_sum   = sum(sales, na.rm = TRUE),
        sales_mean  = mean(sales, na.rm = TRUE)
    ) %>%
    left_join(
        targets_test_tbl
    ) %>%
    replace_na(replace = list(
        spend_90_total = 0,
        spend_90_flag  = 0
    )
    ) %>%
    mutate(spend_90_flag = as.factor(spend_90_flag))

retail_data_clean_tbl %>% 
    filter(! customer_id %in% ids_train) %>% 
    group_by(customer_id) %>% 
    summarise(
        recency     = (max(invoice_date) - max_date_train) / ddays(1),
        frequency   = n(),
        sales_sum   = sum(sales, na.rm = TRUE),
        sales_mean  = mean(sales, na.rm = TRUE)
    )
    


# * Recipes ----

# ** Recipe 1: 90-Day Spend Prediction ----
recipe_spend_total <- recipe(spend_90_total ~ ., data = train_tbl) %>%
    step_rm(spend_90_flag, customer_id)

# ** Recipe 2: 90-Day Spend Probability ----
recipe_spend_prob <- recipe(spend_90_flag ~ ., data = train_tbl) %>%
    step_rm(spend_90_total, customer_id)


# * MODELS ----

# ** Model 1: 90-Day Spend Prediction ----
wflw_spend_total_xgb <- workflow() %>%
    add_model(boost_tree(mode = "regression") %>% set_engine("xgboost")) %>%
    add_recipe(recipe_spend_total) %>%
    fit(train_tbl)

# ** Model 2: 90-Day Spend Probability ----
wflw_spend_prob_xgb <- workflow() %>%
    add_model(boost_tree(mode = "classification") %>% set_engine("xgboost")) %>%
    add_recipe(recipe_spend_prob) %>%
    fit(train_tbl)


# * Accuracy Check ----

# ** Predictions Data ----
predictions_test_tbl <-  bind_cols(
    
    predict(wflw_spend_total_xgb, test_tbl) %>%
        rename(.pred_total = .pred),
    
    predict(wflw_spend_prob_xgb, test_tbl, type = "prob") %>%
        select(.pred_1) %>%
        rename(.pred_prob = .pred_1)
) %>%
    bind_cols(test_tbl) %>%
    select(starts_with(".pred"), starts_with("spend_"), everything()) %>% 
    mutate(spend_actual_vs_pred = spend_90_total - .pred_total)

# ** Accuracy Metrics ----
predictions_test_tbl %>%
    yardstick::mae(spend_90_total, .pred_total)

predictions_test_tbl %>%
    yardstick::roc_auc(spend_90_flag, .pred_prob, event_level = "second")

predictions_test_tbl %>%
    yardstick::roc_curve(spend_90_flag, .pred_prob, event_level = "second")%>%
    autoplot()


# * VIP ----
vip(wflw_spend_prob_xgb$fit$fit)

vip(wflw_spend_total_xgb$fit$fit)


# ******************************************************************************
# FORMAT PREDICTIONS TABLE ----
# ******************************************************************************

# ******************************************************************************
# CONCLUSIONS / RECOMMENDATIONS ----
# ******************************************************************************

# * Conclusions: VIP ----
#'   The most important features for predicting 90-day spend are price sum, recency.
#'   If the goal is to get customers to spend more, then focus on the customers who
#'   have been spending more and more recently in the last 90 days.
#'   
#'   Conversely the most important features for predicting 90-day spend prob are
#'   recency and frequency. If the goal is to retain customers, then focus on increasing 
#'   recency and frequency.


# * Recommendations: Spend ----
#' 1 -  Which customers have the highest spend probability in the next 90 days?
#'    - Target for new products similar to what they have purchased in the past.
#'    - Data: sort by .pred_prob descending
#'  
#' 2 - Which customers have recently purchased but are unlikely to buy?
#'   - Incentive actions to increase spend prob
#'   - Provide discounts, encourage referring a friend, nurture by letting them know what's coming
#'   - Data: filter by recency < 90 and .pred_prob < 0.2, sort by .pred_prob descending
#'   
#' 3 - Which customers are missed opportunities
#'   - Send bundle offers encouraging volume purchases
#'   - Data: filter by spend_90_total == 0, sort by .pred_total descending

# ******************************************************************************
# SAVE ARTIFACTS ----
# ******************************************************************************

# * Recommendations ----

clv_artifacts_list <- list(
    
    # models
    models = list(
        spend_model  = wflw_spend_total_xgb,
        prob_model = wflw_spend_prob_xgb
    ),
    
    # data
    data = list(
        train_data       = train_tbl,
        test_data        = test_tbl,
        predictions_data = predictions_test_tbl
    )
)

# clv_artifacts_list %>% write_rds("../artifacts/clv_artifacts_list.rds")

clv_artifacts_list_saved <- read_rds("../artifacts/clv_artifacts_list.rds")



