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
library(brevis)
library(tidymodels)
library(vip)


# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")
dbListTables(con)

# * First Purchase Data ----
first_purchase_tbl <- tbl(con, "first_purchase_tbl") %>% collect() 

# * RFM Segment Data ----
rfm_segment_tbl <- tbl(con, "rfm_segment_tbl") %>% collect()

# * Load Sales Data ----
sales_tbl <- tbl(con, "retail_data_clean_tbl") %>% 
    collect() %>% 
    mutate(invoice_date = date(invoice_date)) %>% 
    mutate(invoice_date = ymd(invoice_date)) %>% 
    mutate(sales = quantity * price) 


# ******************************************************************************
# DATA PREP ----
# ******************************************************************************

# * Get Analysis Cohort ----
cohort_tbl <- sales_tbl %>% 
    left_join(first_purchase_tbl %>% select(customer_id, flag)) %>% 
    filter(flag == "Old") %>% 
    select(-flag)

# * Summarise Sales by Quarter ----
sales_quarterly_tbl <- cohort_tbl %>% 
    lazy_dt() %>% 
    mutate(
        quarter = paste(
            paste0("Q",lubridate::quarter(invoice_date)),
            lubridate::year(invoice_date), 
            sep = "_"
        )
    ) %>% 
    group_by(customer_id, quarter) %>% 
    summarise(
        sum_sales      = sum(sales),
        avg_sales      = mean(sales),
        count          = n()
    ) %>% 
    ungroup() %>% 
    as_tibble() %>% 
    mutate(quarter = quarter %>% fct_relevel(
        "Q4_2009", "Q1_2010", "Q2_2010", "Q3_2010", "Q4_2010", "Q1_2011",
        "Q2_2011", "Q3_2011", "Q4_2011"
    ))

quarter_tbl <- sales_quarterly_tbl %>% 
    distinct(quarter) %>% 
    arrange(quarter) %>% 
    bind_cols(
        tibble(
            q = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")
        )
    ) %>% 
    mutate(quarter = factor(quarter))

# * Summarise Sales by Quarter (Wide) ----
#   - change quarterly data from long to wide
#   - use most recent quarter as target and prior 3 quarters as predictors
    
sales_quarterly_wide_tbl <- sales_quarterly_tbl %>% 
    left_join(quarter_tbl) %>% 
    rename(quarter_no = q) %>% 
    filter(quarter_no %in% c("Q9", "Q8", "Q7", "Q6")) %>% 
    select(-quarter) %>% 
    pivot_longer(
        cols = c(sum_sales, avg_sales, count)
    ) %>% 
    mutate(flag = case_when(
        quarter_no == "Q9" & name == "avg_sales" ~ 1,
        quarter_no == "Q9" & name == "count"     ~ 1,
        TRUE                                     ~ 0
    )) %>% 
    filter(flag == 0) %>% 
    select(-flag) %>% 
    mutate(name_2 = paste(name, quarter_no, sep = "_")) %>% 
    select(-name, -quarter_no) %>% 
    pivot_wider(names_from = name_2, values_from = value, values_fill = 0)

# * Prep Data For Modeling ----
#   - add rfm segment and recency as additional predictors
model_ready_tbl <- sales_quarterly_wide_tbl %>% 
    left_join(
        rfm_segment_tbl %>% 
            select(customer_id, recency_days, segment)
    ) %>% 
    rename(sum_sales_3m = sum_sales_Q9) %>% 
    mutate(flag_sales_3m = 1) %>% 
    select(customer_id, starts_with("sum_sales_Q"), starts_with("avg"),
           starts_with("count"), recency_days, segment, everything())

model_ready_tbl %>% glimpse()
model_ready_tbl %>% count(flag_sales_3m)

dbWriteTable(con, "model_ready_tbl", model_ready_tbl, overwrite = TRUE)


# ******************************************************************************
# MODELING ----
# ******************************************************************************
# - Problem Statement:
# - What will the customers spend in the next 90 days? (Regression)
# - What is the prob of a customer making a purchase in the next 90 days (Classification)

# * Data Splitting ----
set.seed(100)
data_split <- initial_split(model_ready_tbl, prop = 0.80, strata = sum_sales_3m)

train_tbl <- training(data_split)
test_tbl  <- testing(data_split)


# * Recipes ----

# ** Regression Recipe ----
recipe_spec_sum <- recipe(sum_sales_3m ~ ., data = train_tbl) %>% 
    step_rm(customer_id, flag_sales_3m) %>% 
    step_zv(all_predictors()) %>% 
    step_dummy(segment, one_hot = TRUE)

# recipe_spec_sum %>% prep() %>% juice() %>% glimpse()


# ** Classification Recipe ----
recipe_spec_flag <- recipe(flag_sales_3m ~ ., data = train_tbl) %>% 
    step_rm(customer_id, sum_sales_3m) %>% 
    step_zv(all_predictors()) %>% 
    step_dummy(segment, one_hot = TRUE)

# recipe_spec_flag %>% prep() %>% juice() %>% glimpse()


# * Model Specs / Fits ----

# ** Regression Spec ----
wflw_fit_sum_xgboost <- workflow() %>% 
    add_model(spec = boost_tree("regression") %>% set_engine("xgboost")) %>% 
    add_recipe(recipe_spec_sum) %>% 
    fit(train_tbl)

get_baseline_fit_metrics_regression(
    wflw_fit_sum_xgboost, train_tbl, "sum_sales_3m", "Xgboost"
)

# ** Classification Spec ----
wflw_fit_flag_xgboost <- workflow() %>% 
    add_model(spec = boost_tree("classification") %>% set_engine("xgboost")) %>% 
    add_recipe(recipe_spec_flag) %>% 
    fit(train_tbl %>% mutate(flag_sales_3m = as.factor(flag_sales_3m)))

# get_baseline_fit_metrics_classification(
#     .model       = wflw_fit_flag_xgboost, 
#     .data        = train_tbl, 
#     .target      = "flag_sales_3m", 
#     .event_level = "first",
#     .return      = "metrics",
#     .model_name  = "Xgboost" 
# )

predictions_test_tbl <- predict(wflw_fit_flag_xgboost, new_data = test_tbl) %>% 
    bind_cols(
        predict(wflw_fit_flag_xgboost, test_tbl, type = "prob") %>% 
            select(.pred_1) %>%
            rename(.pred_prob = .pred_1)
    ) %>% 
    bind_cols(
        predict(wflw_fit_sum_xgboost, test_tbl) %>% 
            rename(.pred_sum = .pred)
    ) %>% 
    bind_cols(test_tbl %>% select(customer_id, segment, sum_sales_3m, flag_sales_3m))

predictions_test_tbl %>% yardstick::mae(.pred_sum, sum_sales_3m)

predictions_test_tbl %>% 
    yardstick::roc_curve(factor(flag_sales_3m), .pred_prob, event_level = "second") %>% 
    autoplot()

metrics_list <- yardstick::metric_set(
    yardstick::accuracy, yardstick::sens, yardstick::spec, yardstick::j_index,
    yardstick::f_meas, yardstick::mcc
)

metrics_list(predictions_test_tbl, truth = )


pred_tbl <- bind_cols(
    predict(wflw_fit_flag_xgboost, test_tbl),
    predict(wflw_fit_flag_xgboost, test_tbl, type = "prob"),
    test_tbl %>% select(flag_sales_3m)
) %>% 
    mutate(flag_sales_3m = as.factor(flag_sales_3m))
    
metrics_tbl <- metrics_list(pred_tbl, truth = flag_sales_3m, estimate = .pred_class,
                            event_level = "first")



# Set Cohort Span ----
cohort_tbl %>% 
    pull(invoice_date) %>% 
    range()

# Split Data ----
set.seed(123)
ids_train <- cohort_tbl %>%
    pull(customer_id) %>%
    unique() %>%
    sample(size = round(0.8*length(.))) %>%
    sort()

split_1_train_tbl <- cohort_tbl %>%
    filter(customer_id %in% ids_train)

split_1_test_tbl  <- cohort_tbl %>%
    filter(!customer_id %in% ids_train)


splits_2_train <- time_series_split(
    split_1_train_tbl,
    assess     = "90 days",
    cumulative = TRUE
)


splits_2_test <- time_series_split(
    split_1_test_tbl,
    assess     = "90 days",
    cumulative = TRUE
)

# ** Make in-sample targets from training data ----
targets_train_tbl <- testing(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(price),
        spend_90_flag    = 1
    )

targets_test_tbl <- testing(splits_2_test) %>%
    group_by(customer_id) %>%
    summarise(
        spend_90_total = sum(price),
        spend_90_flag    = 1
    )

max_date_train <- training(splits_2_train) %>%
    pull(invoice_date) %>%
    max()

train_tbl <- training(splits_2_train) %>%
    group_by(customer_id) %>%
    summarise(
        recency   = (max(invoice_date) - max_date_train) / ddays(1),
        frequency = n(),
        price_sum   = sum(price, na.rm = TRUE),
        price_mean  = mean(price, na.rm = TRUE)
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

