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
library(vip)


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

retail_data_clean_tbl %>% pull(invoice_date) %>% range()


# ******************************************************************************
# DATA PREP ----
# ******************************************************************************

# * Get Analysis Cohort ----
cohort_tbl <- retail_data_clean_tbl %>% 
    left_join(first_purchase_tbl %>% select(customer_id, flag)) %>% 
    filter(flag == "Old") %>% 
    select(-flag) %>% 
    filter(invoice_date >= as.Date("2010-12-01"))

cohort_tbl %>% pull(invoice_date) %>% range()

# * Sales by Customer & Invoice ----
sales_by_customer_invoice_summary_tbl <- cohort_tbl %>% 
    group_by(customer_id, invoice) %>% 
    summarise(
        sales        = sum(sales),
        invoice_date = max(invoice_date)
    ) %>% 
    ungroup()

# * Sales by Customer & Customer ----
sales_by_customer_quarterly_tbl <- sales_by_customer_invoice_summary_tbl %>% 
    mutate(quarter_start = round_date(invoice_date, "3 months") %>% as.character()) %>% 
    group_by(customer_id, quarter_start) %>% 
    summarise(
        sales_sum   = sum(sales),
        sales_avg   = mean(sales),
        sales_count = n()
    ) %>% 
    ungroup() %>% 
    mutate(quarter = case_when(
        quarter_start == "2010-10-01" ~ "Q1",
        quarter_start == "2011-01-01" ~ "Q2",
        quarter_start == "2011-04-01" ~ "Q3",
        quarter_start == "2011-07-01" ~ "Q4",
        quarter_start == "2011-10-01" ~ "Q5"
    )) %>% 
    mutate(quarter = quarter %>% fct_relevel("Q1", "Q2", "Q3", "Q4", "Q5")) %>% 
    select(-quarter_start) %>% 
    select(customer_id, quarter, everything(.))

sales_by_customer_quarterly_tbl %>% distinct(quarter) %>% pull()


# * Features Data ----
features_tbl <- sales_by_customer_quarterly_tbl %>% 
    filter(!quarter == "Q5") %>% 
    select(customer_id, quarter, sales_sum) %>% 
    mutate(quarter = paste("sales_sum", quarter, sep = "_")) %>% 
    pivot_wider(names_from = quarter, values_from = sales_sum, values_fill = 0) %>% 
    left_join(
        sales_by_customer_quarterly_tbl %>% 
            filter(!quarter == "Q5") %>% 
            select(customer_id, quarter, sales_avg) %>% 
            mutate(quarter = paste("sales_avg", quarter, sep = "_")) %>% 
            pivot_wider(names_from = quarter, values_from = sales_avg, values_fill = 0)
    ) %>% 
    left_join(
        sales_by_customer_quarterly_tbl %>% 
            filter(!quarter == "Q5") %>%
            select(customer_id, quarter, sales_count) %>% 
            mutate(quarter = paste("sales_count", quarter, sep = "_")) %>% 
            pivot_wider(names_from = quarter, values_from = sales_count, values_fill = 0)
    )


# * Target Data ----
target_tbl <- sales_by_customer_quarterly_tbl %>% 
    filter(quarter == "Q5") %>% 
    select(customer_id, sales_sum) %>% 
    rename(sales_sum_3m = sales_sum) %>% 
    mutate(sales_flag_3m = 1)


# * Modeling Data ----
modeling_tbl <- features_tbl %>% 
    left_join(target_tbl) %>% 
    mutate(sales_sum_3m = ifelse(is.na(sales_sum_3m), 0, sales_sum_3m)) %>% 
    mutate(sales_flag_3m = ifelse(is.na(sales_flag_3m), 0, sales_flag_3m)) %>% 
    mutate(sales_flag_3m = as.factor(sales_flag_3m))



# ******************************************************************************
# MODELING ----
# ******************************************************************************

# - Problem Statement:
# - What will the customers spend in the next 90 days? (Regression)
# - What is the prob of a customer making a purchase in the next 90 days (Classification)

# * Split Data ----
set.seed(123)
split_spec <- initial_split(modeling_tbl, strata = sales_sum_3m)

train_tbl <- training(split_spec)
test_tbl  <- testing(split_spec)

# * Recipes ----
recipe_spec_sum <- recipe(sales_sum_3m ~ ., data = train_tbl) %>%
    step_rm(customer_id, sales_flag_3m)

recipe_spec_prob <- recipe(sales_flag_3m ~ ., data = train_tbl) %>%
    step_rm(customer_id, sales_sum_3m)

# * Model Specs ----
wflw_fit_xgboost_sum <- workflow() %>% 
    add_model(
        boost_tree(mode = "regression") %>% set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spec_sum) %>% 
    fit(train_tbl)

wflw_fit_xgboost_flag <- workflow() %>% 
    add_model(
        boost_tree(mode = "classification") %>% set_engine("xgboost")
    ) %>% 
    add_recipe(recipe_spec_prob) %>% 
    fit(train_tbl)

# * Accuracy Check ----
pred_tbl_sum <- bind_cols(
    predict(wflw_fit_xgboost_sum, test_tbl),
    test_tbl %>% select(sales_sum_3m)
)

pred_tbl_sum %>% yardstick::mae(sales_sum_3m, .pred)


pred_tbl_flag <- bind_cols(
    predict(wflw_fit_xgboost_flag, test_tbl, type = "prob"),
    test_tbl %>% select(sales_flag_3m)
)

pred_tbl_flag %>% yardstick::roc_auc(sales_flag_3m, .pred_1, event_level = "second")
    


# train_tbl <- training(splits_2_train) %>%
#     group_by(customer_id) %>%
#     summarise(
#         recency     = (max(invoice_date) - max_date_train) / ddays(1),
#         frequency   = n(),
#         sales_sum   = sum(sales, na.rm = TRUE),
#         sales_mean  = mean(sales, na.rm = TRUE)
#     ) %>%
#     left_join(
#         targets_train_tbl
#     ) %>%
#     replace_na(replace = list(
#         spend_90_total = 0,
#         spend_90_flag  = 0
#     )
#     ) %>%
#     mutate(spend_90_flag = as.factor(spend_90_flag)) %>% 
#     left_join(
#         country_tbl %>% 
#             select(customer_id, country_2) %>% 
#             rename(country = country_2)
#     )
