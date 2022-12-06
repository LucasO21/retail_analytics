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

sales_tbl <- sales_tbl %>% 
    left_join(first_purchase_tbl %>% select(customer_id, flag)) %>% 
    filter(flag == "Old") %>% 
    select(-flag)

revenue_quarterly_tbl <- sales_tbl %>% 
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

quarter_tbl <- revenue_quarterly_tbl %>% 
    distinct(quarter) %>% 
    arrange(quarter) %>% 
    bind_cols(
        tibble(
            q = c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9")
        )
    ) %>% 
    mutate(quarter = factor(quarter))


sales_quarterly_wide_tbl <- revenue_quarterly_tbl %>% 
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

model_ready_tbl <- sales_quarterly_wide_tbl %>% 
    left_join(
        rfm_segment_tbl %>% 
            select(customer_id, recency_days, segment)
    ) %>% 
    rename(sum_sales_3m = sum_sales_Q9) %>% 
    mutate(flag_sales_3m = if_else(sum_sales_3m > 0, 1, 0)) %>% 
    select(customer_id, starts_with("sum"), starts_with("avg"),
           starts_with("count"), everything())
    





# ******************************************************************************
# MODELING ----
# ******************************************************************************
# - Problem Statement:
# - What will the customers spend in the next 90 days? (Regression)
# - What is the prob of a customer making a purchase in the next 90 days (Classification)

# * Data Splitting ----
set.seed(123)
train_ids_list <- revenue_daily_tbl %>% 
    pull(customer_id) %>% 
    unique() %>% 
    sample(size = round(0.8 * length(.))) %>% 
    sort()

# ** Split 1 (Non - Sequential) ----
split_1_train_tbl <- revenue_daily_tbl %>% 
    filter(customer_id %in% train_ids_list)

split_1_test_tbl <- revenue_daily_tbl %>% 
    filter(!customer_id %in% train_ids_list)

# ** Split 2 (Sequential) ----
split_2_train <- time_series_split(
    data        = split_1_train_tbl,
    date_var    = invoice_date, 
    assess      = "90 days",
    cummulative = TRUE
)

split_1_train_tbl %>% 
    group_by(customer_id) %>% 
    arrange(invoice_date, .by_group = TRUE)

split_2_train %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(invoice_date, revenue)

