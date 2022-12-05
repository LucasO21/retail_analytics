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


# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")
dbListTables(con)

# * Load Data ----
sales_tbl <- tbl(con, "retial_data_clean") %>% 
    collect() %>% 
    mutate(invoice_date = date(invoice_date)) %>% 
    mutate(invoice_date = ymd(invoice_date)) %>% 
    mutate(revenue = quantity * price)

sales_daily_agg_tbl <- sales_tbl %>% 
    group_by(customer_id) %>% 
    summarise_by_time(
        .date_var = invoice_date,
        .by = "day",
        total_revenue = sum(revenue)
    ) %>% 
    ungroup() 

sales_daily_invoice_agg_tbl <- sales_tbl %>% 
    group_by(customer_id, invoice) %>% 
    summarise_by_time(
        .date_var = invoice_date,
        .by = "day",
        total_revenue = sum(revenue)
    ) %>% 
    ungroup() 


# ******************************************************************************
# DATA PREP ----
# ******************************************************************************

rfm_segment_tbl <- tbl(con, "rfm_segment_tbl") %>% 
    select(customer_id, segment) %>% 
    distinct() %>% 
    collect()

# * First Purchase Tibble ----
first_purchase_tbl <- sales_tbl %>% 
    select(invoice_date, customer_id) %>% 
    distinct() %>% 
    lazy_dt() %>% 
    group_by(customer_id) %>% 
    slice_min(invoice_date) %>% 
    ungroup() %>% 
    as_tibble()

first_purchase_tbl %>% 
    pull(invoice_date) %>% 
    range()

first_purchase_tbl %>% 
    arrange(invoice_date) %>% 
    mutate(flag = ifelse(invoice_date >= as.Date("2010-03-01"), "yes", "no")) %>% 
    count(flag)

# ** Save First Purchase Tibble ----
# dbWriteTable(con, "first_purchase_tbl", first_purchase_tbl, overwrite = TRUE)

# * Set Cohort Span ----
start_date <- as.Date("2010-01-01")
end_date   <- as.Date("2010-03-31")

cohort_list <- first_purchase_tbl %>% 
    filter(invoice_date %>% between(start_date, end_date)) %>% 
    distinct(customer_id) %>% 
    pull(customer_id)

cohort_tbl <- sales_tbl %>% 
    filter(customer_id %in% cohort_list) %>% 
    left_join(
        rfm_segment_tbl
    ) %>% 
    filter(!segment %in% "Slipping")

cohort_tbl %>% arrange(desc(invoice_date))
first_purchase_tbl %>% filter(customer_id == 13821)


# dbWriteTable(con, "cohort_tbl", cohort_tbl, overwrite = TRUE)

# * Visualize Revenue by RFM Segments ----
cohort_tbl %>% 
    group_by(segment) %>% 
    summarise_by_time(
        .date_var     =  invoice_date, 
        .by           = "month", 
        total_revenue = sum(revenue, na.rm = TRUE)
    ) %>% 
    plot_time_series(
        .date_var    = invoice_date, 
        .value       = total_revenue, 
        .y_intercept = 0, 
        .facet_ncol  = 2
    )

# * Visualize a Sample of Customers ----
n   <- 10
ids <- cohort_list[1:10]

cohort_tbl %>% 
    filter(customer_id %in% ids) %>% 
    group_by(customer_id) %>% 
    summarise_by_time(invoice_date, "day", revenue = sum(revenue)) %>% 
    plot_time_series(
        invoice_date, revenue, 
        .interactive = FALSE,
        .smooth = FALSE,
        .facet_ncol = 2
    )+
    geom_point(color = "black")

cohort_tbl %>% 
    filter(customer_id == 12373) %>% 
    summarise_by_time(invoice_date, "day", revenue = sum(revenue)) %>% 
    plot_time_series(invoice_date, revenue)

# ******************************************************************************
# MODELING ----
# ******************************************************************************
# - Problem Statement:
# - What will the customers spend in the next 90 days? (Regression)
# - What is the prob of a customer making a purchase in the next 90 days (Classification)

# * Data Splitting ----
set.seed(123)
train_ids_list <- cohort_tbl %>% 
    pull(customer_id) %>% 
    unique() %>% 
    sample(size = round(0.8 * length(.))) %>% 
    sort()

# ** Split 1 (Non - Sequential) ----
split_1_train_tbl <- cohort_tbl %>% 
    filter(customer_id %in% train_ids_list)

split_1_test_tbl <- cohort_tbl %>% 
    filter(!customer_id %in% train_ids_list)

# ** Split 2 (Sequential) ----
split_2_train <- time_series_split(
    data        = split_1_train_tbl,
    date_var    = invoice_date, 
    assess      = "10 days",
    cummulative = TRUE
)

split_1_train_tbl %>% 
    group_by(customer_id) %>% 
    arrange(invoice_date, .by_group = TRUE)

split_2_train %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(invoice_date, revenue)

