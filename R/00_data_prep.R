# RETAIL ANALYTICS PROJECT ----
# DATA PREP SCRIPT ----
# **** ----

# **********************************************************************************************
# SETUP ----
# **********************************************************************************************

# * Set Working Dir ----
setwd(here::here("R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(timetk)
library(lubridate)
library(dtplyr)
library(data.table)
library(DBI)

# **********************************************************************************************
# SETUP DB ----
# - Setup SQL Lite DB to hold data. Save csv file to DB
# **********************************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")

# * Save CSV to DB ----
# dbWriteTable(con, "retail_data_raw", read.csv("../data/online_retail_II.csv"))
dbListTables(con)


# **********************************************************************************************
# DATA PREP 1 ----
# - Clean data and load back to DB
# **********************************************************************************************
retail_data_raw_tbl <- tbl(con, "retail_data_raw") %>% 
    as_tibble() %>% 
    clean_names()

retail_data_clean_tbl <- retail_data_raw_tbl %>% 
    filter(!is.na(customer_id)) %>% 
    filter(quantity > 0) %>% 
    mutate(sales = quantity * price) %>% 
    mutate(description = trimws(description, which = c("both"))) %>% 
    mutate(description = description %>% str_replace_all("[^[:alnum:]]", "")) %>% 
    filter(!country %in% c(
        "EIRE", "Channel Islands", "RSA", "West Indies"
    ))

# dbWriteTable(con, "retail_data_clean_tbl", retail_data_clean_tbl, overwrite = TRUE)


# **********************************************************************************************
# DATA PREP 2 ----
# - First Purchase Data
# **********************************************************************************************

# retail_data_clean_tbl <- tbl(con, "retail_data_clean_tbl")

first_purchase_tbl <- retail_data_clean_tbl %>% 
    mutate(date = lubridate::date(invoice_date)) %>% 
    select(date, customer_id) %>% 
    distinct() %>% 
    lazy_dt() %>% 
    group_by(customer_id) %>% 
    slice_min(date) %>% 
    ungroup() %>% 
    as_tibble() %>% 
    mutate(flag = case_when(
        date < as.Date("2011-08-01") ~ "Old",
        TRUE                         ~ "New"
    )) %>% 
    select(-date)

# dbWriteTable(con, "first_purchase_tbl", first_purchase_tbl, overwrite = TRUE)

# **********************************************************************************************
# DATA PREP 3 ----
# - Country Data
# **********************************************************************************************

country_tbl <- retail_data_clean_tbl %>% 
    select(customer_id, country) %>% 
    distinct()

# dbWriteTable(con, "country_tbl", country_tbl, overwrite = TRUE)
