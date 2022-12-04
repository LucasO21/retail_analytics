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
library(DBI)

# **********************************************************************************************
# SETUP DB ----
# - Setup SQL Lite DB to hold data. Save csv file to DB
# **********************************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")
dbWriteTable(con, "retail_data_raw", read.csv("../data/online_retail_II.csv"))
dbListTables(con)

# **********************************************************************************************
# CLEAN DATA ----
# - Clean data and load back to DB
# **********************************************************************************************

retail_data_raw_tbl <- tbl(con, "retail_data_raw") %>% 
    as_tibble() %>% 
    clean_names()

retail_data_clean_tbl <- retail_data_raw_tbl %>% 
    filter(!is.na(customer_id)) %>% 
    filter(quantity > 0) %>% 
    mutate(description = trimws(description, which = c("both"))) %>% 
    mutate(description = description %>% str_replace_all("[^[:alnum:]]", "")) %>% 
    filter(!country %in% c(
        "EIRE", "Channel Islands", "RSA", "West Indies"
    ))

dbWriteTable(con, "retial_data_clean", retail_data_clean_tbl)
