# RETAIL ANALYTICS PROJECT ----
# RFM ANALYSIS SCRIPT ----
# **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Set Working Dir ----
setwd(here::here("R"))

# * Libraries ----
library(tidyverse)
library(janitor)
library(timetk)
library(lubridate)
library(DBI)
library(rfm)

# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************

# * Setup DB Connection ----
con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")
dbListTables(con)

# * Load Data ----
sales_tbl <- tbl(con, "retail_data_clean_tbl") %>% 
    collect() %>% 
    select(-date) %>% 
    mutate(invoice_date = date(invoice_date)) %>% 
    mutate(invoice_date = ymd(invoice_date)) %>% 
    mutate(revenue = quantity * price)

# * First Purchase Data ----
first_purchase_tbl <- tbl(con, "first_purchase_tbl") %>% collect()

# * Analysis Cohort Data ----
rfm_cohort_tbl <- sales_tbl %>% 
    left_join(first_purchase_tbl) %>% 
    filter(flag == "Old")

# ******************************************************************************
# RFM TABLE ----
# ******************************************************************************

.analysis_date <- as.Date("2011-11-30")

rfm_score_tbl <- rfm_table_order(
    data          = rfm_cohort_tbl,
    customer_id   = customer_id,
    order_date    = invoice_date,
    revenue       = revenue,
    analysis_date = .analysis_date
)


# ******************************************************************************
# RFM SEGMENTATION ----
# ******************************************************************************

# * RFM Scores List ----
rfm_scores_list <- unique(rfm_score_tbl$rfm$rfm_score)

# * RFM Quantiles List ----
rfm_quantiles_tbl <- quantile(
    x     = rfm_scores_list, 
    probs = c(0.20, 0.40, 0.60, 0.80)
) %>% 
    round()

# * RFM Segment Names ----
segment_names <-  c("Core", "Loyal", "Whales", "Promising", "Slipping")

# RFM Segment Tibble ----
rfm_segment_tbl <- rfm_score_tbl$rfm %>% 
    mutate(segment = case_when(
        rfm_score <  rfm_quantiles_tbl[1]                                    ~ "Slipping",
        rfm_score >= rfm_quantiles_tbl[1] & rfm_score < rfm_quantiles_tbl[2] ~ "Promising", 
        rfm_score >= rfm_quantiles_tbl[2] & rfm_score < rfm_quantiles_tbl[3] ~ "Whales",
        rfm_score >= rfm_quantiles_tbl[3] & rfm_score < rfm_quantiles_tbl[4] ~ "Loyal",
        TRUE                                                                 ~ "Core"
    )) %>% 
    mutate(segment = segment %>% fct_relevel("Core", "Loyal", "Whales", "Promising", "Slipping"))

# * Save RFM Segment Table ----
dbWriteTable(con, "rfm_segment_tbl", rfm_segment_tbl, overwrite = TRUE)


# ******************************************************************************
# ANALYZING RFM SEGMENTS ----
# ******************************************************************************
rfm_segment_tbl %>% 
    count(segment, sort = TRUE) %>% 
    mutate(pct = n/sum(n))


# ******************************************************************************
# VISUALIZING RFM SEGMENTS ----
# ******************************************************************************

# * Barplots ----

# ** Median Recency by Segment ----
rfm_segment_tbl %>% 
    group_by(segment) %>% 
    summarise(median_recency = median(recency_days)) %>% 
    ungroup()

# ** Median Frequency by Segment ----
rfm_segment_tbl %>% 
    group_by(segment) %>% 
    summarise(median_frequency = median(transaction_count)) %>% 
    ungroup()

# ** Median Amount by Segment ----
rfm_segment_tbl %>% 
    group_by(segment) %>% 
    summarise(median_amount = median(amount)) %>% 
    ungroup()


# * Scatterplots ----

# ** Recency vs Monetary Value ----
rfm_segment_tbl %>% 
    ggplot(aes(log1p(amount), log1p(recency_days), col = segment))+
    geom_point()
