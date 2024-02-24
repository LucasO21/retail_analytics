# RETAIL ANALYTICS PROJECT ----
# PRODUCT RECOMMENDATION SCRIPT ----
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
library(coop)


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
    filter(invoice_date <= as.Date("2011-11-30")) %>% 
    mutate(description = gsub("\"", "", description))

# retail_data_clean_tbl %>% pull(invoice_date) %>% range()


# ******************************************************************************
# ANALYSIS COHORT ----
# ******************************************************************************
data                    <-  retail_data_clean_tbl
.first_purchase_cohort  <- "Q1-2010"
.country                <- "United Kingdom"
start_date              <- max(retail_data_clean_tbl$invoice_date) - 90
end_date                <- max(retail_data_clean_tbl$invoice_date)

get_analysis_cohort_data <- function(data, .first_purchase_cohort = NULL, 
                                     .country = NULL, start_date, end_date){
    
    # First purchase cohort setup
    if(is.null(.first_purchase_cohort)){
        .first_purchase_cohort = unique(first_purchase_tbl$first_purchase_cohort)
    }else{
        .first_purchase_cohort = c(.first_purchase_cohort)
    }
    
    # Country setup
    if(is.null(.country)){
        .country = unique(first_purchase_tbl$country)
    }else{
        .country = c(.country)
    }
    
    # Data setup
    data_tbl <- data %>% 
        left_join(
            first_purchase_tbl %>% 
                select(customer_id, first_purchase_cohort) 
        ) %>% 
        filter(first_purchase_cohort %in% .first_purchase_cohort) %>% 
        filter(country %in% .country) %>% 
        filter(between(invoice_date, start_date, end_date))
    
    # Message
    message(str_glue("Analysis Period: {start_date} - {end_date}"))
    
    # Return
    return(data_tbl)
    
    
}

analysis_cohort_tbl <- get_analysis_cohort_data(
    data                   = retail_data_clean_tbl,
    .first_purchase_cohort = "Q1-2010",
    .country               = "United Kingdom",
    start_date             = as.Date("2011-09-01"),
    end_date               = as.Date("2011-11-30") 
)


# ******************************************************************************
# USER ITEM MATRIX ----
# ******************************************************************************
data <- analysis_cohort_tbl

get_user_item_matrix <- function(data){
    
    matrix <- data %>% 
        select(customer_id, stock_code, quantity) %>% 
        mutate(quantity = ifelse(quantity > 0, 1, 0)) %>% 
        distinct() %>% 
        pivot_wider(names_from = stock_code, values_from = quantity, values_fill = 0)
    
    return(matrix)
    
}

user_item_matrix <- get_user_item_matrix(data = analysis_cohort_tbl)


# ******************************************************************************
# USER TO USER SIMILARITY MATRIX ----
# ******************************************************************************
data <- user_item_matrix

get_user_to_user_cosine_matrix <- function(data){

    user_to_user_matrix <- cosine(
        as.matrix(
            t(data[, 2:dim(data)[2]])
        )
    ) %>% 
        as_tibble() %>% 
        `colnames<-`(c(data$customer_id)) %>% 
        mutate(customer_id = data$customer_id) %>% 
        select(customer_id, everything())
    
    return(matrix)
    
}

user_to_user_matrix <- get_user_to_user_cosine_matrix(data = user_item_matrix)

# ******************************************************************************
# USER TO USER SIMILARITY ANALYSIS / RECOMMENDATIONS ----
# ******************************************************************************
#   - Pick a customer. Call them customer A
#   - Pick the customer closest to customer A in terms of similarity (customer B)
#   - Find products bought by customer B
#   - Find products bought by customer B that customer A did not buy
#   - Recommend these products to customer A

data         <- user_to_user_matrix
.customer_id <- "13305"
n_closest    <- 5

get_user_product_recommendations <- function(data, .customer_id, n_closest){
    
    # N similar customers to customer A
    top_n_closest_customers_to_customer_a <- data %>%
        filter(customer_id == .customer_id) %>%
        gather() %>%
        filter(!str_detect(key, "customer_id")) %>%
        arrange(desc(value)) %>%
        filter(!value == 0) %>% 
        slice(2:(n_closest + 1)) %>% 
        `colnames<-`(c("customer_id", "cosine_similarity_value"))
    
    # Closest customer list
    n_closest_list <- top_n_closest_customers_to_customer_a %>% pull(customer_id)
    
    # Items bought by customer A
    items_bought_by_customer_a <- get_user_item_matrix(data = analysis_cohort_tbl) %>% 
        filter(customer_id == customer_a) %>% 
        select(-customer_id) %>% 
        gather() %>% 
        filter(value == 1) %>% 
        pull(key) %>% 
        sort()
    
    # Items bought by customer B
    items_bought_by_closest_customers <- get_user_item_matrix(data = analysis_cohort_tbl) %>% 
        filter(customer_id %in% n_closest_list) %>% 
        select(-customer_id) %>% 
        gather() %>% 
        filter(value == 1) %>% 
        pull(key) %>% 
        sort()
    
    # Items to recommend to A (list)
    items_to_recommend_to_customer_a <- setdiff(
        items_bought_by_closest_customers, 
        items_bought_by_customer_a
    )
    
    # Items to recommend to A (table)
    items_to_recommend_tbl <- items_to_recommend_to_customer_a %>% 
        as_tibble() %>% 
        rename(stock_code = value) %>% 
        left_join(
            analysis_cohort_tbl %>% 
                select(stock_code, description) %>% 
                distinct()
        )
    
    # Opportunity (table)
    opportunity_tbl <- analysis_cohort_tbl %>% 
        filter(customer_id %in% n_closest_list) %>% 
        filter(stock_code %in% items_bought_by_closest_customers) %>% 
        group_by(stock_code, description) %>% 
        summarise(
            mean_quantity = mean(quantity),
            mean_sales    = mean(sales)
        ) %>% 
        ungroup() %>% 
        arrange(desc(mean_sales))
    
    # Final product recommender (table)
    final_recommend_tbl <- items_to_recommend_tbl %>% 
        left_join(opportunity_tbl) %>% 
        drop_na()
    
    # Return
    return(final_recommend_tbl)
    
}

get_user_product_recommendations(
    data         = user_to_user_matrix,
    .customer_id = "15910",
    n_closest    = 2
)



# ******************************************************************************
# ITEM TO ITEM SIMILARITY ANALYSIS / RECOMMENDATIONS ----
# ******************************************************************************

# item_to_item_matrix <- cosine(
#     as.matrix(
#         user_item_matrix[, 2:dim(user_item_matrix)[2]]
#     )
# ) %>% 
#     as_tibble() %>% 
#     mutate(stock_code = colnames(user_item_matrix)[-1]) %>% 
#     select(stock_code, everything())





