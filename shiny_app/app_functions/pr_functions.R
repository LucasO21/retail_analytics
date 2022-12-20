# FUNCTIONS FOR PRODUCT RECOMMENDATION ----
# **** ----

# Libraries ----
# library(tidyverse)
# library(coop)

# ******************************************************************************
# GET ANALYSIS COHORT ----
# ******************************************************************************
# start_date              <- max(retail_data_clean_tbl$invoice_date) - 90
# end_date                <- max(retail_data_clean_tbl$invoice_date)
# 
# analysis_cohort_tbl <-  retail_data_clean_tbl %>% 
#     filter(customer_id %in% unique(predictions_tbl$customer_id)) %>% 
#     filter(between(invoice_date, start_date, end_date))


# ******************************************************************************
# USER ITEM MATRIX ----
# ******************************************************************************
get_user_item_matrix <- function(data){
    
    matrix <- data %>% 
        select(customer_id, stock_code, quantity) %>% 
        mutate(quantity = ifelse(quantity > 0, 1, 0)) %>% 
        distinct() %>% 
        pivot_wider(names_from = stock_code, values_from = quantity, values_fill = 0)
    
    return(matrix)
    
}

# user_item_matrix <- get_user_item_matrix(data = analysis_cohort_tbl)


# ******************************************************************************
# USER TO USER SIMILARITY MATRIX ----
# ******************************************************************************
get_user_to_user_cosine_matrix <- function(data){

    matrix <- cosine(
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

# user_to_user_matrix <- analysis_cohort_tbl %>% 
#     get_user_item_matrix() %>% 
#     get_user_to_user_cosine_matrix()


# ******************************************************************************
# USER TO USER SIMILARITY ANALYSIS / RECOMMENDATIONS ----
# ******************************************************************************
#   - Pick a customer. Call them customer A
#   - Pick the customer closest to customer A in terms of similarity (customer B)
#   - Find products bought by customer B
#   - Find products bought by customer B that customer A did not buy
#   - Recommend these products to customer A

# data          <- user_to_user_matrix
# .customer_id  <- "12416"
# .n_closest    <- 3
# .sales_data   <- analysis_cohort_tbl

get_user_product_recommendations <- function(data, .sales_data,
                                             .customer_id, .n_closest){

    # N similar customers to customer A
    top_n_closest_customers_to_customer_a <- data %>%
        filter(customer_id == .customer_id) %>%
        gather() %>%
        filter(!str_detect(key, "customer_id")) %>%
        arrange(desc(value)) %>%
        filter(!value == 0) %>%
        dplyr::slice(2:(.n_closest + 1)) %>%
        `colnames<-`(c("customer_id", "cosine_similarity_value"))

    # Closest customer list
    n_closest_list <- top_n_closest_customers_to_customer_a %>% pull(customer_id)

    # Items bought by customer A
    items_bought_by_customer_a <- get_user_item_matrix(data = .sales_data) %>%
        filter(customer_id == .customer_id) %>%
        select(-customer_id) %>%
        gather() %>%
        filter(value == 1) %>%
        pull(key) %>%
        sort()

    # Items bought by closest customers
    items_bought_by_closest_customers <- get_user_item_matrix(data = .sales_data) %>%
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
    opportunity_tbl <- .sales_data %>%
        filter(customer_id %in% n_closest_list) %>%
        filter(stock_code %in% items_to_recommend_to_customer_a) %>%
        group_by(stock_code, description) %>%
        summarise(
            mean_quantity = mean(quantity),
            mean_sales    = mean(sales)
        ) %>%
        ungroup() %>%
        arrange(desc(mean_sales)) %>% 
        dplyr::slice(1:10) %>% 
        mutate(mean_quantity = mean_quantity %>% scales::comma(accuracy = 1)) %>% 
        mutate(mean_sales = mean_sales %>% scales::dollar(accuracy = 0.1)) %>% 
        setNames(names(.) %>% str_replace_all("_", " ")) %>% 
        setNames(names(.) %>% str_to_title())

    # Final product recommender (table)
    # final_recommend_tbl <- items_to_recommend_tbl %>%
    #     left_join(opportunity_tbl) %>%
    #     drop_na()

    # Return
    return(opportunity_tbl)

}

# t <- analysis_cohort_tbl %>%
#     get_user_item_matrix() %>%
#     get_user_to_user_cosine_matrix() %>%
#     get_user_product_recommendations(
#         .customer_id = "12380",
#         .n_closest   = 3,
#         .sales_data = analysis_cohort_tbl
#     )
# 
# l <- t %>% 
#     select(Description) %>% 
#     pull()
# 
# toString(l)
