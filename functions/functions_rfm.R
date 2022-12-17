# FUNCTIONS FOR RFM ANALYSIS
# *** ---

# * Working Dir ----
setwd(here::here("functions"))

# * Libraries
library(tidyverse)
library(plotly)


# ******************************************************************************
# UTILITIES ----
# ******************************************************************************

# * Custom Axis Theme ----
custom_axis_theme <- function(){
    
    theme(
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 18, color = "black", face = "bold")
    )
}


# ******************************************************************************
# FUNCTIONS ----
# ******************************************************************************

# * RFM Barplots ----
get_rfm_barplot <- function(.data, .var, .interactive = FALSE){
    
    # Colnames & Axis Names
    col_names <- c("segment", "value")
    y_var <- col_names[[1]] %>% str_to_title()
    x_var <- .var %>% str_replace("_", " ") %>%  str_to_title()
   
    # Change Data Colnames
    colnames(.data) <- col_names 
    
    # Data Prep
    .data <- .data %>% 
        mutate(label_text = value %>%  scales::comma(accuracy = 1)) %>% 
        mutate(label_text_2 = str_glue("
                                       Segment: {segment}
                                       Median {word(x_var, 2)}: {label_text}
                                       "))
    
    # Plot
    p <- .data %>% 
        ggplot(aes(value, segment))+
        geom_col(aes(text = label_text_2), width = 0.7)+
        theme_minimal()+
        custom_axis_theme()+
        labs(
            title = str_glue("{x_var} by {y_var}"), 
            y     = y_var, 
            x     = x_var
        )
    
    if(.interactive){
        p <- ggplotly(p + labs(y = NULL), tooltip = "text")
    } else {
        p <- p + geom_text(aes(label = label_text), size = 4, fontface = "bold", hjust = -0.3)
    }
    
    return(p)
    
}

get_rfm_barplot(.data = sample_data, .var = "median_recency", .interactive = TRUE)


# * Scatterplots ----
get_rfm_scatterplots <- function(.data, .x, .y, .interactive = FALSE){
    
    # tidy eval
    .x_expr <- rlang::enquo(.x)
    .y_expr <- rlang::enquo(.y)
    .y_name <- rlang::quo_name(.y_expr)
    .x_name <- rlang::quo_name(.x_expr)
    .y_axis_label <- .y_name %>% str_replace("_", " ") %>% str_to_title()
    .x_axis_label <- .x_name %>% str_to_title()
    
    data_prep <- .data %>% 
        mutate(label_text = str_glue("
                                     Customer ID: {customer_id}
                                     Segment: {segment}
                                     Recency Days: {recency_days}
                                     Amount: {amount %>% scales::dollar(accuracy = 1)}
                                     
                                     ")) 
    
    p <- data_prep %>% 
        ggplot(aes(x = log1p(!!.x_expr), y = log1p(!!.y_expr), col = segment))+
        geom_point(aes(text = label_text), alpha = 0.8, size = 1.5)+
        theme_minimal()+
        custom_axis_theme()+
        labs(
            title = str_glue("{.y_axis_label} by {.x_axis_label}"),
            y     = str_glue("{.y_axis_label} (Log Scale)"),
            x     = str_glue("{.x_axis_label} (Log Scale)")
        )
    
    if(.interactive){
        p <- ggplotly(p + labs(title = NULL), tooltip = "text")
    } else {
        p <- p
    }
    
    return(p)
    
    
}

get_rfm_scatterplots(
    .data        = rfm_segment_tbl, 
    .x           = amount, 
    .y           = transaction_count,
    .interactive = FALSE
)



