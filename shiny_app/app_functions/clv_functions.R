# FUNCTIONS FOR CLV ANALYSIS
# *** ---

# * Working Dir ----
setwd(here::here("functions"))

# * Libraries
library(tidyverse)
library(plotly)

# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************
# clv_artifacts <- read_rds("../artifacts/clv_artifacts_list.rds")
# 
# predictions_tbl <- clv_artifacts$data$predictions_data


# ******************************************************************************
# CUSTOM AXIS THEME ----
# ******************************************************************************
custom_axis_theme <- function(){
    
    theme(
        axis.text = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 9),
        plot.title = element_text(size = 18, color = "black", face = "bold")
    )
}


# ******************************************************************************
# TOOLTIP TEXT ----
# ******************************************************************************
get_tooltip_text <- function(data){
    
    data %>% 
        mutate(text = str_glue("Customer ID: {customer_id}
                            90-Day Spend Prob: {scales::percent(.pred_prob, accuracy = 0.1)}
                            90-Day Spend Pred: {scales::dollar(.pred_total)}
                            Actual 90-Day Spend: {scales::dollar(spend_90_total)}
                            Actual vs Pred: {scales::dollar(spend_actual_vs_pred)}
                            ---
                            Recency: {recency}
                            Frequency: {frequency}")) %>% 
        select(customer_id, text)
    
}

# ******************************************************************************
# DATA PREP FOR SCATTER PLOT ----
# ******************************************************************************
get_scatter_plot_data <- function(data, sample_prop = 1){
    
    data_prep <- data %>% 
        sample_frac(size = sample_prop) %>% 
        left_join(get_tooltip_text(data = data))
    
    return(data_prep)
}

# get_scatter_plot_data(data = predictions_tbl)


# ******************************************************************************
# SCATTER PLOT ----
# ******************************************************************************
get_scatter_plot <- function(data){
    
    p <- data %>% 
        ggplot(aes(frequency, .pred_prob, color = spend_actual_vs_pred))+
        geom_point(aes(text = text), size = 3)+
        geom_smooth(se = FALSE, color = "black", method = "gam")+
        scale_colour_gradientn(colours = c("#cc4125", "#ea9999", "#ffd966", "#6aa84f", "#274e13"))+
        labs(x = "Purchase Frequency", y = "Probability of Future 90-Day Purchase")+
        theme_minimal()+
        custom_axis_theme()
    
    p <- ggplotly(p, tooltip = "text")
    
    return(p)
    
}

# data %>% get_scatter_plot_data() %>% get_scatter_plot()

# 
# # * Features Plot Data ----
# get_features_plot_data <- function(data){
#     
#     data_prep <- data %>% 
#         pivot_longer(
#             cols = c(recency, frequency, sales_sum),
#             names_to = "feature", values_to = "value"
#         ) %>% 
#         group_by(feature) %>% 
#         mutate(value_scaled = scale(value) %>% as.numeric()) %>% 
#         ungroup() %>% 
#         left_join(get_tooltip_text(data = data))
#     
#     return(data_prep)
#     
# }
# 
# # * Features Plot ----
# get_features_plot <- function(data){
#     
#     p <- data %>% 
#         ggplot(aes(feature, value_scaled))+
#         geom_violin(col = "grey")+
#         geom_jitter(aes(text = text, color = spend_actual_vs_pred), size = 2, alpha = 0.8)+
#         coord_flip()+
#         theme_minimal()+
#         custom_axis_theme()+
#         scale_colour_gradientn(colours = c("#cc4125", "#ea9999", "#ffd966", "#6aa84f", "#274e13"))+
#         labs(x = "", y = "Probability of Future 90-Day Purchase")
#     
#     p <- ggplotly(p, tooltip = "text")
#     
#     return(p)
#     
# }
# 
# # predictions_test_tbl %>% get_features_plot_data() %>% get_features_plot()
# 
#     
