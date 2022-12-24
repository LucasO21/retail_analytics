# FUNCTIONS FOR CLV ANALYSIS
# *** ---

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Working Dir ----
# setwd(here::here("functions"))
# 
# # * Libraries
# library(tidyverse)
# library(plotly)
# library(DT)

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
get_scatter_plot_data <- function(data){
    
    data_prep <- data %>% 
        left_join(get_tooltip_text(data))
    
    return(data_prep)
}

# get_scatter_plot_data(data = predictions_tbl)


# ******************************************************************************
# SCATTER PLOT ----
# ******************************************************************************
get_scatter_plot <- function(data){
    
    p <- data %>% 
        ggplot(aes(frequency, .pred_prob, color = spend_actual_vs_pred))+
        geom_point(aes(text = text), size = 2)+
        geom_smooth(se = FALSE, color = "black", method = "gam")+
        # scale_colour_gradientn(colours = c("#cc4125", "#ea9999", "#ffd966", "#6aa84f", "#274e13"))+
        scale_color_gradient2(low = "red", mid = "#78c2ad", high = "black", midpoint = 0)+
        labs(x = "Purchase Frequency", y = "Probability of Future 90-Day Purchase")+
        theme_minimal()+
        custom_axis_theme()+
        theme(legend.key.size = unit(0.3, "cm"))+
        theme(legend.title = element_text(size = 7))+
        theme(legend.text = element_text(size = 7))
    
    p <- ggplotly(p, tooltip = "text") 
    
    return(p)
    
}

# predictions_tbl %>% get_scatter_plot_data() %>% get_scatter_plot()


# ******************************************************************************
# FEATURES PLOT DATA ----
# ******************************************************************************
get_features_plot_data <- function(data){
    
    tooltip_data <- data %>% 
        get_tooltip_text()

    data_prep <- data %>%
        select(customer_id, .pred_prob, .pred_total, spend_90_total, spend_actual_vs_pred,
               recency, frequency, sales_sum) %>% 
        pivot_longer(cols = -c(customer_id, .pred_prob, .pred_total, spend_90_total, spend_actual_vs_pred), names_to = "feature", values_to = "value") %>%
        group_by(feature) %>%
        mutate(value_scaled = scale(value) %>% as.numeric()) %>%
        mutate(value_scaled = scales::rescale(value_scaled, to = c(0, 10))) %>% 
        ungroup() %>% 
        left_join(tooltip_data)

    return(data_prep)
    

        

}

# data %>% get_features_plot_data()

# ******************************************************************************
# FEATURES PLOT ----
# ******************************************************************************
get_features_plot <- function(data){

    p <- data %>%
        ggplot(aes(feature, value_scaled))+
        #geom_boxplot(col = "grey")+
        geom_jitter(aes(text = text, color = spend_actual_vs_pred), size = 2, alpha = 0.9)+
        coord_flip()+
        theme_minimal()+
        custom_axis_theme()+
        #scale_colour_gradientn(colours = c("#cc4125", "#ea9999", "#ffd966", "#6aa84f", "#274e13"))+
        scale_color_gradient2(low = "red", mid = "#78c2ad", high = "black", midpoint = 0)+
        theme(legend.key.size = unit(0.3, "cm"))+
        theme(legend.title = element_text(size = 7))+
        theme(legend.text = element_text(size = 7))+
        labs(x = "", y = "Scaled Feature Value")

    p <- ggplotly(p, tooltip = "text")

    return(p)

}

# predictions_tbl %>% get_features_plot_data() %>% get_features_plot()


# ******************************************************************************
# DATA TABLE ----
# ******************************************************************************
get_clv_dt <- function(data){
    
    data %>% 
        select(customer_id, everything()) %>% 
        mutate(.pred_prob = .pred_prob %>% scales::percent(accuracy = 0.1)) %>% 
        mutate(across(
            .cols = c(.pred_total, spend_90_total, sales_sum, sales_mean, spend_actual_vs_pred),
            .fns = scales::dollar_format(accuracy = 0.1)
        )) %>% 
        setNames(names(.) %>% str_replace_all("_", " ")) %>% 
        setNames(names(.) %>% str_to_title())
    
}


# ******************************************************************************
# HELP ----
# ******************************************************************************

# * Help: Main ----
get_clv_main_help <- function(){
    
    modalDialog(
        title = "Customer Lifetime Value",
        p("Use this tab to explore future spend amount and probability of customers."),
        p("Using RFM (Recency Frequency Monetary features as predictors,
          an XGBOOST model predicst the probability of a customer making a purchase,
          in the next 90 days along with a prediction of how much they might spend."),
        p("Click below to see examples"),
        actionButton("clv_customer_red", "How to interpret customers in light green/red"),
        actionButton("clv_customer_green", "How to interpret customers in dark green/black"),
          size = "l", easyClose = TRUE, fade=FALSE,
          footer = modalButton("Close (Esc)")
    )
    
    
}

# * Help: Red Customer Example ----
get_clv_red_help <- function(){
    
    modalDialog(
        title = "Customers In Light Green/Red",
        p("On the y-axis is spend probabilty, while on the x-axis is purchase frequency."),
        p("Customers in light green/red have a high spend probabilty, however these customers
          have spent much lower than predicted."),
        br(),
        strong("Action"),
        p("Reach out  to these customers with product recommendations. (See Product Recommendation tab)."),
        p(tags$a(img(src = "clv_customer_red.png", width = "60%", height = "60%"))),
        br(),
        size = "l", easyClose = TRUE, fade=FALSE,
        footer = tagList(
            actionButton("clv_customer_green", "How to interpret customers in dark green/black"),
            actionButton("clv_help_back", "Back"),
            modalButton("Close (Esc)")
        )
    )
    
    
}

# * Help: Green Customer Example ----
get_clv_green_help <- function(){
    
    modalDialog(
        title = "Customers In Light Dark Green/Black",
        p("On the y-axis is spend probabilty, while on the x-axis is purchase frequency."),
        p("Customers in dark green/black have a high spend probabilty, and are spending much lower than predicted."),
        br(),
        strong("Action"),
        p("Further analyze these customers spending habits."),
        p(tags$a(img(src = "clv_customer_green.png", width = "60%", height = "60%"))),
        br(),
        size = "l", easyClose = TRUE, fade=FALSE,
        footer = tagList(
            actionButton("clv_customer_red", "How to interpret customers in light green/red"),
            actionButton("clv_help_back", "Back"),
            modalButton("Close (Esc)")
        )
    )
    
    
}



