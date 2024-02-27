# FUNCTIONS FOR CLV ANALYSIS
# *** ---

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Working Dir ----
# setwd(here::here("shiny_app", "app_functions"))
 
# # * Libraries
# library(tidyverse)
# library(plotly)
# library(DT)

# ******************************************************************************
# LOAD DATA ----
# ******************************************************************************
# clv_artifacts <- read_rds("../app_artifacts/clv_artifacts_list.rds")
 
# predictions_tbl <- clv_artifacts$data$predictions_data
# data <- predictions_tbl



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


currency_shorthand_format <- function() {
    function(x) {
        paste0("$", round(x / 1000, 1), "K")
    }
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

#data <- get_scatter_plot_data(data = predictions_tbl)

get_scatter_plot <- function(data, midpoint = 0){
  
  color_vec <-  c("#f88379", "#e59562", "#acb36c", "#88ab7b", "#7f9a71", "#676147", "#4d3e32", "#2b211f", "#000000")
  
  breaks_vec <- c(-500, -250, 0, 250, 500, 750, 1000, 1250, 1500)
    
    p <- data %>% 
        #filter(spend_actual_vs_pred <= 2500 & spend_actual_vs_pred >= - 500) %>% 
        ggplot(aes(frequency, .pred_prob, color = spend_actual_vs_pred))+
        geom_point(aes(text = text), size = 2)+
        #geom_smooth(se = FALSE, color = "black", method = "gam")+
        scale_colour_gradientn(
          colours = color_vec, 
          breaks = breaks_vec,
          labels = c("-$1K", "-$500", "-$250", "$0", "$250", "$500", "$750", "$1K", "$1.5K"),
          #labels = currency_shorthand_format(),
          limits = c(-500, 1500)
        )+
        # scale_color_gradient2(
        #     low      = "red", 
        #     mid      = "#78c2ad", 
        #     high     = "black", 
        #     midpoint = midpoint,
        #     labels   = currency_shorthand_format()
        # )+
        theme_minimal()+
        custom_axis_theme()+
        theme(legend.key.size = unit(0.5, "cm"))+
        theme(legend.title = element_text(size = 9))+
        theme(legend.text = element_text(size = 8)) +
        theme(legend.position = "right")+
        labs(
            x = "Purchase Frequency", 
            y = "Probability of Future 90-Day Purchase",
            color = "Actual vs Predicted Spend"
        )
    
    p <- ggplotly(p, tooltip = "text")
    p
    
    return(p)
    
}

# p1 <- predictions_tbl %>% get_scatter_plot_data() %>% get_scatter_plot()


# ******************************************************************************
# FEATURES PLOT DATA ----
# ******************************************************************************
get_features_plot_data <- function(data){
    
    tooltip_data <- data %>% get_tooltip_text()

    data_prep <- data %>%
        select(
            customer_id, .pred_prob, .pred_total, spend_90_total, 
            spend_actual_vs_pred, recency, frequency, sales_sum
        ) %>% 
        pivot_longer(
            cols = -c(customer_id, .pred_prob, .pred_total, spend_90_total, spend_actual_vs_pred), 
            names_to = "feature", 
            values_to = "value"
        ) %>%
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
  
  color_vec <-  c("#f88379", "#e59562", "#acb36c", "#88ab7b", "#7f9a71", "#676147", "#4d3e32", "#2b211f", "#000000")
  
  breaks_vec <- c(-500, -250, 0, 250, 500, 750, 1000, 1250, 1500)

    p <- data %>%
        #filter(spend_actual_vs_pred <= 2500 & spend_actual_vs_pred >= - 500) %>% 
        ggplot(aes(feature, value_scaled))+
        geom_jitter(aes(text = text, color = spend_actual_vs_pred), size = 2, alpha = 0.9)+
        coord_flip()+
        theme_minimal()+
        custom_axis_theme()+
        scale_colour_gradientn(
          colours = color_vec, 
          breaks = breaks_vec,
          labels = c("-$1K", "-$500", "-$250", "$0", "$250", "$500", "$750", "$1K", "$1.5K"),
          #labels = currency_shorthand_format(),
          limits = c(-500, 1500)
        )+
        #scale_colour_gradientn(colours = c("#cc4125", "#ea9999", "#ffd966", "#6aa84f", "#274e13"))+
        #scale_color_gradient2(low = "red", mid = "#78c2ad", high = "black", midpoint = 0)+
        theme(legend.key.size = unit(0.5, "cm"))+
        theme(legend.title = element_text(size = 7))+
        theme(legend.text = element_text(size = 7))+
        labs(x = "", y = "Scaled Feature Value", color = "Actual vs Predicted Spend")+
        theme(legend.position = "right")

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


# * Numeric Range Inputs ----

# feature <- "spend_90_flag"

get_numeric_range_input <- function(id, data, feature, label){
    
    if (feature %in% c(".pred_total", "spend_90_total")) {
        round = 0
        step = 100
        
    } else if (feature == ".pred_prob") {
        round = 2
        step = 0.05
    } else if (feature %in% c("recency", "frequency")) {
        round = 0
        step = 10
    }
    
    if (feature == "spend_90_flag") {
        widget <-   pickerInput(
            inputId  = id,
            label    = div(id = "plain_label", label),
            choices  = unique(data[[feature]]),
            selected = unique(data[[feature]]),
            multiple = TRUE
        )
        #widget <- NULL
        
    } else {
        
        widget <- numericRangeInput(
            inputId = id, 
            label = div(id = "plain_label", label),
            value = c(round(min(data[[feature]]), round), round(max(data[[feature]]), round)),
            width = "120%",
            min   = min(data[[feature]] %>% round(round)),
            max   = max(data[[feature]] %>% round(round)),
            step  = step
            
        )
        
    }
    
    return(widget)

    
}

# numericRangeInput(
#     inputId = "id_actual_spend",
#     label   = "Actual Spend ($)",
#     value   = c(
#         min(clv_predictions_data$spend_90_total %>% round(0)),
#         max(clv_predictions_data$spend_90_total %>% round(0))
#     ),
#     width   = "120%",
#     min     = min(clv_predictions_data$spend_90_total %>% round(0)),
#     max     = max(clv_predictions_data$spend_90_total %>% round(0)),
#     step    = 100
# )


clv_picker_ui <- function(id, label = "Picker Label") {
    
  ns <- NS(id)
  
  tagList(
    shinyWidgets::pickerInput(
          inputId  = ns("clv"),
          label    = h4(label),
          choices  = NULL,  # These will be set in the server part
          selected = NULL,
          multiple = TRUE,
          options  = list(
              `actions-box`          = TRUE,
              `deselect-all-text`    = "Deselect All",
              `select-all-text`      = "Select All",
              `none-selected-text`   = "Select Country",
              `selected-text-format` = "count > 3",
              liveSearch             = TRUE
          )
      )
  )
}

clv_picker_server <- function(id, data = reactive(NULL), feature = reactive()) {
  
  moduleServer(id, function(input, output, session) {
        
        ns <- getDefaultReactiveDomain()$ns
       
        shiny::observe({
          
          # error handling
          if (is.null(data()) || is.null(data()[[feature()]])) {
            return()
          }
          
            print( sort(unique(data()[[feature()]])) )
            
            shinyWidgets::updatePickerInput(
                session  = getDefaultReactiveDomain(),
                inputId  = ns("clv"),
                choices  = sort(unique(data()[[feature()]])), 
                selected = sort(unique(data()[[feature()]]))
            )
        })
      
    }
  )
}


# * CLV Prediction Data Prep ----
get_clv_predictions_dt_data <- function(data){
    
  data_prep <- data %>% 
    filter(spend_actual_vs_pred <= 2500 & spend_actual_vs_pred >= - 500) %>% 
      select(
        customer_id, .pred_prob, .pred_total, starts_with("spend"), 
        recency, frequency, starts_with("sales"),
        first_purchase_cohort, country
      ) %>%
      
      # spend vs actual color flag
      mutate(color = case_when(
        spend_actual_vs_pred %>% between(-500, -250)  ~ "color0",
        spend_actual_vs_pred %>% between(-249, 0)     ~ "color1",
        spend_actual_vs_pred %>% between(0, 250)      ~ "color2",
        spend_actual_vs_pred %>% between(251, 500)    ~ "color3",
        spend_actual_vs_pred %>% between(501, 1000)   ~ "color4",
        spend_actual_vs_pred %>% between(1001, 1250)  ~ "color5",
        spend_actual_vs_pred %>% between(1251, 1500)  ~ "color6",
        TRUE                                          ~ "color7"
      )) %>% 
      mutate(across(ends_with("total"), ~ scales::dollar(., accuracy = 0.01))) %>%
      mutate(across(starts_with("sales"), ~ scales::dollar(., accuracy = 0.01))) %>%
      mutate(across(spend_actual_vs_pred, ~ scales::dollar(., accuracy = 0.01))) %>%
      mutate(across(ends_with("prob"), ~ scales::percent(., accuracy = 0.01))) %>% 
      setNames(names(.) %>% str_replace_all("_", " ") %>% str_to_title())
  
  return(data_prep)
    
}

# * DT Table ----
get_dt_table <- function(data, align_center_cols = "_all", invisible_cols = NULL,
                         clv_format = TRUE) {
  
  dt <- data %>%
    datatable(
      #rownames = FALSE,
      options = list(
        pageLength = 10,
        columnDefs = list(
          list(className = "dt-center", targets = align_center_cols),
          list(visible = FALSE, targets = invisible_cols)
        )
      )
    )
  
  if (clv_format) {
      # format color
    
    dt <- dt %>% 
      formatStyle(
        columns = "Spend Actual Vs Pred",
        valueColumns = "Color",
        backgroundColor = styleEqual(
          c("color0", "color1", "color2", "color3", "color4", "color5", "color6", "color7"),
          c("#f88379", "#e59562", "#acb36c", "#88ab7b", "#7f9a71", "#676147", "#4d3e32", "#2b211f")
        )
      ) %>% 
      formatStyle(
        columns = "Spend Actual Vs Pred",
        valueColumns = "Color",
        color = "white"
      ) 
  } 
  
  # return
  return(dt)
}



