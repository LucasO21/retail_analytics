# RETAIL ANALYTICS APP VERSION 2 ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
# setwd(here::here("shiny_app"))

# * Libraries ----
# * Core ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)
library(dtplyr)

# * Shiny ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(rintrojs)
library(bslib)
library(shinythemes)

# * Visualization ----
library(plotly)
library(DT)

# * Modeling ----
library(xgboost)
library(ranger)
library(rules)
library(coop)
library(modeltime)

# * Source ----
source("app_functions/ui_server_functions.R")
source("app_functions/clv_functions.R")
source("app_functions/pr_functions.R")
source("app_functions/forecast_functions.R")


# *****************************************************************************
# DATA ----
# *****************************************************************************
first_purchase_data <- read_rds("app_data/first_purchase_data.rds")

# * CLV Predictions Data ----
clv_predictions_data <- read_rds("app_artifacts/clv_artifacts_list.rds")[[2]][[3]] %>%
    left_join(
      first_purchase_data %>%
        select(customer_id, first_purchase_cohort, country)
    )



# *****************************************************************************
# **** ----
# UI ----
# *****************************************************************************
ui <- tagList(
    shinyjs::useShinyjs(),
    useShinydashboard(),
    tags$script(src="https://kit.fontawesome.com/77fcf700e6.js"),
    tags$head(tags$style(HTML("#plain_label .control-label { font-weight: normal; }"))),
    
    navbarPage(
        id = "tabset",
        title = "Retail Analytics App",
        
        
        # * Home Tab ----
        tabPanel(
            title = "Home", icon = icon("home"),
            fluidRow(
                column(
                    width = 10,
                    offset = 1,
                    div(
                        class = "page-header",
                        h1("Welcome to the Retail Analytics App"),
                        h4("This app is designed to help you analyze retail data and make data-driven decisions."),
                        h4("Use the tabs below to navigate through the app.")
                    )
                )
            ),
            
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        "Fluid Row 2 Placeholder"
                    )
                )
            ),
            
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        "Fluid Row 3 Placeholder"
                    )
                )
            )
        ),
        
        
        # * CLV TAB ----
        tabPanel(
            title = "CLV Analysis", icon = icon("hand-holding-dollar"),
            value = "clv_tab",
            
            # ** Header ----
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        class = "page-header",
                        h1("Customer Lifetime Value Analysis"),
                        h4("Analyze the next 90 day spend probability and spend total prediction.")
                    )
                )
            ),
            
            # ** Input Panel ----
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        wellPanel(
                            fluidRow(
                                div(
                                    style = "margin-left: 10px;",
                                    actionButton("toggle_clv_input", "Toggle Inputs", icon("caret-down"))
                                ), 
                                br(),
                                
                                div(
                                    id = "clv_inputs",
                                    div(
                                        class = "row",
                                        div(
                                            style = "margin-left: 10px;",
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId = "country_picker",
                                                label = h4("Select Country"),
                                                choices = sort(unique(clv_predictions_data$country)),
                                                selected = sort(unique(clv_predictions_data$country)),
                                                multiple = TRUE,
                                                options = list(
                                                    `actions-box` = TRUE,
                                                    `deselct-all-text` = "Deselect All",
                                                    `select-all-text` = "Select All",
                                                    `none-selected-text` = "Select Country",
                                                    `selected-text-format` = "count > 3",
                                                    liveSearch = TRUE
                                                )
                                            )
                                            #clv_picker_ui("clv_picker_id", "Choose Countries")
                                        ),
                                        div(
                                            style = "margin-left: 10px;",
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId = "purchase_cohort",
                                                label = h4("Select Purchase Cohort"),
                                                choices = sort(unique(clv_predictions_data$first_purchase_cohort)),
                                                selected = sort(unique(clv_predictions_data$first_purchase_cohort)),
                                                multiple = TRUE,
                                                options = list(
                                                    `actions-box` = TRUE,
                                                    `deselct-all-text` = "Deselect All",
                                                    `select-all-text` = "Select All",
                                                    `none-selected-text` = "Select Purchase Cohort",
                                                    `selected-text-format` = "count > 3",
                                                    liveSearch = TRUE
                                                )
                                            )
                                        )
                                    ),
                                    
                                    hr(),
                                    
                                    # * Pred Spend & Prob Filter Ranges ----
                                    div(
                                        class = "row",
                                        
                                        # * Pred Spend & Prob Filter Ranges ----
                                        div(
                                            style = "margin-left: 10px;",
                                            class = "col-md-2",
                                            numericRangeInput(
                                              inputId = "id_pred_spend",
                                              label   = "Pred Spend ($)",
                                              value   = c(
                                                min(clv_predictions_data$.pred_total %>% round(0)),
                                                max(clv_predictions_data$.pred_total %>% round(0))
                                              ),
                                              width   = "120%",
                                              min     = min(clv_predictions_data$.pred_total %>% round(0)),
                                              max     = max(clv_predictions_data$.pred_total %>% round(0)),
                                              step    = 100

                                            ),
                                            
                                            numericRangeInput(
                                              inputId = "id_pred_prob",
                                              label   = "Pred Prob (%)",
                                              value   = c(
                                                min(clv_predictions_data$.pred_prob %>% round(2)),
                                                max(clv_predictions_data$.pred_prob %>% round(2))
                                              ),
                                              width   = "120%",
                                              min     = min(clv_predictions_data$.pred_prob %>% round(2)),
                                              max     = max(clv_predictions_data$.pred_prob %>% round(2)),
                                              step    = 0.1
                                            )
                                        ),
                              
                                        # * Actual Spend & Prob Filter Ranges ----
                                        div(
                                            style = "margin-left: 10px;",
                                            class = "col-md-2",
                                            numericRangeInput(
                                              inputId = "id_actual_spend",
                                              label   = "Actual Spend ($)",
                                              value   = c(
                                                min(clv_predictions_data$spend_90_total %>% round(0)),
                                                max(clv_predictions_data$spend_90_total %>% round(0))
                                              ),
                                              width   = "120%",
                                              min     = min(clv_predictions_data$spend_90_total %>% round(0)),
                                              max     = max(clv_predictions_data$spend_90_total %>% round(0)),
                                              step    = 100
                                            ),
                                            
                                            pickerInput(
                                              inputId  = "id_actual_flag",
                                              label    = "Actual Spend Flag",
                                              choices  = unique(clv_predictions_data$spend_90_flag),
                                              selected = unique(clv_predictions_data$spend_90_flag),
                                              multiple = TRUE
                                            )
                                            
                                        ),
                                        
                                        # * Recency & Frequency Filter Ranges ----
                                        div(
                                            style = "margin-left: 10px;",
                                            class = "col-md-2",
                                            numericRangeInput(
                                              inputId = "recency_range",
                                              label   = "Recency (Days)",
                                              value   = c(
                                                min(clv_predictions_data$recency %>% round(0)),
                                                max(clv_predictions_data$recency %>% round(0))
                                              ),
                                              width   = "120%",
                                              min     = min(clv_predictions_data$recency %>% round(0)),
                                              max     = max(clv_predictions_data$recency %>% round(0)),
                                              step    = 1
                                            ),
                                            
                                            numericRangeInput(
                                              inputId = "frequency_range",
                                              label   = "Frequency",
                                              value   = c(
                                                min(clv_predictions_data$frequency %>% round(0)),
                                                max(clv_predictions_data$frequency %>% round(0))
                                              ),
                                              width   = "120%",
                                              min     = min(clv_predictions_data$frequency %>% round(0)),
                                              max     = max(clv_predictions_data$frequency %>% round(0)),
                                              step    = 1
                                            )
                                        )
                                    ),
                                    
                                    hr(),
                                    
                                    div(
                                        class = "row",
                                        div(
                                            style = "margin-left: 20px;",
                                            actionButton("apply_clv", "Apply", icon = icon("play"), width = "140px"),
                                            actionButton("reset_clv", "Reset", icon = icon("redo"), width = "140px"),
                                            actionButton("download_clv", "Download", icon = icon("download"), width = "140px"),
                                            actionButton("help_clv", "Help", icon = icon("question"), width = "140px")
                                        )
                                    )
                                ) %>% shinyjs::hidden()
                            )
                        )
                    )
                )
            ),
            br(),
            
            # ** Output Panel ----
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        box(
                            width = 6,
                            solidHeader = TRUE,
                            rounded = TRUE,
                            h3("90 Day Spend Probability & Spend Total Prediction", tags$span(id = "clv_pred_dt"), icon("info-circle")),
                            plotlyOutput("clv_pred_plot")
                        ),
                        box(
                            width = 6,
                            solidHeader = TRUE,
                            rounded = TRUE,
                            h3("90 Day Spend Prob & Total Prediction (Key Features)", tags$span(id = "clv_feat_dt"), icon("info-circle")),
                            plotlyOutput("clv_feat_plot")
                        )
                    )
                )
            ),
            
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                        style = "text-align: center;",
                        img(src = "legend.png", width = "30%")
                    )
                )
            ),
            
            fluidRow(
                column(
                    width = 10, offset = 1,
                    div(
                      box(
                        width = 12,
                        solidHeader = TRUE,
                        rounded = TRUE,
                        h3("90-Day Spend Probability & Key Features Details", tags$span(id = "clv_data"), icon("info-circle")),
                        DT::dataTableOutput("clv_data")
                      )
                    )
                )
            )
        )
    )
)

  




# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output, session) {
  
  # 1.0 DATA -------------------------------------------------------------------
  
  # * 1.1 CLV Predictions Filtered (Apply Button) ----
  clv_predictions_filtered_tbl <- eventReactive(input$apply_clv, valueExpr = {
    
    clv_predictions_data %>% 
      filter(country %in% input$country_picker) %>%
      filter(first_purchase_cohort %in% input$purchase_cohort) %>%
      filter(.pred_total >= input$id_pred_spend[1] & .pred_total <= input$id_pred_spend[2]) %>% 
      filter(.pred_prob >= input$id_pred_prob[1] & .pred_prob <= input$id_pred_prob[2]) %>% 
      filter(spend_90_total >= input$id_actual_spend[1] & spend_90_total <= input$id_actual_spend[2]) %>%
      filter(spend_90_flag %in% input$id_actual_flag) %>% 
      filter(recency >= input$recency_range[1] & recency <= input$recency_range[2]) %>%
      filter(frequency >= input$frequency_range[1] & frequency <= input$frequency_range[2])
    
  }, ignoreNULL = FALSE)
  
  # * 1.2 First Purchase Data (Apply) ----
  first_purchase_tbl <- eventReactive(input$apply_clv, valueExpr = {
    
    first_purchase_data %>% 
      filter(country %in% input$country_picker) %>%
      filter(first_purchase_cohort %in% input$purchase_cohort)
    
  }, ignoreNULL = FALSE)
  
  # **** -----------------------------------------------------------------------
  
    
  # 2.0 CLV TAB ----------------------------------------------------------------
    
  # * 2.1 Toggle Inputs ----
  shinyjs::onclick(id = "toggle_clv_input", expr = {
      shinyjs::toggle(id = "clv_inputs", anim = TRUE, animType = "slide")
  })
  
  # * 2.2 CLV Spend/Prob Plot ----
  output$clv_pred_plot <- renderPlotly({
    clv_predictions_filtered_tbl() %>%
      filter(spend_actual_vs_pred <= 1500 & spend_actual_vs_pred >= -100) %>% 
      get_scatter_plot_data() %>%
      get_scatter_plot()
  })
  
  # * 2.3 CLV Feature Plot ----
  output$clv_feat_plot <- renderPlotly({
    
    clv_predictions_filtered_tbl() %>%
      filter(spend_actual_vs_pred <= 1500 & spend_actual_vs_pred >= -100) %>% 
      #data %>% 
      get_features_plot_data() %>%
      left_join(
        first_purchase_tbl() %>%
          select(customer_id, country, first_purchase_cohort)
      ) %>%
      get_features_plot()
  })
  
  # * 2.4 CLV Data Table ----
  output$clv_data <- DT::renderDT({
    clv_predictions_filtered_tbl() %>%
      #data %>% 
      select(
        customer_id, .pred_prob, .pred_total, starts_with("spend"), 
        recency, frequency, starts_with("sales"),
        first_purchase_cohort, country
      ) %>%
      
      # spend vs actual color flag
      mutate(color = case_when(
        spend_actual_vs_pred < 0                    ~ "color0",
        spend_actual_vs_pred %>% between(0, 500)    ~ "color1",
        spend_actual_vs_pred %>% between(501, 1000) ~ "color2",
        TRUE                                        ~ "color3"
      )) %>% 
      mutate(across(ends_with("total"), ~ scales::dollar(., accuracy = 0.01))) %>%
      mutate(across(starts_with("sales"), ~ scales::dollar(., accuracy = 0.01))) %>%
      mutate(across(spend_actual_vs_pred, ~ scales::dollar(., accuracy = 0.01))) %>%
      mutate(across(ends_with("prob"), ~ scales::percent(., accuracy = 0.01))) %>% 
      setNames(names(.) %>% str_replace_all("_", " ") %>% str_to_title()) %>%
      
      # data table
      datatable(
        #rownames = FALSE,
        options = list(
          pageLength = 10,
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(visible = FALSE, targets = c(13))
          )
        )
      ) %>% 
      
      # format color
      formatStyle(
        columns = "Spend Actual Vs Pred",
        valueColumns = "Color",
        backgroundColor = styleEqual(
          c("color0", "color1", "color2", "color3"),
          c("#f88379", "#78c2ad", "#68a996", "#487568")
        )
      )
  })
  
  # * 2.5 Reset Button ----
  observeEvent(eventExpr = input$reset_clv, handlerExpr = {
    
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "country_picker",
      choices = sort(unique(clv_predictions_data$country)),
      selected = sort(unique(clv_predictions_data$country))
    )
    
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "purchase_cohort",
      choices = sort(unique(clv_predictions_data$first_purchase_cohort)),
      selected = sort(unique(clv_predictions_data$first_purchase_cohort))
    )
    
    updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "id_pred_spend",
      value = c(
        min(clv_predictions_data$.pred_total %>% round(0)),
        max(clv_predictions_data$.pred_total %>% round(0))
      )
    )
    
    updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "id_pred_prob",
      value = c(
        min(clv_predictions_data$.pred_prob %>% round(2)),
        max(clv_predictions_data$.pred_prob %>% round(2))
      )
    )
    
    updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "id_actual_spend",
      value = c(
        min(clv_predictions_data$spend_90_total %>% round(0)),
        max(clv_predictions_data$spend_90_total %>% round(0))
      )
    )
    
    updatePickerInput(
      session = getDefaultReactiveDomain(),
      inputId = "id_actual_flag",
      choices = unique(clv_predictions_data$spend_90_flag),
      selected = unique(clv_predictions_data$spend_90_flag)
    )
    
    updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "recency_range",
      value = c(
        min(clv_predictions_data$recency %>% round(0)),
        max(clv_predictions_data$recency %>% round(0))
      )
    )
    
    updateNumericRangeInput(
      session = getDefaultReactiveDomain(),
      inputId = "frequency_range",
      value = c(
        min(clv_predictions_data$frequency %>% round(0)),
        max(clv_predictions_data$frequency %>% round(0))
      )
    )
    
    shinyjs::delay(ms = 300, expr = {
      shinyjs::click(id = "apply_clv")
    })
  })
  
  # **** -----------------------------------------------------------------------
  
 
    
  
    
    
  
      

 
    

    
   
    
  
    
}


# *****************************************************************************
# **** ----
# RUN APP ----
# *****************************************************************************
shinyApp(ui, server)

