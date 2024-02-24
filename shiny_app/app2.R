# RETAIL ANALYTICS APP VERSION 2 ----
# *** ----

# *****************************************************************************
# SETUP ----
# *****************************************************************************

# * Set Working Dir ----
setwd(here::here("shiny_app"))

# * Libraries ----
# * Core ----
library(tidyverse)
library(janitor)
library(lubridate)
library(timetk)

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
# **** ----
# UI ----
# *****************************************************************************
ui <- tagList(
    shinyjs::useShinyjs(),
    useShinydashboard(),
    tags$script(src="https://kit.fontawesome.com/77fcf700e6.js"),
    
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
                                                choices = NULL,
                                                selected = NULL,
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
                                        ),
                                        div(
                                            style = "margin-left: 10px;",
                                            class = "col-md-2",
                                            pickerInput(
                                                inputId = "purchase_cohort",
                                                label = h4("Select Purchase Cohort"),
                                                choices = NULL,
                                                selected = NULL,
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
                                        ),
                                        div(
                                            style = "margin-left: 10px;",
                                            class = "col-md-2",
                                            sliderInput(
                                                inputId = "sample_prop",
                                                label   = h4("Proportion of Data Shown"),
                                                min     = 0.1,
                                                max     = 1,
                                                value   = 1,
                                                step    = 0.25
                                            )
                                        )
                                    ),
                                    
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
            )
        )
    )
)






# *****************************************************************************
# **** ----
# SERVER ----
# *****************************************************************************
server <- function(input, output, session) {
    
    # * DATA ----
    
    # ** First Purchase Date ----
    first_purchase_tbl <- reactive({
        read_rds("app_data/first_purchase_data.rds")
    })
    
    clv_predictions_tbl <- reactive({
        read_rds("app_artifacts/clv_artifacts_list.rds")[[2]][[3]] %>% 
            left_join(
                first_purchase_tbl() %>% 
                    select(customer_id, country)
            )
    })
    
    # * CLV TAB ----
    
    # ** Toggle Inputs ----
    shinyjs::onclick(id = "toggle_clv_input", expr = {
        shinyjs::toggle(id = "clv_inputs", anim = TRUE, animType = "slide")
    })
    
    # ** Update Picker Inputs ----
    shiny::observe({
        updatePickerInput(
            session = session,
            inputId = "country_picker",
            choices = unique(clv_predictions_tbl()$country),
            selected = unique(clv_predictions_tbl()$country)
        )
        
        updatePickerInput(
            session = session,
            inputId = "purchase_cohort",
            choices = unique(first_purchase_tbl()$first_purchase_cohort),
            selected = unique(first_purchase_tbl()$first_purchase_cohort)
        )
    })
    
    # ** CLV Spend/Prob Plot ----
    output$clv_pred_plot <- renderPlotly({
        
        clv_predictions_tbl() %>% 
            get_scatter_plot_data() %>% 
            filter(country %in% input$country_picker) %>%
            sample_frac(size = input$sample_prop) %>%
            get_scatter_plot()
    })
    
    # ** CLV Feature Plot ----
    output$clv_feat_plot <- renderPlotly({
        
        clv_predictions_tbl() %>% 
            get_features_plot_data() %>%
            left_join(
                first_purchase_tbl() %>% 
                    select(customer_id, country)
            ) %>%
            filter(country %in% input$country_picker) %>%
            sample_frac(size = input$sample_prop) %>%
            get_features_plot()
    })
    
}


# *****************************************************************************
# **** ----
# RUN APP ----
# *****************************************************************************
shinyApp(ui, server)


# *****************************************************************************
# **** ----
# SECTION NAME ----
# *****************************************************************************
