# SHINY APP UI & SERVER SCRIPT ----
# **** ----

# Set Working Dir ----
# setwd(here::here("shiny_app"))

# Libraries ----

# * Core ----
library(tidyverse)
library(janitor)
library(lubridate)
# library(DBI)

# * Shiny ----
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)
library(shinyjs)

# * Visualization ----
library(plotly)
library(DT)

# * Modeling ----
library(xgboost)

# * Source ----
# source("app_functions/ui_server_functions.R")
source("app_functions/clv_functions.R")


# Data Import
# * Setup DB Connection ----
# con <- dbConnect(RSQLite::SQLite(), dbname = "../data/database.db")
# dbListTables(con)

# * First Purchase Data ----
# first_purchase_tbl <- tbl(con, "first_purchase_tbl") %>% collect() 
# first_purchase_tbl %>% write_rds("app_data/first_purchase_data.rds")
fp_tbl <- read_rds("../shiny_app/app_data/first_purchase_data.rds")

# * Load Sales Data ----
# retail_data_clean_tbl <- tbl(con, "retail_data_clean_tbl") %>% 
#     collect() %>% 
#     mutate(invoice_date = lubridate::date(invoice_date)) %>% 
#     filter(invoice_date <= as.Date("2011-11-30"))
# retail_data_clean_tbl %>% write_rds("app_data/retail_clean_data.rds")
rdc_tbl <- read_rds("../shiny_app/app_data/retail_clean_data.rds")

# * CLV Data ----
clv_pred_tbl <- read_rds("../shiny_app/app_artifacts/clv_artifacts_list.rds")[[2]][[3]] %>% 
    left_join(fp_tbl %>% select(customer_id, country))

# clv_pred_tbl <- clv_list[[2]][[3]] %>% 
#     left_join(fp_tbl %>% select(customer_id, country))


# ******************************************************************************
# UI ----
# ******************************************************************************
ui <- tagList(
    # useShinyjs(), 
    
    navbarPage(
        
        title = "Retail Analytics App",
        
        # * CLV Page ----
        tabPanel(
            title = "CLV Analysis",
            fluidPage(
                sidebarLayout(
                    
                    # * Side Bar Panel ----
                    sidebarPanel(
                        width = 2,
                        
                        # ** Country Picker Input ----
                        pickerInput(
                            inputId  = "country_picker",
                            label    = h4("Select Country"),
                            choices  = unique(rdc_tbl$country),
                            selected = unique(rdc_tbl$country),
                            multiple = TRUE,
                            options  = list(
                                `actions-box`          = TRUE,
                                size                   = 10,
                                `selected-text-format` = "count > 3"
                            )
                        ),
                        
                        br(),
                        
                        # ** Percent of Data Input ----
                        sliderInput(
                            inputId = "sample_prop",
                            label   = h4("Proportion of Data Shown"),
                            min     = 0.1,
                            max     = 1,
                            value   = 1,
                            step    = 0.05 
                        )
                    ),
                    
                    # * Main Panel ----
                    mainPanel(
                        width = 10,
                        
                        # ** Fluid Row 1 ----
                        fluidRow(
                      
                            # *** Column 1 ----
                            column(
                                width = 6,
                                tags$fieldset(
                                    tags$legend(
                                        "CLV Spend Probability", 
                                        tags$span(id = "info1", icon("info-circle"))
                                    ),
                                    plotlyOutput("spend_prob_p", height = "400px")
                                )
                            ), # End Column 1
                            
                            # *** Column 2 ----
                            column(
                                width = 6,
                                tags$fieldset(
                                    tags$legend(
                                        "CLV Features", 
                                        tags$span(id = "info2", icon("info-circle"))
                                    ),
                                    plotlyOutput("features_p", height = "400px")
                                )
                            ) # End Column
                        ),
                        
                        br(), br(), 
                        
                        
                        # ** Fluid Row 2 ----
                        fluidRow(
                            
                            column(
                                width = 12,
                                tags$fieldset(
                                    tags$legend(
                                        "CLV Features Data", 
                                        tags$span(id = "info3", icon("info-circle"))
                                    ),
                                    dataTableOutput("clv_data", height = "400px")
                                )
                            ) # End Column
                        )
                        
                    ) # End Main Panel
                      
                )
            )
        )
    )
)
    



# ******************************************************************************
# SERVIER ----
# ******************************************************************************
server <- function(input, output) {
    
    # * CLV Data Filtered ----
    clv_filtered_tbl <- reactive({

        clv_pred_tbl %>%
            get_scatter_plot_data() %>% 
            filter(country %in% input$country_picker) %>% 
            sample_frac(size = input$sample_prop)

    })
    
    # * CLV Spend Prob Scatterplot ----
    output$spend_prob_p <- renderPlotly({

        clv_filtered_tbl() %>% 
            get_scatter_plot_data() %>% 
            get_scatter_plot()

    })
    
    # * CLV Features Plot ----
    output$features_p <- renderPlotly({
        
        clv_filtered_tbl() %>% 
            get_features_plot_data() %>% 
            get_features_plot()
    })
    
    # * CLV Spend/Features Data ----
    output$clv_data <- DT::renderDataTable({
        clv_filtered_tbl() %>% 
            select(-text) %>% 
            get_clv_dt() 

    })
    
 
    
 
    
    
    

}

shinyApp(ui, server)