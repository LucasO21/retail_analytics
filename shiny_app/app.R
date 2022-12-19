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
source("app_functions/ui_server_functions.R")
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
clv_pred_tbl <- read_rds("../shiny_app/app_artifacts/clv_artifacts_list.rds")[[2]][[3]]


# ******************************************************************************
# SERVIER ----
# ******************************************************************************
ui <- tagList(
    navbarPage(
        
        title = "Retail Analytics App",
        
        # Home Panel
        tabPanel(
            title = "CLV Analysis",
            
            fluidPage(
                sidebarLayout(
                    sidebarPanel(
                        width = 2,
                        
                        # Country Picker Input
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
                        
                        # Percent of Data Input
                        sliderInput(
                            inputId = "sample_prop",
                            label   = h4("Proportion of Data Shown"),
                            min     = 0,
                            max     = 1,
                            value   = 1,
                            step    = 0.05 
                        )
                    ),
                    mainPanel(
                        width = 10,
                        
                        fluidRow(
                            get_plot_box(
                                .title     = "Spend Probablity", 
                                .id        = "spend_prob_plot_info",
                                .output    = "plot"
                            ),
                            
                            get_plot_box(.title = "Key Features", .id = "key_features")
                        
                               
                            ),
                        
                        fluidRow(
                            get_plot_box(
                                .col_width = 12, 
                                .title     = "Spend Probability Data", 
                                .id        = "spend_prob_data"
                            )
                        )
                        
                    )
                      
                )
            )
        )
    )
)
    



# ******************************************************************************
# SERVIER ----
# ******************************************************************************
server <- function(input, output) {
    
    # * Data Filtered ----
    clv_filtered_tbl <- reactive({

        clv_pred_tbl %>%
            filter(country %in% input$country_picker) %>%
            sample_frac(size = input$sample_prop)

    })


    # * Spend Prob Scatterplot ----
    output$spend_prob_plot <- renderPlotly({
        
        clv_pred_tbl %>% 
            get_scatter_plot_data()
            get_scatter_plot()
        
    })
    
}

shinyApp(ui, server)