# SHINY APP UI & SERVER SCRIPT ----
# **** ----

# Set Working Dir ----
# setwd(here::here("shiny_app"))

# Libraries ----

# * Core ----
library(tidyverse)
library(janitor)
library(lubridate)

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
library(coop)

# * Source ----
source("../shiny_app/app_functions/clv_functions.R")
source("../shiny_app/app_functions/pr_functions.R")


# Data Import ----

# * First Purchase Data ----
fp_tbl <- read_rds("../shiny_app/app_data/first_purchase_data.rds")

# * Load Sales Data ----
rdc_tbl <- read_rds("../shiny_app/app_data/retail_clean_data.rds")

# * CLV Data ----
clv_pred_tbl <- read_rds("../shiny_app/app_artifacts/clv_artifacts_list.rds")[[2]][[3]] %>% 
    left_join(fp_tbl %>% select(customer_id, country))

customer_country_tbl <- clv_pred_tbl %>% 
    select(customer_id, country) %>% 
    distinct() %>% 
    arrange(country) %>% 
    mutate(customer_id = as.character(customer_id))


# ******************************************************************************
# UI ----
# ******************************************************************************
ui <- tagList(
    # useShinyjs(), 
    useShinydashboard(),
    
    navbarPage(
        
        title = "Retail Analytics App",
        
        # * CLV Page ----
        tabPanel(
            title = "CLV Analysis",
            fluidPage(
                sidebarLayout(
                    
                    # ** Side Bar Panel ----
                    sidebarPanel(
                        width = 2,
                        
                        # *** Country Picker Input ----
                        pickerInput(
                            inputId  = "country_picker",
                            label    = h4("Select Country"),
                            choices  = sort(unique(customer_country_tbl$country)),
                            selected = sort(unique(customer_country_tbl$country)),
                            multiple = TRUE,
                            options  = list(
                                `actions-box`          = TRUE,
                                size                   = 10,
                                `selected-text-format` = "count > 3"
                            )
                        ),
                        
                        br(),
                        
                        # *** Percent of Data Input ----
                        sliderInput(
                            inputId = "sample_prop",
                            label   = h4("Proportion of Data Shown"),
                            min     = 0.1,
                            max     = 1,
                            value   = 1,
                            step    = 0.05 
                        ),
                        
                        br(), hr(), br(),
                        
                        # *** Download Button Input ----
                        downloadButton(
                            outputId = "download_clv_data",
                            label    = "Download CLV Data" 
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
                                box(
                                    width = 12,
                                    tags$fieldset(
                                        tags$legend(
                                            "Future Forecast", 
                                            tags$span(id = "ff_plot_info", icon("info-circle"))
                                        ),
                                        plotlyOutput("spend_prob_p", height = "400px")
                                    )
                                )
                            ),
                
                            # *** Column 2 ----
                            column(
                                width = 6,
                                box(
                                    width = 12,
                                    tags$fieldset(
                                        tags$legend(
                                            "CLV Features Plot", 
                                            tags$span(id = "info2", icon("info-circle")),
                                    ),
                                    plotlyOutput("features_p", height = "400px")
                                )
                            )
                        )
                    ),
                        
                        
                        # ** Fluid Row 2 ----
                        fluidRow(
                            
                            # *** Column 1 ----
                            column(
                                width = 12,
                                box(
                                    width = 24,
                                    tags$fieldset(
                                        tags$legend(
                                            "CLV Features Data", 
                                            tags$span(id = "info3", icon("info-circle"))
                                        ),
                                        dataTableOutput("clv_data", height = "400px")
                                    )
                                )
                            ) 
                        )
                        
                    )
                      
                )
            )
        ), # End CLV tabPanel
        
        # * Product Recommender Tab ----
        tabPanel(
            title = "Product Recommender",
            
            fluidPage(
                sidebarLayout(
                    
                    # ** Side Bar Panel ----
                    sidebarPanel(
                        width = 2,
                        
                        # *** Country Picker Input ----
                        pickerInput(
                            inputId  = "country_picker_2",
                            label    = h4("Select Country"),
                            choices  = sort(unique(customer_country_tbl$country)),
                            selected = sort(unique(customer_country_tbl$country)),
                            multiple = TRUE,
                            options  = list(
                                `actions-box`          = TRUE,
                                size                   = 10,
                                `selected-text-format` = "count > 3"
                            )
                        ),
                        
                        # *** Customer ID Input ----
                        selectizeInput(
                            inputId  = "customer_id",
                            label    = h4("Select Customer ID"),
                            choices  = NULL,
                            selected = NULL,
                            multiple = FALSE
                        ),
                        
                        br(), hr(), br(),
                        
                        # *** Download Product Recommender Data ----
                        downloadButton(
                            outputId = "download_clv_data",
                            label    = "Download Data" 
                        )
                    ),
                    
                    # * Main Panel ----
                    mainPanel(
                        
                        # ** Fluid Row 1 ----
                        fluidRow(
                            
                            # *** Column 1 ----
                            column(
                                width = 12,
                                box(
                                    width = 24,
                                    tags$h3("Product Recommender"),
                                    HTML(
                                        "
                                        <p>This tab contains info on personalized product recommendations for each customer. The product
                                        recommendations are based on similarities among customers, meaning product recommendations for a
                                        particular customer are based on what other similar customers have purchased in the past.
                                        To learn more on user-based collaborative filtering, visit
                                        <a href='https://en.wikipedia.org/wiki/Collaborative_filtering'>this link.</a></p>
                                        ")
                                    )               
                                )
                            ),
                        
                        # ** Fluid Row 2 ----
                        fluidRow(
                            
                            # *** Column 1 ----
                            column(
                                width = 12,
                                box(
                                    width = 24,
                                    tags$h3("Product Recommendations"),
                                    textOutput("test")
                                )
                            )
                        ),
                        
                        # ** Fluid Row 3 ----
                        fluidRow(
                            
                            # *** Column 1 ----
                            column(
                                width = 12,
                                box(
                                    width = 24,
                                    tags$h3("Average Amount Spent by Customer on Recommended Products"),
                                    dataTableOutput("opportunity")
                                )
                            )
                        )
                    ), # end mainPanel
                )
            )
        )
    ) # End navbarPage
)





# ******************************************************************************
# SERVIER ----
# ******************************************************************************
server <- function(input, output) {
    
    # * CLV Tab Server Functions ----
    
    # ** CLV Data Filtered ----
    clv_filtered_tbl <- reactive({

        clv_pred_tbl %>%
            get_scatter_plot_data() %>% 
            filter(country %in% input$country_picker) %>% 
            sample_frac(size = input$sample_prop)

    })
    
    # ** CLV Spend Prob Scatterplot ----
    output$spend_prob_p <- renderPlotly({

        clv_filtered_tbl() %>% 
            get_scatter_plot_data() %>% 
            get_scatter_plot()

    })
    
    # ** CLV Features Plot ----
    output$features_p <- renderPlotly({
        
        clv_filtered_tbl() %>% 
            get_features_plot_data() %>% 
            get_features_plot()
    })
    
    # ** CLV Spend/Features Data ----
    output$clv_data <- DT::renderDataTable({
        clv_filtered_tbl() %>% 
            select(-text) %>% 
            get_clv_dt() %>% 
            mutate_if(is.numeric, as.character) %>% 
            mutate_if(is.factor, as.character)

    })
    
    # ** CLV Data Download ----
    output$download_clv_data <- downloadHandler(
        
        filename = function(){
            paste("clv_data", "csv", sep = ".")
        },
        
        content = function(file){
            write.csv(
                clv_filtered_tbl() %>% 
                    select(-text) %>% 
                    get_clv_dt(),
                file
            )
        }
    )
    
    # **************************************************************************
    
    # * Product Recommender Server Functions ----
    
    # ** Update Filtering Logic ----
    observe({
        
        x <- customer_country_tbl %>% 
            filter(country %in% input$country_picker_2) %>% 
            select(customer_id)
        
        updateSelectInput(
            session  = getDefaultReactiveDomain(),
            inputId  = "customer_id",
            choices  = unique(x$customer_id),
            selected = unique(x$customer_id)[1]
        )
            
    })
    
    # ** Product Recommender Data Filtered ----
    start_date              <- max(retail_data_clean_tbl$invoice_date) - 90
    end_date                <- max(retail_data_clean_tbl$invoice_date)
    
    pr_recommender_filtered_tbl <- reactive({
    
        rdc_tbl %>% 
            filter(customer_id %in% clv_pred_tbl$customer_id) %>% 
            filter(between(invoice_date, start_date, end_date))
        
    })
    
    # ** Recommended Products List ----
    output$test <- renderText({
        
        l <- pr_recommender_filtered_tbl() %>% 
            get_user_item_matrix() %>% 
            get_user_to_user_cosine_matrix() %>%
            get_user_product_recommendations(
                .sales_data  = pr_recommender_filtered_tbl(),
                .customer_id = input$customer_id,
                .n_closest   = 3
            ) %>% 
            pull(Description)
        
       paste(toString(l))
    })
    
    
    # ** Opportunity DT Table ----
    output$opportunity <- renderDataTable({
        
        pr_recommender_filtered_tbl() %>% 
            get_user_item_matrix() %>% 
            get_user_to_user_cosine_matrix() %>%
            get_user_product_recommendations(
                .sales_data  = pr_recommender_filtered_tbl(),
                .customer_id = input$customer_id,
                .n_closest   = 3
            )
    })
    
    
    

}

shinyApp(ui, server)