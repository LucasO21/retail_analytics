# SHINY APP UI & SERVER SCRIPT ----
# **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# *Set Working Dir ----
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
source("app_functions/ui_server_functions.R")
source("app_functions/clv_functions.R")
source("app_functions/pr_functions.R")
source("app_functions/forecast_functions.R")


# ******************************************************************************
# DATA IMPORT ----
# ******************************************************************************

# * First Purchase Data ----
first_purchase_data <- read_rds("app_data/first_purchase_data.rds")

# * Load Sales Data ----
retail_data <- read_rds("app_data/retail_clean_data.rds")

# * CLV Data ----
clv_pred_data <- read_rds("app_artifacts/clv_artifacts_list.rds")[[2]][[3]] %>% 
    left_join(first_purchase_data %>% select(customer_id, country))

# * Country Data ----
customer_country_data <- clv_pred_data %>%
    select(customer_id, country) %>%
    distinct() %>%
    arrange(country) %>%
    mutate(customer_id = as.character(customer_id))

ids_for_pr_recommender <- clv_pred_data %>% 
  filter(spend_90_flag == 1) %>% 
  pull(customer_id)

# * Forecast Data ----
future_forecast_data <- read_rds("app_artifacts/forecast_artifacts_list.rds")$data$future_forecast_tbl


# ******************************************************************************
# UI ----
# ******************************************************************************
ui <- tagList(
    useShinydashboard(),
    
    navbarPage(
        title = "Retail Analytics App",
        
        # * CLV Analysis Tab ----
        tabPanel(
            title = "CLV Analysis",
            fluidPage(
                sidebarLayout(
                    
                    # ** Sidebar Panel ----
                    sidebarPanel(
                        width = 2,
                        
                        # *** Country Picker Input ----
                        pickerInput(
                            inputId  = "country_picker",
                            label    = h4("Select Country"),
                            choices  = sort(unique(customer_country_data$country)),
                            selected = sort(unique(customer_country_data$country)),
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
                        
                        br(),
                        hr(),
                        br(),
                        
                        # *** Download Button Input ----
                        downloadButton(
                            outputId = "download_clv_data",
                            label    = "Download Data"
                        )
                        
                        
                        
                    ),
                    
                    # ** Main Panel ----
                    mainPanel(
                        width = 10,
                        
                        # *** Fluid Row 1 ----
                        fluidRow(
                          
                          # **** Column 1 ----
                          column(
                            width = 12,
                            box(
                              width = 24,
                              tags$h3("Product Recommender"),
                              get_clv_tab_info_text()
                            )               
                          )
                        ),
                        
                        # *** Fluid Row 2 ----
                        fluidRow(
                            
                            # **** Column 1 ----
                            column(
                                width = 6,
                                box(
                                    width = 12,
                                    tags$fieldset(
                                        tags$legend(
                                            "CLV: Probability of Future Spend", 
                                            tags$span(id = "info1", icon("info-circle"))
                                        ),
                                        plotlyOutput("spend_prob_p", height = "400px")
                                    )
                                )
                            ),
                            
                            # **** Column 2 ----
                            column(
                                width = 6,
                                box(
                                    width = 12,
                                    tags$fieldset(
                                        tags$legend(
                                            "CLV: Key Features For Future Spend", 
                                            tags$span(id = "info2", icon("info-circle"))
                                        ),
                                        plotlyOutput("features_p", height = "400px")
                                    )
                                )
                            )
                            
                        ),
                        
                        # *** Fluid Row 3 ----
                        fluidRow(
                            
                            # **** Column 1 ----
                            column(
                                width = 12,
                                box(
                                    width = 24,
                                    tags$fieldset(
                                        tags$legend(
                                            "CLV: Features Data", 
                                            tags$span(id = "info3", icon("info-circle"))
                                        ),
                                        dataTableOutput("clv_data", height = "400px")
                                    )
                                )
                            ) 
                        ),
                        
                        # *** Info Circles ----
                        bsPopover(
                          id        = "info1", 
                          title     = "Probability of Future Spend",
                          content   = "Use this plot to analyze trend.",
                          placement = "left"
                        ),
                        
                        bsPopover(
                          id        = "info2", 
                          title     = "Key Features",
                          content   = "Use this plot to analyze how key features affect future spend probability",
                          placement = "left"
                        ),
                        
                        bsPopover(
                          id        = "info3", 
                          title     = "Data",
                          content   = "Datatable with clv spend probabilty and key features data.",
                          placement = "left"
                        )
                        
                        
                    ) # end clv analysis mainPanel
                )
            )
        ), # end clv analysis tabPanel
        
        # * Product Recommender ----
         tabPanel(
             title = "Product Recommender",
             fluidPage(
                sidebarLayout(

                    # ** Sidebar Panel ----
                    sidebarPanel(
                        width = 2,

                        # *** Country Picker Input ----
                        pickerInput(
                            inputId  = "country_picker_2",
                            label    = h4("Select Country"),
                            choices  = sort(unique(customer_country_data$country)),
                            selected = sort(unique(customer_country_data$country)),
                            multiple = TRUE,
                            options  = list(
                                `actions-box`          = TRUE,
                                size                   = 10,
                                `selected-text-format` = "count > 3"
                            )
                        ),

                        br(),

                        # *** Customer ID Input ----
                        selectizeInput(
                            inputId  = "customer_id",
                            label    = h4("Select Customer ID"),
                            choices  = NULL,
                            selected = NULL,
                            multiple = FALSE
                        ),

                        br(),
                        hr(),
                        br(),

                        # *** Download Product Recommender Data ----
                        downloadButton(
                            outputId = "download_pr_data",
                            label    = "Download Data"
                        )
                    ),

                    # ** Main Panel ----
                    mainPanel(
                      width = 10,
                      
                      # *** Fluid Row 1 ----
                      fluidRow(
                        
                        # **** Column 1 ----
                        column(
                          width = 12,
                          box(
                            width = 24,
                            tags$h3("Product Recommender"),
                            get_product_recommender_tab_info_text()
                          )               
                        )
                      ),
                      
                      # *** Fluid Row 2 ----
                      fluidRow(
                        
                        # **** Column 1 ----
                        column(
                          width = 12,
                          box(
                            width = 24,
                            tags$h3("Product Recommendations"),
                            textOutput("product_recommendations")
                          )
                        )
                      ),
                      
                      # *** Fluid Row 3 ----
                      fluidRow(
                        
                        # **** Column 1 ----
                        column(
                          width = 12,
                          box(
                            width = 24,
                            tags$h3("Average Amount Spent by Customer on Recommended Products"),
                            dataTableOutput("opportunity")
                          )
                        )
                      )
                    ) # end product recommender mainPanel
                )
            )
        ), # end product recommender tabPanel
        
        # * Forecast Tab ----
        tabPanel(
          title = "Forecast",
          
          fluidPage(
            
            # ** Fluid Row 1 ----
            fluidRow(
              
              # *** Column 1 ----
              column(
                width = 12,
                box(
                  width = 24,
                  tags$h3("Forecast Information"),
                  get_forecast_tab_info_text()
                )               
              )
            ),
            
            # ** Fluid Row 2 ----
            fluidRow(
              
              # *** Column 1 ----
              column(
                width = 4,
                box(
                  width = 12,
                  tags$h3("Forecast (Total)"),
                  plotlyOutput("forecast_plot_all", height = "300px")
                )
              ),
              
              # *** Column 2 ----
              column(
                width = 4,
                box(
                  width = 12,
                  tags$h3("Forecast (United Kingdom)"),
                  plotlyOutput("forecast_plot_uk", height = "300px")
                )
              ),
              
              # *** Column 3 ----
              column(
                width = 4,
                box(
                  width = 12,
                  tags$h3("Forecast (All Other Countries)"),
                  plotlyOutput("forecast_plot_others", height = "300px")
            
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
                  tags$h3("Forecast Data"),
                  downloadButton(
                    outputId = "download_forecast_data",
                    label    = "Download Data"
                  ),
                  
                  br(),
                  
                  dataTableOutput("forecast_data_dt")
                  
                )
              )
              
            )
           
          )
        ) # end Forecast tabPanel
    )

  
)



# ******************************************************************************
# SERVIER ----
# ******************************************************************************
server <- function(input, output) {
    
    # * CLV Tab Server Functions ----
    
    # ** CLV Data Filtered ----
    clv_filtered_tbl <- reactive({
        
        clv_pred_data %>%
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
                get_clv_dt() %>% 
                mutate_if(is.numeric, as.character) %>% 
                mutate_if(is.factor, as.character),
              file
            )
        }
    )
    
    # **************************************************************************
    
    # * Product Recommender Server Functions ----
    
    # ** Update Filtering Logic ----
    observe({
      
      x <- customer_country_data %>% 
        filter(country %in% input$country_picker_2) %>% 
        select(customer_id)
      
      x <- x %>% filter(customer_id %in% ids_for_pr_recommender)
      
      updateSelectInput(
        session  = getDefaultReactiveDomain(),
        inputId  = "customer_id",
        choices  = unique(x$customer_id),
        selected = unique(x$customer_id)[1]
      )
      
    })
    
    # ** Product Recommender Data Filtered ----
    start_date              <- max(retail_data$invoice_date) - 90
    end_date                <- max(retail_data$invoice_date)
    
    pr_recommender_filtered_tbl <- reactive({
      
      retail_data %>% 
        filter(customer_id %in% clv_pred_data$customer_id) %>% 
        filter(between(invoice_date, start_date, end_date))
      
    })
    
    # ** Recommended Products List ----
    output$product_recommendations <- renderText({
      
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
    
    # ** Product Recommender Data Download ----
    output$download_pr_data <- downloadHandler(
      
      filename = function(){
        paste("product_recommendation", "csv", sep = ".")
      },
      
      content = function(file){
        write.csv(
          pr_recommender_filtered_tbl() %>% 
            get_user_item_matrix() %>% 
            get_user_to_user_cosine_matrix() %>%
            get_user_product_recommendations(
              .sales_data  = pr_recommender_filtered_tbl(),
              .customer_id = input$customer_id,
              .n_closest   = 3
            ),
          file
        )
      }
    )
    
    
    
    # **************************************************************************
    
    # * Forecast Tab Server Functions ----
    
    # ** Forecast Data Filtered ----
    future_forecast_data_filtered <- reactive({future_forecast_data})
    
    # ** Forecast Plot (All) ----
    output$forecast_plot_all <- renderPlotly({
      
      future_forecast_data_filtered() %>% 
        get_forecast_data() %>%
        plot_modeltime_forecast(
          .title       = "",
          .legend_show = FALSE
        )
      
    })
    
    # ** Forecast Plot (UK) ----
    output$forecast_plot_uk <- renderPlotly({
      
      future_forecast_data_filtered() %>% 
        get_forecast_data(.country = "United Kingdom") %>%
        plot_modeltime_forecast(
          .title       = "",
          .legend_show = FALSE
        )
      
    })
    
    # ** Forecast Plot (All Others) ----
    output$forecast_plot_others <- renderPlotly({
      
      future_forecast_data_filtered() %>% 
        get_forecast_data(.country = "All Others") %>%
        plot_modeltime_forecast(
          .title       = "",
          .legend_show = FALSE
        )
      
    })
    
    
    # ** Forecast Datatable ----
    output$forecast_data_dt <- renderDataTable({
      
      future_forecast_data_filtered() %>% 
        get_forecast_data_dt()
    })
    
    # ** Forecast Data Download ----
    output$download_forecast_data <- downloadHandler(
      
      filename = function(){
        paste("future_forecast", "csv", sep = ".")
      },
      
      content = function(file){
        write.csv(
          future_forecast_data_filtered() %>% 
            get_forecast_data_dt(),
          file
        )
      }
    )
   
      
    
    
   
    
    

}

shinyApp(ui, server)