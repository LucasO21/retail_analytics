# SHINY APP UI & SERVER SCRIPT ----
# **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Set Working Dir ----
# setwd(here::here("shiny_app"))

# * Package Requirements ----
# remotes::install_github("mdancho84/bslib")

# Libraries ----

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
library(coop)
library(modeltime)

# * Source ----
source("app_functions/ui_server_functions.R")
source("app_functions/clv_functions.R")
source("app_functions/pr_functions.R")
source("app_functions/forecast_functions.R")

# * App Attributes ----
# TITLE      <- "Retail Analytics App"
# FIRST_TAB  <- "Home"
# SECOND_TAB <- "CLV Analysis"
# THIRD_TAB  <- "Product Recommender"
# FOURTH_TAB <- "Forecast"
# 
# FONT_HEADING <- "Montserrat"
# FONT_BASE    <- "Open Sans"
# PRIMARY      <- "#2C3E50"
# SUCCESS      <- "#18BC9C"
# INFO         <- "#A6CEE3"
# WARNING      <- "#FDBF6F"
# DANGER       <- "#E31A1C"
# FG           <- PRIMARY
# BG           <- "#FFFFFF"
# 
# app_theme_base <- bs_theme(
#   font_scale   = 1.0,
#   heading_font = font_google(FONT_HEADING, wght = c(300, 400, 500, 600, 700, 800), ital = c(0, 1)),
#   base_font    = font_google(FONT_BASE, wght = c(300, 400, 500, 600, 700, 800), ital = c(0, 1)),
#   primary      = PRIMARY,
#   success      = SUCCESS,
#   info         = INFO,
#   warning      = WARNING,
#   danger       = DANGER,
#   fg           = FG,
#   bg           = BG,
#   "navbar-bg"  = PRIMARY,
#   "body-color" = PRIMARY,
#   "accordion-button-active-bg"    = "white",
#   "accordion-button-active-color" = PRIMARY,
#   "bs-accordion-color"            = PRIMARY,
#   "light"                         = BG
# )


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
future_forecast_data <- read_rds("app_artifacts/forecast_artifacts_list.rds")$data$future_forecast

# * Theme ----
# bs_theme_new()
# 
# bs_theme_add_variables(
#   
#   "body-pg" = "pink"
    
# )


# ******************************************************************************
# UI ----
# ******************************************************************************
ui <- tagList(
    useShinydashboard(),
    shinyjs::useShinyjs(),
    introjsUI(),
    # shiny::bootstrapLib(),
    tags$script(src="https://kit.fontawesome.com/77fcf700e6.js"),
    
    navbarPage(
      # theme = app_theme_base,
      theme = shinytheme("sandstone"),
      id = "tabset",
      title = "Retail Analytics App",
        
        
        # * Home Tab ----
        tabPanel(
          title = "Home", icon = icon("home"),
          # tags$img(
          #   src = "background_home.jpg",
          #   style = 'position: absolute'
          # ),
          mainPanel(
            width = 12,
          
          
          
          # Intro Box Info ----
          fluidRow(
            column(
              width = 12,
                box(
                width = 24,
                title = h1("Welcome to the Retail Analytics Application"),
                h3("A project to deliver analytics solutions for a hypothetical online retailer."),
                h4("Click on any box below to explore solution.")
              )
            )
          ),
        
          
          fluidRow(
            
            # * CLV Tab Box ----
            get_box_hyperlink(
              .input_id = "home_clv_link",
              .label    = "Customer Lifetime Value Tab",
              .footer   = "View 90 Day Spend Probability For Customers",
              .img_id    = "customer", .src_name = "hand-holding-dollar-solid.svg",
              .height = "30%", .width = "30%"
            ),
            
            # ** Product Recommender Tab Box ----
            get_box_hyperlink(
              .input_id = "home_pr_link",
              .label    = "Product Recommender Tab",
              .footer   = "View Product Recommendations For Customers",
              .img_id   = "shopping_cart", .src_name = "cart-shopping-solid.svg",
              .height = "30%", .width = "30%"
            ),

            # ** Forecast Tab Box ----
            get_box_hyperlink(
              .input_id = "home_forecast_link",
              .label    = "Forecast Tab",
              .footer   = "View 90 Day Product Forecast",
              .img_id   = "business", .src_name = "arrow-trend-up-solid.svg",
              .height = "30%", .width = "30%"
            )
      
          ),
          
          # ** Inspiration Info ----
          fluidRow(
            column(
              width = 12,
              box(
                title = h3("Information", align = "center"),
                width = 24,
                get_home_tab_info_text()
              )
            )
          )
          
        )
      ),
        
        # * CLV ANALYSIS TAB ----
        tabPanel(
            title = "CLV Analysis", icon = icon("hand-holding-dollar"),
            value = "tab1",
            
            fluidPage(
                sidebarLayout(
                    
                    # ** Sidebar Panel ----
                    sidebarPanel(
                        width = 2,
                        
                        br(),
                        
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
                        
                        br(), hr(), br(), br(), br(), br(), br(), br(), br(),
                        
                        # *** Download Button Input ----
                        downloadButton(
                            outputId = "download_clv_data",
                            label    = "Download Data"
                        )
                        
                        # *** Help Button Input ----
                        
                        
                        
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
        
        # * PRODUCT RECOMMENDER ----
         tabPanel(
             title = "Product Recommender", icon = icon("cart-shopping"),
             value = "tab2",
             
             fluidPage(
                sidebarLayout(

                    # ** Sidebar Panel ----
                    sidebarPanel(
                        width = 2,
                        
                        br(),

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
                        
                        br(), hr(), br(), br(), br(), br(), br(), br(), br(),
                        
                        # *** Download Data Input ----
                        downloadButton(
                          outputId = "download_pr_data",
                          label    = "Download Data"
                        ),
                        
                        br(), br(),
                        
                        # *** Help Button Input ----
                        actionButton(
                          inputId = "help_bttn_pr", 
                          label   = "Help", 
                          icon    = icon("info"),
                          width   = "133px" 
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
                            tags$h4("Product Recommender"),
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
                            tags$h4("Product Recommendations"),
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
                            tags$h4("Average Amount Spent by Customer on Recommended Products"),
                            dataTableOutput("opportunity")
                          )
                        )
                      )
                    ) # end product recommender mainPanel
                )
            )
        ), # end product recommender tabPanel
        
        # * FORECAST TAB ----
        tabPanel(
          title = "Forecast", icon = icon("arrow-trend-up"),
          value = "tab3",
          
          fluidPage(
            sidebarLayout(
              
              # ** Side Bar Panel ----
              sidebarPanel(
                width = 2,
                
                br(),
                
                # *** Forecast Horizon Input ----
                numericInput(
                  inputId = "forecast_days", 
                  label   = h4("Forecast Period (Days)"),
                  min     = 30,
                  max     = 90,
                  value   = 90 
                  ),
                
                br(),
                
                # *** Look Back Months Input ----
                numericInput(
                  inputId = "lookback_months",
                  label   = h4("Look Back (Months)"),
                  min     = 3,
                  max     = 11,
                  value   = 11
                ),
                
                br(), hr(), br(), br(), br(), br(), br(), br(), br(),
                
                # *** Download Data Input ----
                downloadButton(
                  outputId = "download_forecast_data",
                  label    = "Download Data"
                ),
                
                br(), br(),
                
                # *** Help Button Input ----
                actionButton(
                  inputId = "help_bttn_forecast", 
                  label   = "Help", 
                  icon    = icon("info"),
                  width   = "133px" 
                )
                
                
              ),
              mainPanel(
                width = 10,
              
            
            # ** Fluid Row 1 ----
            fluidRow(
              
              # *** Column 1 ----
              # column(
              #   width = 4,
              #   box(
              #     width = 12,
              #     tags$h3("Forecast (Total)"),
              #     plotlyOutput("forecast_plot_all", height = "300px")
              #   )
              # ),
              
              # *** Column 2 ----
              column(
                width = 6,
                box(
                  width = 12,
                  tags$h4("Forecast (United Kingdom | 85% of Quantity Sold)"),
                  plotlyOutput("forecast_plot_uk", height = "300px")
                )
              ),
              
              # *** Column 3 ----
              column(
                width = 6,
                box(
                  width = 12,
                  tags$h4("Forecast (All Other Countries | 15% of Quantity Sold)"),
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
                  tags$h4("Forecast Data"),
                  dataTableOutput("forecast_data_dt")
                  
                )
              )
              
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
server <- function(input, output, session) {
  
  # * Theme ----
  # observe({session$setCurrentTheme(app_theme_base)})
  
  # observe({bs_theme(version = 5)})
  
  # * Home Tab Server Functions ----
  observeEvent(input$home_clv_link, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab1")
    
  })
  
  observeEvent(input$home_pr_link, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab2")
    
  })
  
  observeEvent(input$home_forecast_link, { 
    updateTabsetPanel(inputId = "tabset", selected = "tab3")
    
  })
    
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
    shiny::observe({
      
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
    future_forecast_filtered_data <- reactive({future_forecast_data})
    
    # ** Forecast Plot (UK) ----
    uk_forecast_reactive_data <- reactive({
      
      future_forecast_filtered_data() %>% 
        get_forecast_data(
          .country         = "United Kingdom",
          .forecast_days   = input$forecast_days,
          .lookback_months = input$lookback_months
        )
      
    })
    
    output$forecast_plot_uk <- renderPlotly({
      
      uk_forecast_reactive_data() %>% 
        get_time_series_plot()
      
    })
    
    # ** Forecast Plot (All Others) ----
    others_forecast_reactive_data <- reactive({
      
      future_forecast_filtered_data() %>% 
        get_forecast_data(
          .country         = "All Others",
          .forecast_days   = input$forecast_days,
          .lookback_months = input$lookback_months
        )
      
    })
    
    output$forecast_plot_others <- renderPlotly({
      
      others_forecast_reactive_data() %>% 
        get_time_series_plot()
      
    })
    
    
    # ** Forecast Datatable ----
    output$forecast_data_dt <- renderDataTable({
      
      get_forecast_data_dt(
        uk_data     = uk_forecast_reactive_data(),
        others_data = others_forecast_reactive_data()
      )
      
    }) 

    
    # ** Forecast Data Download ----
    output$download_forecast_data <- downloadHandler(
      
      filename = function(){
        paste("future_forecast", "csv", sep = ".")
      },
      
      content = function(file){
        write.csv(
          get_forecast_data_dt(
            uk_data     = uk_forecast_reactive_data(),
            others_data = others_forecast_reactive_data()
          ),
          file
        )
      }
    )
    
    # ** Help Button Reactive ----

    observeEvent(input$help_bttn_forecast, {showModal(forecast_help_main_modal)})
    forecast_help_main_modal <- get_forecast_main_help()
    
    observeEvent(input$help_bttn_pr, {showModal(pr_help_main_modal)})
    pr_help_main_modal <- get_pr_main_help()
    
    

}

shinyApp(ui, server)