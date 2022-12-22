# FUNCTIONS FOR APP UI ----
# **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# Libraries ----
library(shiny)
library(plotly)
library(DT)


# ******************************************************************************
# HOME TAB UI FUNCTIONS ----
# ******************************************************************************

# * Boxes With Hyperlinks ----
get_box_hyperlink <- function(.input_id,
                              .label,
                              .footer,
                              .img_id,
                              .src_name,
                              .height,
                              .width){
    
    box(
        width = 4,
        actionLink(inputId = .input_id, label = h3(.label, align = "center")),
        actionLink(
            inputId = .input_id,
            label =   div(
                tags$img(id     = .img_id,
                         src    = .src_name,
                         height = .height,
                         width  = .width),
                style = "text-align: center;"
            )
        ),
        actionLink(inputId = .input_id, label = h5(.footer, align = "center"))
    )
    
    
}

# * Information Box ----
get_home_tab_info_text <- function(){
    
    HTML(
        "
        <p> App Creator: Lucas Okwudishu. Get in touch on <a href = 'https://www.linkedin.com/in/lucasokwudishu/'>Linkedin,</a>
        or view sourcode on <a href = 'https://github.com/LucasO21/retail_analytics'>Github.</a>
        </p>
        
        <p> Inspiration: <a href = 'https://www.business-science.io/'>Business Science University</a> Learning Lab 58.</p>
        
        <p> Dataset: <a href = 'https://www.kaggle.com/datasets/mashlyn/online-retail-ii-uci'>Online Retaill Dataset.</a></p>
        "
    )
    
}


# ******************************************************************************
# CLV TAB UI FUNCTIONS ----
# ******************************************************************************

# * CLV Tab Info Text ----
get_clv_tab_info_text <- function(){
    
    HTML(
        "
        <p>
       This tab show a scoring of customers future spending. With machine learning, 
       we can predict the probability and amount the customer will spend within the next 90 days.
        </p>
        "
    )
}


# ******************************************************************************
# PRODUCT RECOMMENDER TAB UI FUNCTIONS ----
# ******************************************************************************

# * Product Recommender Tab Info Text ----
get_product_recommender_tab_info_text <- function(){
    
    HTML(
        "
        <p>
        This tab contains info on personalized product recommendations for each customer. The product
        recommendations are based on similarities among customers, meaning product recommendations for a
        particular customer are based on what other similar customers have purchased in the past.
        To learn more on user-based collaborative filtering, visit
        <a href='https://en.wikipedia.org/wiki/Collaborative_filtering'>this link.</a>
        </p>
        "
        )
}

# ******************************************************************************
# FORECAST TAB UI FUNCTIONS ----
# ******************************************************************************

# * Forecast Tab Info Text ----
get_forecast_tab_info_text <- function(){
    
    HTML(
        "
        <p>This tab contains info 90 day forecast (quantity). Forecast
        is broken out by United Kingdom and all other countries. On
        average per day, United Kingdom accounts for 85% of total
        quantity sold. The top half of the tab helps the user visualize the trailing
        actual quantity sold (in blue), and the 90 day future forecast (in red).
        The bottom half of the tab contains the forecast data which can be downloaded
        to a csv file.
        </p>
        "
        )
    
    
}


