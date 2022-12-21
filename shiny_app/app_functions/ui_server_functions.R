# FUNCTIONS FOR APP UI ----
# **** ----

# Libraries ----
library(shiny)
library(plotly)
library(DT)

# CLV Tab Info Text ----
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


# Product Recommender Tab Info Text ----
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

# Forecast Tab Info Text ----
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


