
library(shiny)
library(plotly)
library(DT)

get_plot_box <- function(.col_width = 6, .col_title, .col_id, .output = "plot", 
                         .output_id, .output_height = "400px"){
    
    # Output Setup
    # if(.output == "plot"){
    #     .plot = plotlyOutput(outputId = "spend_prob_p", height = .output_height)
    # } else if (.output == "data_table"){
    #     .plot = dataTableOutput(outputId = "spend_prob_p", height = .output_height)
    # }
    
    # Column Setup
    column(
        width = .col_width,
        tags$fieldset(
            tags$legend(.col_title, tags$span(id = .col_id, icon("info-circle"))),
            plotlyOutput(outputId = "spend_prob_p", height = .output_height)
        )
    )
    
}


get_info_circle <- function(.id, .title, .content, .placement = "left"){
    
    bsPopover(
        id        = .id,
        title     = .title,
        content   = .content,
        placement = .placement
    )
    
    
    
}

