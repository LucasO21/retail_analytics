
library(shiny)
library(plotly)
library(DT)

get_plot_box <- function(.col_width = 6, .title, .id, .output = "plot", 
                         .output_id, .height){
    
    # Output Setup
    if(.output == "plot"){
        .plot = plotlyOutput(outputId = "spend_prob_plot", height = "400px")
    } else if (.output == "data_table"){
        .plot = dataTableOutput(outputId = "spend_prob_plot", height = "400px")
    }
    
    # Column Setup
    column(
        width = .col_width,
        tags$fieldset(
            tags$legend(.title, tags$span(id = .id, icon("info-circle"))),
            .plot
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