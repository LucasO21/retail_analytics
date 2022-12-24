# FUNCTIONS FOR APP UI ----
# **** ----

# ******************************************************************************
# SETUP ----
# ******************************************************************************

# * Libraries ----
library(shiny)

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





     

    



