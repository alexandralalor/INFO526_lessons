#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

tree_rgb_filter <- read.csv("data_raw/final_project/tree_rgb/tree_rgb_filter_all.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Colors", 
                     selectInput("PonderosaPine",
                                 "Select seedling:",
                                 choices = tree_rgb_filter %>% distinct(SpeciesID) %>% pull(SpeciesID)
                     ),
                     sliderInput("week",
                                 "select week:",
                                 choices = tree_rgb_filter %>% distinct(week) %>% pull(week),
                                 min = min(tree_rgb_filter$Week),
                                 max = max(tree_rgb_filter$Week),
                                 value = 13,
                                 sep = ""),
                     plotOutput("timelinePlot"))
        )
    )
))


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$timelinePlot <- renderPlot({
      tree_rgb_filter %>% 
        filter(Seedling == input$PonderosaPine & Week == input$week) %>% 
        plot_colors_3d(sample_size = 5000, marker_size = 2.5, color_space = "RGB")
    })
}


# Run the application 
shinyApp(ui = ui, server = server)
