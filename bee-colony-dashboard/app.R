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
library(scales)

setwd("C:/Users/allielalor/Documents/INFO526_lessons/bee-colony-dashboard")

colony <- read_csv("module_6_data/colony.csv")
stressor <- read_csv("module_6_data/stressor.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bee Colony Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Data on Bee Colonies"),
            p("The data displayed here was retrieved from:", HTML('<a href="https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-11/readme.md">Tidy Tuesday</a>'),
              ),
            p("Navigate through the differnt visualizations using the tabs.")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Histogram", 
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 50,
                                 value = 30),
                     plotOutput("distPlot")),
            tabPanel("Timeline", 
                     selectInput("state",
                                 "Select state:",
                                 choices = colony %>% distinct(state) %>% pull(state)
                                ),
                     sliderInput("year",
                                 "select year:",
                                 min = min(colony$year),
                                 max = max(colony$year),
                                 value = 2015,
                                 sep = ""),
                     plotOutput("timelinePlot"))
          )
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- colony %>% pull(colony_lost_pct)
        bins <- seq(min(x, na.rm = T), max(x, na.rm = T), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$timelinePlot <- renderPlot({
      colony %>%
        filter(state == input$state & year == input$year) %>% 
        mutate(months = factor(months,
                               levels = c("January-March","April-June","July-September","October-December"))) %>% 
        ggplot(aes(x = months,
                   y = colony_lost_pct,
                   group = 1,
                   label = percent(colony_lost_pct, scale=1))) +
        geom_point() +
        geom_line() +
        geom_label() +
        theme_minimal() +
        facet_wrap(~year) +
        labs(y = "colony lost percentage",
             title = paste("Percentage of Bee Colonies lost in",
                           input$state)) +
        scale_y_continuous(labels = percent_format(scale = 1),
                           limits = c(0,40))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
