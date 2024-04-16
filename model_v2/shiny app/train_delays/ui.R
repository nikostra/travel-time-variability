#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Train Delays"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          # selectInput(
          #   "model",
          #   "Choose a model",
          #   c("Mu+sigma","Mu","Mu+theta"),
          #   selected = NULL,
          #   multiple = FALSE,
          #   selectize = TRUE,
          #   width = NULL,
          #   size = NULL
          # ),
          # 
          # hr(),
          
          selectInput(
            "weekday",
            "Choose a weekday",
            c("weekday","weekend"),
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          selectInput(
            "time",
            "Time of the day",
            c("morning","mid_day","afternoon","evening","night"),
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            "arrOperator",
            "Operator of the first train",
            c("SJ","Snälltåget"),
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          selectInput(
            "arrProduct",
            "Product name of the first train",
            c("SJ Snabbtåg","Snälltåget", "SJ Euronight"),
            selected = NULL,
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          
          sliderInput("connections",
                        "Number of Connections:",
                        min = 1,
                        max = 4,
                        value = 2),
          
          uiOutput("input_groups")
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
          div(textOutput("warnings"), style="color:red"),
          plotOutput("delayPlot")
        )
    )
)
