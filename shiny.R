# This is a Shiny web application for Geospatial Data Structures
# Author: 'Yibo Liang, Pippa Lin, Chris Zhao'

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geospatial Data Structures"),

    # Sidebar with a slider input for 
    sidebarLayout(
        sidebarPanel(
            sliderInput("years",
                        "Select the years:",
                        min = 1,
                        max = 50,
                        value = 1),
            sliderInput("scale",
                        "Select the years:",
                        min = 1,
                        max = 20,
                        value = 10)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x <- seq(1, 500, 1)
        y <- dgamma(x, input$years, scale=input$scale)

        # draw the histogram with the specified number of bins
        plot(x, y, type="l", col = 'red', lwd=3,
             xlab = 'x values',
             main = 'plot of Gamma distrubution')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
