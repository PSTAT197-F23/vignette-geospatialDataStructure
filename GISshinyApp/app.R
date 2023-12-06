# This is a Shiny web application for Geospatial Data Structures
# Author: 'Yibo Liang, Pippa Lin, Chris Zhao'

library(shiny)
library(sf)
library(tidyverse)
library(tidymodels)
library(ggplot2)
library(dplyr)

us_species <- read_csv('./dat_species.csv')
us_species <- us_species %>% filter(group == "Mammals"| group == "Reptiles" | group == "Amphibians")
us_geo <- read_csv("./dat_spatial.csv")
us_geo <- us_geo %>% 
  filter(name != "Hawaii" & name != "Alaska")

names <- c(unique(us_species$sciname))

colnames(us_geo)[1] <- "state"
us_geo_merged <- merge(us_geo,us_species,by="state", all=T)

us_geo_merged <- us_geo_merged[is.na(us_geo_merged$geometry) == FALSE, ]

# Create an sf object (SQ) from the WKT geometries
SQ <- st_as_sf(us_geo_merged, wkt = "geometry")
SQ1 <- st_as_sf(us_geo, wkt = "geometry")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Occurances of Myocastor coypus and Hydrochoerus hydrochaeris"),

    # Sidebar with a slider input for 
    sidebarLayout(
        sidebarPanel(
          sliderInput("year", "Year:",
                      min = 1950, max = 2015,
                      value = 2000),
          
          radioButtons("name", "Species Name:",
                       names)),
        
    # Show a plot of the generated distribution
          mainPanel(
            plotOutput("distPlot")
          )
    )
)

# Define server logic required to draw a plot
server <- function(input, output) { 

    output$distPlot <- renderPlot({
      
      SQ_f <- SQ %>%
        filter(sciname == input$name) %>% 
        filter(year <= input$year) %>% 
        group_by(state) %>% 
        summarise(occurrence = sum(occurrence))
      
      # Create a ggplot object
      ggplot() +
        # Add the filtered sf object as a layer
        geom_sf(data = SQ1["state"]) +
        geom_sf(data = SQ_f, aes(fill = occurrence)) +
        
        # Customize the plot as needed
        theme_minimal() +
        labs(title = "Occurrences Plot")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
