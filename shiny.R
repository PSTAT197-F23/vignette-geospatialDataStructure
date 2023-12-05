# This is a Shiny web application for Geospatial Data Structures
# Author: 'Yibo Liang, Pippa Lin, Chris Zhao'

library(shiny)

us_species <- read_csv("data/dat_species.csv")
us_species <- us_species %>% filter(sciname == "Myocastor coypus" | sciname == "Hydrochoerus hydrochaeris")
us_geo <- read_csv("data/dat_spatial.csv")
us_geo <- us_geo %>% 
  filter(name != "Hawaii" & name != "Alaska")

colnames(us_geo)[1] <- "state"
us_geo_merged <- merge(us_geo,us_species,by="state", all=T)
names(us_geo_merged) 

us_geo_merged <- us_geo_merged[is.na(us_geo_merged$geometry) == FALSE, ]

# Create an sf object (SQ) from the WKT geometries
SQ <- st_as_sf(us_geo_merged, wkt = "geometry")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geospatial Data Structures"),

    # Sidebar with a slider input for 
    sidebarLayout(
        sidebarPanel(
          sliderInput("range", "Range:",
                      min = 1950, max = 2020,
                      value = c(1950,2020))
        ),
        
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
        filter(year >= input$range[1] & year <= input$range[2])
      
      # Create a ggplot object
      ggplot() +
  
        # Add the filtered sf object as a layer
        geom_sf(data = SQ["state"]) +
        geom_sf(data = SQ_f, aes(fill = occurrence)) +
        
        # Customize the plot as needed
        theme_minimal() +
        labs(title = "Occurrences Plot")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
