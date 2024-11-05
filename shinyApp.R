# Load necessary libraries
library(shiny)
library(leaflet)

initial_lat = -29.331089
initial_long = -53.08744052154482

# Function to create a leaflet map based on the year
createPlotByStateShiny <- function(df, num_quantis = 7, year) {
  
  df = df %>% filter(Year == year)
  num_quantis = num_quantis / (num_quantis*num_quantis)
  df <- df %>%
    mutate(quantis = cut(populacao, breaks = quantile(populacao, probs = seq(0, 1, by = num_quantis), na.rm = TRUE), include.lowest = TRUE))
  
  pal <- colorFactor(palette = "RdYlBu", domain = df$quantis)
  
  nome_dos_municipios = df$nome_munic
  populacao = df$populacao
  
  p <- leaflet(df) %>%
    addTiles() %>%
    setView(lat =  initial_lat, lng = initial_long, zoom = 7) %>% 
    addPolygons(
      fillColor = ~pal(quantis),
      weight = 2,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste(nome_dos_municipios, "População:", populacao),
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
    ) %>%
    addLegend(pal = pal, values = ~quantis, opacity = 0.7, title = "População em Quantis", position = "bottomright")
  
  return(p)
} 

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        margin: 0;
      }
      #map {
        width: 1000px;   /* Set the width of the map */
        height: 1000px;  /* Set the height of the map */
      }
    "))
  ),
  
  titlePanel("Shiny App with Year Slider and Leaflet Map"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearSlider", "Select Year:",
                  min = 2000, max = 2020, value = 2000, step = 1),
      textOutput("yearText")
    ),
    mainPanel(
      leafletOutput("leafletMap", width = "1000px", height = "1000px")  # Full width, fixed height
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Render the leaflet map
  output$leafletMap <- renderLeaflet({
    createPlotByStateShiny(df_mun_inteiro, num_quantis = 5, input$yearSlider)  # Use the selected year from the slider
  })
  
  # Display the current year
  output$yearText <- renderText({
    paste("Current Year:", input$yearSlider)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
