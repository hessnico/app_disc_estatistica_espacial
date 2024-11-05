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
      selectInput("ano", "Selecione o Ano:", choices = 2010:2021, selected = 2010),
      
      # Radio buttons para três opções
      radioButtons("opcoes", "Escolha uma Opção:",
                   choices = c("População", "PIB", "PIB per capita")),
      
      # Botão de ação para confirmar a seleção
      actionButton("Acao", "Confirmar Seleção")
      
    ),
    mainPanel(
      leafletOutput("leafletMap", width = "70%", height = "1000px"),
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$Acao, {
    output$leafletMap <- renderLeaflet({
      switch(input$opcoes,
             "População" = createPlotByStateShiny(df_populacao, num_quantis = 2, input$ano),
             "PIB" = createPlotByStateShiny(df_populacao, num_quantis = 7, input$ano),
             "PIB per capita" = createPlotByStateShiny(df_populacao, num_quantis = 10, input$ano)
      )
    })
  })
  
  output$yearText <- renderText({
    paste("Current Year:", input$yearSlider)
  })
}

shinyApp(ui = ui, server = server)
