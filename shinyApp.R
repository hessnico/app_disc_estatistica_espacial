

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100vh;
        margin: 0;
        margin-top: 5px;
        margin-bottom: 5px;
      }
      #map {
        width: 1000px;   /* Set the width of the map */
        height: 1000px;  /* Set the height of the map */
      }
    "))
  ),
  
  titlePanel("App shiny"),
  sidebarLayout(
    sidebarPanel(
      selectInput("ano", "Selecione o Ano:", choices = 2010:2021, selected = 2010),
      
      # Radio buttons para três opções
      radioButtons("opcoes", "Escolha uma Opção de gráficos:",
                   choices = c("População", "PIB", "PIB per capita")),
      
      # Botão de ação para confirmar a seleção
      actionButton("Acao", "Confirmar Seleção")
      
    ),
    mainPanel(
      leafletOutput("leafletMap", width = "900px", height = "900px"),
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
    paste("Ano selecionado:", input$yearSlider)
  })
}

shinyApp(ui = ui, server = server)
