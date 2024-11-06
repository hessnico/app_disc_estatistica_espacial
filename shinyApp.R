source('./utils_est_espacial.R')

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
      radioButtons("opcoes", "Escolha uma opção de gráfico:",
                   choices = c("População", "PIB", "PIB per capita")),
      
      
      sliderInput("num_quantis", 
                  "Escolha o número de quantis para os gráficos:", 
                  min = 1, 
                  max = 10, 
                  value = 6),
      
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
             "População" = createPlotByStateShiny(df_populacao, num_quantis = input$num_quantis, input$ano),
             "PIB" = leaf_pib(df_populacao, num_quantis = input$num_quantis, input$ano),
             "PIB per capita" = leaf_pib_capita(df_populacao, num_quantis = input$num_quantis, input$ano),
      )
    })
  })
  
  output$yearText <- renderText({
    paste("Ano selecionado:", input$yearSlider)
  })
}

shinyApp(ui = ui, server = server)
