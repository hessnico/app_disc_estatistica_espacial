source('./utils_est_espacial.R')

ui <- dashboardPage(
  dashboardHeader(title = "Shiny Estatística Espacial"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos", tabName = "gráficos", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "gráficos",
              h2("Shiny App", style = "color: #000000"),
              fluidRow(
                box(
                  title = "Configurações", status = "primary", solidHeader = TRUE, width = 3,
                  selectInput("ano", "Selecione o Ano:", choices = 2010:2021, selected = 2010),
                  radioButtons("opcoes", "Escolha uma opção de gráfico:",
                               choices = c("População", "PIB", "PIB per capita", "Mesorregião", "Hierarquia Urbana")),
                  sliderInput("num_quantis", 
                              "Escolha o número de quantis para os gráficos:", 
                              min = 1, 
                              max = 10, 
                              value = 6),
                  actionButton("Acao", "Confirmar Seleção", class = "btn btn-danger")
                ),
                box(
                  title = "Visualização", status = "info", solidHeader = TRUE, width = 9,
                  leafletOutput("leafletMap", width = "100%", height = 700)
                )
              )
      )
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
             "Mesorregião" = leaf_meso(df_populacao, input$ano),
             "Hierarquia Urbana" = createPlotByStateShiny_hierarquia(df = df_populacao, year = input$ano)
      )
    })
  })
  
}

shinyApp(ui = ui, server = server)