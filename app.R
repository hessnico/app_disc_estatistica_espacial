# constroi banco de dados
#main <- function() {
#   source('./createDataFrame.R')
#   source('./shinyApp.R')
# }; main()

library(readxl)
library(httr)
library(sf)
library(dplyr)
library(viridis)
library(ggplot2)
library(geojsonio)
library(scales)
library(shiny)
library(leaflet)
library(glue)
library(shinydashboard)

initial_lat = -29.331089
initial_long = -53.08744052154482

######################################################################
# Hierarquia

createPlotByStateShiny_hierarquia <- function(df, year) {
  
  df = df %>% filter(Year == year)
  
  pal <- colorFactor(palette = "RdYlBu", domain = df$`Hierarquia Urbana`)
  
  nome_dos_municipios = df$nome_munic
  hierarquia = df$`Hierarquia Urbana`
  
  p <- leaflet(df) %>%
    addTiles() %>%
    setView(lat =  initial_lat, lng = initial_long, zoom = 7) %>% 
    addPolygons(
      fillColor = ~pal(hierarquia),
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
      label = ~paste(nome_dos_municipios, "Hierarquia:", hierarquia),
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
    ) %>%
    addLegend(pal = pal, 
              values = ~hierarquia, 
              opacity = 0.7, title = "Hierarquia", position = "bottomright")
  
  return(p)
}


######################################################################
# PIB

leaf_pib <- function(df, num_quantis = 7, year) {
  
  df = df %>% filter(Year == year)
  num_quantis = num_quantis / (num_quantis*num_quantis)
  df <- df %>%
    mutate(quantis = cut(PIB,
                         breaks = round(quantile(PIB, probs = seq(0, 1, by = num_quantis), na.rm = TRUE),2), 
                         include.lowest = TRUE,
                         dig.lab = 15))
  
  pal <- colorFactor(palette = "RdYlBu", domain = df$quantis)
  
  nome_dos_municipios = df$nome_munic
  PIB = df$PIB
  
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
      label = ~paste(nome_dos_municipios, "\nPIB:", PIB_notacao_real),
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
    ) %>%
    addLegend(pal = pal,
              values = ~quantis,
              opacity = 0.7,
              title = "PIB (R$) em Quantis",
              position = "bottomright",
    )
  
  return(p)
} 

leaf_pib_capita <- function(df, num_quantis = 7, year) {
  
  df = df %>% filter(Year == year)
  num_quantis = num_quantis / (num_quantis*num_quantis)
  df <- df %>%
    mutate(quantis = cut(PIB_per_capita,
                         breaks = round(quantile(PIB_per_capita, probs = seq(0, 1, by = num_quantis), na.rm = TRUE),2), 
                         include.lowest = TRUE,
                         dig.lab = 15))
  
  pal <- colorFactor(palette = "RdYlBu", domain = df$quantis)
  
  nome_dos_municipios = df$nome_munic
  PIB_per_capita = df$PIB_per_capita
  
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
      label = ~paste(nome_dos_municipios, "\nPIB per capita:", PIB_per_capita_notacao_real),
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
    ) %>%
    addLegend(pal = pal, values = ~quantis, opacity = 0.7, title = "PIB per capita (R$) em Quantis", position = "bottomright")
  
  return(p)
} 

leaf_meso <- function(df, year) {
  
  df <- df %>% filter(Year == year)
  pal <- colorFactor(palette = "RdYlBu", domain = df$`Nome da Mesorregião`)
  
  nome_dos_municipios <- df$nome_munic
  meso <- df$`Nome da Mesorregião`
  
  p <- leaflet(df) %>%
    addTiles() %>%
    setView(lat = initial_lat, lng = initial_long, zoom = 7) %>% 
    addPolygons(
      weight = 2,
      opacity = 1,
      color = 'white',
      dashArray = '3',
      fillOpacity = 0.7,
      fillColor = ~pal(meso),
      highlightOptions = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = ~paste(nome_dos_municipios, "\nMesorregiao", meso),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(pal = pal, values = ~meso, opacity = 0.7, title = "Mesorregião", position = "bottomright")
  
  return(p)
} 

#### principal atividade economica

leaf_ativ <- function(df, year) {
  
  df = df %>% filter(Year == year)
  
  pal <- colorFactor(palette = "RdYlBu", domain = df$`Atividade com maior valor adicionado bruto`)
  
  nome_dos_municipios = df$nome_munic
  PIB_per_capita = df$PIB_per_capita
  
  p <- leaflet(data = df) %>%
    addTiles() %>%
    setView(lat = initial_lat, lng = initial_long, zoom = 7) %>%
    addPolygons(
      fillColor = ~pal(`Atividade com maior valor adicionado bruto`),
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
      # Usar 'label' ao invés de 'popup'
      label = ~paste0(nome_dos_municipios, ": ", `Atividade com maior valor adicionado bruto`),
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    ) %>%
    addLegend(
      pal = pal,
      values = ~`Atividade com maior valor adicionado bruto`,
      opacity = 0.7,
      title = "Atividade Econômica",
      position = "bottomright"
    )
  return(p)
}


createPlotByStateShiny <- function(df, num_quantis = 7, year) {
  
  df = df %>% filter(Year == year)
  num_quantis = num_quantis / (num_quantis*num_quantis)
  df <- df %>%
    mutate(quantis = cut(populacao,
                         breaks = round(quantile(populacao, probs = seq(0, 1, by = num_quantis), na.rm = TRUE),2), 
                         include.lowest = TRUE,
                         dig.lab = 15))
  
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
      label = ~paste(nome_dos_municipios, "\nPopulação:", populacao_formatada),
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
    ) %>%
    addLegend(pal = pal, values = ~quantis, opacity = 0.7, title = "População em Quantis", position = "bottomright")
  
  return(p)
}


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
                               choices = c("População",
                                           "PIB",
                                           "PIB per capita",
                                           "Mesorregião",
                                           "Hierarquia Urbana",
                                           "Principal atividade econômica")),
                  
                  sliderInput("num_quantis", 
                              "Escolha o número de quantis para os gráficos:", 
                              min = 1, 
                              max = 10, 
                              value = 6),
                  actionButton("Acao", "Confirmar Seleção", class = "btn btn-primary")
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
  
  df_app <- readRDS(file = "bancodedados.rds")
  observeEvent(input$Acao, {
    output$leafletMap <- renderLeaflet({
      switch(input$opcoes,
             "População" = createPlotByStateShiny(df_app, num_quantis = input$num_quantis, input$ano),
             "PIB" = leaf_pib(df_app, num_quantis = input$num_quantis, input$ano),
             "PIB per capita" = leaf_pib_capita(df_app, num_quantis = input$num_quantis, input$ano),
             "Mesorregião" = leaf_meso(df_app, input$ano),
             "Hierarquia Urbana" = createPlotByStateShiny_hierarquia(df = df_app, year = input$ano),
             "Principal atividade econômica" = leaf_ativ(df = df_app, year = input$ano)
      )
    })
  })
}

shinyApp(ui = ui, server = server)