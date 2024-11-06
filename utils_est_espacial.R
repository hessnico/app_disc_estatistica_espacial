# https://github.com/tbrugz/ribge
  # install.packages("devtools")
  # devtools::install_github("tbrugz/ribge")

library(readxl)
library(httr)
library(sf)
library(ribge)
library(dplyr)
library(viridis)
library(ggplot2)
library(geojsonio)
library(scales)
library(shiny)
library(leaflet)
library(glue)
library(shinydashboard)
options(scipen = 999)

df_app <- load('./appDataRS.RData')

initial_lat = -29.331089
initial_long = -53.08744052154482

printEstadosBrasileirosFull <- function() {
  
  # Criar um hashmap (lista nomeada) para os estados brasileiros
  estados_brasileiros <- list(
    "AC" = "Acre",
    "AL" = "Alagoas",
    "AP" = "Amapá",
    "AM" = "Amazonas",
    "BA" = "Bahia",
    "CE" = "Ceará",
    "DF" = "Distrito Federal",
    "ES" = "Espírito Santo",
    "GO" = "Goiás",
    "MA" = "Maranhão",
    "MT" = "Mato Grosso",
    "MS" = "Mato Grosso do Sul",
    "MG" = "Minas Gerais",
    "PA" = "Pará",
    "PB" = "Paraíba",
    "PR" = "Paraná",
    "PE" = "Pernambuco",
    "PI" = "Piauí",
    "RJ" = "Rio de Janeiro",
    "RN" = "Rio Grande do Norte",
    "RS" = "Rio Grande do Sul",
    "RO" = "Rondônia",
    "RR" = "Roraima",
    "SC" = "Santa Catarina",
    "SP" = "São Paulo",
    "SE" = "Sergipe",
    "TO" = "Tocantins"
  )
  
  return(estados_brasileiros)
}

printEstadosBrasileiros <- function() {
  
  # Criar um hashmap (lista nomeada) para os estados brasileiros com siglas como chave e IDs como valor
  estados_brasileiros_sigla <- list(
    "AC" = 12,  # Acre
    "AL" = 27,  # Alagoas
    "AP" = 13,  # Amapá
    "AM" = 13,  # Amazonas
    "BA" = 29,  # Bahia
    "CE" = 23,  # Ceará
    "DF" = 53,  # Distrito Federal
    "ES" = 32,  # Espírito Santo
    "GO" = 52,  # Goiás
    "MA" = 21,  # Maranhão
    "MT" = 51,  # Mato Grosso
    "MS" = 50,  # Mato Grosso do Sul
    "MG" = 31,  # Minas Gerais
    "PA" = 15,  # Pará
    "PB" = 25,  # Paraíba
    "PR" = 41,  # Paraná
    "PE" = 26,  # Pernambuco
    "PI" = 22,  # Piauí
    "RJ" = 33,  # Rio de Janeiro
    "RN" = 24,  # Rio Grande do Norte
    "RS" = 43,  # Rio Grande do Sul
    "RO" = 11,  # Rondônia
    "RR" = 14,  # Roraima
    "SC" = 42,  # Santa Catarina
    "SP" = 35,  # São Paulo
    "SE" = 28,  # Sergipe
    "TO" = 17   # Tocantins
  )
  
  # Exibir o hashmap
  print(estados_brasileiros_sigla)
  
  return(estados_brasileiros_sigla)
}

## ggplot graph
createBasicGGplot <- function(df, num_quantis = 5) {

  num_quantis = num_quantis / (num_quantis*num_quantis)
  df <- df %>%
    mutate(quantis = cut(populacao, breaks = quantile(populacao, probs = seq(0, 1, by = 0.2), na.rm = TRUE), include.lowest = TRUE))
  
  ggplot(data = df) +
    geom_sf(aes(fill = quantis), color = "black") +  # Cor da borda
    scale_fill_viridis_d(option = "plasma", name = "População em Quantis", direction = -1) +
    theme_minimal(base_size = 14) +  # Aumentar o tamanho da fonte
    labs(title = "Distribuição da População por Cidade no Estado do Rio Grande do Sul (em Quantis)",
         subtitle = "Cidades agrupadas em quantis com base na população",
         caption = "Fonte: IBGE") +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.caption = element_text(size = 10)
  ) 
}

## leaf
createPlotByState <- function(df, num_quantis = 10) {
  
  num_quantis = num_quantis / (num_quantis*num_quantis)
  df <- df %>%
    mutate(quantis = cut(populacao, breaks = quantile(populacao, probs = seq(0, 1, by = num_quantis), na.rm = TRUE), include.lowest = TRUE))

  pal <- colorFactor(palette = "RdYlBu", domain = df$quantis)
  
  nome_dos_municipios = df$nome_munic
  populacao = df$populacao
  
  leaflet(df) %>%
    addTiles() %>%
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
      label = ~paste(nome_dos_municipios, "População:", populacao_formatada),
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
    ) %>%
    addLegend(pal = pal, values = ~quantis, opacity = 0.7, title = "População em Quantis", position = "bottomright")
  
} 

getMap <- function(url, estado) {
  
  save_path_kml = sprintf('./downloaded/malha_%s.kml', estado)
  save_path_shp = sprintf('./downloaded/malha_%s.shp', estado)
  save_path_json = sprintf('./downloaded/malha_%s.json', estado)
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    writeBin(content(response, "raw"), save_path_json)
    sprintf("Arquivo salvo com sucesso.\n")
  } else {
    sprintf("Erro ao obter dados:", status_code(response), "\n")
  }
  
  return(save_path_json)
}

printEstadosBrasileiros()

limpaBancoPIB <- function(ano) {
  
  df_pib_cru <- read_excel('./PIB dos Municípios - base de dados 2010-2021.xlsx')
  df_pib_rs <- df_pib_cru %>% filter(`Sigla da Unidade da Federação` == "RS")
  rm(df_pib_cru)
  
  colnames(df_pib_rs)[39] = "PIB"
  colnames(df_pib_rs)[40] = "PIB_per_capita"
  colnames(df_pib_rs)[8] = "nome_municipio"
  df_pib_rs$`Código do Município` = as.character(df_pib_rs$`Código do Município`)
  
  
  df_pib_rs_limpo <- df_pib_rs %>% dplyr::select(
    `Código do Município`,
    `Ano`,
    `nome_municipio`,
    `Nome da Mesorregião`,
    `Hierarquia Urbana`,
    `PIB`,
    `PIB_per_capita`,
    `Atividade com maior valor adicionado bruto`
  )
  
  df_pib_rs_retorno <- df_pib_rs_limpo %>% filter(Ano == ano)
  return(df_pib_rs_retorno)
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

criaBanco <- function(ano) {
  
  estado = "RS"
  glue("\nEstado setado para: {estado}\n")
  
  estadosFull <- printEstadosBrasileirosFull()
  printEstado <- sprintf("\n\nGerando para %s\n", toupper(estadosFull[[estado]]))
  sprintf(printEstado)
  
  # URL da API para a malha do estado de qualquer estado brasileiro
  #url <- sprintf("https://servicodados.ibge.gov.br/api/v3/malhas/estados/%s?formato=application/json", estado)
  url <- sprintf("https://servicodados.ibge.gov.br/api/v3/malhas/estados/%s?formato=application/json&qualidade=maxima&intrarregiao=municipio", estado)
  
  jsonPath <- getMap(url, estado)
  
  print("...")
  print("    Transformando JSON para SF...")
  topo_data <- geojson_read(jsonPath, what = "sp")  # "sp" lê como objeto Spatial
  df_mun <- st_as_sf(topo_data)
  rm(topo_data)
  
  print("...")
  print("    Recuperando dados da população de cada município...")
  pop2022 <- populacao_municipios(ano)
  pop <- pop2022 |> filter(uf == estado)
  rm(pop2022)
  
  paste(
    cat("    Os bancos de dados possuem o mesmo tamanho?  "),
    cat(dim(pop |> unique())[1] == dim(pop)[1])
  )
  
  pop <- pop |> 
    mutate(CD_MUN = as.character(cod_municipio))
  
  cat("    Fazendo o full join no banco")
  df_mun <- df_mun |>
    full_join(pop, by = c("codarea" = "CD_MUN"))
  
  df_pib <- limpaBancoPIB(ano = ano)
  df_mun_pib <- df_mun |>
    inner_join(df_pib, by = c("codarea" = "Código do Município"))
  
  
  glue("    Banco PIB tamanho: {dim(df_pib)}\n    Banco do retorno: {dim(df_mun_pib)}")
  
  rm(pop)
  rm(df_pib)
  
  # cat("Criando o plot...")
  # createPlotByState(df_mun, num_quantis = 6)
  
  return(df_mun_pib)
}



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

