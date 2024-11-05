# https://github.com/tbrugz/ribge
  # install.packages("devtools")
  # devtools::install_github("tbrugz/ribge")

library(httr)
library(sf)
library(ribge)
library(dplyr)
library(leaflet)
library(viridis)
library(ggplot2)
library(geojsonio)

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
      label = ~paste(nome_dos_municipios, "População:", populacao),
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

main <- function(ano) {
  
  estado = "RS"
  
  estadosFull <- printEstadosBrasileirosFull()
  printEstado <- sprintf("Gerando para %s", toupper(estadosFull[[estado]]))
  sprintf(printEstado)
  
  # URL da API para a malha do estado de qualquer estado brasileiro
  #url <- sprintf("https://servicodados.ibge.gov.br/api/v3/malhas/estados/%s?formato=application/json", estado)
  url <- sprintf("https://servicodados.ibge.gov.br/api/v3/malhas/estados/%s?formato=application/json&qualidade=maxima&intrarregiao=municipio", estado)
  
  jsonPath <- getMap(url, estado)
  
  print("...")
  print("Transformando JSON para SF...")
  topo_data <- geojson_read(jsonPath, what = "sp")  # "sp" lê como objeto Spatial
  df_mun <- st_as_sf(topo_data)
  rm(topo_data)
  
  print("...")
  print("Recuperando dados da população de cada município...")
  pop2022 <- populacao_municipios(ano)
  pop <- pop2022 |> filter(uf == estado)
  rm(pop2022)
  
  paste(
    cat("Os bancos de dados possuem o mesmo tamanho?  "),
    cat(dim(pop |> unique())[1] == dim(pop)[1])
  )
  
  pop <- pop |> 
    mutate(CD_MUN = as.character(cod_municipio))
  
  cat("\n\nFazendo o full join no banco")
  df_mun <- df_mun |>
    full_join(pop, by = c("codarea" = "CD_MUN"))
  
  rm(pop)
  
  cat("Criando o plot...")
  createPlotByState(df_mun, num_quantis = 6)
  
  return(df_mun)
}