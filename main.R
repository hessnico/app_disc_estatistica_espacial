setwd('./')

paths = 'downloaded'
if (!dir.exists(paths)) {
  dir.create(paths, showWarnings = TRUE, recursive = FALSE, mode = "0777")
}

source('./utils_est_espacial.R')
cat(sprintf("Em:"), getwd())

df_populacao = data.frame()
for (i in 2010:2021) {
  print(i)
  df_tmp = criaBanco(ano = i)
  df_tmp$Year = i
  df_populacao <- rbind(df_populacao, 
                        df_tmp) 
}

#####
# banco do PIB foi pego por aqui:
# https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2010_2021_xlsx.zip

# TODO
## Falta fazer um slider para selecionar a qtd de quantis
## Falta fazer o gráfico do PIB
## Falta fazer o gráfico do PIB_per_capita
## Falta fazer o gráfico do Mesorregião
## Falta fazer o gráfico do "Atividade com maior valor adicionado bruto"
## Falta fazer o gráfico do "Hierarquia Urbana" 