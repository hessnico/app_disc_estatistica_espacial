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

rm(df_tmp)

df_populacao$PIB_notacao_real <-paste0("R$ ", format(df_populacao$PIB, big.mark = ".", decimal.mark = ",", scientific = FALSE))
df_populacao$PIB_per_capita_notacao_real <-paste0("R$ ", format(df_populacao$PIB_per_capita, big.mark = ".", decimal.mark = ",", scientific = FALSE))
df_populacao$populacao_formatada <- format(df_populacao$populacao, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)

# save(df_populacao, file = "./appDataRS.RData")

#####
# banco do PIB foi pego por aqui:
# https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2010_2021_xlsx.zip

## Falta fazer o gráfico do "Atividade com maior valor adicionado bruto"