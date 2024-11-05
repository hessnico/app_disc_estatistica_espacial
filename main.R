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
  df_tmp = main(ano = i)
  df_tmp$Year = i
  df_populacao <- rbind(df_populacao, 
                        df_tmp) 
}

#####
# banco do PIB foi pego por aqui:
# https://ftp.ibge.gov.br/Pib_Municipios/2021/base/base_de_dados_2010_2021_xlsx.zip

