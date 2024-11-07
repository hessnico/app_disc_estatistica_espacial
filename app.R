# constroi banco de dados
#main <- function() {
#   source('./createDataFrame.R')
#   source('./shinyApp.R')
# }; main()

df_app <- readRDS(file = "bancodedados.rds")

source('./utils_est_espacial.R')
source('./shinyApp.R')

shinyApp(ui = ui, server = server)
