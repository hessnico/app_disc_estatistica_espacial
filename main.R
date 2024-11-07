main <- function() {
  source('./createDataFrame.R')
  source('./shinyApp.R')
}; main()

shinyApp(ui = ui, server = server)
