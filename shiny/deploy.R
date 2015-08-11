require(shiny)
require(shinyapps)
deployApp(appName = "predictApp")
shinyapps::configureApp("predictApp", size="xlarge")
shinyapps::showLogs(appName = "predictApp")

