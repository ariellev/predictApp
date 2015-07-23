require(shinydashboard)

header <- dashboardHeader(title = "Capstone")

sidebar <- dashboardSidebar(
  h4("Language modelling", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  h6("Data cleaning, model and ShinyApp by Ariel Lev.", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;")
  )

body <- dashboardBody(
  includeCSS("style.css"),
  fluidRow(
    column(width=12,
             box(width=12, height=550, solidHeader = T, 
                 HTML("<textarea rows='10' cols='51' id='canvas' autocomplete='off' autocorrect='off' autocapitalize='off' spellcheck='false'></textarea>")
                   #textInput("canvas", "", value = "")
             )
           )
    )
)

dashboardPage(
  header,
  sidebar,
  body
)