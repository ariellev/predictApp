require(shinydashboard)

header <- dashboardHeader(title = "Capstone")

sidebar <- dashboardSidebar(
  h4("Language modelling", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  h6("Data cleaning, model and ShinyApp by Ariel Lev.", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;")
  )

body <- dashboardBody(
  includeCSS("style.css"),
  tags$script('document.getElementById("textInput").onKeypress = function(event) {
                  if (event.which == 13){
                     Shiny.onInputChange("#pinned", e.which);
                  }
                  };'),
  fluidRow(
             box(width=12, height=100, solidHeader = T, 
                 div(style="height: 100%", autocomplete='off', textInput("textInput", "", ""))
                #HTML("<textarea rows='5' cols='60' id='textArea'autocorrect='off' autocapitalize='off' spellcheck='false'></textarea>")
             )
           ),
  fluidRow(
    uiOutput("buttons")),
  fluidRow(
    uiOutput("pinned"))
  )
dashboardPage(
  header,
  sidebar,
  body
)