require(shinydashboard)

header <- dashboardHeader(title = "Capstone")

sidebar <- dashboardSidebar(
  h4("Language modelling", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  h6("Data cleaning, model and ShinyApp by Ariel Lev.", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;")  )

body <- dashboardBody(
  includeCSS("style.css"),
  tags$script('Shiny.addCustomMessageHandler("display_predictions",
        function(message) {
          document.getElementById("pleaseWait").style.display = "none";
          document.getElementById("predictions").style.display = "block";
        });'),
  
  fluidRow( box(width=12, height=120, solidHeader = T, 
                div(style="height: 100%; display: block", autocomplete='off', textInput("textInput", "", "")),                
                div(style="display:inline-block; font-size:8pt; float: right;", actionButton("reset", "reset")),
                div(style="display:inline-block; font-size:8pt; float: right; padding-right:5px", actionButton("clear", "clear")),              
                div(style="display:inline-block; font-size:8pt; float: right; padding-right:5px", actionButton("post", "post"))
             )
           ),
  div(style="height: 100%; display: block", id="pleaseWait", 
      fluidRow(
        box(width=12, height=75, solidHeader = T,  
            div(style="display:inline-block;", actionButton("text4_loading", "Please")), 
            div(style="display:inline-block;", actionButton("text5_loading", "wait")),             
            div(style="display:inline-block;", actionButton("text6_loading", "Loading")),
            div(style="display:inline-block;", actionButton("text6_loading", "..."))          
            )
        )
    ),
  div(style="height: 100%; display: block", id="predictions", fluidRow(uiOutput("buttons"))),
  fluidRow(uiOutput("history"))
)
dashboardPage(
  header,
  sidebar,
  body
)