require(shinydashboard)

header <- dashboardHeader(title = "PredictApp")

sidebar <- dashboardSidebar(
  h4("Data Science Capstone", style  = "margin: 40px 10px 0px 10px; color:#cccccc;line-height: 100%;"),
  h4("Coursera", style  = "margin: 5px 10px 0px 10px; color:#cccccc;line-height: 140%;"),
  
  h6("The language model was developed on a sample of 15 thousand (0.3%) sentences taken from HC Corpora corpus. 
      The corpus was assembled by crawling twitter, blog and news websites. I removed profanity and non-alphabetic tokens, 
      and trained unigram, bigram, trigram and 4-gram by assigning maximum-likelihood probabilities.
      Model produced 532.5k unigrams and 24.9k word types.", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;"),  
  h6("github.com/ariellev/predictApp", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%; display:block"),
  h6("Ariel Lev", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;")
  )
  #,
  #h6("Data cleaning, model and ShinyApp by Ariel Lev.", style  = "margin: 20px 10px 0px 10px; color:#cccccc;line-height: 140%;")  )

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
        box(width=12, height=75, solidHeader = T,  div(style="display:inline-block;",
            actionButton("text5_loading", "Please"), 
            actionButton("text6_loading", "wait"),
            actionButton("text4_loading", "while"),            
            actionButton("text6_loading", "Loading"),
            actionButton("text6_loading", "..."))          
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