print(paste(Sys.time(), "starting application"))
require(stringi)
require(hash)
load("model_s.RData")
# load("dictionary.RData")

suffix <- function(mStr, n) {
  suffixes <- rep("", length(mStr))
  if (n > 0) {
    for (i in 1:length(mStr)) {
      vector <- unlist(strsplit(mStr[i], "[ ]+"))
      if (length(vector) > 0) {
      l <- length(vector)
      m <- max(1,l-n+1)
      suff <- stri_flatten(vector[m:l], collapse = " ")
      } else {
        suff <- ""
      }
      suffixes[i] <- suff
    }
  }
  suffixes
}

find_decoded_candidates <- function(encoded, prefix, n = 3) {
  if (length(encoded) == 0) {
    return(NA)
  }
  decoded <- unlist(strsplit(encoded, "\\|"))
  decoded <- decoded[2:length(decoded)]
  
  candidates <- decoded[grepl(paste0("^", prefix), decoded, ignore.case = T)]
  if (length(candidates) == 0) {
    return(NA)
  }
  
  #print(paste0("length_candidates=",length(candidates)))
  
  candidates <- sapply(candidates, function(x) strsplit(x, "@")[[1]][1])
  unique(candidates[1:min(length(candidates),n)])  
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)


# uses stupid backoff
lm_predict_hash <- function(model_hash, sentences, prefix, n = 3) {
  sentences <- gsub("[^A-Za-z .']", "", sentences)
#  TOOD: add filter prefixes <- filter(prefixes)
  predictions <- list()
  for (i in 1:length(sentences)) {    
    sentence <- trim(sentences[i])
    vector <- unlist(strsplit(sentence, "[ ]+")) 
    l <- max(length(vector), 1)
    from <-  min(l,3)
    res <- c()
    for (k in from:0) {
      key <- suffix(sentence,k)
      print(paste0("key=",key, ", k=", k))      
      if (is.na(key) || key == "")
        key <- "<NA>"
      a <- find_decoded_candidates(model_hash[[key]], prefix, n)  
      #print(paste0("class_a=",class(a)))
      res <- c(res, a[!is.na(a)])
    }
    #print(paste0("res1=",res))
    unq <- unique(res)
    predictions[[i]] <- unq[1:min(length(unq), n)]
  }
  predictions
}

endsWithSpaces <- function(sentence) {
  if (nchar(sentence) == 0)
    return(TRUE)
  
  trailing_spaces <- grep("[^a-zA-Z']+$", sentence)
  length(trailing_spaces) > 0  
}

resolve_last_token <- function(sentence) {
  last_token <- ""
  
  if (!endsWithSpaces(sentence)) {
    split <- unlist(strsplit(sentence, "[^a-zA-Z']+"))
    if (length(split) > 0) {
      last_token <- split[length(split)]    
    }    
  } 
  
  print(paste0("last_token=", last_token)) 
  last_token
}

find_candidates <- function(sentence, prefix, n = 3) {
  if (prefix != "") {  
    #candidates <- dictionary[grepl(paste0("^", prefix), dictionary, ignore.case = T)]
    split <- unlist(strsplit(sentence, "[^a-zA-Z']+"))
    split <- split[1:(length(split)-1)]
    sentence <- stri_flatten(split, collapse = " ")    
    
  } else {
    #candidates <- lm_predict_hash(model_hash, sentence, prefix, n)[[1]]    
    
    #candidates <- dictionary[sample(1:length(dictionary), n)]
  }
  
  print(paste0("sentence=", sentence))   
  candidates <- lm_predict_hash(model_hash, sentence, prefix, n)      
  uq <- unique(candidates[[1]])
  length_uq <- length(uq)
  uq[1:min(length_uq, n)]
}

resolve_current_sentence <- function(sentence) {
  current <- ""
  if (length(grep("[\\.\\?\\!]$", sentence)) > 0)
    return (current)
  
  split <- unlist(strsplit(sentence, "[\\.\\?\\!]+"))
  if (length(split) > 0) {
    current <- split[length(split)]
  }
  current
} 

predict <- function(sentence, n = 3) {
  current_sentence <- resolve_current_sentence(sentence)  
  print(paste0("current_sentence=", current_sentence))  
  
  last_token <- resolve_last_token(current_sentence)  
  candidates <- find_candidates(current_sentence, last_token, n)
  print(paste0("candidates=", paste(candidates, collapse=", ")))
  candidates
}

#preds <- predict("")
max_predictions <- 6
print(paste(Sys.time(), "startup completed"))
shinyServer(
  function(input, output, session) {
    #  initalization with most common unigrams ;|the|and|to|of|in|for|that
    v <- reactiveValues(i = 0, clicked = NULL, preds = c("the", "and", "to"))
    
    observe({
      text <- isolate(input$textInput)
      clicked <- v$clicked
      textEndsWithSpaces <- endsWithSpaces(text)
      
      print("---------------------")
      print("item was clicked")      
      print("---------------------")
      print(paste0("clicked=", clicked))       
      print(paste0("input=", text))       
      print(paste0("endsWithSpace=", textEndsWithSpaces)) 
      
      # replacing last token
      if (!textEndsWithSpaces) {        
        split <- unlist(strsplit(text, "[ ]+"))
        split_length <- length(split)
        if ( split_length > 0) {
          split[split_length] <- clicked          
          text <- paste0(paste(split, collapse = " "), " ")
        }
        
      } else {
        # add token
        text <- paste0(text, clicked, " ")
      }
      print(paste0("output=", text)) 
      
      updateTextInput(session, "textInput", value  = text)      
    })

    observe({
      print("---------------------")
      print("user types..")      
      print("---------------------")      
      v$preds <- predict(input$textInput, max_predictions)
          
      if (!is.null(v$preds[[1]])){
      output$buttons <- renderUI({
           box(width=12, height=75, solidHeader = T,  
               if (!is.na(v$preds[1])) div(style="display:inline-block;", actionButton("text1", v$preds[1])), 
               if (!is.na(v$preds[2])) div(style="display:inline-block;", actionButton("text2", v$preds[2])), 
               if (!is.na(v$preds[3])) div(style="display:inline-block;", actionButton("text3", v$preds[3])),
               if (!is.na(v$preds[4])) div(style="display:inline-block;", actionButton("text4", v$preds[4])),
               if (!is.na(v$preds[5])) div(style="display:inline-block;", actionButton("text5", v$preds[5])),
               if (!is.na(v$preds[6])) div(style="display:inline-block;", actionButton("text6", v$preds[6]))
               )                
        })   
      } else {
        output$buttons <- renderUI({box(width=12, height=75, solidHeader = T)})      
      }
    })
    
    observe({
      print("---------------------")
      print("pin")      
      print("---------------------")  
      output$pinned <- renderUI(
        #text <- isolate(input$textInput),
        #updateTextInput(session, "textInput", value  = ""),
        h2(isolate(input$textInput))
      )      
    })
        
    observeEvent(input$text1, {
      v$clicked <- v$preds[1]
    })

    observeEvent(input$text2, {
      v$clicked <- v$preds[2]
    })
    
    observeEvent(input$text3, {
      v$clicked <- v$preds[3]
    })

    observeEvent(input$text4, {
      v$clicked <- v$preds[4]
    })
    
    observeEvent(input$text5, {
      v$clicked <- v$preds[5]
    })
    
    observeEvent(input$text6, {
      v$clicked <- v$preds[6]
    })    
  })