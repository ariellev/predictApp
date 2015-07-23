require(stringr)

shinyServer(
  function(input, output, session) {
    
    observe({
      x <- input$canvas
      print(x)
      n <- nchar(x)
      last <- substr(x, n , n)
      pre <- substr(x, n-1 , n-1)
      #print(paste("last", last))
      #print(paste("pre", pre))
      if (pre != " " && last == " " ) {
        sp <- str_split(str_trim(x), "[ ]+")[[1]]
        l <- length(sp)
        sp <- c(sp[-l], l)
        print(sp)
        
        s <- str_c(sp, sep = " ")
        
        updateTextInput(session, "canvas", value = str_c(s, " ", sep=""))
      }
      })
    
  })