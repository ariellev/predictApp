require(SnowballC)

if (!exists("profanity_hash")) {
  profanity <- tolower(readLines("final/en_US/profanity.txt"))
  profanity_hash <- hash(keys = profanity, values=rep("", length(profanity)))
}

if (!exists("dictionary_hash")) {
  dictionary <- tolower(readLines("final/en_US/dictionary.txt"))
  dictionary_hash <- hash(keys = dictionary, values=rep("", length(dictionary))) 
}

if (!exists("stem_hash")) {
  stem_dictionary <- wordStem(dictionary)
  stem_dictionary <- stem_dictionary[nchar(stem_dictionary) > 0]
  stem_hash <- hash(keys = stem_dictionary, values=rep("", length(stem))) 
}

# the function filters a data vector by eliminating profanity and by checking existence against a dictionary.
filter <- function(data) {
  cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(cluster)
  data <- tolower(data)
  
  filtered <- c()
  filtered <- sapply(data, function(line) {
        split <- unlist(strsplit(line, "[ ]+"))
        
        idx <- which(nchar(split) > 0)
        split <- split[idx]
        
        if (length(split) > 0) {
          idx <- sapply(split, function(x) {
            if (has.key(x, dictionary_hash)) return(TRUE)
            
            # contractions
            contractions <- grep("i'(ll|m|d|ve)$|(he|she|it)'(ll|s|d)$|(you|we|they)'(ll|re|d|ve)$|(is|are|ca|could|would|do|does|did|have|has|wo|were|should|was|had)n't$|(what|who)'(s)$", x)
            if (length(contractions) > 0) {
              dictionary_hash[[x]] <- ""          
              return(TRUE)          
            }
            
            # stemming
            stemmed <- wordStem(x)        
            # removing possesive
            stemmed <- gsub("'$", "", stemmed)
            if (length(stemmed) > 0 && nchar(stemmed) > 0 && has.key(stemmed, stem_hash)) return(TRUE)                    
            return (FALSE)
          })
          split <- split[idx]
          
          idx <- which(!has.key(split, profanity_hash))
          split <- split[idx]               
        }
        
        split <- paste(split, collapse = " ")        
        split
  })
#   for (i in 1:length(data)) {
#     line <- data[i]
#     split <- unlist(strsplit(line, "[ ]+"))
#     
#     idx <- which(nchar(split) > 0)
#     split <- split[idx]
#     
#     if (length(split) > 0) {
#       idx <- sapply(split, function(x) {
#         if (has.key(x, dictionary_hash)) return(TRUE)
#         
#         # contractions
#         contractions <- grep("i'(ll|m|d|ve)$|(he|she|it)'(ll|s|d)$|(you|we|they)'(ll|re|d|ve)$|(is|are|ca|could|would|do|does|did|have|has|wo|were|should|was|had)n't$|(what|who)'(s)$", x)
#         if (length(contractions) > 0) {
#           dictionary_hash[[x]] <- ""          
#           return(TRUE)          
#         }
#         
#         # stemming
#         stemmed <- wordStem(x)        
#         # removing possesive
#         stemmed <- gsub("'$", "", stemmed)  
#         # babies -> babi -> babi(i|y)
#         stemmed <- gsub("i$", "(i|y)", stemmed)
#         if (sum(grepl(paste0("^(", stemmed, ")"), dictionary)) > 0) {
#           #if (length(stemmed) > 0) {     
#             #print(paste0("stemmed=", stemmed))
#             #dictionary_hash[[stemmed]] <- ""
#           #}
#           return (TRUE)
#         }
#                 
#         return (FALSE)
#       })
#       split <- split[idx]
#       
#       idx <- which(!has.key(split, profanity_hash))
#       split <- split[idx]               
#     }
#     
#     split <- paste(split, collapse = " ")
#     filtered[i] <- split
#   }

  filtered[nchar(filtered) > 0]
}

# The function returns a data frame containing n-grams with their corresponding counts and frequencies in corpus
tokenize <- function(corpus, min, max) {
  tokenizer <- function(x) {NGramTokenizer(x, Weka_control(min = min, max = max, delimiters = " ."))}
  dtm  <- DocumentTermMatrix(corpus, control = list(tokenize = tokenizer, wordLengths = c( 1, Inf)))
  
  sM <- sparseMatrix(i=dtm$i, j = dtm$j, x = dtm$v)
  dt <- data.frame( term = as.character(dtm$dimnames$Terms), count = colSums(sM))
  dt$term <- as.character(dt$term)
  dt
}

create_model <- function(pattern, dir_name = NULL) {
  files <- Sys.glob(pattern)
  if (is.null(dir_name)) {
    dir_name <- paste0("model_en_US_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"))
  }
  dir.create(dir_name)
  options(mc.cores=1)
  cluster <- makeCluster(detectCores() - 1)
  registerDoParallel(cluster)
  
  print(paste0(Sys.time(), " total files [", length(files), "]"))
  for (i in 1:length(files)) {
    print(paste0(Sys.time(), " reading file [", i, "] [", files[i], "]"))
    lines <- readLines(files[i])    
    print(paste0(Sys.time(), " filtering file [", i, "] [", files[i], "]"))    
    filtered <- filter(lines)    
    #filtered <- paste("<s>", filtered, "</s>")
    print(paste0(Sys.time(), " tokenizing file [", i, "] [", files[i], "]"))        
    corpus <- Corpus(VectorSource(filtered))
    # tidying up a bit more
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, PlainTextDocument)
    
    tokenized <- tokenize(corpus, 1, 4)
    
    print(paste0(Sys.time(), " grouping file [", i, "] [", files[i], "]"))    
    tokenized <- group_by(tokenized, term) %>% summarize(count = sum(count))
    #tokenized$prefix <- unlist(apply(tokenized, 1, function(x) prefix_str(x[2], as.numeric(x[1])-1)))
    
    #tokenized <- tokenized[, c(2,3,1,4)]
    write.table(tokenized, paste0(dir_name, "/", i), row.names = F)
    print("-------------------------------")    
  }
  
  stopCluster(cluster)
  "done"
}