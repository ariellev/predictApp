require(SnowballC)
require(data.table)

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

  print(paste0(Sys.time(), " total files [", length(files), "]"))
  for (i in 1:length(files)) {
    options(mc.cores=1)
    cluster <- makeCluster(detectCores() - 1)
    registerDoParallel(cluster)
    
    print(paste0(Sys.time(), " reading file [", i, "] [", files[i], "]"))
    lines <- readLines(files[i])   
    print(paste0(Sys.time(), " filtering file [", i, "] [", files[i], "]"))    
    filtered <- filter(lines)    
    rm(lines)    
    #filtered <- paste("<s>", filtered, "</s>")
    print(paste0(Sys.time(), " tokenizing file [", i, "] [", files[i], "]"))        
    corpus <- Corpus(VectorSource(filtered))
    rm(filtered)    
    # tidying up a bit more
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, PlainTextDocument)
    
    tokenized <- tokenize(corpus, 1, 4)
    rm(corpus)
    
    print(paste0(Sys.time(), " grouping file [", i, "] [", files[i], "]"))    
    tokenized <- group_by(tokenized, term) %>% summarize(count = sum(count))
    #tokenized$prefix <- unlist(apply(tokenized, 1, function(x) prefix_str(x[2], as.numeric(x[1])-1)))
    
    #tokenized <- tokenized[, c(2,3,1,4)]
    write.table(tokenized, paste0(dir_name, "/", i), row.names = F)
    rm(tokenized)
    print("-------------------------------")    
    stopCluster(cluster)    
  }
  "done"
}

create_model_par <- function(pattern, dir_name = NULL) {
  files <- Sys.glob(pattern)
  if (is.null(dir_name)) {
    dir_name <- paste0("model_en_US_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"))
  }
  dir.create(dir_name)

  #cluster <- makeCluster(detectCores() - 1, type="FORK", output="create_model.txt")
  print(paste0(Sys.time(), " total files [", length(files), "]"))  
  file_lines <- lapply(files, readLines)
  print(paste0(Sys.time(), " going parallel"))
  outs <- parLapply(cluster, file_lines, function(lines) {
    print(paste0(Sys.time(), " processing file"))        
    #lines <- readLines(file_name) 
    filtered <- filter(lines)    
    
    corpus <- Corpus(VectorSource(filtered))
    # tidying up a bit more
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, PlainTextDocument)
    
    tokenized <- tokenize(corpus, 1, 4)
    
    print(paste0(Sys.time(), " grouping file"))    
    tokenized <- group_by(tokenized, term) %>% summarize(count = sum(count))
    
    file_output <- paste0(dir_name, "/", file_name)
    write.table(tokenized, file_output, row.names = F)
    file_output
  })  
  #stopCluster(cluster)
  outs
}

aggregate_model <- function(pattern) {
  file_names <- Sys.glob(pattern)
  print(paste0(Sys.time(), " aggregating files [", length(file_names), "]"))
  cluster <- makeCluster(detectCores() - 4, type="FORK", output="create_model.txt")
  df_list <- parLapply(cluster, file_names, fread)
  print(paste0(Sys.time(), " reducing"))
  
  #agg_model <- rbindlist(df_list)
  agg_model <- Reduce(function(x,y) {
    reduced <- rbind(x,y)
    reduced <- group_by(reduced, term) %>% summarize(count = sum(count))
    reduced
  }, df_list)
  file_name <- paste0("aggregated_model_en_US_", format(Sys.time(), "%d_%m_%Y_%H_%M_%S"))  
  write.table(agg_model, file_name, row.names = F)
  
  print(paste0(Sys.time(), " computing N and prefixes"))  
  cols <- t(parSapply( cluster, agg_model$term, function(x) {
    v <- unlist(strsplit(x, "[ ]+"))
    m <- length(v) - 1
    pr <- stri_flatten(v[1:m], collapse = " ")
    c((m+1), pr)
  }))
  agg_model$n = as.numeric(cols[,1])
  agg_model$prefix <- cols[,2]
  print(paste0(Sys.time(), " grouping"))  
  agg_model <- group_by(agg_model, n, term) %>% arrange(n, desc(count))  
  stopCluster(cluster) 
  write.table(agg_model, file_name, row.names = F)  
  print(paste0(Sys.time(), " done"))  
  agg_model
}

# post process
# cat master_small | awk -f ../post_processing.awk >> model_post
# master <- fread("model_post")
# setnames(master,c("prefix", "next", "p", "encoded") )
# master <- group_by(master, prefix) %>% arrange(prefix, desc(p))
# sm <- master %>% summarize(encoded = stri_flatten(encoded, collapse = ";"))
# sm$prefix[1] <- "<NA>"
# model_hash <- hash(keys = sm$prefix, values = sm$encoded)
# save(model_hash, file = "model.RData", compress = T)