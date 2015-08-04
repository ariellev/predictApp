require(dplyr)
require(R.utils)
require(hash)
require(Matrix)
require(parallel) 
require(doParallel)
require(ggplot2)
require(knitr)
require(RWeka)
require(tm)
require(stringi)

source("process.R")
source("MLE.R")
source("KN.R")
source("Good-Turing.R")

prefix_str <- function(mStr, n) {
  sapply(mStr, function(x){ v <- unlist(strsplit(x, "[ ]+")); m <- min(length(v), n); if (m == 0) "" else stri_flatten(v[1:m], collapse = " ")})
}

prefix_str_n_minus_one <- function(mStr) {
  print(paste0(Sys.time(), " splitting"))  
  vec <- sapply(mStr, function(x){ unlist(strsplit(x, "[ ]+")) })
  print(paste0(Sys.time(), " truncating"))  
  vec <- sapply( vec, function(v){ m <- length(v) - 1; if (m < 1) "" else stri_flatten(v[1:m], collapse = " ") })
  vec
}

suffix <- function(mStr, n) {
  suffixes <- rep("", length(mStr))
  if (n > 0) {
    for (i in 1:length(mStr)) {
      vector <- unlist(strsplit(mStr[i], "[ ]+"))
      l <- length(vector)
      m <- max(1,l-n+1)
      suff <- stri_flatten(vector[m:l], collapse = " ")
      suffixes[i] <- suff
    }
  }
  suffixes
}

p <- function(model, mStr) {  
  sub <- subset(model, term == mStr)
  sub
}

p_continuation <- function(model, suffix, n) {
  #     if (n == -1) {
  #       ngrams <- length(unlist(strsplit(suffix, "[ ]+")))
  #       sub <- model[model$n > ngrams, ]
  #     }
  #     else {
  #         sub <- model[model$n %in% n,]
  #     }
  sub <- model[model$n == n,]
  total <- nrow(sub)    
  idx <- grepl(paste0("( ",suffix, ")$"), sub$term)
  sum(idx) / total
}

# good_turing_numbers <- function(df, V) {
#   n <- df[1,3]  
#   counts <- df %>% group_by(count) %>% summarize(Nc = n())  
#   counts <- rbind(c(0, V^n - nrow(df)), counts, c(max(counts$count) + 1, Inf))
#   
#   counts$good_turing <- sapply(counts$count, function(x) {
#     Nc <- counts[counts$count >= x, 2][1,]$Nc    
#     Nc1 <- counts[counts$count >= (x + 1), 2][1,]$Nc
#     ((x + 1)*Nc1) / Nc
#   })
#   counts
# }

validate_prefixes <- function(model) {
  su <- group_by(model, prefix)  %>% summarize(total_p = sum(p))  
  (sum(su$total_p) / nrow(su)) == 1
}

lm_probability_rec <- function(model, sentence, alpha = .4) {
  if (sentence == "") {
    return (0)
  }
  
  p <- model[model$term == sentence, ]$p
  if (length(p) == 0) {
    n <- length(unlist(strsplit(sentence, "[ ]+")))
    p <- alpha*lm_probability_rec(model, suffix(sentence, n - 1))
  } 
  return (p)
}

lm_probability <- function(model, sentences) {
  predictions <- rep(0, length(sentences))
  for (i in 1:length(sentences)) {
    p <- lm_probability_rec(model, sentences[i])
    predictions[i] <- p
  }
  predictions
}

lm_train <- function(model, train_base = train_MLE_base, train_method = train_MLE, payload = NULL) {
  model_split <- split(model, model$n)
  # unigrams
  model_split[[1]] <- train_base(model, payload)
  
  # bigrams  
  model_split[[2]] <- train_method(model_split[[2]], model_split[[1]], payload)
  
  # trigrams  
  model_split[[3]] <- train_method(model_split[[3]], model_split[[2]], payload)
  
  # quadrigrams  
  model_split[[4]] <- train_method(model_split[[4]], model_split[[3]], payload)
  
  rbind(model_split[[1]], model_split[[2]], model_split[[3]], model_split[[4]])
}

probability <- function(model, mStr, n) {  
  suff <- suffix(mStr, n - 1)  
  idx <- grepl(paste0("^(",suff, " )"), model$term) & model$n == n
  model[idx, ]
}

# uses stupid backoff
lm_predict <- function(model, prefixes, threshold = 0, alpha = .4) {
  prefixes <- gsub("[^A-Za-z .']", "", prefixes)
  prefixes <- filter(prefixes)
  m <- model[model$p > threshold, ]
  predictions <- list()
  for (i in 1:length(prefixes)) {
    df <- data.frame(term = character(), count = numeric(), n = numeric(), prefix = character(), p = numeric(), next_term = character())    
    prefix <- prefixes[i]
    vector <- unlist(strsplit(prefix, "[ ]+")) 
    from <-  min(length(vector),3)
    for (k in from:1) {
      a <- probability(m, suffix(prefix,k), k+1)
      a$p <- a$p*alpha^(from-k)
      a <- a[a$p > threshold, ]
      a$next_term <- sapply(a$term, function(x) { v <- unlist(strsplit(x, "[ ]+")); v[length(v)]} )
      df <- rbind(df, a)
    }
    predictions[[i]] <- df
  }
  predictions
}

next_word <- function(model, prefixes, threshold = 0.01) {
  l <- lm_predict(model, prefixes, threshold)
  lapply(l, function(x) { if (length(x$next_term) == 0) "" else unique(x$next_term)})
}

evaluate_quiz <- function(model, quiz, threshold = 0.01) {
  l <- next_word(model, quiz[,1], threshold)
  answers <- rep(NA, nrow(quiz))
  preds <- rep(NA, nrow(quiz))
  intersects <- rep(NA, nrow(quiz))
  matches <- rep(NA, nrow(quiz))  
  #, pred = character(), intersect = character(), match = numeric())
  for (i in 1:nrow(quiz)) {
    answers[i] <- paste(c(quiz[i,2], quiz[i,3], quiz[i,4], quiz[i,5]), collapse = ", ")
    preds[i] <- paste(l[[i]], collapse = ", ")    
    intersection <- intersect(l[[i]], c(quiz[i,2], quiz[i,3], quiz[i,4], quiz[i,5]))
    matches[i] <- length(intersection) / 4
    intersects[i] <- paste( intersection, collapse = ", ")
    #df <- rbind(df, c(quiz[i,1], answers))
  }
  data.frame(sentence = paste0("..", suffix(quiz[,1], 5)), answers = answers, predictions = preds, intersects = intersects, match = matches)
}