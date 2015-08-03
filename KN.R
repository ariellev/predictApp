kneser_ney_unigrams <- function(unigrams, bigrams) {
  bigrams$suffix <- sapply(bigrams$term, function(x) suffix(x, 1))
  total <- nrow(bigrams)
  p_continuation <- bigrams %>% group_by(suffix) %>% summarise(p = (n() / total))
  h_p <- hash(p_continuation$suffix, p_continuation$p)    
  unigrams$p <- sapply(unigrams$term, function(x) {d <- h_p[[x]]; if (!is.null(d)) return(d) else return (-1)})
  
  # all unigrams which has no preceders gain 1/bigram-types
  unigrams$p[unigrams$p == -1] <- 1/total 
  
  # normalizing    
  unigrams$p <- unigrams$p/sum(unigrams$p)
  
  #unigrams$p_log <- log(unigrams$p)
  unigrams
}

kneser_ney_step <- function(df, df_prefix, V) {
  #Ney et al. [NEK94] estimate the discount value D based on the total number of
  #n-grams occurring exactly once (n1) and twice (n2) [CG99]:
  numbers <- good_turing_numbers(df, V)
  n <- df[1,]$n  
  d <- numbers[2,]$count / (numbers[2,]$count + 2*numbers[3,]$count)
  
  h_count <- hash(df_prefix$term, df_prefix$count)    
  h_pkn<- hash(df_prefix$term, df_prefix$p)   
  
  prefixes <- df %>% group_by(prefix) %>% summarize(count = n())
  h_n1plus <- hash(prefixes$prefix, prefixes$count)    
  
  df$p <- unlist(apply(df, 1, function(x) { 
    print("----------")
    print(paste("x1", x[1], sep="="))
    
    Cprefix <- h_count[[x[4]]]
    print(paste("n-1", n-1, sep="="))
    
    s <-suffix(x[1], n - 1)
    Pkn <- h_pkn[[s]]
    N1plus <- h_n1plus[[x[4]]]
    print(paste("Cprefix", Cprefix, sep="="))
    print(paste("s", s, sep="="))
    print(paste("Pkn", Pkn, sep="="))
    print(paste("N1plus", N1plus, sep="="))
    
    if (is.null(Cprefix) || is.null(Pkn) || is.null(N1plus)) {
      return (-1)
    }
    print(paste("d", d, sep="="))
    #d <- 0
    lambda <- (d / Cprefix) * N1plus
    #lambda <- 1
    print(paste("lambda", lambda, sep="="))
    print(paste("x2", x[2], sep="="))
    
    (max(as.numeric(x[2]) - d, 0) / Cprefix) + lambda*Pkn 
  }))
  df
}

kn <- function(model, d, s) { 
  print(paste("---------", sep="="))
  print(paste("s", s, sep="="))
  v <- unlist(strsplit(s, "[ ]+"))
  n <- length(v)
  if (n > 4) {
    s <- suffix(s, 4)
    n = 4
  }
  
  if ( n == 1) {
    p <- model[model$term == s, ]$p
    if (length(p) == 0)
      p <- 0.000024
    return(p)
  }
  
  s_prefix <- prefix_str(s, n - 1)
  s_suffix <- suffix(s, n - 1)
  
  Cprefix <- model[model$term == s, ]$count
  Count <-  model[model$term == s, ]$count  
  if (length(Count) == 0) {
    Count <- 0.0000000024
    Cprefix <- 0.0000000024
  }
  N1plus <- nrow(model[model$prefix == s_prefix, ])
  
  
  print(paste("s_prefix", s_prefix, sep="="))  
  print(paste("s_suffix", s_suffix, sep="="))
  
  print(paste("Cprefix", Cprefix, sep="="))
  
  print(paste("N1plus", N1plus, sep="="))
  
  print(paste("d", d, sep="="))
  
  lambda <- (d / Cprefix) * N1plus
  #lambda <- 1
  print(paste("lambda", lambda, sep="="))
  print(paste("Count", Count, sep="="))
  
  p <- (max(Count - d, 0) / Cprefix) + lambda*kn(model, d, s_suffix)  
  print(paste("p", p, sep="="))
  
  p
}

kneser_ney_probability <- function(model, d, sentences) {
  kns <- rep(0, length(sentences))
  for (i in 1:length(sentences)) {
    kns[i] <- kn(model, d, sentences[i])
  }
  kns
}

kneser_ney <- function(model, V = sum(model$n == 1)) {
  model_split <- split(model, model$n)
  print(model_split[[2]])
  # unigrams
  model_split[[1]] <- kneser_ney_unigrams(model_split[[1]], model_split[[2]])
  
  # bigrams  
  model_split[[2]] <- kneser_ney_step(model_split[[2]], model_split[[1]], V)
  
  # trigrams  
  model_split[[3]] <- kneser_ney_step(model_split[[3]], model_split[[2]], V)
  
  # quadrigrams  
  model_split[[4]] <- kneser_ney_step(model_split[[4]], model_split[[3]], V)
  
  rbind(model_split[[1]], model_split[[2]], model_split[[3]], model_split[[4]])
  #model_split
}