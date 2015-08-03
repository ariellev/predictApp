train_MLE_base <- function(model, payload = NULL) { 
  sub <- subset(model, n == 1)
  total_unigrams <- sum(sub$count)
  sub$p <- sub$count / total_unigrams
  sub
}

# calculates likelihood using relative frequencies
train_MLE <- function(df, df_prefix, payload = NULL) {
  n <- df[1,3]$n
  total <- sum(df$count)
  
  h_counts <- hash(df_prefix$term, df_prefix$count)
  # setting deafult value
  df$p <- total
  
  df$p <- sapply(df$prefix, function(x) {d <- h_counts[[x]]; if (!is.null(d)) return(d) else return (-1)})
  df %>% mutate(p = ifelse(p == -1, -1, count / p))
}
