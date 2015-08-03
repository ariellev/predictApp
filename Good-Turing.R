discount <- function(df, V) {
  n <- df[1,3]  
  counts <- df %>% group_by(count) %>% summarize(Nc = n())
  #counts[1, ]$Nc <- V^n - nrow(df)
  counts <- rbind(list(count = 0, Nc = V^n - nrow(df)), counts, c(max(counts$count) + 1, Inf))
  
#   counts$good_turing <- sapply(counts$count, function(x) {
#     Nc <- counts[counts$count >= x, 2][1,]$Nc    
#     Nc1 <- counts[counts$count >= (x + 1), 2][1,]$Nc
#     ((x + 1)*Nc1) / Nc
#   })
#   counts 
    discount <- 1*counts[2,]$Nc/counts[1,]$Nc
    discount
}

train_good_turing_base <- function(model, payload) {
  sub <- model[model$n == 1, ]
  total <- nrow(sub)
  d <- discount(sub, payload)
  sub$count <- sub$count - d / total 
  sub <- rbind(sub, list(term = "<UNK>", count = d, n = 1, prefix = ""))
  sub
}

train_good_turing <- function(df, df_prefix, payload) {
  n <- df[1,3]  
  total <- nrow(df)
  d <- discount(df, payload)
  df$count <- df$count - d / total 
  df <- rbind(df, list(term = paste(rep("<UNK>", n), collapse = " "), count = d, n = 2, prefix = paste(rep("<UNK>", n-1), collapse = " ")))
  df
}



# good_turing_numbers <- function(df, V) {
#   n <- df[1,3]  
#   counts <- df %>% group_by(count) %>% summarize(Nc = n())
#   #counts[1, ]$Nc <- V^n - nrow(df)
#   counts <- rbind(list(count = 0, Nc = V^n - nrow(df)), counts, c(max(counts$count) + 1, Inf))
#   
#   counts$good_turing <- sapply(counts$count, function(x) {
#     Nc <- counts[counts$count >= x, 2][1,]$Nc    
#     Nc1 <- counts[counts$count >= (x + 1), 2][1,]$Nc
#     ((x + 1)*Nc1) / Nc
#   })
#   counts   
# }
# 
# train_good_turing_base <- function(model, payload) {
#   sub <- model[model$n == 1, ]
#   gt_counts <- good_turing_numbers(sub, payload)
#   joined <- left_join(sub, gt_counts)
#   print(gt_counts)
#   model[model$n == 1, ]
# }
# 
# train_good_turing <- function(df, df_prefix, payload) {
#   gt_counts <- good_turing_numbers(df, payload)
#   #print(counts)
#   df
# }