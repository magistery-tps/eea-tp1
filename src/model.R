library(pacman)
p_load(tidyverse, tidymodels)

show_train_test_props <- function(train_set, test_set) {
  n_test     <- nrow(test_set)
  n_train    <- nrow(train_set)
  n_total    <- n_train + n_test 
  train_perc <- round((n_train / n_total) * 100, 2)
  test_perc  <- round((n_test / n_total) * 100, 2)

  print(paste('Train: ', train_perc,'%', ', Test: ', test_perc, '%', sep=''))  
}
