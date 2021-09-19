library(pacman)
p_load(dplyr)

preprocess <- function(
  df, 
  excluded_variables = c('record')
) {
  load_train_set() %>% 
    select(-all_of(excluded_variables))  %>%
    mutate_if(is.character, factor)
}