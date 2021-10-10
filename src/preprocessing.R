library(pacman)
p_load(dplyr)

preprocess <- function(df, excluded_variables = c('record')) {
  df %>% 
    dplyr::select(-all_of(excluded_variables))  %>%
    mutate_if(is.character, factor)
}