library(pacman)
p_load(tidyverse, dplyr)
p_load_gh('adrianmarino/commons')

target_column <- 'peso'

load_train_set <- function() load_df_from_csv('../dataset/encuesta_salud_train.csv')
load_test_set  <- function() load_df_from_csv('../dataset/encuesta_salud_test.csv')

feat <- function(df) features(df, target_column)
tar  <- function(df) target(df, target_column)


missings_summary <- function(
  df, 
  missings = c(NA, NULL, 'Dato perdido')
) {
  df %>% 
    gather(., key = Variable, value = value) %>%
    mutate(is_missing = if_else(value %in% missings, 1, 0)) %>%
    group_by(Variable) %>% 
    summarise(
      `Nª Categorias` = n_distinct(value),
      `Nª Faltantes`   = sum(is_missing),
      `% Faltantes`    = round(`Nª Faltantes` / nrow(df) * 100, 2)
    ) %>%
    arrange(desc(`% Faltantes`), `Nª Categorias`)
}

drop_missings <- function(df, special_missings = c('Dato perdido')) {
  df <- df %>% drop_na()
  column_names <- df %>% colnames()
  
  for(col in  column_names) {
    for(missing in special_missings) {
      df <-df %>% filter(!grepl(missing, !!sym(col)))
    }
  }
  df
}

show_values <- function(df , columns=c()) {
  if(is_empty(columns)) {
    columns <- df %>% colnames()
  }
  for(column in columns) {
    p <- df %>% group_by(!!sym(column)) %>% tally()
    print(p)
  }
}


important_variables_set <- function(result, top=2, metrics=c("%IncMSE", "IncNodePurity")) {
  metrics %>% 
    map(function(metric) top_acc_features(result, top, metric) ) %>% 
    unlist() %>% 
    unique()
}