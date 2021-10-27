library(pacman)
p_load(tidyverse, dplyr, compareGroups)
p_load_gh('adrianmarino/commons')

target_column <- 'peso'

load_train_set          <- function() load_df_from_csv('../dataset/encuesta_salud_train.csv')
load_original_train_set <- function() load_df_from_csv('../dataset/encuesta_salud_modelo6.csv')
load_test_set           <- function() load_df_from_csv('../dataset/encuesta_salud_test.csv')

p_load(tidyverse, tidymodels, compareGroups)

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

missings_columns <- function(ds, column_max_missings = 0.5) {
  missings_summary(ds) %>% 
    dplyr::filter(`% Faltantes` > (column_max_missings * 100)) %>%
    dplyr::select(Variable) %>%
    dplyr::pull()
}

process_missings <- function(
  df, 
  missings_values = c('Dato perdido'), 
  column_max_missings = 0.5
) {
  # Reemplaza los valores considerados missings por NA...
  for(missings_value in missings_values) {
    df <- df %>% mutate_all(na_if, missings_value)
  }

  # Borra las columnas que tiene un alto porcentaje de missings (Ej: y).
  many_misings_columns <- df %>% missings_columns(column_max_missings = column_max_missings)
  df %>% select(-many_misings_columns)
}

show_values <- function(df , columns=c()) {
  if(is_empty(columns)) {
    columns <- df %>% colnames()
  }
  for(column in columns) {
    printTable(df %>% group_by(!!sym(column)) %>% tally())
  }
}


important_variables_set <- function(result, top=2, metrics=c("%IncMSE", "IncNodePurity")) {
  metrics %>% 
    map(function(metric) top_acc_features(result, top, metric) ) %>% 
    unlist() %>% 
    unique()
}