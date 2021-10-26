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

missings_columns <- function(ds, max_col_missings = 0.5) {
  missings_summary(ds) %>% 
    dplyr::filter(`% Faltantes` > (max_col_missings * 100)) %>%
    dplyr::select(Variable) %>%
    dplyr::pull()
}

drop_missings <- function(df, special_missings = c('Dato perdido'), max_col_missings = 0.5) {
  mising_column_names <- df %>% missings_columns()
  column_names        <- setdiff(df %>% colnames(), mising_column_names)

  table <- df
  for(col in  column_names) {
    for(missing in special_missings) {
      table <- table %>% filter(!grepl(missing, !!sym(col)))
    }
  }

  table %>% 
    select(-mising_column_names) %>% 
    drop_na()
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