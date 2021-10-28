library(pacman)
p_load(tidyverse, dplyr)
p_load_gh('adrianmarino/commons')

mapping_cols = c(
  "consumo_semanal_comida_grasa",
  "consumo_semanal_verdura",
  "consumo_semanal_snacks",
  "consumo_semanal_gaseosas",
  "consumo_semanal_frutas",
  "nivel_educativo",
  "edad_consumo_alcohol"
)

 preprocess <- function(df, excluded_variables = c('record')) {
  # Excluded colums and transform char columns to factors. 
  df <- df %>% 
    dplyr::select(-all_of(excluded_variables))  %>%
    mutate_if(is.character, factor)
}


shorten_values <- function(df) {
  mapping <- load_df_from_csv('../dataset/mapping.csv')
  
  table <- df %>% 
    left_join(mapping, by = c("nivel_educativo" = "source")) %>% 
    mutate(nivel_educativo = target) %>% 
    select(-target)
  
  table <- table %>% 
    left_join(mapping, by = c("consumo_semanal_frutas" = "source")) %>% 
    mutate(consumo_semanal_frutas = target) %>% 
    select(-target)
  
  table <- table %>% 
    left_join(mapping, by = c("consumo_semanal_comida_grasa" = "source")) %>% 
    mutate(consumo_semanal_comida_grasa = target) %>% 
    select(-target)
  
  table <- table %>% 
    left_join(mapping, by = c("consumo_semanal_snacks" = "source")) %>% 
    mutate(consumo_semanal_snacks = target) %>% 
    select(-target)
  
  table <- table %>% 
    left_join(mapping, by = c("consumo_semanal_verdura" = "source")) %>% 
    mutate(consumo_semanal_verdura = target)  %>% 
    select(-target)
  
  table <- table %>%
    left_join(mapping, by = c("consumo_semanal_gaseosas" = "source")) %>% 
    mutate(consumo_semanal_gaseosas = target) %>% 
    select(-target)
  
  table <- table %>%
    left_join(mapping, by = c("edad_consumo_alcohol" = "source")) %>% 
    mutate(edad_consumo_alcohol = target) %>% 
    select(-target)
  
  table$edad_consumo_alcohol <- factor(
    table$edad_consumo_alcohol,
    levels=c("0", "<=7", "8-9", "10-11", "12-13", "14-15", "16-17", ">=18"), 
    ordered=TRUE
  )
  
  table$nivel_educativo <- factor(
    table$nivel_educativo,
    levels=c("8", "9", "1", "2", "3"), 
    ordered=TRUE
  )
  
  table$frecuencia_hambre_mensual <- factor(
    table$frecuencia_hambre_mensual,
    levels=c("Nunca", "Rara vez", "Algunas veces", "Casi siempre", "Siempre"), 
    ordered=TRUE
  )
  
  table$consumo_semanal_frutas <- factor(
    table$consumo_semanal_frutas,
    levels=c("0", "<=3", "4-6", "7", "14", "21", ">=28"), 
    ordered=TRUE
  )
  
  table$consumo_semanal_verdura <- factor(
    table$consumo_semanal_verdura,
    levels=c("0", "<=3", "4-6", "7", "14", "21", ">=28"), 
    ordered=TRUE
  )
  
  table$consumo_semanal_gaseosas <- factor(
    table$consumo_semanal_gaseosas,
    levels=c("0", "<=3", "4-6", "7", "14", "21", ">=28"), 
    ordered=TRUE
  )
  
  table$consumo_semanal_snacks <- factor(
    table$consumo_semanal_snacks,
    levels=c("0", "<=3", "4-6", "7", "14", "21", ">=28"), 
    ordered=TRUE
  )
  
  table$consumo_semanal_comida_grasa <- factor(
    table$consumo_semanal_comida_grasa,
    levels=c("0", "<=3", "4-6", "7", "14", "21", ">=28"), 
    ordered=TRUE
  )

  table$genero <- factor(
    table$genero,
    levels=c("Femenino", "Masculino"), 
    ordered=FALSE
  )

  table
}


binning_mean_quantiles_to_3_levels <- function(
  df,
  group_column,
  mean_column,
  quantiles,
  levels  = c('Bajo', 'Medio', 'Alto')
) {
  quantiles <- df %>% 
    group_by(!!sym(group_column)) %>%
    summarise(value = mean(!!sym(mean_column))) %>% 
    pull(value) %>%
    quantile()
  
  print('Cuantiles:')
  print(quantiles)
  
  result <- df %>%
    mutate(
      !!group_column :=  case_when(
        !!sym(mean_column) <  quantiles[2] ~ levels[1],
        !!sym(mean_column) >= quantiles[4] ~ levels[3],
        TRUE ~ levels[2]
      )
    ) %>% 
    mutate(!!group_column := as.factor(!!sym(group_column)))
  
  show_groups(result, group_column)
  
  result
}



show_groups <- function(result, group_column) {
  print('Grupos:')
  groups <- result %>%
    group_by(!!sym(group_column)) %>%
    tally() %>% 
    arrange(desc(n))
  printTable(groups)
}

binning_mean_multiply_quantiles_to_3_levels <- function(
  df,
  group_column,
  column_a,
  column_b,
  levels  = c('Bajo', 'Medio', 'Alto')
) {
  quantiles <- df %>% 
    group_by(!!sym(group_column)) %>%
    summarise(value = mean(!!sym(column_a) * !!sym(column_b))) %>% 
    pull(value) %>% 
    quantile()

  print('Cuantiles:')
  print(quantiles)

  result <- df %>%
    mutate(
      !!group_column := case_when(
        !!sym(column_a) * !!sym(column_b) <  quantiles[2] ~ levels[1],
        !!sym(column_a) * !!sym(column_b) >= quantiles[4] ~ levels[3],
        TRUE ~ levels[2]
      )
    ) %>%
    mutate(!!group_column := as.factor(!!sym(group_column)))

  show_groups(result, group_column)
  
  result
}

