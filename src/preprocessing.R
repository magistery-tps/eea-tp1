library(pacman)
p_load(tidyverse, dplyr)
p_load_gh('adrianmarino/commons')


import('../src/dataset.R')
import('../src/preprocessing.R')

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


simplify_values <- function(df) {
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
  
  table
}
