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

model_coefficients_summary <- function(df, p_value_threshold=0.05) {
  df %>% 
    mutate(
      Signiticativo = ifelse(p.value < p_value_threshold, "Si", "No"),
      "IC incluye al cero" = ifelse(conf.low <= 0 & conf.high >=0, "Si", "No")
    ) %>%
    rename(Termino = term, Coeficiente = estimate) %>% 
    select(Termino, Coeficiente, Signiticativo, "IC incluye al cero") 
}

plot_tidy_coefficients <- function(df) {
  ggplot(
    df %>% arrange(p.value), 
    aes(estimate, term, xmin = conf.low, xmax = conf.high, height = 0)
  ) +
    geom_point(color = "forestgreen") +
    geom_vline(xintercept = 0, lty = 4, color = "black") +
    geom_errorbarh(color = "forestgreen") +
    theme_bw() +
    labs(y = "Coeficientes β", x = "Estimación") +
    ggtitle("Diferencia de coeficientes β contra el cero")
}


coefficients_summary <- function(model) {
  tidy_sumamry <- tidy(model, conf.int = TRUE)
  print(tidy_sumamry)
  
  model_coefficients_summary <- model_coefficients_summary(tidy_sumamry)
  print(model_coefficients_summary)

  plot_tidy_coefficients(tidy_sumamry)
}

anova_summary <- function(model, p_value_threshold=0.05) {
  tidy(anova(model)) %>% 
    mutate(
      signiticativa = ifelse(is.na(p.value), NA,  ifelse(p.value < p_value_threshold, "Si", "No"))
    )
}


eval_metric_summary <- function(
  models, 
  test_set = NULL, 
  truth_column='peso', 
  metric_fn = rmse
) {
  train_preds <- map(.x = models, .f = augment, newdata = test_set)
  
  map_dfr(
    .x       = train_preds, 
    .f       = metric_fn, 
    truth    = !!sym(truth_column), 
    estimate = .fitted, 
    .id="model"
  ) %>% 
    select(model, .estimate) %>%
    arrange(.estimate)
}

models_evaluation_summary <- function(
  model_1, model_2, model_3, model_4, model_5,
  test_set, test_set2, test_set3,
  metric_fn
) {
  train_sumary <- eval_metric_summary(
    list('Modelo 1'=model_1, 'Modelo 2'=model_2, 'Modelo 3'=model_3, 'Modelo 4'=model_4, 'Modelo 5'=model_5), 
    metric_fn = metric_fn
  ) %>% 
    rename(train_metric = .estimate)
  
  test_sumary <-  eval_metric_summary(
    list('Modelo 1'=model_1, 'Modelo 2'=model_2), 
    test_set, 
    metric_fn = metric_fn
  ) %>%
    union_all(eval_metric_summary(
      list('Modelo 3'=model_3), 
      test_set2, 
      metric_fn = metric_fn
    )) %>%
    union_all(eval_metric_summary(
      list('Modelo 4'=model_4),
      test_set2, 
      metric_fn = metric_fn
    )) %>%
    union_all(eval_metric_summary(
      list('Modelo 5'=model_5),
      test_set3,
      metric_fn = metric_fn
    )) %>%
    rename(test_metric = .estimate)
  
  test_sumary %>% 
    inner_join(train_sumary, by='model') %>%
    mutate(metric_diff =  abs(test_metric -train_metric)) %>%
    arrange(metric_diff, test_metric)
}

