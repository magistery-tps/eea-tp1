library(pacman)
p_load(tidyverse, tidymodels, compareGroups, MASS, caret)

show_train_test_props <- function(train_set, test_set) {
  n_test     <- nrow(test_set)
  n_train    <- nrow(train_set)
  n_total    <- n_train + n_test 
  train_perc <- round((n_train / n_total) * 100, 2)
  test_perc  <- round((n_test / n_total) * 100, 2)

  print(paste('Train: ', train_perc,'%', ', Test: ', test_perc, '%', sep=''))  
}

model_coefficients_summary <- function(df, p_value_threshold=0.05) {
  if(!("p.value" %in% colnames(df))) {
    print("WARN: p.value column is required to make model coefficients summary!\n");
    return(NULL)
  }
  if(!("conf.low" %in% colnames(df))) {
    print("WARN: conf.low column is required to make model coefficients summary!\n");
    return(NULL)
  }
  if(!("conf.high" %in% colnames(df))) {
    print("WARN: conf.high column is required to make model coefficients summary!\n");
    return(NULL)
  }

  df %>% 
    mutate(
      Signiticativo = ifelse(p.value < p_value_threshold, "Si", "No"),
      "IC incluye al cero" = ifelse(conf.low <= 0 & conf.high >=0, "Si", "No")
    ) %>%
    rename(Termino = term, Coeficiente = estimate) %>% 
    select(Termino, Coeficiente, Signiticativo, "IC incluye al cero") 
}

plot_tidy_coefficients <- function(df) {
  if(!("p.value" %in% colnames(df))) {
    print("WARN: p.value column is required to plot tidy coefficients!\n");
    return(NULL)  
  }
  if(!("conf.low" %in% colnames(df))) {
    print("WARN: conf.low column is required to plot tidy coefficients!\n");
    return(NULL)
  }
  if(!("conf.high" %in% colnames(df))) {
    print("WARN: conf.high column is required to plot tidy coefficients!\n");
    return(NULL)
  }
  
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


coefficients_summary <- function(model, show_tidy_sumamry = TRUE) {
  tidy_sumamry <- tidy(model, conf.int = TRUE)
  
  if(show_tidy_sumamry) {
    printTable(as.data.frame(tidy_sumamry))
  }
  
  model_summary <- model_coefficients_summary(tidy_sumamry)
  if(!is.null(model_summary)) {
    printTable(as.data.frame(model_summary))
  }

  plot_tidy_coefficients(tidy_sumamry)
}

anova_summary <- function(model, p_value_threshold=0.05) {
  tidy(anova(model)) %>% 
    mutate(
      signiticativa = ifelse(is.na(p.value), NA,  ifelse(p.value < p_value_threshold, "Si", "No"))
    )
}

train_test_eval_metric_summary <- function(
  models, 
  test_set = NULL, 
  truth_column='peso', 
  metric_fn = rmse
) {
  train_eval <- eval_metric_summary(
    models, 
    truth_column = truth_column, 
    metric_fn    = metric_fn
  ) %>% rename(train = .estimate)

  test_eval <- eval_metric_summary(
    models, 
    truth_column = truth_column, 
    metric_fn    = metric_fn,
    test_set     = test_set
  ) %>% rename(test = .estimate)

  train_eval %>%
    inner_join(test_eval, by= 'model') %>%
    mutate(train_test_diff = abs(train - test))
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
    dplyr::select(model, .estimate) %>%
    dplyr::arrange(.estimate)
}

custom_models_evaluation_summary <- function(
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


models_evaluation_summary <- function(models, test_set, metric_fn = rmse) {
  train_sumary <- eval_metric_summary(models, metric_fn = metric_fn) %>% 
    rename(train_metric = .estimate)
  
  test_sumary  <- eval_metric_summary(models, test_set, metric_fn = metric_fn) %>%
    rename(test_metric = .estimate)

  train_sumary %>% 
      inner_join(test_sumary, by='model') %>%
      mutate(metric_diff =  abs(train_metric -test_metric)) %>%
      arrange(metric_diff, test_metric)
}

compare_r2 <- function(model_1, model_2) {
  r2_model_1 <- glance(model_1)$adj.r.squared
  r2_model_2 <- glance(model_2)$adj.r.squared
  diff       <- abs(r2_model_2- r2_model_1) / max(r2_model_1, r2_model_2) * 100 
  
  print(paste(
    'R2 - Modelo A: ', round(r2_model_1, 3), 
    'Modelo B:', round(r2_model_2, 3), 
    'Diff: ', round(diff, 3), '%'
  ))
}

eval_models_summary <- function(models, test_set,  metric_fn = rmse) {
  summary <- models %>% 
  train_test_eval_metric_summary(test_set = test_set, metric_fn = metric_fn)

  min_test_error <- summary %>% 
    pull(test) %>% 
    min()
  
  summary %>% 
    arrange(test) %>%
    mutate(diff_min_test = abs(test - min_test_error))
}

join_eval_summaries <- function(sumarry_a, summary_b) {
  summary <- sumarry_a %>% union_all(summary_b)
  
  min_test_error <- summary %>% pull(test) %>% min()

  summary %>%
    arrange(test) %>%
    mutate(diff_min_test = abs(test - min_test_error))
}



