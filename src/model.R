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

