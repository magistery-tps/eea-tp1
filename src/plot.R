library(pacman)
p_load(tidyverse, GGally, plotly, ggcorrplot, ggmosaic)

hist_plots <- function(df, columns=c()) {
  if(is_empty(columns)) {
    columns <- df %>% dplyr::select(is.numeric) %>% colnames()
  }
  for(col in columns) {
    p <- ggplot(df, aes(!!sym(col))) + 
      geom_histogram(col="red", aes(fill=..count..), alpha = .65) +
      scale_fill_gradient("Frecuencia", low="green", high="red") +
      theme(
        axis.text.x = element_text(vjust = 1, hjust = 1), 
        legend.position = "bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        legend.key.width=unit(1.5,"cm")
      )
    plot(p)
  }
}

box_plots <- function(df, title=NULL) {
  df %>% 
    pivot_longer(is.numeric, names_to = "Variables", values_to = "Frecuencia") %>%
    ggplot(aes(x = Variables, y = Frecuencia, fill = Variables)) +
    scale_y_continuous(limits = c(0, 180)) +
    geom_boxplot(width=0.7) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1), legend.position = "bottom") +
    labs(title = title)
}


bar_plots <- function(
  df, 
  columns    = c(),
  width      = 0.9, 
  count_size = 2
) {
  if(is_empty(columns)) {
    columns <- train_set %>% dplyr::select(!is.numeric) %>% colnames()
  }
  for(column in columns) {
    p <- ggplot(
      df %>% 
        group_by(!!sym(column)) %>% 
        tally() %>%
        mutate(Frecuencia = n), 
      aes(x=!!sym(column), y=Frecuencia, fill=!!sym(column))
    ) +
      geom_col(stat="identity", width=width, position = "dodge") +
      geom_text(aes(label=Frecuencia), vjust=2, color="white", size=count_size)
    print(p)
  }
}

segmented_bar_plot <- function(df, column, segmented_by) {
  df %>%
    group_by(!!sym(column), !!sym(segmented_by)) %>% 
    tally() %>%
    mutate(Frecuencia =  n / nrow(df)) %>%
    ggplot(aes(
      x=reorder(!!sym(column), Frecuencia),
      y=Frecuencia,
      fill=!!sym(segmented_by)
    )) +
    geom_bar(position="stack", stat="identity") +
    xlab(column)
}

mosaic_plot <- function(df, column, segmented_by, title=NULL) {
  ggplot(df) +
  geom_mosaic(aes(x=product(!!sym(column)), fill=!!sym(segmented_by))) +
  theme(axis.text.x = element_text(angle=90, hjust=.1, vjust=-0.3), legend.position = "bottom") +
  labs(title = title)
}

pairs_plot <- function(df, segment_column) {
  df %>% 
    ggpairs(
      aes(color = !!sym(segment_column)),
      progress = FALSE,
      upper = list(continuous = wrap("cor", size = 3, hjust=0.7)),
      legend = 3
    ) +
    theme(axis.text.x = element_text(angle=45, vjust=0.5), legend.position = "bottom")
}

corr_plot <- function(df, title=NULL) {
  num_df <- df %>% select_if(is.numeric)
  ggcorrplot(
    cor(num_df),
    hc.order = TRUE,
    type     = "upper",
    insig    = "blank",
    # Compute a matrix of correlation p-values
    p.mat    = cor_pmat(num_df),
    lab      = TRUE,
    colors   = c("#6D9EC1", "white", "#E46726")
  ) +
  labs(title = title)
}
