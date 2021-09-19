target_column <- 'peso'

load_train_set <- function() load_df_from_csv('../dataset/encuesta_salud_train.csv')
load_test_set  <- function() load_df_from_csv('../dataset/encuesta_salud_test.csv')

feat <- function(df) features(df, target_column)
tar  <- function(df) target(df, target_column)