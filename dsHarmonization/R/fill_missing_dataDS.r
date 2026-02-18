#' @title Fill missing values by patient and visit order
#' @description For each patient, sort visits by time from diagnosis and
#'   propagate non-missing values of a specified column.
#'   Missing values are replaced with the most recent non-missing value
#'   observed in chronological order within the same patient.
#'
#' @param df A data frame containing at least the patient identifier column,
#'   the visit time column, and the variable to be filled.
#' @param pat_id_col A character string specifying the name of the
#'   patient identifier column.
#' @param visit_col A character string specifying the name of the
#'   visit time column.
#' @param value_col A character string specifying the name of the
#'   column whose missing values should be filled.
#'
#' @return A data frame where missing values in \code{value_col}
#'   are filled forward within each patient group according to
#'   increasing visit time.
#' @export
#'

fill_missing_dataDS <- function(df, pat_id_col, visit_col, value_col) {

  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(tidyr)
  }))

  print(pat_id_col)
  print(visit_col)

  filled_df <- df %>%
    group_by(across(all_of(pat_id_col))) %>%
    arrange(across(all_of(visit_col)), .by_group = TRUE) %>%
    fill(all_of(value_col), .direction = "down") %>% ungroup()

  return(filled_df)
}
