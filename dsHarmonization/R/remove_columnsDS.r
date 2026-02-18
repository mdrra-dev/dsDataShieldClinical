#' @title Remove specified columns from a data frame
#' @description Remove one or more columns from a data frame based on their names.
#'
#' @param df A data frame from which columns will be removed.
#' @param col_names A character vector specifying the names of the columns to be removed.
#'
#' @return The input data frame with the specified columns removed.
#' @export
#'

remove_columnsDS <- function(df, col_names) {

  columns <- strsplit(col_names, "$", fixed = TRUE)[[1]]

  df_clean <- df[ , !(names(df) %in% columns)]

  return(df_clean)
}
