#' @title Cast specified columns to factor
#' @description Convert one or more columns of a data frame to factor type.
#'
#' @param df A data frame containing the columns to be converted.
#' @param columns A single character string specifying the column names
#'   separated by "$".
#'
#' @return The input data frame with the specified columns converted to factors.
#' @export

cast_to_factorDS <- function(df, columns) {

  cols <- strsplit(columns, "$", fixed = TRUE)[[1]]
  df[cols] <- lapply(df[cols], as.factor)

  return(df)
}
