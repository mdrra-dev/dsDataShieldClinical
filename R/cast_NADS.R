#' @title Cast "NA" string into type NA
#' @description Convert "NA" strings of a data frame to proper NA values and report whether any replacement occurred.
#'
#' @param df A data frame containing values to be converted.
#'
#' @return A list containing:
#' \itemize{
#'   \item data: the input data frame with "NA" strings converted to NA
#'   \item flag: logical indicating whether any replacement was performed
#' }
#' @export

cast_NADS <- function(df){

  changed <- FALSE
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      if (any(x == "NA", na.rm = TRUE)) {
        changed <<- TRUE
      }
      x[x == "NA"] <- NA
    }
    x
  })

  return(list(
    data = df,
    flag = changed
  ))
}

#' @title Check if NA string replacement occurred
#' @description Returns whether the cast_NA operation modified the dataset.
#'
#' @param x A DataSHIELD object returned by cast_NADS.
#'
#' @return Logical value indicating if any "NA" strings were replaced.
#' @export
get_cast_NA_changedDS <- function(x) {
  x$flag
}
