#' @title Check validity of categorical variables
#' @description Verify that selected categorical variables in a data frame
#'   contain only predefined valid values.
#'
#' @param df A data frame containing the categorical variables to be checked.
#'
#' @return A character vector with the names of columns containing invalid values.
#'   Returns an empty character vector if all checked variables are valid.
#' @export

check_categoricalDS <- function(df) {

  valid_values <- list(
    Sex = c(0, 1, NA),
    RF_positivity = c(0, 1, NA),
    anti_CCP = c(0, 1, NA),
    GC = c(1, NA),
    GC_type = c(1, 2, 3, 4, NA),
    csDMARD1 = c(1, 2, 3, 4, 5, NA),
    csDMARD2 = c(1, 2, 3, 4, 5, NA),
    csDMARD3 = c(1, 2, 3, 4, 5, NA),
    bDMARD = c(1, 2, 3, 4, 5),
    tsDMARD = c(1, 2, 3, 4)
  )
  invalid_cols <- c()

  for (col_name in names(valid_values)) {
    if (col_name %in% names(df)) {
      has_invalid <- any(!df[[col_name]] %in% valid_values[[col_name]], na.rm = FALSE)
      if (has_invalid) {
        invalid_cols <- c(invalid_cols, col_name)
      }
    }
  }

  return(invalid_cols)
}
