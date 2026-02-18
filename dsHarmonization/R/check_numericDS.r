#' @title Check validity of numeric variables
#' @description Verify that selected numeric variables in a data frame
#'   fall within predefined ranges and satisfy integer constraints where required.
#'   Also checks the format of the patient identifier (pat_ID), if present.
#'
#' @param df A data frame containing the numeric variables to be checked.
#'
#' @return A character vector with the names of columns containing invalid values.
#'   Returns an empty character vector if all checked variables are valid.
#' @export

check_numericDS <- function(df) {

  check_range <- function(x, min = NULL, max = NULL, integer = FALSE) {
    invalid <- rep(FALSE, length(x))

    if (!is.null(min))
      invalid <- invalid | (!is.na(x) & x < min)
    if (!is.null(max))
      invalid <- invalid | (!is.na(x) & x > max)
    if (integer)
      invalid <- invalid | (!is.na(x) & x != round(x))

    any(invalid)
  }

  range_checks <- list(
    Visit_months_from_diagnosis = list(min = 0),
    Age_diagnosis = list(min = 18, max = 110, integer = TRUE),
    DAS28 = list(min = 0),
    Pat_global = list(min = 0, max = 100),
    Pain = list(min = 0, max = 100),
    Ph_global = list(min = 0, max = 100),
    CRP = list(min = 0),
    ESR = list(min = 0, integer = TRUE),
    SJC28 = list(min = 0, max = 28, integer = TRUE), # devo tenere integer ?
    TJC28 = list(min = 0, max = 28, integer = TRUE),
    conc_MTX_dose = list(min = 0),
    N_prev_csDMARD = list(min = 0, integer = TRUE),
    N_prev_bDMARD = list(min = 0, integer = TRUE),
    N_prev_tsDMARD = list(min = 0, integer = TRUE),
    GC_dose = list(min = 0),
    eq5d = list(min = 0),
    HAQ = list(min = 0, max = 3),
    Year_diagnosis = list(min = 2010, integer = TRUE),
    month_diagnosis = list(min = 1, max = 12, integer = TRUE),
    Symptom_duration = list(min = 0)
  )

  invalid_cols <- c()

  for (col_name in names(range_checks)) {
    if (col_name %in% names(df)) {
      params <- range_checks[[col_name]]
      has_invalid <- do.call(
        check_range,
        c(list(x = df[[col_name]]), params)
      )
      if (has_invalid) {
        invalid_cols <- c(invalid_cols, col_name)
      }
    }
  }

  if ("pat_ID" %in% names(df)) {
    if (any(!grepl("^SE[0-9]+$", df$pat_ID))) {
      invalid_cols <- c(invalid_cols, "pat_ID")
    }
  }

  return(invalid_cols)
}

