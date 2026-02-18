#' @title Check that required variables are present in a data frame
#' @description Verify that a data frame contains the expected variables.
#'
#' @param df A data frame to be checked.
#' @param variables_string A single character string specifying the expected
#'   variable names separated by "$".
#'
#' @return A named list with two elements:
#'   \itemize{
#'     \item \code{missing}: variables specified but not found in the data frame.
#'     \item \code{extra}: variables found in the data frame but not specified.
#'   }
#' @export
#'

check_variablesDS <- function(df, variables_string){
  variables <- strsplit(variables_string, "$", fixed = TRUE)[[1]]

  missing_df_cols <- setdiff(variables, colnames(df)) # variables missing in the df
  extra_df_cols <- setdiff(colnames(df), variables)   # variables extra present in the df

  return(list(missing = missing_df_cols, extra = extra_df_cols))
}
