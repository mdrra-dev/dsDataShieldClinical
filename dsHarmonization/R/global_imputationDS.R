#' @import dplyr
#' @import mice
#' @title Global imputation of missing data using MICE
#' @description Perform a global imputation of missing values in a data frame using
#' the Multiple Imputation by Chained Equations (MICE) algorithm. The function
#' automatically identifies continuous and categorical variables, assigns appropriate
#' imputation methods (pmm, logreg, polyreg), and returns a single imputed data frame.
#'
#' @param df A data frame containing the variables to be imputed. Both numeric and
#' categorical variables are supported.
#' @param id_col A string specifying the name of the identifier column.
#' This column is excluded from the imputation process and used to aggregate the
#' results across multiple imputations.
#' @param m Number of imputations to generate (passed to \code{mice}).
#' @param maxit Maximum number of MICE iterations per imputation chain.
#'
#' @return A data frame where missing values have been imputed using the MICE
#' @export

global_imputationDS <- function(df, id_col, m, maxit) {
  suppressWarnings(suppressPackageStartupMessages({
    library(dplyr)
    library(mice)
  }))

  if (id_col %in% names(df)) {
    id_values <- df[[id_col]]
    df_work <- df[, names(df) != id_col]
  } else {
    df_work <- df
    id_values <- NULL
  }

  # Identify continuous and categorical variables
  continuous_vars <- names(df_work)[sapply(df_work, function(x) {is.numeric(x) && length(unique(na.omit(x))) > 20})]
  categorical_vars <- setdiff(names(df_work), continuous_vars)

  method_vector <- rep("", ncol(df_work))
  names(method_vector) <- names(df_work)

  # Assign "pmm" method to continuous variables
  method_vector[continuous_vars] <- "pmm"

  # Assign "logreg" to binary variables / "polyreg" to variables with multiple categories
  for (var in categorical_vars) {
    if (any(is.na(df_work[[var]]))) {
      n_unique <- length(unique(na.omit(df_work[[var]])))
      method_vector[var] <- ifelse(n_unique == 2, "logreg", "polyreg")
    }
  }

  for (var in names(method_vector)) {
    if (method_vector[var] %in% c("logreg", "polyreg")) {
      df_work[[var]] <- as.factor(df_work[[var]])
    }
  }

  # Imputation step
  imp <- mice(df_work,
              m = m,
              method = method_vector,
              predictorMatrix = mice::quickpred(df_work),
              maxit = maxit,
              printFlag = FALSE,
              seed = 123)

  # Access and index each of the 5 imputed data sets
  imp_list_indexed <- lapply(1:5, function(i) {
    df_imp <- complete(imp, i)
    if (!is.null(id_values)) {
      df_imp[[id_col]] <- id_values}
    df_imp$imputation <- i
    df_imp
  })

  df_imp_all <- bind_rows(imp_list_indexed)

  df_final <- df_imp_all %>%
    group_by(.data[[id_col]]) %>%
    summarise(
      across(all_of(continuous_vars), ~ mean(.x, na.rm = TRUE)),
      across(all_of(categorical_vars), ~ names(sort(table(.x), decreasing = TRUE))[1]),
      .groups = "drop")

  return(as.data.frame(df_final))
}
