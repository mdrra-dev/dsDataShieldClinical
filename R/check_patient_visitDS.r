#' @title Count patient-visit combinations
#' @description Create and count all patient-visit combinations in a data frame,
#'   grouping by patient identifier and visit time from diagnosis.
#'
#' @param df A data frame containing at least the columns \code{pat_ID}
#'   and \code{Visit_months_from_diagnosis}.
#'
#' @return A data frame with one row per patient-visit combination and
#'   a column \code{n} indicating the number of occurrences.
#' @export
#'

check_patient_visitDS <- function(df) {
  library(dplyr)

  pat_visit_counts <- df %>%
    group_by(pat_ID, Visit_months_from_diagnosis) %>%
    summarise(n = dplyr::n(), .groups = "drop")

  return(pat_visit_counts)

}
