#' Count Occurrences of Values in Specified Columns
#'
#' `countValues` is a function that groups data by specified columns and counts the number of occurrences in each group.
#'
#' @param data A data frame containing the data to be grouped and counted.
#' @param group_by A character vector of column names to group by.
#' @param na.rm Logical indicating whether to remove rows with NA values in the grouping columns.
#' @importFrom dplyr %>%
#' @return A data frame with one row per unique combination of values in the specified columns, with a `count` column indicating the occurrences.
#' @examples
#' library(dplyr)
#' test_data <- data.frame(
#'   species_name = c("SERRULATA", "CERASIFERA", "PLATANOIDES", "RUBRUM", NA, "AMERICANA"),
#'   neighbourhood = c("Downtown", "West End", "Downtown", "Kitsilano", "West End", "Kitsilano"),
#'   diameter = c(10, 20, 30, 40, NA, 15)
#' )
#' countValues(test_data, group_by = c("species_name", "neighbourhood"), na.rm = TRUE)
#' @export
countValues <- function(data, group_by, na.rm = FALSE) {
  # Declare `.` to avoid CRAN check notes
  . <- NULL
  # Ensure group_by columns exist in the data
  if (!all(group_by %in% names(data))) {
    stop("One or more columns specified in `group_by` do not exist in the data.")
  }

  # Group and count occurrences
  counts <- data %>%
    {
      if (na.rm)
        dplyr::filter(., rowSums(is.na(dplyr::select(., dplyr::all_of(group_by)))) == 0)
      else
        .
    } %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_by))) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
    as.data.frame()

  # Return
  return(counts)
}
