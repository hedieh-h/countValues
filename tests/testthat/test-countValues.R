library(dplyr)

test_data <- data.frame(
  species_name = c("SERRULATA", "CERASIFERA", "PLATANOIDES", "RUBRUM", NA, "AMERICANA"),
  neighbourhood = c("Downtown", "West End", "Downtown", "Kitsilano", "West End", "Kitsilano"),
  diameter = c(10, 20, 30, 40, NA, 15)
)

test_that("Checks that the function properly removes the NA Values", {
  result_na_removed <- countValues(test_data, group_by = "species_name", na.rm = TRUE)
  expect_false(any(is.na(result_na_removed %>% pull(species_name))))
  expect_true(nrow(result_na_removed) < nrow(test_data))
})

test_that("Checks if the function accurately handles invalid column names", {
  expect_error(countValues(test_data, group_by = "invalid_column"),
               "One or more columns specified in `group_by` do not exist in the data.")
})

test_that("Checks that the function works accurately with an empty dataset", {
  empty_data <- test_data[0, ]  # Create an empty data frame with same columns as test_data
  result_empty <- countValues(empty_data, group_by = "species_name")
  expect_equal(nrow(result_empty), 0)
})
