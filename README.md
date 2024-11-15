
<!-- README.md is generated from README.Rmd. Please edit that file -->

# countValues

<!-- badges: start -->
<!-- badges: end -->

The goal of countValues is to provide an easy way to count occurrences
of values in specified columns of a dataset. By grouping data by one or
more columns, the function generates a summary count of unique value
combinations within those columns.

## Installation

You can install the development version of countValues from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("hedieh-h/countValues")
```

## Example

This is a basic example that demonstrates how to count occurrences
within a dataset using the countValues pacakge:

``` r
library(countValues)

# Sample data
test_data <- data.frame(
  species_name = c("SERRULATA", "CERASIFERA", "PLATANOIDES", "RUBRUM", NA, "AMERICANA"),
  neighbourhood = c("Downtown", "West End", "Downtown", "Kitsilano", "West End", "Kitsilano"),
  diameter = c(10, 20, 30, 40, NA, 15)
)

# Count occurrences by species and neighbourhood, removing NA values
countValues(test_data, group_by = c("species_name", "neighbourhood"), na.rm = TRUE)
#>   species_name neighbourhood count
#> 1    AMERICANA     Kitsilano     1
#> 2   CERASIFERA      West End     1
#> 3  PLATANOIDES      Downtown     1
#> 4       RUBRUM     Kitsilano     1
#> 5    SERRULATA      Downtown     1
```
