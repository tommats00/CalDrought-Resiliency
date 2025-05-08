library(tidyverse)
library(testthat)

# Loading in data
five_year_outlook <- read_csv(here::here("data", "five_year_water_shortage_outlook.csv"))

# Write testthat function
test_that("All values in the benefit demand reduction column are between 0 and positive infinity",{
  
  column_values <- five_year_outlook$benefit_demand_reduction_acre_feet
  
  expect_true(
    is.numeric(column_values) &&
    all(is.finite(column_values)) &&
    all(column_values >= 0),
    info = paste("Some values are not numeric, NA, infinite or negative. Problematic values:",
                 paste(column_values[!(is.finite(column_values) & column_values >= 0)],
                       collapse = ", "))
  )
  
  
})

test_that("{col} has all numeric values between 0 and positive infinity", {
  
  columns <- c("benefit_demand_reduction_acre_feet", "benefit_supply_augmentation_acre_feet",
               "water_use_acre_feet", "water_supply_acre_feet")
  
  # Create a for loop to iterate for each column
  for (col in columns){
    
    column_values <- five_year_outlook[[col]]
    
    # Check the column is numeric
    expect_true(
      is.numeric(column_values),
      info = "Column is not numeric"
    )
    
    # Identify invalid values: NA, NaN, Inf, -Inf, or < 0
    bad_values <- !is.finite(column_values) | column_values < 0
    total_bad <- sum(bad_values, na.rm = TRUE)
    bad_rows <- which(bad_values)
    
    expect(
      total_bad == 0,
      failure_message = paste(
        total_bad, "invalid values found.", paste(bad_rows, collapse = ", "))
    )
  }
})





