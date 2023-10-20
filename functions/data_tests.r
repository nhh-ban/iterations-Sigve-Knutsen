# This file contains tests to be applied to 
# the Vegvesen stations-data *after* being transformed
# to a data frame. 
# 
# All tests are packed in a function test_stations_metadata that apples
# all the aforementioned tests



# Function to check if a data frame's column names match the expected ones.
test_stations_metadata_colnames <-
  function(df) {
    
    # List of expected column names.
    expected_colnames <- c("id", "name", "latestData", "lat", "lon")
    
    # Check if all column names in the data frame match the expected names.
    if (all(colnames(df) == expected_colnames) == TRUE) {
      print("PASS: Data has the correct columns")
    } else{
      print("FAIL: Columns do not match the correct specification")
    }
  }




# Function to verify if the data frame's row count falls within a predefined range.
test_stations_metadata_nrows <- function(df) {
    
  # Set the minimum and maximum expected number of rows.
    min_expected_rows <- 5000
    max_expected_rows <- 10000
    
    # Check if row count of the data frame is within the expected range.
    if (nrow(df) > min_expected_rows & nrow(df) < max_expected_rows) {
      print("PASS: Data has a reasonable number of rows")
    } else if (nrow(df) <= min_expected_rows) {
      print("FAIL: Data has suspiciously few rows")
    } else {
      print("FAIL: Data has suspiciously many rows")
    }
  }



# Function to verify that each column in the data frame matches the expected type.
test_stations_metadata_coltypes <-
  function(df) {
    
    # Define the expected data types for each column.
    expected_coltypes <-
      c("character", "character", "double", "double", "double")
    
    # Check if the data type of each column matches the expected type.
    if (all(df %>%
            map_chr( ~ typeof(.)) == expected_coltypes) == TRUE) {
      print("PASS: All cols have the correct specifications")
    } else{
      print("FAIL: Columns do not have the correct specification")
    }
  }




# Function to verify if the data frame has an excessive number of missing values.
test_stations_metadata_nmissing <-
  function(df) {
    
    # Define the maximum acceptable number of missing values.
    max_miss_vals <- 200
    
    # Check the total number of missing values in the data frame.
    if (df %>% map_int( ~ sum(is.na((.)))) %>% sum(.) < max_miss_vals) {
      print("PASS: Amount of missing values is reasonable")
    } else {
      print("FAIL: Too many missing values in data set")
    }
  }



# Function to verify that the 'latestData' column in the data frame is in UTC timezone.
test_stations_metadata_latestdata_timezone <-
  function(df) {
    
    # Check the time zone attribute of the 'latestData' column.
    if (attr(df$latestData,"tzone")=="UTC") {
      print("PASS: latestData has UTC-time zone")
    } else {
      print("FAIL: latestData does not have expected UTC-time zone")
    }
  }



# Function to verify that a data frame adheres to various specified criteria.
test_stations_metadata <- 
  function(df){
    test_stations_metadata_colnames(df)               # Check if column names match the expected ones.
    test_stations_metadata_coltypes(df)               # Check if column data types are as expected.
    test_stations_metadata_nmissing(df)               # Verify that the number of missing values is within acceptable limits.
    test_stations_metadata_nrows(df)                  # Ensure that the number of rows is within the defined range.
    test_stations_metadata_latestdata_timezone(df)    # Confirm that the 'latestData' column is in the UTC timezone.

  }
