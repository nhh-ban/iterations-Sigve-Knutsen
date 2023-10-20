# Function converts meta data into a dataframe format.
transform_metadata_to_df <- function(data){
  data[[1]] %>% 
    map(as_tibble) %>% 
    list_rbind %>% 
    mutate(latestData = map_chr(latestData, 1, .default = NA_character_)) %>% 
    mutate(latestData = as_datetime(latestData, tz = "UTC")) %>% 
    mutate(location = map(location, unlist)) %>% 
    mutate(
      lat = map_dbl(location, "latLon.lat"),
      lon = map_dbl(location, "latLon.lon")
    ) %>% 
    select(-location)
}

# Function to return a datetime in ISO8601 format after adding a specified offset in days.
to_iso8601 <- function(datetime, offset){
  # Convert the input datetime to a datetime object with UTC timezone.
  datetime <- as_datetime(datetime, tz = "UTC")
  
  # Ensure successful conversion of input datetime.
  if(is.na(datetime)) stop("Datetime conversion failed for input: ", datetime)
  
  # Apply the specified offset in days to the datetime.
  offset_datetime <- datetime + lubridate::days(offset) 
  
  # Check that the offset does not lead to an invalid datetime.
  if(is.na(offset_datetime)) stop("Offset results in an invalid datetime for input: ", datetime)
  
  # Convert the offset datetime to ISO8601 format with "Z" indicating UTC timezone.
  time <- paste0(anytime::iso8601(offset_datetime), "Z")
  return(time)
}

# Function to transform a query output into a tidy data frame format for plotting traffic volumes.
transform_volumes <- function(query_output) {
  query_output %>% 
    pluck("trafficData", "volume", "byHour", "edges") %>% 
    map_dfr(~{
      tibble(
        from = anytime::anytime(.x$node$from),
        to = anytime::anytime(.x$node$to),
        volume = .x$node$total$volumeNumbers$volume
      )
    })
}

