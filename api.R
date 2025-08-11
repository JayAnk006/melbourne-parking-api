library(plumber)
library(httr)
library(dplyr)
library(jsonlite)

# ADD THIS LINE:
source("prediction_model.R")

# Global cache variables
sensor_cache <- NULL
cache_timestamp <- NULL
cache_duration_minutes <- 3

#* @apiTitle Melbourne Parking Backend API - Simple & Reliable
#* @apiDescription Basic parking search that works
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
  
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}

#* Test endpoint
#* @get /test
function() {
  list(
    message = as.character("Melbourne Parking API - Simple Version!"),
    timestamp = as.character(Sys.time()),
    status = as.character("success"),
    version = as.character("3.0-simple")
  )
}

#* Main street search endpoint with comprehensive summary
#* @param street Street name to search
#* @get /api/parking/street
function(street = "Collins Street") {
  tryCatch({
    
    # Load and validate data - CHECK ALL DATASETS
    datasets_available <- list()
    
    if (file.exists("on-street-parking-bays.csv")) {
      datasets_available$parking_bays <- read.csv("on-street-parking-bays.csv")
    }
    
    if (file.exists("parking-zones-linked-to-street-segments.csv")) {
      datasets_available$parking_zones <- read.csv("parking-zones-linked-to-street-segments.csv")
    }
    
    # Try multiple data sources to find the street
    street_found <- FALSE
    street_spots <- data.frame()
    data_source <- "none"
    
    # Strategy 1: Check parking bays CSV (original method)
    if (!is.null(datasets_available$parking_bays)) {
      sensor_bays <- datasets_available$parking_bays[!is.na(datasets_available$parking_bays$KerbsideID) & datasets_available$parking_bays$KerbsideID != "", ]
      street_spots_bays <- sensor_bays[grepl(street, sensor_bays$RoadSegmentDescription, ignore.case = TRUE), ]
      
      if (nrow(street_spots_bays) > 0) {
        street_spots <- street_spots_bays
        street_found <- TRUE
        data_source <- "parking_bays"
      }
    }
    
    # Strategy 2: Check parking zones CSV (like prediction model)
    if (!street_found && !is.null(datasets_available$parking_zones)) {
      street_spots_zones <- datasets_available$parking_zones[grepl(street, datasets_available$parking_zones$OnStreet, ignore.case = TRUE), ]
      
      if (nrow(street_spots_zones) > 0) {
        # Create a compatible structure for zones data
        street_spots <- data.frame(
          KerbsideID = paste0("zone_", street_spots_zones$ParkingZone),
          RoadSegmentDescription = street_spots_zones$OnStreet,
          ParkingZone = street_spots_zones$ParkingZone,
          stringsAsFactors = FALSE
        )
        street_found <- TRUE
        data_source <- "parking_zones"
      }
    }
    
    # Strategy 3: Provide prediction-based response if street exists in prediction model
    if (!street_found) {
      # Check if prediction model knows about this street
      tryCatch({
        test_prediction <- predict_availability(street, 14, 1)
        if (!is.null(test_prediction) && test_prediction > 0) {
          return(list(
            status = as.character("partial_success"),
            street_searched = as.character(street),
            message = as.character("Street recognized but no real-time sensor data available"),
            prediction_available = as.logical(TRUE),
            suggested_action = as.character("Use /api/predict/now for availability prediction"),
            current_prediction = list(
              availability_percentage = as.numeric(test_prediction),
              note = as.character("Based on historical patterns and restriction data")
            )
          ))
        }
      }, error = function(e) {
        # Continue to error response
      })
      
      # Enhanced error with better suggestions
      all_streets <- c()
      if (!is.null(datasets_available$parking_bays)) {
        bays_streets <- unique(datasets_available$parking_bays$RoadSegmentDescription)
        all_streets <- c(all_streets, bays_streets)
      }
      if (!is.null(datasets_available$parking_zones)) {
        zones_streets <- unique(datasets_available$parking_zones$OnStreet)
        all_streets <- c(all_streets, zones_streets)
      }
      
      # Find similar street names
      similar_streets <- all_streets[grepl(paste(strsplit(street, " ")[[1]], collapse = "|"), all_streets, ignore.case = TRUE)]
      
      return(list(
        status = as.character("error"), 
        message = as.character(paste("No real-time sensor data found for", street)),
        data_sources_checked = c("parking_bays", "parking_zones"),
        suggestions = if(length(similar_streets) > 0) head(similar_streets, 5) else c("Spencer Street", "William Street", "Queen Street", "Collins Street", "Elizabeth Street"),
        note = as.character("Try /api/predict/now for prediction-based availability")
      ))
    }
    
    # Get cached sensor data
    all_sensor_data <- get_sensor_data()
    if (is.null(all_sensor_data)) {
      return(list(status = as.character("error"), message = as.character("Sensor data unavailable")))
    }
    
    # Filter for target street - IMPROVED MATCHING
    if (data_source == "parking_bays") {
      target_kerbsides <- street_spots$KerbsideID
      street_sensors <- all_sensor_data[all_sensor_data$kerbsideid %in% target_kerbsides, ]
    } else {
      # For zones data, try to match by zone numbers or do broader search
      target_zones <- street_spots$ParkingZone
      street_sensors <- all_sensor_data[all_sensor_data$zone_number %in% target_zones, ]
    }
    
    # Calculate statistics
    coverage_pct <- as.numeric(round((nrow(street_sensors) / nrow(street_spots)) * 100, 1))
    
    if (nrow(street_sensors) > 0) {
      available_count <- as.numeric(sum(street_sensors$status_description == "Unoccupied", na.rm = TRUE))
      occupied_count <- as.numeric(sum(street_sensors$status_description == "Occupied", na.rm = TRUE))
      present_count <- as.numeric(sum(street_sensors$status_description == "Present", na.rm = TRUE))
      
      # Build comprehensive response with summary
      response <- list(
        status = as.character("success"),
        street_searched = as.character(street),
        summary = list(
          spots_available_for_parking = as.numeric(available_count),
          spots_unavailable_occupied = as.numeric(occupied_count + present_count),
          availability_percentage = as.numeric(round((available_count / nrow(street_sensors)) * 100, 1))
        ),
        data_quality = list(
          total_spots_on_street = as.numeric(nrow(street_spots)),
          sensors_reporting = as.numeric(nrow(street_sensors)),
          coverage_percentage = as.numeric(coverage_pct),
          cache_size = as.numeric(nrow(all_sensor_data)),
          confidence_level = as.character(if(coverage_pct > 10) "High" else if(coverage_pct > 2) "Medium" else "Low"),
          data_source = as.character(data_source)
        )
      )
      
      # Add available spots if any
      if (available_count > 0) {
        available_spots <- street_sensors[street_sensors$status_description == "Unoccupied", ]
        response$available_spots <- available_spots[, c("kerbsideid", "zone_number", "location", "lastupdated")]
      }
      
      # Add prediction for comparison
      tryCatch({
        current_prediction <- predict_availability(street, as.numeric(format(Sys.time(), "%H")), as.numeric(format(Sys.time(), "%u")))
        response$prediction_comparison = list(
          current_real_time = as.numeric(round((available_count / nrow(street_sensors)) * 100, 1)),
          current_prediction = as.numeric(current_prediction),
          note = as.character("Real-time vs predicted availability")
        )
      }, error = function(e) {
        # Skip prediction if it fails
      })
      
      return(response)
    } else {
      # No real-time data but street exists - provide prediction
      tryCatch({
        current_prediction <- predict_availability(street, as.numeric(format(Sys.time(), "%H")), as.numeric(format(Sys.time(), "%u")))
        
        return(list(
          status = as.character("partial_success"),
          street_searched = as.character(street),
          message = as.character("Street found but no real-time sensors available in current data sample"),
          total_spots_mapped = as.numeric(nrow(street_spots)),
          data_source = as.character(data_source),
          alternative_prediction = list(
            predicted_availability = as.numeric(current_prediction),
            note = as.character("Based on historical patterns and restrictions"),
            suggestion = as.character("Use /api/predict/now for detailed prediction")
          )
        ))
      }, error = function(e) {
        return(list(
          status = as.character("partial_success"),
          street_searched = as.character(street),
          message = as.character("Street mapped but no real-time sensors available"),
          total_spots_mapped = as.numeric(nrow(street_spots)),
          data_source = as.character(data_source)
        ))
      })
    }
    
  }, error = function(e) {
    list(status = as.character("error"), message = as.character(paste("Enhanced search error:", e$message)))
  })
}

#* Simple data fetch - no complex strategies
get_sensor_data <- function() {
  current_time <- Sys.time()
  
  # Check cache
  if (!is.null(sensor_cache) && !is.null(cache_timestamp)) {
    cache_age <- as.numeric(difftime(current_time, cache_timestamp, units = "mins"))
    if (cache_age < cache_duration_minutes) {
      cat("Using cached data\n")
      return(sensor_cache)
    }
  }
  
  # Fetch fresh data
  cat("Fetching fresh data...\n")
  url <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/on-street-parking-bay-sensors/records"
  
  tryCatch({
    response <- GET(url, query = list(limit = 100), timeout(20))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      if (length(data$results) > 0) {
        sensor_cache <<- data$results
        cache_timestamp <<- current_time
        cat("Fresh data cached:", nrow(data$results), "records\n")
        return(data$results)
      }
    }
    
    cat("API call failed, using old cache if available\n")
    return(sensor_cache)
    
  }, error = function(e) {
    cat("Error fetching data:", e$message, "\n")
    return(sensor_cache)
  })
}



#* Predict parking availability for specific time and location
#* @param street Street name (e.g., "Collins Street")
#* @param hour Hour in 24h format (0-23)
#* @param day Day of week (1=Monday, 7=Sunday)
#* @get /api/predict/availability
function(street = "Collins Street", hour = 14, day = 1) {
  tryCatch({
    # Validate inputs
    hour <- as.numeric(hour)
    day <- as.numeric(day)
    
    if (hour < 0 || hour > 23) {
      return(list(status = as.character("error"), message = as.character("Hour must be between 0-23")))
    }
    
    if (day < 1 || day > 7) {
      return(list(status = as.character("error"), message = as.character("Day must be between 1-7 (1=Monday)")))
    }
    
    # Get prediction
    predicted_availability <- predict_availability(street, hour, day)
    
    # Create day name
    day_names <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    day_name <- as.character(day_names[day])
    time_formatted <- as.character(sprintf("%02d:00", hour))
    
    # Create recommendation
    recommendation <- as.character(if (predicted_availability >= 60) {
      "Excellent time to find parking"
    } else if (predicted_availability >= 40) {
      "Good chance of finding parking"
    } else if (predicted_availability >= 20) {
      "Challenging but possible to find parking"
    } else {
      "Very difficult to find parking - consider alternative times"
    })
    
    list(
      status = as.character("success"),
      prediction = list(
        street = as.character(street),
        day = day_name,
        time = time_formatted,
        predicted_availability_percentage = as.numeric(predicted_availability),
        recommendation = recommendation
      )
    )
  }, error = function(e) {
    list(status = as.character("error"), message = as.character(paste("Prediction error:", e$message)))
  })
}

#* Get quick prediction summary for current time
#* @param street Street name (e.g., "Collins Street")
#* @get /api/predict/now
function(street = "Collins Street") {
  tryCatch({
    # Get current time
    current_time <- Sys.time()
    current_hour <- as.numeric(format(current_time, "%H"))
    current_day <- as.numeric(format(current_time, "%u"))
    
    # Get prediction for now
    current_prediction <- predict_availability(street, current_hour, current_day)
    
    list(
      status = as.character("success"),
      current_prediction = list(
        street = as.character(street),
        current_time = as.character(format(current_time, "%Y-%m-%d %H:%M")),
        current_availability = as.numeric(current_prediction),
        recommendation = as.character(if (current_prediction >= 40) {
          "Good time to look for parking"
        } else {
          "Parking will be challenging right now"
        })
      )
    )
  }, error = function(e) {
    list(status = as.character("error"), message = as.character(paste("Current prediction error:", e$message)))
  })
}

#* Get cache status
#* @get /api/data/cache-status  
function() {
  list(
    status = "success",
    cache_info = list(
      records_in_cache = if(!is.null(sensor_cache)) nrow(sensor_cache) else 0,
      cache_age_minutes = if(!is.null(cache_timestamp)) {
        round(as.numeric(difftime(Sys.time(), cache_timestamp, units = "mins")), 1)
      } else { "no cache" }
    )
  )
}

#* Force refresh cache
#* @get /api/data/refresh
function() {
  sensor_cache <<- NULL
  cache_timestamp <<- NULL
  
  new_data <- get_sensor_data()
  
  if (!is.null(new_data)) {
    list(
      status = "success",
      message = "Cache refreshed",
      records = nrow(new_data)
    )
  } else {
    list(
      status = "error", 
      message = "Failed to refresh"
    )
  }
}

#* Basic data pull for monitoring
#* @get /api/data/pull
function() {
  tryCatch({
    url <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/on-street-parking-bay-sensors/records"
    response <- GET(url, query = list(limit = 5), timeout(15))
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      list(
        status = "success",
        message = "API connection working",
        sample_records = length(data$results)
      )
    } else {
      list(
        status = "error", 
        message = paste("API returned status:", status_code(response))
      )
    }
  }, error = function(e) {
    list(
      status = "error", 
      message = paste("Connection failed:", e$message)
    )
  })
}