# ==============================================================================
# ENHANCED PREDICTION MODEL - Functions Only (API Endpoints Removed)
# ==============================================================================

library(dplyr)

# Load all datasets
load_enhanced_datasets <- function() {
  datasets <- list()
  if (file.exists("on-street-parking-bays.csv")) {
    datasets$parking_bays <- read.csv("on-street-parking-bays.csv")
  }
  if (file.exists("sign-plates-located-in-each-parking-zone.csv")) {
    datasets$sign_plates <- read.csv("sign-plates-located-in-each-parking-zone.csv")
  }
  if (file.exists("parking-zones-linked-to-street-segments.csv")) {
    datasets$parking_zones <- read.csv("parking-zones-linked-to-street-segments.csv")
  }
  return(datasets)
}

# Create comprehensive street-zone mapping
create_street_zone_mapping <- function(datasets) {
  # Map streets to their zones and restrictions
  street_zones <- datasets$parking_zones %>%
    group_by(OnStreet) %>%
    summarise(
      zone_count = n_distinct(ParkingZone),
      zones = list(unique(ParkingZone)),
      .groups = 'drop'
    )
  
  # Add restriction data for each zone
  zone_restrictions <- datasets$sign_plates %>%
    group_by(ParkingZone) %>%
    summarise(
      weekday_start = ifelse(any(grepl("Mon|Tue|Wed|Thu|Fri", Restriction_Days)), 
                             first(Time_Restrictions_Start[grepl("Mon|Tue|Wed|Thu|Fri", Restriction_Days)]), NA),
      weekday_end = ifelse(any(grepl("Mon|Tue|Wed|Thu|Fri", Restriction_Days)), 
                           first(Time_Restrictions_Finish[grepl("Mon|Tue|Wed|Thu|Fri", Restriction_Days)]), NA),
      weekend_restrictions = any(grepl("Sat|Sun", Restriction_Days)),
      parking_limit = first(Restriction_Display),
      .groups = 'drop'
    )
  
  # Combine street zones with restrictions
  enhanced_mapping <- street_zones %>%
    rowwise() %>%
    mutate(
      restriction_info = list({
        zone_list <- unlist(zones)
        zone_restrictions[zone_restrictions$ParkingZone %in% zone_list, ]
      })
    )
  
  return(enhanced_mapping)
}

# Enhanced prediction function using real data
predict_availability_with_restrictions <- function(street, hour, day_of_week, datasets = NULL) {
  
  if (is.null(datasets)) {
    datasets <- load_enhanced_datasets()
  }
  
  # Get street mapping
  street_mapping <- create_street_zone_mapping(datasets)
  
  # Find street data
  street_data <- street_mapping[street_mapping$OnStreet == street, ]
  
  if (nrow(street_data) == 0) {
    # Fallback to basic prediction for unknown streets
    return(predict_availability_basic(street, hour, day_of_week))
  }
  
  # Get base prediction
  base_availability <- get_base_availability(hour, day_of_week)
  
  # Apply restriction-based adjustments
  restriction_factor <- calculate_restriction_factor(street_data, hour, day_of_week)
  
  # Apply zone complexity factor
  complexity_factor <- calculate_complexity_factor(street_data$zone_count)
  
  # Calculate final prediction
  final_availability <- base_availability * restriction_factor * complexity_factor
  
  # Keep realistic bounds
  final_availability <- max(5, min(95, final_availability))
  
  # Determine confidence based on data quality
  confidence <- determine_confidence(street, street_data$zone_count)
  
  return(list(
    availability = round(final_availability, 1),
    confidence = confidence,
    zones_analyzed = street_data$zone_count,
    restriction_factor = restriction_factor
  ))
}

# Calculate restriction-based adjustments
calculate_restriction_factor <- function(street_data, hour, day_of_week) {
  
  if (length(street_data$restriction_info[[1]]) == 0) {
    return(1.0)  # No restriction data
  }
  
  restrictions <- street_data$restriction_info[[1]]
  
  # Check if current time falls within restricted hours
  is_weekday <- day_of_week <= 5
  current_time <- sprintf("%02d:00:00", hour)
  
  restriction_active <- FALSE
  turnover_factor <- 1.0
  
  for (i in 1:nrow(restrictions)) {
    if (is_weekday && !is.na(restrictions$weekday_start[i])) {
      start_time <- restrictions$weekday_start[i]
      end_time <- restrictions$weekday_end[i]
      
      if (!is.na(start_time) && !is.na(end_time)) {
        if (current_time >= start_time && current_time <= end_time) {
          restriction_active <- TRUE
          
          # Different parking limits affect turnover
          limit <- restrictions$parking_limit[i]
          if (!is.na(limit)) {
            if (grepl("1P", limit)) {
              turnover_factor <- 1.2  # 1-hour limit = higher turnover = more availability
            } else if (grepl("2P", limit)) {
              turnover_factor <- 1.1  # 2-hour limit = moderate turnover
            } else if (grepl("4P", limit)) {
              turnover_factor <- 0.9  # 4-hour limit = lower turnover
            }
          }
          break
        }
      }
    }
  }
  
  if (restriction_active) {
    # During restricted hours: paid parking reduces demand but increases turnover
    return(0.95 * turnover_factor)  # Slight reduction in demand but turnover helps
  } else {
    # Outside restricted hours: often free parking, higher demand
    return(1.15)  # 15% more availability when restrictions don't apply
  }
}

# Calculate zone complexity factor
calculate_complexity_factor <- function(zone_count) {
  # More zones = more complexity = varied availability
  if (zone_count >= 20) {
    return(0.95)  # Very complex streets (like Collins) have varied patterns
  } else if (zone_count >= 10) {
    return(0.98)  # Moderately complex
  } else {
    return(1.0)   # Simple streets
  }
}

# Base availability patterns
get_base_availability <- function(hour, day_of_week) {
  is_weekend <- day_of_week %in% c(6, 7)
  
  if (is_weekend) {
    # Weekend patterns
    if (hour %in% 6:8) return(70)
    if (hour %in% 9:11) return(55)
    if (hour %in% 12:17) return(35)
    if (hour %in% 18:21) return(40)
    if (hour %in% 22:23) return(65)
    return(85)  # Night hours
  } else {
    # Weekday patterns
    if (hour %in% 5:6) return(85)
    if (hour %in% 7:8) return(20)
    if (hour %in% 9:10) return(15)
    if (hour %in% 11:11) return(25)
    if (hour %in% 12:13) return(22)
    if (hour %in% 14:16) return(28)
    if (hour %in% 17:19) return(18)
    if (hour %in% 20:21) return(45)
    if (hour %in% 22:23) return(65)
    return(88)  # Night hours
  }
}

# Determine confidence level
determine_confidence <- function(street, zone_count) {
  if (grepl("Collins", street, ignore.case = TRUE)) {
    return("very_high")  # Best data for Collins Street
  } else if (zone_count >= 10) {
    return("high")       # Good zone data available
  } else if (zone_count >= 5) {
    return("medium")     # Some zone data
  } else {
    return("low")        # Limited data
  }
}

# Fallback for unknown streets
predict_availability_basic <- function(street, hour, day_of_week) {
  base <- get_base_availability(hour, day_of_week)
  
  # Simple street adjustments
  if (grepl("Collins|Bourke", street, ignore.case = TRUE)) {
    base <- base * 0.9
  } else if (grepl("Queen|Elizabeth", street, ignore.case = TRUE)) {
    base <- base * 0.95
  }
  
  return(list(
    availability = round(max(5, min(95, base)), 1),
    confidence = "low",
    zones_analyzed = 0,
    restriction_factor = 1.0
  ))
}

# Helper function for restriction-aware predictions
predict_with_restrictions <- function(street, hour, day, datasets) {
  
  # Find zones for this street
  street_zones <- datasets$parking_zones[
    grepl(street, datasets$parking_zones$OnStreet, ignore.case = TRUE), ]
  
  if (nrow(street_zones) == 0) {
    # Fallback for unknown streets
    return(list(
      availability = get_time_based_availability(hour, day),
      confidence = "low",
      zones_analyzed = 0,
      restriction_factor = 1.0
    ))
  }
  
  zone_numbers <- unique(street_zones$ParkingZone)
  
  # Get restrictions for these zones
  restrictions <- datasets$sign_plates[
    datasets$sign_plates$ParkingZone %in% zone_numbers, ]
  
  # Calculate base availability
  base_availability <- get_time_based_availability(hour, day)
  
  # Apply restriction adjustments
  restriction_factor <- 1.0
  
  if (nrow(restrictions) > 0) {
    # Check if restrictions are active
    is_weekday <- day <= 5
    current_time <- sprintf("%02d:00:00", hour)
    
    restrictions_active <- FALSE
    turnover_bonus <- 1.0
    
    for (i in 1:nrow(restrictions)) {
      if (is_weekday && grepl("Mon|Tue|Wed|Thu|Fri", restrictions$Restriction_Days[i])) {
        start_time <- restrictions$Time_Restrictions_Start[i]
        end_time <- restrictions$Time_Restrictions_Finish[i]
        
        if (!is.na(start_time) && !is.na(end_time)) {
          if (current_time >= start_time && current_time <= end_time) {
            restrictions_active <- TRUE
            
            # Parking time limits affect turnover
            display <- restrictions$Restriction_Display[i]
            if (!is.na(display)) {
              if (grepl("1P", display)) {
                turnover_bonus <- 1.15  # Higher turnover
              } else if (grepl("2P", display)) {
                turnover_bonus <- 1.05  # Moderate turnover
              }
            }
            break
          }
        }
      }
    }
    
    if (restrictions_active) {
      restriction_factor <- 0.92 * turnover_bonus  # Paid parking effect + turnover
    } else {
      restriction_factor <- 1.1  # Free parking = higher demand
    }
  }
  
  # Calculate final availability
  final_availability <- base_availability * restriction_factor
  
  # Street-specific adjustments
  if (grepl("Collins", street, ignore.case = TRUE)) {
    final_availability <- final_availability * 0.9  # Premium location
  } else if (grepl("Bourke", street, ignore.case = TRUE)) {
    final_availability <- final_availability * 0.95
  }
  
  final_availability <- max(5, min(95, final_availability))
  
  return(list(
    availability = round(final_availability, 1),
    confidence = if(length(zone_numbers) >= 10) "high" else "medium",
    zones_analyzed = length(zone_numbers),
    restriction_factor = restriction_factor
  ))
}

# Time-based availability helper
get_time_based_availability <- function(hour, day) {
  is_weekend <- day %in% c(6, 7)
  
  if (is_weekend) {
    if (hour %in% 6:8) return(65)
    if (hour %in% 9:11) return(50)
    if (hour %in% 12:17) return(32)
    if (hour %in% 18:21) return(38)
    return(80)
  } else {
    if (hour %in% 7:8) return(18)
    if (hour %in% 9:10) return(12)
    if (hour %in% 11:13) return(20)
    if (hour %in% 14:16) return(25)
    if (hour %in% 17:19) return(15)
    if (hour %in% 20:22) return(42)
    return(85)
  }
}

# ==============================================================================
# ESSENTIAL FUNCTION FOR API COMPATIBILITY
# ==============================================================================

# Simple predict_availability function that your API endpoints need
predict_availability <- function(street, hour, day) {
  tryCatch({
    # Try to use the sophisticated model
    datasets <- load_enhanced_datasets()
    
    if (length(datasets) >= 2 && !is.null(datasets$sign_plates) && !is.null(datasets$parking_zones)) {
      # Use the most sophisticated prediction
      result <- predict_availability_with_restrictions(street, hour, day, datasets)
      return(result$availability)
    } else {
      # Fallback to time-based prediction
      return(get_time_based_availability(hour, day))
    }
  }, error = function(e) {
    # Ultimate fallback
    base <- get_base_availability(hour, day)
    if (grepl("Collins", street, ignore.case = TRUE)) {
      base <- base * 0.9
    }
    return(round(max(5, min(95, base))))
  })
}

# ==============================================================================
# TEST FUNCTIONS
# ==============================================================================

# Test the enhanced model
test_enhanced_model <- function() {
  cat("=== TESTING ENHANCED PREDICTION MODEL ===\n")
  
  tryCatch({
    datasets <- load_enhanced_datasets()
    
    if (length(datasets) >= 3) {
      # Test Collins Street (should be very sophisticated)
      collins_pred <- predict_availability_with_restrictions("Collins Street", 9, 1, datasets)
      cat("Collins Street Monday 9 AM:\n")
      cat("  Availability:", collins_pred$availability, "%\n")
      cat("  Confidence:", collins_pred$confidence, "\n")
      cat("  Zones analyzed:", collins_pred$zones_analyzed, "\n")
      cat("  Restriction factor:", round(collins_pred$restriction_factor, 3), "\n\n")
      
      # Test during vs outside restriction hours
      collins_evening <- predict_availability_with_restrictions("Collins Street", 20, 1, datasets)
      cat("Collins Street Monday 8 PM (outside restrictions):\n")
      cat("  Availability:", collins_evening$availability, "%\n")
      cat("  Restriction factor:", round(collins_evening$restriction_factor, 3), "\n\n")
      
      # Test weekend
      collins_weekend <- predict_availability_with_restrictions("Collins Street", 14, 6, datasets)
      cat("Collins Street Saturday 2 PM:\n")
      cat("  Availability:", collins_weekend$availability, "%\n")
      cat("  Restriction factor:", round(collins_weekend$restriction_factor, 3), "\n")
    } else {
      cat("Some CSV files missing, using fallback prediction\n")
      simple_test <- predict_availability("Collins Street", 9, 1)
      cat("Collins Street Monday 9 AM (fallback):", simple_test, "%\n")
    }
  }, error = function(e) {
    cat("Test error, using basic prediction\n")
    simple_test <- predict_availability("Collins Street", 9, 1)
    cat("Collins Street Monday 9 AM (basic):", simple_test, "%\n")
  })
  
  cat("\n=== ENHANCED MODEL TEST COMPLETE ===\n")
}

# Run test when sourced
test_enhanced_model()