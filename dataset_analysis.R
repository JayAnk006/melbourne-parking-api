# ==============================================================================
# EPIC 2 DATASET ANALYSIS
# Explore all Melbourne parking datasets to enhance predictions
# ==============================================================================

library(dplyr)
library(jsonlite)

# ==============================================================================
# LOAD ALL DATASETS
# ==============================================================================

load_all_datasets <- function() {
  cat("=== LOADING ALL EPIC 2 DATASETS ===\n")
  
  datasets <- list()
  
  # 1. On-street Parking Bays (already working)
  if (file.exists("on-street-parking-bays.csv")) {
    datasets$parking_bays <- read.csv("on-street-parking-bays.csv")
    cat("✅ Loaded parking bays:", nrow(datasets$parking_bays), "records\n")
  } else {
    cat("❌ Missing: on-street-parking-bays.csv\n")
  }
  
  # 2. Sign Plates (CORRECTED FILENAME)
  if (file.exists("sign-plates-located-in-each-parking-zone.csv")) {
    datasets$sign_plates <- read.csv("sign-plates-located-in-each-parking-zone.csv")
    cat("✅ Loaded sign plates:", nrow(datasets$sign_plates), "records\n")
  } else {
    cat("❌ Missing: sign-plates-located-in-each-parking-zone.csv\n")
  }
  
  # 3. Parking Zones (CORRECTED FILENAME)
  if (file.exists("parking-zones-linked-to-street-segments.csv")) {
    datasets$parking_zones <- read.csv("parking-zones-linked-to-street-segments.csv")
    cat("✅ Loaded parking zones:", nrow(datasets$parking_zones), "records\n")
  } else {
    cat("❌ Missing: parking-zones-linked-to-street-segments.csv\n")
  }
  
  cat("\n=== LOADING COMPLETE ===\n\n")
  return(datasets)
}

# ==============================================================================
# ANALYZE EACH DATASET
# ==============================================================================

analyze_parking_bays <- function(data) {
  cat("=== PARKING BAYS ANALYSIS ===\n")
  
  if (is.null(data)) {
    cat("No parking bays data available\n")
    return()
  }
  
  cat("Columns:", paste(names(data), collapse = ", "), "\n")
  cat("Total records:", nrow(data), "\n")
  
  # Count sensors vs non-sensors
  has_sensor <- sum(!is.na(data$KerbsideID) & data$KerbsideID != "", na.rm = TRUE)
  cat("Records with sensors:", has_sensor, "\n")
  cat("Records without sensors:", nrow(data) - has_sensor, "\n")
  
  # Sample street descriptions
  cat("\nSample street descriptions:\n")
  sample_streets <- unique(data$RoadSegmentDescription)[1:5]
  for(i in 1:length(sample_streets)) {
    if(!is.na(sample_streets[i])) {
      cat("  -", sample_streets[i], "\n")
    }
  }
  
  cat("\n")
}

analyze_sign_plates <- function(data) {
  cat("=== SIGN PLATES ANALYSIS ===\n")
  
  if (is.null(data)) {
    cat("No sign plates data available\n")
    return()
  }
  
  cat("Columns:", paste(names(data), collapse = ", "), "\n")
  cat("Total records:", nrow(data), "\n")
  
  # Look for interesting columns
  interesting_cols <- names(data)[grepl("sign|plate|text|description|restriction|time|hour", 
                                        names(data), ignore.case = TRUE)]
  cat("Potentially interesting columns:", paste(interesting_cols, collapse = ", "), "\n")
  
  # Show sample data
  cat("\nSample records:\n")
  print(head(data[, 1:min(5, ncol(data))], 3))
  
  # Look for unique values in key columns
  if ("SignPlateText" %in% names(data)) {
    unique_signs <- unique(data$SignPlateText)[1:10]
    cat("\nSample sign plate texts:\n")
    for(sign in unique_signs) {
      if(!is.na(sign)) {
        cat("  -", sign, "\n")
      }
    }
  }
  
  cat("\n")
}

analyze_parking_zones <- function(data) {
  cat("=== PARKING ZONES ANALYSIS ===\n")
  
  if (is.null(data)) {
    cat("No parking zones data available\n")
    return()
  }
  
  cat("Columns:", paste(names(data), collapse = ", "), "\n")
  cat("Total records:", nrow(data), "\n")
  
  # Look for zone-related columns
  zone_cols <- names(data)[grepl("zone|street|segment", names(data), ignore.case = TRUE)]
  cat("Zone-related columns:", paste(zone_cols, collapse = ", "), "\n")
  
  # Show sample data
  cat("\nSample records:\n")
  print(head(data[, 1:min(5, ncol(data))], 3))
  
  # Analyze unique streets if available
  if ("OnStreet" %in% names(data)) {
    unique_streets <- unique(data$OnStreet)[1:10]
    cat("\nSample streets in zones:\n")
    for(street in unique_streets) {
      if(!is.na(street)) {
        cat("  -", street, "\n")
      }
    }
  }
  
  # Analyze parking zones if available
  if ("ParkingZone" %in% names(data)) {
    zone_summary <- table(data$ParkingZone)
    cat("\nMost common parking zones:\n")
    print(head(sort(zone_summary, decreasing = TRUE), 5))
  }
  
  cat("\n")
}

# ==============================================================================
# FIND ENHANCEMENT OPPORTUNITIES
# ==============================================================================

find_enhancement_opportunities <- function(datasets) {
  cat("=== PREDICTION ENHANCEMENT OPPORTUNITIES ===\n")
  
  # Check what data we can use for predictions
  opportunities <- list()
  
  # 1. Time restrictions from sign plates
  if (!is.null(datasets$sign_plates)) {
    sign_cols <- names(datasets$sign_plates)
    time_related <- sign_cols[grepl("time|hour|restriction|limit", sign_cols, ignore.case = TRUE)]
    
    if (length(time_related) > 0) {
      opportunities$time_restrictions <- paste("Found time-related columns:", 
                                               paste(time_related, collapse = ", "))
    }
    
    # Look for common parking restrictions
    if ("SignPlateText" %in% sign_cols) {
      sample_signs <- head(unique(datasets$sign_plates$SignPlateText), 20)
      time_limits <- sample_signs[grepl("HR|HOUR|MIN|TIME", sample_signs, ignore.case = TRUE)]
      if (length(time_limits) > 0) {
        opportunities$parking_limits <- paste("Found parking time limits:", 
                                              paste(time_limits, collapse = "; "))
      }
    }
  }
  
  # 2. Zone patterns
  if (!is.null(datasets$parking_zones)) {
    zone_cols <- names(datasets$parking_zones)
    opportunities$zone_mapping <- paste("Zone columns available:", 
                                        paste(zone_cols, collapse = ", "))
    
    # Count zones per street
    if ("OnStreet" %in% zone_cols && "ParkingZone" %in% zone_cols) {
      zone_counts <- datasets$parking_zones %>%
        group_by(OnStreet) %>%
        summarise(zone_count = n_distinct(ParkingZone)) %>%
        arrange(desc(zone_count))
      
      opportunities$complex_streets <- paste("Streets with multiple zones:", 
                                             paste(head(zone_counts$OnStreet, 5), collapse = ", "))
    }
  }
  
  # 3. Cross-dataset opportunities
  if (!is.null(datasets$parking_bays) && !is.null(datasets$parking_zones)) {
    opportunities$data_integration <- "Can link parking bays to zones for enhanced predictions"
  }
  
  # Print opportunities
  for (name in names(opportunities)) {
    cat("🎯", name, ":", opportunities[[name]], "\n")
  }
  
  cat("\n")
}

# ==============================================================================
# COLLINS STREET DEEP DIVE
# ==============================================================================

analyze_collins_street <- function(datasets) {
  cat("=== COLLINS STREET DEEP DIVE ===\n")
  
  # 1. Parking bays on Collins Street
  if (!is.null(datasets$parking_bays)) {
    collins_bays <- datasets$parking_bays[grepl("Collins", datasets$parking_bays$RoadSegmentDescription, ignore.case = TRUE), ]
    cat("Collins Street parking bays:", nrow(collins_bays), "\n")
    
    # With sensors
    collins_sensors <- collins_bays[!is.na(collins_bays$KerbsideID) & collins_bays$KerbsideID != "", ]
    cat("Collins Street with sensors:", nrow(collins_sensors), "\n")
  }
  
  # 2. Sign plates for Collins Street
  if (!is.null(datasets$sign_plates)) {
    # Look for Collins Street in any text fields
    text_cols <- names(datasets$sign_plates)[sapply(datasets$sign_plates, is.character)]
    collins_signs <- datasets$sign_plates[0, ]  # Empty dataframe
    
    for (col in text_cols) {
      collins_matches <- datasets$sign_plates[grepl("Collins", datasets$sign_plates[[col]], ignore.case = TRUE), ]
      if (nrow(collins_matches) > 0) {
        collins_signs <- rbind(collins_signs, collins_matches)
      }
    }
    
    collins_signs <- collins_signs[!duplicated(collins_signs), ]
    cat("Collins Street sign plates:", nrow(collins_signs), "\n")
    
    if (nrow(collins_signs) > 0) {
      cat("Sample Collins Street signs:\n")
      if ("SignPlateText" %in% names(collins_signs)) {
        sample_collins_signs <- head(unique(collins_signs$SignPlateText), 5)
        for(sign in sample_collins_signs) {
          if(!is.na(sign)) {
            cat("  -", sign, "\n")
          }
        }
      }
    }
  }
  
  # 3. Parking zones for Collins Street  
  if (!is.null(datasets$parking_zones)) {
    if ("OnStreet" %in% names(datasets$parking_zones)) {
      collins_zones <- datasets$parking_zones[grepl("Collins", datasets$parking_zones$OnStreet, ignore.case = TRUE), ]
      cat("Collins Street zones:", nrow(collins_zones), "\n")
      
      if (nrow(collins_zones) > 0) {
        cat("Collins Street zone numbers:", paste(unique(collins_zones$ParkingZone), collapse = ", "), "\n")
      }
    }
  }
  
  cat("\n")
}

# ==============================================================================
# MAIN ANALYSIS FUNCTION
# ==============================================================================

run_complete_analysis <- function() {
  cat("🔍 MELBOURNE PARKING DATASETS - COMPLETE ANALYSIS\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")  # FIXED: Use paste(rep()) instead of * operator
  
  # Load all datasets
  datasets <- load_all_datasets()
  
  # Analyze each dataset
  analyze_parking_bays(datasets$parking_bays)
  analyze_sign_plates(datasets$sign_plates) 
  analyze_parking_zones(datasets$parking_zones)
  
  # Find enhancement opportunities
  find_enhancement_opportunities(datasets)
  
  # Deep dive on Collins Street
  analyze_collins_street(datasets)
  
  cat("🎯 ANALYSIS COMPLETE - Ready for prediction enhancements!\n")
  
  return(datasets)
}

# ==============================================================================
# RUN THE ANALYSIS
# ==============================================================================

# Run the complete analysis
datasets <- run_complete_analysis()