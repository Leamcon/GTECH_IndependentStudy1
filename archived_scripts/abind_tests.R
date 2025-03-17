#' Tests for Precipitation Array Generator
#' 
#' This script contains tests for validating the precipitation array generator functions.
#' Run this script after sourcing the main array generator script.

# Load required testing packages
library(testthat) # Optional, if you want more structured testing
library(terra)

# Test for get_annual_means function
test_annual_means_function <- function(nc_files_for_binding) {
  # Test with a known file
  test_file <- nc_files_for_binding[1]
  
  # Get result
  result <- get_annual_means(test_file)
  
  # Basic checks
  cat("Testing get_annual_means() function:\n")
  cat("- Return value is a matrix:", is.matrix(result), "\n")
  cat("- Dimensions:", paste(dim(result), collapse="x"), "\n")
  cat("- Contains numeric data:", is.numeric(result), "\n")
  cat("- Range of values (expected to be reasonable for mm/day):",
      paste(round(range(result, na.rm=TRUE), 2), collapse=" to "), "\n")
  cat("- NAs present:", any(is.na(result)), "\n\n")
  
  # Check unit conversion
  nc <- nc_open(test_file)
  pr_original <- ncvar_get(nc, "pr")
  manual_mean <- apply(pr_original, c(1,2), mean) * 86400
  nc_close(nc)
  
  max_diff <- max(abs(result - manual_mean), na.rm=TRUE)
  cat("- Maximum difference from manual calculation:", max_diff, "\n")
  cat("  (should be very close to 0)\n\n")
  
  return(invisible(result))
}

# Test for the array binding process
test_array_binding <- function(nc_files_for_binding) {
  # Test with 3 files
  if(length(nc_files_for_binding) < 3) {
    stop("Need at least 3 files for testing")
  }
  
  test_files <- nc_files_for_binding[1:3]
  
  # Process test files individually
  test_means <- list()
  for(i in 1:3) {
    test_means[[i]] <- get_annual_means(test_files[i])
  }
  
  # Manually bind
  manual_array <- abind(test_means[[1]], test_means[[2]], test_means[[3]], along=3)
  
  # Use function
  auto_array <- precip_array_binder(test_files)
  
  # Compare results
  cat("Testing array binding:\n")
  cat("- Manual dimensions:", paste(dim(manual_array), collapse="x"), "\n")
  cat("- Function dimensions:", paste(dim(auto_array), collapse="x"), "\n")
  
  # Compare values (ignoring attributes)
  values_equal <- all.equal(
    as.vector(manual_array), 
    as.vector(auto_array), 
    check.attributes=FALSE
  )
  
  cat("- Values are equal:", isTRUE(values_equal), "\n")
  if(!isTRUE(values_equal)) cat("  Difference:", values_equal, "\n")
  
  # Check attributes
  expected_attrs <- c("years", "longitude", "latitude", "spatial_dims", 
                      "units", "variable", "creation_date", "source_files")
  actual_attrs <- names(attributes(auto_array))
  
  cat("- Has all expected attributes:", 
      all(expected_attrs %in% actual_attrs), "\n\n")
  
  return(invisible(auto_array))
}

# Full pipeline test
test_full_pipeline <- function(nc_files_for_binding) {
  # Time the full process
  start_time <- Sys.time()
  
  result <- try({
    precip_3d_array <- precip_array_binder(nc_files_for_binding)
    
    # Basic validation
    cat("Full pipeline test:\n")
    cat("- Successfully created array:", !inherits(precip_3d_array, "try-error"), "\n")
    if(!inherits(precip_3d_array, "try-error")) {
      cat("- Final dimensions:", paste(dim(precip_3d_array), collapse="x"), "\n")
      cat("- Years covered:", min(attr(precip_3d_array, "years")), "to", 
          max(attr(precip_3d_array, "years")), "\n")
      cat("- Spatial resolution:", length(attr(precip_3d_array, "longitude")), "x",
          length(attr(precip_3d_array, "latitude")), "points\n")
    }
    
    return(precip_3d_array)
  })
  
  end_time <- Sys.time()
  cat("- Processing time:", difftime(end_time, start_time, units="secs"), "seconds\n\n")
  
  return(invisible(result))
}

# Edge case tests
test_edge_cases <- function() {
  cat("Testing edge cases:\n")
  
  # Test with empty file list
  empty_result <- try(precip_array_binder(character(0)), silent=TRUE)
  cat("- Handles empty file list:", inherits(empty_result, "try-error"), "\n")
  
  # Test with non-existent files
  bad_files <- c("nonexistent_file_1.nc", "nonexistent_file_2.nc")
  bad_result <- try(precip_array_binder(bad_files), silent=TRUE)
  cat("- Handles non-existent files:", inherits(bad_result, "try-error"), "\n")
  
  # Test filename parsing with non-standard names
  weird_names <- c("/path/to/file_notstandard_format.nc", 
                   "/path/to/another_12345_weird.nc")
  weird_result <- try(precip_array_binder(weird_names), silent=TRUE)
  cat("- Handles non-standard filenames:", inherits(weird_result, "try-error"), 
      "or produces warning\n\n")
}

# Memory usage test
test_memory_usage <- function(nc_files_for_binding) {
  cat("Testing memory usage:\n")
  
  # Track memory before
  gc()
  mem_before <- sum(gc()[,2])
  
  # Run function
  result <- try({
    precip_3d_array <- precip_array_binder(nc_files_for_binding)
  })
  
  # Track memory after
  gc()
  mem_after <- sum(gc()[,2])
  
  # Calculate memory used
  mem_used <- mem_after - mem_before
  
  cat("- Memory usage:", round(mem_used, 2), "MB\n")
  cat("- Estimated array size:", 
      round(prod(dim(precip_3d_array)) * 8 / 1024^2, 2), "MB (raw data)\n\n")
  
  return(invisible(NULL))
}

test_visualization <- function(nc_files_for_binding) {
  tryCatch({
    # Create the precipitation array
    precip_3d_array <- precip_array_binder(nc_files_for_binding)
    
    # Access metadata
    years <- attr(precip_3d_array, "years")
    lon <- attr(precip_3d_array, "longitude")
    lat <- attr(precip_3d_array, "latitude")
    
    # Print information for debugging
    cat("Array dimensions:", paste(dim(precip_3d_array), collapse=" x "), "\n")
    cat("Longitude range:", paste(range(lon), collapse=" to "), "\n")
    cat("Latitude range:", paste(range(lat), collapse=" to "), "\n")
    
    # Create test directory if it doesn't exist
    test_dir <- "test_outputs"
    if(!dir.exists(test_dir)) dir.create(test_dir)
    
    # Pick middle year for visualization
    mid_year_idx <- ceiling(length(years)/2)
    if(is.na(mid_year_idx) || mid_year_idx < 1 || mid_year_idx > length(years)) {
      mid_year_idx <- 1  # Fallback if index calculation fails
    }
    
    test_year <- years[mid_year_idx]
    test_slice <- precip_3d_array[,,mid_year_idx]
    
    # Debug print matrix dimensions
    cat("Matrix dimensions before raster creation:", paste(dim(test_slice), collapse="x"), "\n")
    
    # Check if matrix needs to be transposed based on dimension names
    if(dim(precip_3d_array)[1] == length(lon) && 
       dim(precip_3d_array)[2] == length(lat)) {
      # Matrix is [lon, lat] - which is correct for terra
      transpose_needed <- FALSE
    } else {
      # Matrix may need transposing
      transpose_needed <- TRUE
      cat("Warning: Matrix dimensions don't match lon/lat lengths - transposing\n")
    }
    
    # Create two versions of the raster to compare
    if(transpose_needed) {
      r1 <- rast(test_slice)
      r2 <- rast(t(test_slice))
    } else {
      r1 <- rast(test_slice)
      r2 <- r1  # No need for two versions
    }
    
    # Set extent correctly - ensure longitude is x and latitude is y
    ext(r1) <- c(min(lon), max(lon), min(lat), max(lat))
    if(transpose_needed) {
      ext(r2) <- c(min(lon), max(lon), min(lat), max(lat))
    }
    
    # Set projection
    crs(r1) <- "EPSG:4326"
    if(transpose_needed) {
      crs(r2) <- "EPSG:4326"
    }
    
    # Plot both versions if needed
    png(filename=paste0(test_dir, "/precip_debug_", test_year, ".png"), 
        width=1800, height=900)
    
    if(transpose_needed) {
      par(mfrow=c(1,2))
      plot(r1, main=paste("Original Orientation\nYear:", test_year),
           col=hcl.colors(100, "Blues"))
      plot(r2, main=paste("Transposed Orientation\nYear:", test_year),
           col=hcl.colors(100, "Blues"))
    } else {
      plot(r1, main=paste("Precipitation (mm/day)\nYear:", test_year),
           col=hcl.colors(100, "Blues"))
    }
    dev.off()
    
    cat("Debug visualization created at:", 
        paste0(test_dir, "/precip_debug_", test_year, ".png"), "\n")
    
  }, error = function(e) {
    cat("Error in visualization test:", e$message, "\n")
  })
}

# Main test runner
run_all_tests <- function(nc_files_for_binding) {
  cat("===== PRECIPITATION ARRAY GENERATOR TESTS =====\n\n")
  
  # Run all tests
  test_annual_means_function(nc_files_for_binding)
  test_array_binding(nc_files_for_binding)
  test_full_pipeline(nc_files_for_binding)
  test_edge_cases()
  test_memory_usage(nc_files_for_binding)
  test_visualization(nc_files_for_binding)
  
  cat("===== ALL TESTS COMPLETED =====\n")
}

# Note: Don't run automatically - wait for invocation from main script