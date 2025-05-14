# Tailored .nc Subset Application Script
#  - Makes sure directory contains all requisite file years
#  - Applies a spatial subset function across a given list of files
#  - Saves subsetted files to a new directory

library(ncdf4)
library(sf)
library(fs)

source("subset_netcdf_with_shp.R")

# input and output directories
input_dir <- "data/gcm_conus/gfdl_cm4_gr2/historical"
output_dir <- "data/gcm_ne_subset/gfdl_cm4_gr2/historical"
shapefile_path <- "data/bounds/ne_plus_ny_bounds.shp"

# create output directory if it doesn't exist
#fs::dir_create(output_dir, recursive = TRUE)

# list nc files from input dir
nc_files <- fs::dir_ls(input_dir, regexp = "\\.nc$")

# check all years are included
expected_years <- 1950:2014
year_list <- numeric()

for (nc_file in nc_files) {
  filename <- basename(nc_file)
  year_str <- gsub(".*gr2_(\\d{4})_v1\\.1.*", "\\1", filename)
  if (year_str != filename) {
    year_list <- c(year_list, as.numeric(year_str))
  } else {
    warning(sprintf("Could not extract year from filename: %s", filename))
  }
}

# Check for missing years
missing_years <- setdiff(expected_years, year_list)
if (length(missing_years) > 0) {
  warning(sprintf("Missing years: %s", paste(missing_years, collapse = ", ")))
}

# Check for duplicate years
duplicate_years <- year_list[duplicated(year_list)]
if (length(duplicate_years) > 0) {
  warning(sprintf("Duplicate years: %s", paste(unique(duplicate_years), collapse = ", ")))
}

# Print summary
cat(sprintf("Found %d files covering %d unique years\n", 
            length(nc_files), length(unique(year_list))))
cat(sprintf("Year range: %d to %d\n", min(year_list), max(year_list)))
cat(sprintf("Expected years: %d to %d\n\n", min(expected_years), max(expected_years)))



# main processing loop
for (nc_file in nc_files) {
  
  filename <- basename(nc_file)
  output_file <- file.path(output_dir, filename)
  
  # log
  cat(sprintf("Processing file: %s\n", filename))
  
  # subset function call
  subset_netcdf_with_shp(
    input_nc = nc_file,
    output_nc = output_file,
    shapefile_path = shapefile_path
  )
  
  # test our output file
  nc_out <- nc_open(output_file)
  
  # dims info for logging
  lon_len <- nc_out$dim$lon$len
  lat_len <- nc_out$dim$lat$len
  lon_vals <- nc_out$dim$lon$vals
  lat_vals <- nc_out$dim$lat$vals
  
  # log dimension information
  cat(sprintf("File: %s\n", filename))
  cat(sprintf("Longitude extent: %d points, range: [%.4f, %.4f]\n", 
              lon_len, min(lon_vals), max(lon_vals)))
  cat(sprintf("Latitude extent: %d points, range: [%.4f, %.4f]\n", 
              lat_len, min(lat_vals), max(lat_vals)))
  
  # Log the actual index values (first few and last few to avoid excessive output)
  max_values_to_show <- 5
  
  cat("Longitude values: ")
  if (length(lon_vals) <= 2 * max_values_to_show) {
    cat(sprintf("%.4f ", lon_vals))
  } else {
    cat(sprintf("%.4f ", lon_vals[1:max_values_to_show]))
    cat("... ")
    cat(sprintf("%.4f ", lon_vals[(length(lon_vals) - max_values_to_show + 1):length(lon_vals)]))
  }
  cat("\n")
  
  cat("Latitude values: ")
  if (length(lat_vals) <= 2 * max_values_to_show) {
    cat(sprintf("%.4f ", lat_vals))
  } else {
    cat(sprintf("%.4f ", lat_vals[1:max_values_to_show]))
    cat("... ")
    cat(sprintf("%.4f ", lat_vals[(length(lat_vals) - max_values_to_show + 1):length(lat_vals)]))
  }
  cat("\n\n")
  
  nc_close(nc_out)
}

cat("All files processed successfully!\n")