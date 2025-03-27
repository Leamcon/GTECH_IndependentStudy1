# Northeast Region Specific .nc to 3D array processor

library(ncdf4)
library(abind)
library(sf)
library(terra)
library(tmap)

# load the array visualization script
source("multi_use_visualizer.R")

#################
# FUNCTIONS

# Input/Output handling function
#   - takes a df of model names and periods (historical or ssp585)
#   - iterates through df to construct paths to the .nc file locations
#   - applies processing functions to each batch of nc (historical or ssp585)
#   - creates custom named variable to hold each resultant array
#   - saves all created vars to .rdata file
#   - tracks status and errors throughout
climate_model_io <- function(models_df, 
                             save_file = "rdata/modeled_precipitation_ne_subset.RData") {
  
  # list to hold var names (mainly for status checking)
  all_var_names <- character()
  
  # status counter vars
  processed_count <- 0
  error_count <- 0
  
  # load the cpc array for masking
  load(file = "rdata/cpc_ne_annual_mean_precipitation.RData")
  
  # loop through df to get each model name, period
  # apply functions
  for (i in 1:nrow(models_df)) {
    model_name <- models_df$model[i]
    period <- models_df$period[i]
    
    cat(sprintf("Processing (%d/%d): %s - %s\n", i, nrow(models_df), model_name, period))
    
    # construct file path then build file list from model subdir
    file_path <- file.path("data", "gcm_ne_subset", model_name, period)
    nc_files_for_binding <- list.files(file_path, pattern = "\\.nc$", full.names = TRUE)
    
    # skip if no files found & report error
    if (length(nc_files_for_binding) == 0) {
      warning(paste("No .nc files found for", model_name, period, "in path:", file_path))
      error_count <- error_count + 1
      next
    }
    
    # further status reporting
    cat("  Found", length(nc_files_for_binding), "netCDF files in path:", file_path, "\n")
    
    result <- try({
      
      # main processing stage
      cat("  Creating 3D precipitation array...\n")
      result_array <- precip_array_binder(nc_files_for_binding)
      cat("  Trimming axes...\n")
      result_array <- axes_trimmer(result_array)
      cat("  Applying mask...\n")
      result_array <- array_masker(result_array, cpc_annual_means_ne_subset)
      
      # build var name from df, assign() and add to list
      var_name <- paste0(model_name, "_", period, "_annual_means_ne_subset")
      assign(var_name, result_array, envir = .GlobalEnv)
      all_var_names <- c(all_var_names, var_name)
      
      # report success and iterate counter
      processed_count <- processed_count + 1
      cat("  Successfully processed:", var_name, "\n")
      
      TRUE # error handling to be passed
    })
    
    # error reporting
    if (inherits(result, "try-error")) {
      error_count <- error_count + 1
      cat("  ERROR processing", model_name, period, ":", attr(result, "condition")$message, "\n")
    }
    cat("----------------------------------------------------------\n")
  }
  
  # status reporting
  cat("\nProcessing Summary:\n")
  cat("  Successfully processed:", processed_count, "model-period combinations\n")
  cat("  Errors encountered:", error_count, "model-period combinations\n")
  
  # save if there is data
  if (length(all_var_names) > 0) {
    cat("\nSaving", length(all_var_names), "arrays to", save_file, "\n")
    
    save(list = all_var_names, file = save_file, envir = .GlobalEnv)
    
    cat("Save complete.\n")
  } else {
    cat("\nNo arrays were processed successfully. Nothing to save.\n")
  }
  
  return(all_var_names)
    
}

# this function creates the 3D array of annual precip values (lon, lat, time)
#  - takes a list of .nc files
#  - dimensions are checked against one file in list to confirm dims match exactly
#  - loops across the list and applies the annual mean function
#  - binds annual mean matrices into 3D array
#  - attempts to preserve some metadata from each file (years)
precip_array_binder <- function(nc_file_list) {
  
  # init a list to hold annual mean matrices for binding
  annual_mean_matrix_list <- list()
  
  # get year information from filenames
  years <- as.numeric(gsub(".*gn_(\\d{4})_v1\\.1\\.nc", "\\1", nc_file_list))
  
  # test year extraction and sort list by year
  if (any(is.na(years)) || length(years) != length(nc_file_list)) {
    warning("Could not extract years from all filenames. Using sequential indices instead.")
    years <- seq_along(nc_file_list)
  } else {
    year_order <- order(years)
    years <- years[year_order]
    nc_file_list <- nc_file_list[year_order]
  }
  
  # get lon and lat ref from the first .nc file in list
  nc_reference <- ncdf4::nc_open(nc_file_list[1])
  lon_values <- ncdf4::ncvar_get(nc_reference, "lon")
  lat_values <- ncdf4::ncvar_get(nc_reference, "lat")
  ncdf4::nc_close(nc_reference)
  
  # main processing loop
  for(file in seq_along(nc_file_list)) {
    
    current_means <- get_annual_means(nc_file_list[file])
    
    # get reference dims from file 1
    if(file == 1) {
      reference_dims <- dim(current_means)
    } else {
      # verify dims
      if(!identical(dim(current_means), reference_dims)) {
        stop(sprintf(
          "Dimension mismatch in file %d (year %d)\nExpected: %s\nGot: %s",
          file, years[file],
          paste(reference_dims, collapse=" x "),
          paste(dim(current_means), collapse=" x ")
        ))
      }
    }
    
    # Add to list if dimensions are correct
    annual_mean_matrix_list[[file]] <- current_means
  } # end of main processing loop
  
  # combine the matrices in list to 3D array
  precipitation_array <- do.call(abind, c(annual_mean_matrix_list, list(along = 3)))
  
  
  # create names for array dims
  dimnames(precipitation_array) <- list(
    longitude = as.character(round(lon_values, 2)),
    latitude = as.character(round(lat_values, 2)),
    year = as.character(years)
  )
  
  # add metadata
  attr(precipitation_array, "years") <- years
  attr(precipitation_array, "longitude") <- lon_values
  attr(precipitation_array, "latitude") <- lat_values
  attr(precipitation_array, "spatial_dims") <- reference_dims
  attr(precipitation_array, "units") <- "mm/day"
  attr(precipitation_array, "variable") <- "precipitation"
  attr(precipitation_array, "creation_date") <- format(Sys.time(), "%Y-%m-%d")
  attr(precipitation_array, "source_files") <- basename(nc_file_list)
  
  return(precipitation_array)
}

# function takes the avg across all 365 indices of var "pr" at each grid point
# function also converts values from mm/s to mm/day
get_annual_means <- function(nc_file) {
  
  nc <- ncdf4::nc_open(nc_file)
  
  # "pr" precipitation
  pr <- ncdf4::ncvar_get(nc, "pr")
  
  # apply a mean across both margins of the matrix
  annual_mean <- apply(pr, c(1,2), mean)
  
  # convert from mm/s to mm/d
  annual_mean <- annual_mean * 60 * 60 * 24
  
  ncdf4::nc_close(nc)
  
  return(annual_mean)
}

# function to trim x & y axis of the target array to conform with the size of observed data array
axes_trimmer <- function(target_array) {
  
  # from the cmip array grab everying from x axis between [2] and -1 from end, y[2] to y[end]
  xtrim <- 2:(dim(target_array)[1]-1)
  ytrim <- 2:dim(target_array)[2]
  target_trimmed <- target_array[xtrim, ytrim, ]
  
  # preserve lon/lat vectors
  lon_values <- as.numeric(dimnames(target_trimmed)[[1]])
  lat_values <- as.numeric(dimnames(target_trimmed)[[2]])
  attr(target_trimmed, "years") <- attr(target_array, "years")
  attr(target_trimmed, "longitude") <- lon_values
  attr(target_trimmed, "latitude") <- lat_values
  
  return(target_trimmed)
}

# function to mask the cmip6 array so its matrix slices conform with ones in the cpc array
array_masker <- function(target_array, masking_array) {
  
  target_dims <- dim(target_array)
  
  # takes a slice of our main array to use as a basis for the mask
  masking_array_slice <- masking_array[,,1]
  
  # build a matrix to match size of one from masking array then set its  
  matrix_mask <- matrix(1, nrow = dim(masking_array_slice)[1], 
                        ncol = dim(masking_array_slice)[2])
  
  matrix_mask[is.na(masking_array_slice)] <- NA
  
  masked_array <- array(NA, dim = target_dims)
  
  # apply mask to each matrix in the array
  for (t in 1:target_dims[3]){
    masked_array[,,t] <- target_array[,,t] * matrix_mask
  }
  
  # preserve the attributes
  dimnames(masked_array) <- dimnames(target_array)
  attr(masked_array, "years") <- attr(target_array, "years")
  attr(masked_array, "longitude") <- attr(target_array, "longitude")
  attr(masked_array, "latitude") <- attr(target_array, "latitude")
  
  return(masked_array)
}

#################
# EXECUTION

# model name & period dataframe
models_by_period <- expand.grid(
  model = c("access_cm2", "access_esm1_5", "bcc_csm2_mr", "cesm2", "cesm2_waccm", "cmcc_cm2_sr5"),
  period = c("historical", "ssp585"),
  stringsAsFactors = FALSE
)

exec_func <- climate_model_io(models_by_period)

# viz testing
multi_use_visualizer(cmcc_cm2_sr5_historical_annual_means_ne_subset[,,45])
