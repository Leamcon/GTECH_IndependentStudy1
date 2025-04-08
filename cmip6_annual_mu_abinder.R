#' Annual Mean Precipitation Array Generator
#' 
#' Process:
#'  - takes .nc files of daily precipitation by year as input
#'  - files are converted into a 3D array of annual mean precipitation in mm/day
#'  - output structure: (lon, lat, time)
#'  - a mapping step is included for visualization and testing
#'  - array is saved in .rdata format for further use
#'  
#' TODO: add dynamic handling of different gcm and scenario
#' TODO: perhaps break out some of these function into seperate scripts

library(ncdf4)
library(abind)
library(sf)
library(ggplot2)

# load the ggplot2 array visualization script
source("array_ggplot_visualizer.R")

# function takes the avg across all 365 indices of var "pr" at each grid point
# function also converts values from mm/s to mm/day
# TODO: add control for cpc "precip" var name
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

# file list
# TODO: add user prompting to control file ingestion as in gcm downloader
# nc_files_for_binding <- list.files("data/gcm_ne_subset/cmcc_cm2_sr5/historical", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/cmcc_cm2_sr5/ssp585", full.names = TRUE)
nc_files_for_binding <- list.files("data/gcm_ne_subset/cesm2_waccm/historical", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/cesm2_waccm/ssp585", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/cesm2/historical", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/cesm2/ssp585", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/bcc_csm2_mr/historical", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/bcc_csm2_mr/ssp585", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/access_esm1_5/historical", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/access_esm1_5/ssp585", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/access_cm2/historical", full.names = TRUE)
# nc_files_for_binding <- list.files("data/gcm_ne_subset/access_cm2/ssp585", full.names = TRUE)

# wrapped execution in try to print some extra info
try({
  # cmcc_cm2_sr5_historical_annual_means_ne_subset <- precip_array_binder(nc_files_for_binding)
  cesm2_waccm_historical_annual_means_ne_subset <- precip_array_binder(nc_files_for_binding)
  # access_cm2_historical_annual_means_ne_subset <- precip_array_binder(nc_files_for_binding)
  # access_cm2_ssp585_annual_means_ne_subset <- precip_array_binder(nc_files_for_binding)
  # cat("Array creation successful!\n")
  # cat("Dimensions:", paste(dim(access_cm2_ssp585_annual_means_ne_subset), collapse=" x "), "\n")
  # cat("Timescale:", attr(access_cm2_ssp585_annual_means_ne_subset, "years"), "\n")
  # cat("Units:", attr(access_cm2_ssp585_annual_means_ne_subset, "units"), "\n")
})

# load the cpc array for masking
load(file = "rdata/cpc_ne_annual_mean_precipitation.RData")

# cmcc_cm2_sr5_historical_annual_means_ne_subset <- axes_trimmer(cmcc_cm2_sr5_historical_annual_means_ne_subset)
cesm2_waccm_historical_annual_means_ne_subset <- axes_trimmer(cesm2_waccm_historical_annual_means_ne_subset)
# access_cm2_historical_annual_means_ne_subset <- axes_trimmer(access_cm2_historical_annual_means_ne_subset)
# access_cm2_ssp585_annual_means_ne_subset <- axes_trimmer(access_cm2_ssp585_annual_means_ne_subset)

# masking, make sure to load cpc data FIRST
# cmcc_cm2_sr5_historical_annual_means_ne_subset <- array_masker(cmcc_cm2_sr5_historical_annual_means_ne_subset, cpc_annual_means_ne_subset)
cesm2_waccm_historical_annual_means_ne_subset <- array_masker(cesm2_waccm_historical_annual_means_ne_subset, cpc_annual_means_ne_subset)
# access_cm2_historical_annual_means_ne_subset <- array_masker(access_cm2_historical_annual_means_ne_subset, cpc_annual_means_ne_subset)
# access_cm2_ssp585_annual_means_ne_subset <- array_masker(access_cm2_ssp585_annual_means_ne_subset, cpc_annual_means_ne_subset)

# visualizer, set the second argument to the t index you would like visualized
# array_ggplot_visualizer(cmcc_cm2_sr5_historical_annual_means_ne_subset, 45)
array_ggplot_visualizer(cesm2_waccm_historical_annual_means_ne_subset, 45)
# array_ggplot_visualizer(access_cm2_historical_annual_means_ne_subset, 45)
# array_ggplot_visualizer(access_cm2_ssp585_annual_means_ne_subset, 45)

# saving
save(cmcc_cm2_sr5_historical_annual_means_ne_subset, file = "rdata/cmcc_cm2_sr5_historical_annual_mean_precip.RData")
save(cesm2_waccm_historical_annual_means_ne_subset, file = "rdata/cesm2_waccm_historical_annual_mean_precip.RData")
save(access_cm2_historical_annual_means_ne_subset, file = "rdata/access_cm2_historical_annual_mean_precip.RData")
save(access_cm2_ssp585_annual_means_ne_subset, file = "rdata/access_cm2_ssp585_annual_mean_precip.RData")
