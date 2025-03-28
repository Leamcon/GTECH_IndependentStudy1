# script to create a 3d array of annual precipitation from cpc observed data

library(ncdf4)
library(lubridate)
library(abind)
library(ggplot2)
library(sf)

#source("multi_use_visualizer.R")

# function to aggregate monthly to annual means from a single .nc
# creates an index of year values and month values
# uses these as pointers to collect desired time frame and aggregate 12 month sections
# binds a new 3D array of annual means
cpc_precipitation_aggregator <- function(nc_file_path) {
  
  nc <- ncdf4::nc_open(nc_file_path)
  
  # get .nc dimension info
  lon <- ncdf4::ncvar_get(nc, "lon")
  lat <- ncdf4::ncvar_get(nc, "lat")
  time <- ncdf4::ncvar_get(nc, "time")
  
  # extract precip variable (3D array)
  precip <- ncdf4::ncvar_get(nc, "precip")
  
  # time data conversion for better readability
  # Step 1 get a reference that can be handled by POSIX
  time_units <- ncdf4::ncatt_get(nc, "time", "units")$value
  ref_date <- substr(time_units, regexpr("since", time_units) + 6, nchar(time_units))
  ref_date <- trimws(ref_date)
  
  # Step 2: using the as.POSIX and origin data, convert hours since into dates
  dates <- as.POSIXct(time * 3600, origin = ref_date)  # hours to seconds for POSIX
  years <- lubridate::year(dates)
  months <- lubridate::month(dates)
  
  nc_close(nc)
  
  # to conform with the cmip6 data grab only from 1950-2014 (to match t of model historical)
  years_idx <- (years >= 1950) & (years <= 2014)
  
  # filter the data by the constraints explained above
  filtered_precip <- precip[, , years_idx]
  
  
  # unique years and lengths will be used as pointers
  filtered_years <- years[years_idx]
  unique_years <- unique(filtered_years)
  n_years <- length(unique_years)
  
  
  my_agg <- function(x) { 
    as.vector(aggregate(x,list(filtered_years), mean)$x)
    }
  
  annual_means <- apply(filtered_precip, c(1, 2), my_agg)
  
  annual_means <- aperm(annual_means, c(2, 3, 1))
  
  # create names for array dims
  dimnames(annual_means) <- list(
    longitude = as.character(lon, 2),
    latitude = as.character(lat, 2),
    year = as.character(unique_years)
  )
  
  # add metadata
  attr(annual_means, "years") <- unique_years
  attr(annual_means, "longitude") <- lon
  attr(annual_means, "latitude") <- lat
  # attr(annual_means, "units") <- "mm/day"
  # attr(annual_means, "variable") <- "precipitation"
  # attr(annual_means, "creation_date") <- format(Sys.time(), "%Y-%m-%d")
  # attr(annual_means, "source_files") <- basename(nc_file_path)
  
  
  return(annual_means)
  
}

cpc_annual_means_ne_subset <- cpc_precipitation_aggregator("data/cpc_conus_precip/precip_mon_mean_ne_shp_subset.nc")

#multi_use_visualizer(cpc_annual_means_ne_subset, 45)

image(cpc_annual_means_ne_subset[,,45])

save(cpc_annual_means_ne_subset, file = "rdata/cpc_ne_annual_mean_precipitation.RData")
