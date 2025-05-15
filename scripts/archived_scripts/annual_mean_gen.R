# this script aggregates the precip data into annual mean values (mm/d)
# arrays are then bound together

library(ncdf4)
library(abind)

# testing stuff
#nc <- nc_open(access_cm2_files[1])
#pr <- ncvar_get(nc, "pr")
#annual_mean <- apply(pr, c(1,2), mean)
#annual_mean <- annual_mean * 60 * 60 * 24

# function takes the avg across all 365 indices of var "pr" at each grid point
# function also converts values from mm/s to mm/day
get_annual_mean <- function(nc_file) {
  
  nc <- nc_open(nc_file)
  
  # "pr" precipitation
  pr <- ncvar_get(nc, "pr")
  
  # apply a mean across both margins of the matrix
  annual_mean <- apply(pr, c(1,2), mean)
  
  # convert from mm/s to mm/d
  annual_mean <- annual_mean * 60 * 60 * 24
  
  nc_close(nc)
  
  return(annual_mean)
}

# our file list
access_cm2_files <- list.files("data/gcm_ne_subset/access_cm2/ssp585", full.names = TRUE)

# get generic info to use in instantiating empty array
nc_generic <- nc_open(access_cm2_files[1])
lon <- dim(ncvar_get(nc_generic, "lon"))
lat <- dim(ncvar_get(nc_generic, "lat"))
n_files <- length(access_cm2_files)
lon_vect <- ncvar_get(nc_generic, "lon")
lat_vect <- ncvar_get(nc_generic, "lat")
nc_close(nc_generic)

# instantiate our array, will be populated later
access_cm2_array <- array(NA, dim = c(lon, lat, n_files))


# loops through our list of files, applies function to each filename, populates array
for(i in seq_along(access_cm2_files)) {
  setequal(lon, lon) #test for equality between the lon/lat of each .nc (maybe in func)
  focus_data <- get_annual_mean(access_cm2_files[i])
  access_cm2_array[,,i] <- focus_data
}

# test image, adjust the index to change year
image(access_cm2_array[,,23])

test <- apply(access_cm2_array, c(1,2), mean, na.rm = TRUE)
