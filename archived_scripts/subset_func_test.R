# test_subset_netcdf.R

library(ncdf4)
source("subset_netcdf.R")  

# Define northeastern US bounding box coordinates
ne_coords <- matrix(c(
  -80.5, 37.0,  
  -66.9, 37.0,  
  -66.9, 47.5,  
  -80.5, 47.5,  
  -80.5, 37.0   
), ncol = 2, byrow = TRUE)

# Test function
tryCatch({
  subset_netcdf(
    input_nc = "data/cpc_conus_precip/precip.V1.0.mon.mean.nc",
    output_nc = "data/cpc_conus_precip/ne_us_subset.nc",
    coords = ne_coords
  )
  
  # Verify the output file was created
  if(file.exists("ne_us_subset.nc")) {
    # Open and print basic info about the subset file
    nc_test <- nc_open("ne_us_subset.nc")
    print("Subsetting successful!")
    print("Output file dimensions:")
    print(paste("Longitude:", length(nc_test$dim$lon$vals)))
    print(paste("Latitude:", length(nc_test$dim$lat$vals)))
    print(paste("Time:", length(nc_test$dim$time$vals)))
    nc_close(nc_test)
  }
}, error = function(e) {
  print(paste("Error occurred:", e$message))
})
