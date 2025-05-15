# netCDF data interrogator
# this script will check our downloaded .nc data using some basic steps (from ncdf docs)

library(ncdf4)

nc_files_cpc <- list.files("data/cpc_conus_precip", pattern = "\\.nc$", full.names = TRUE)

nc_cpc <- ncdf4::nc_open(nc_files_cpc[2])


print(nc_cpc)

# get specific information about the precipitation variable
pr_var_cpc <- ncdf4::ncvar_get(nc_cpc, "pr")
pr_dim_cpc <- dim(pr_var_cpc)

# get dimension info
time_cpc <- ncdf4::ncvar_get(nc_cpc, "time")
lat_cpc <- ncdf4::ncvar_get(nc_cpc, "lat")
lon_cpc <- ncdf4::ncvar_get(nc_cpc, "lon")

# print some summaries
cat("\nDimensions of precipitation data:", paste(pr_dim_cpc, collapse = " x "), "\n")
cat("Time steps:", length(time_cpc), "\n")
cat("Latitude range:", range(lat_cpc), "\n")
cat("Longitude range:", range(lon_cpc), "\n")

print(diff(range(lat_cpc)))
print(diff(range(lon_cpc)))
diff(lat_cpc)
diff(lon_cpc)

# confirm the continuity of the coords 
lonlat_grid_cpc <- expand.grid(lon_cpc, lat_cpc)
plot(lonlat_grid_cpc)

# check the attributes
pr_units_cpc <- ncdf4::ncatt_get(nc_cpc, "pr", "units")
cat("\nPrecipitation units:", pr_units_cpc$value, "\n")

# some nodata checks

cat("\nData range and potential issues:\n")
cat("Min value:", min(pr_var_cpc, na.rm = TRUE), "\n")
cat("Max value:", max(pr_var_cpc, na.rm = TRUE), "\n")
cat("Number of NA values:", sum(is.na(pr_var_cpc)), "\n")
cat("Number of infinite values:", sum(is.infinite(pr_var_cpc)), "\n")
cat("Number of negative values:", sum(pr_var_cpc < 0, na.rm = TRUE), "\n")

# check for missing data value specified in metadata (double check metadata when copying!)
missing_val <- 1.00000002004088e+20
missing_count <- sum(pr_var_cpc == missing_val, na.rm = TRUE)
cat("Number of missing data entries (1.00000002004088e+20):", missing_count, "\n")

# monthly_precip_calc function test var (subsitute for param)
#pr_timeseries <- pr_var[1, 1, ]

# the following function takes the precip data dim from a .nc
# data is converted to monthly and from mm/s to mm/day
monthly_precip_calc <- function(pr_timeseries) {
  
  # create a calendar (based on nc_files[1])
  #d_start <- as.Date("2015-01-01")
  #d_end <- as.Date("2015-12-31")
  d_start <- as.Date("1950-01-01")
  d_end <- as.Date("1950-12-31")
  calendar <- seq(d_start, d_end, 1)
  # get only months from calendar
  months <- format(calendar, "%m")
  
  # grab the average of pr for each month
  monthly_mean <- aggregate(pr_timeseries, list(as.numeric(months)), mean)
  
  # mean of jan for testing
  # jan <- mean(pr_timeseries[months=="01"])
  
  # convert pr from mm/s to mm/d
  monthly_mean$x <- monthly_mean$x * 60 * 60 * 24
  
  return(monthly_mean$x)
}


# visual check of the pr values (avg mm/yr)
# use apply() to create a 3d array of monthly values instead of daily
pr_mmpm <- apply(pr_var_cpc, c(1,2), monthly_precip_calc)
# we need to reorganize the array, I realized apply() put the date info first for some reason
pr_mmpm <- aperm(pr_mmpm, c(2, 3, 1))
# test image to make sure things look right
image(lon_cpc, lat_cpc, pr_mmpm[,,2])

nc_close(nc_cpc)