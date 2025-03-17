# netCDF data interrogator
# this script will check our downloaded .nc data using some basic steps (from ncdf docs)

library(ncdf4)

#checking the base download

nc_files <- list.files("data/cpc_conus_precip", pattern = "\\.nc$", full.names = TRUE)

nc <- nc_open(nc_files[1])


print(nc)

# get specific information about the precipitation variable
pr_var <- ncvar_get(nc, "precip")
pr_dim <- dim(pr_var)

# get dimension info
time <- ncvar_get(nc, "time")
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

# print some summaries
cat("\nDimensions of precipitation data:", paste(pr_dim, collapse = " x "), "\n")
cat("Time steps:", length(time), "\n")
cat("Latitude range:", range(lat), "\n")
cat("Longitude range:", range(lon), "\n")

# check the attributes
pr_units <- ncatt_get(nc, "pr", "units")
cat("\nPrecipitation units:", pr_units$value, "\n")

# some nodata checks

cat("\nData range and potential issues:\n")
cat("Min value:", min(pr_var, na.rm = TRUE), "\n")
cat("Max value:", max(pr_var, na.rm = TRUE), "\n")
cat("Number of NA values:", sum(is.na(pr_var)), "\n")
cat("Number of infinite values:", sum(is.infinite(pr_var)), "\n")
cat("Number of negative values:", sum(pr_var < 0, na.rm = TRUE), "\n")

# check for missing data value specified in metadata (double check metadata when copying!)
missing_val <- -9.96920996838687e+36
missing_count <- sum(pr_var == missing_val, na.rm = TRUE)
cat("Number of missing data entries (-9.96920996838687e+36):", missing_count, "\n")


nc_close(nc)

#checking the subset file (CLEAR ENV FIRST)

nc_files <- list.files("data/cpc_conus_precip", pattern = "\\.nc$", full.names = TRUE)

nc <- nc_open(nc_files[1])


print(nc)

# get specific information about the precipitation variable
pr_var <- ncvar_get(nc, "precip")
pr_dim <- dim(pr_var)

# get dimension info
time <- ncvar_get(nc, "time")
lat <- ncvar_get(nc, "lat")
lon <- ncvar_get(nc, "lon")

# print some summaries
cat("\nDimensions of precipitation data:", paste(pr_dim, collapse = " x "), "\n")
cat("Time steps:", length(time), "\n")
cat("Latitude range:", range(lat), "\n")
cat("Longitude range:", range(lon), "\n")

# check the attributes
pr_units <- ncatt_get(nc, "pr", "units")
cat("\nPrecipitation units:", pr_units$value, "\n")

# some nodata checks

cat("\nData range and potential issues:\n")
cat("Min value:", min(pr_var, na.rm = TRUE), "\n")
cat("Max value:", max(pr_var, na.rm = TRUE), "\n")
cat("Number of NA values:", sum(is.na(pr_var)), "\n")
cat("Number of infinite values:", sum(is.infinite(pr_var)), "\n")
cat("Number of negative values:", sum(pr_var < 0, na.rm = TRUE), "\n")

# check for missing data value specified in metadata (double check metadata when copying!)
missing_val <- -9.96920996838687e+36
missing_count <- sum(pr_var == missing_val, na.rm = TRUE)
cat("Number of missing data entries (-9.96920996838687e+36):", missing_count, "\n")


nc_close(nc)

