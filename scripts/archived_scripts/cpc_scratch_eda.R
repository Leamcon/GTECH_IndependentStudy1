library(ncdf4)

nc <- nc_open("data/cpc_conus_precip/precip.V1.0.mon.mean.nc")

precip_data <- ncvar_get(nc, "precip")

time_dim <- dim(precip_data)[3]

cat("Total number of time steps:", time_dim, "\n")

# Check a few different time steps
# Try the middle of the time series
middle_month <- ceiling(time_dim/2)
middle_slice <- precip_data[,,middle_month]
cat("Summary of middle month (time step", middle_month, "):\n")
print(summary(as.vector(middle_slice)))

# Try the last month
last_slice <- precip_data[,,time_dim]
cat("Summary of last month (time step", time_dim, "):\n")
print(summary(as.vector(last_slice)))

# Sample a few random months
random_months <- sample(1:time_dim, 3)
for(month in random_months) {
  slice <- precip_data[,,month]
  cat("Summary of random month (time step", month, "):\n")
  print(summary(as.vector(slice)))
}

# Count NAs in each time step to see if some months have more valid data than others
na_counts <- sapply(1:time_dim, function(t) sum(is.na(precip_data[,,t])))
cat("Months with fewest NAs:\n")
print(head(sort(na_counts)))
cat("Months with most NAs:\n")
print(tail(sort(na_counts)))