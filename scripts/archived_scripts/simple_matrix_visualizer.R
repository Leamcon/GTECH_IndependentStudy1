# a visualizer leveraging mostly base package functions

library(sf)
library(jpeg)
library(terra)

#simple_matrix_visualizer <- 

load("rdata/cpc_ne_annual_mean_precipitation.RData")

########
# slicer for testing
running_mean_range_calculator <- function(ts_array, window) {
  # create an array of running means specified thru param 2
  running_mean_array <- apply(ts_array, c(1, 2), function(ts_slice){ 
    filter(ts_slice, rep(1/window, window), sides = 1)
  })
  # reset the dims after apply() usage
  running_mean_array <- aperm(running_mean_array, c(2, 3, 1))
  
  # generate a matrix of range diffs across z axis at each x,y
  # TODO: prevent creation of inf values - see frei_read_cpc_dat
  range_matrix <- apply(running_mean_array, c(1,2), function(rm_slice) {
    diff(range(rm_slice, na.rm = TRUE))
  })
  
  return(range_matrix)
}

cpc_rm_test <- running_mean_range_calculator(cpc_annual_means_ne_subset, 10)

dims <- dimnames(cpc_rm_test)

lon_values <- as.numeric(dims[[1]])
lat_values <- as.numeric(dims[[2]])

xmin <- min(lon_values)
xmax <- max(lon_values)
ymin <- min(lat_values)
ymax <- max(lat_values)


r <- rast(ncols = length(lon_values),
          nrows = length(lat_values),
          xmin = xmin, 
          xmax = xmax,
          ymin = ymin, 
          ymax = ymax,
          crs = "EPSG:4326")

cpc_rot <- NULL
for (i in 1:length(lon_values)) { 
  cpc_rot <- cbind(cpc_rot, rev(cpc_rm_test[i,])) 
}

values(r) <- cpc_rot

terra::plot(r, 
            main = "Northeast US Precipitation",
            col = rev(hcl.colors(100, "Blues")),
            legend = TRUE,                  
            axes = TRUE)                    



# testing a tmap arrange 

library(terra)
library(tmap)
library(gridExtra)

r1 <- rast(nrows=10, ncols=10, vals=runif(100))
r2 <- rast(nrows=10, ncols=10, vals=runif(100))
r3 <- rast(nrows=10, ncols=10, vals=runif(100))
r4 <- rast(nrows=10, ncols=10, vals=runif(100))

tm1 <- tm_shape(r1) + tm_raster() + tm_layout(title="Plot 1")
tm2 <- tm_shape(r2) + tm_raster() + tm_layout(title="Plot 2")
tm3 <- tm_shape(r3) + tm_raster() + tm_layout(title="Plot 3")
tm4 <- tm_shape(r4) + tm_raster() + tm_layout(title="Plot 4")

tmap_arrange(tm1, tm2, tm3, tm4, ncol=2)
