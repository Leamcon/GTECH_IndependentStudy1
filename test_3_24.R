library(ncdf4)
library(terra)
library(sf)

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

load("rdata/cpc_ne_annual_mean_precipitation.RData")

# LOOKING AT CATSKILLS
# GRID POINTS FROM SAM
# 42.12 N -74.38 W. is the only grid fully in the catskills
# (42.12N -74.62W), (42.12N -74.88W), (42.12N -75.12W), 
# (42.12N -75.38W), (42.38N -74.38W), (42.38N -74.62W), 
# (42.38N -74.88W), (41.88N -74.38W), and (41.88N -74.62W) 
lon_cat2 <- c(285.625, 285.375, 285.125, 284.875, 284.625,
              285.625, 285.375, 285.125, 285.625, 285.375, 285.875, 285.875)
lat_cat2 <- c(42.125, 42.125, 42.125, 42.125, 42.125,
              42.375, 42.375, 42.375, 41.875, 41.875, 41.875, 42.125)

#lon_cat2 <- lon_cat2+0.25

# nc <- ncdf4::nc_open("data/gcm_ne_subset/access_cm2/historical/pr_day_ACCESS-CM2_historical_r1i1p1f1_gn_1950_v1.1.nc")
# nc_dims <- nc$dim
# print(nc_dims$lon$vals)

observed_rm_diff_matrix <- running_mean_range_calculator(cpc_annual_means_ne_subset, 10)

dims <- dimnames(input_matrix_rotated)

lon_values <- as.numeric(dims[[1]])
lat_values <- as.numeric(dims[[2]])

lon_values_converted <- lon_values - 360
lon_cat_converted <- lon_cat2 - 360
lon_idx <- order(lon_values_converted)
lon_values_converted <- sort(lon_values_converted)
input_matrix_reordered <- observed_rm_diff_matrix[lon_idx, ]

basin_coords <- cbind(lon_cat_converted, lat_cat2)

xmin <- min(lon_values_converted)
xmax <- max(lon_values_converted)
ymin <- min(lat_values)
ymax <- max(lat_values)

r <- terra::rast(ncols = length(lon_values_converted),
                 nrows = length(lat_values),
                 xmin = xmin, 
                 xmax = xmax,
                 ymin = ymin, 
                 ymax = ymax,
                 crs = "EPSG:4326")

input_matrix_rotated <- NULL
for (i in 1:length(lon_values_converted)) { 
  input_matrix_rotated <- cbind(input_matrix_rotated, rev(input_matrix_reordered[i,])) 
}

values(r) <- input_matrix_rotated

basin_pts <- terra::vect(basin_coords, type = "points", atts = NULL, crs = "EPSG:4326")
basin_shp <- terra::vect("data/bounds/basin24.shp")
ne_ny_shp <- terra::vect("data/bounds/ne_plus_ny_indv_state_bounds.shp")
basin_shp <- terra::project(basin_shp, "EPSG:4326")
ne_ny_shp <- terra::project(ne_ny_shp, "EPSG:4326")

grat <- terra::graticule(
  lon = seq(floor(xmin), ceiling(xmax), by = 2),
  lat = seq(floor(ymin), ceiling(ymax), by = 2),
  crs = "EPSG:4326"
)

terra::plot(r, 
            main = "Observed Running Mean Range Precipitation",
            col = rev(hcl.colors(100, "Blues")),
            xlab = "Longitude",
            ylab = "Latitude",
            legend = TRUE,
            plg = list(title = "mm/day"),
            axes = TRUE)

terra::lines(grat, col = "grey70", lwd = 0.5)
points(basin_pts, col = "black", cex = 0.5)
terra::plot(basin_shp, col = NA, border = "red", lwd = 0.75, add = TRUE)
terra::plot(ne_ny_shp, col = NA, border = "black", lwd = 0.75, add = TRUE)
