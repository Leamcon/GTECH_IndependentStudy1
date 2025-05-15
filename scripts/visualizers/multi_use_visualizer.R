# Visualization function that can handle viz of various grid types
# takes input grid and title, outputs plotted grid with reference vectors overlaid

library(terra)

multi_use_visualizer <- function(input_matrix, title = "Northeast US Precipitation"){
  
  dims <- dimnames(input_matrix)
  
  lon_values <- as.numeric(dims[[1]])
  lat_values <- as.numeric(dims[[2]])
  
  # test lon and convert if 0-360 while maintaining order
  lon_values_converted <- ifelse(lon_values > 180, lon_values - 360, lon_values)
  lon_idx <- order(lon_values_converted)
  lon_values_converted <- sort(lon_values_converted)
  input_matrix_reordered <- input_matrix[lon_idx, ]
  
  # set raster bounds
  xmin <- min(lon_values_converted)
  xmax <- max(lon_values_converted)
  ymin <- min(lat_values)
  ymax <- max(lat_values)
  
  # create blank raster
  r <- terra::rast(ncols = length(lon_values_converted),
            nrows = length(lat_values),
            xmin = xmin, 
            xmax = xmax,
            ymin = ymin, 
            ymax = ymax,
            crs = "EPSG:4326")
  
  # rotate & tranpose for proper viz
  input_matrix_rotated <- NULL
  for (i in 1:length(lon_values_converted)) { 
    input_matrix_rotated <- cbind(input_matrix_rotated, rev(input_matrix_reordered[i,])) 
  }
  
  values(r) <- input_matrix_rotated
  
  # load shapefiles and enforce crs
  basin_shp <- terra::vect("data/bounds/basin24.shp")
  ne_ny_shp <- terra::vect("data/bounds/ne_plus_ny_indv_state_bounds.shp")
  basin_shp <- terra::project(basin_shp, "EPSG:4326")
  ne_ny_shp <- terra::project(ne_ny_shp, "EPSG:4326")
  
  # create graticule
  grat <- terra::graticule(
    lon = seq(floor(xmin), ceiling(xmax), by = 2),
    lat = seq(floor(ymin), ceiling(ymax), by = 2),
    crs = "EPSG:4326"
  )
  
  terra::plot(r, 
              main = title,
              col = rev(hcl.colors(100, "Blues")),
              xlab = "Longitude",
              ylab = "Latitude",
              legend = TRUE,
              plg = list(title = "mm/day"),
              axes = TRUE)
  
  # add graticule
  terra::lines(grat, col = "grey70", lwd = 0.5)
  
  # overlay shapes
  terra::plot(basin_shp, col = NA, border = "red", lwd = 1.5, add = TRUE)
  terra::plot(ne_ny_shp, col = NA, border = "black", lwd = 1.5, add = TRUE)

}

