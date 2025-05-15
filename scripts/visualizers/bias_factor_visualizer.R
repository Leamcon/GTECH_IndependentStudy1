# Bias Factor Visualizer
#   - implements a function to visualize bias factor matrices
#   - uses a diverging color scheme to highlight positive and negative bias values
#   - accepts optional fixed min/max range parameters to maintain visual consistency
#   - provides option to use log scale for bias factor visualization (recommended)
#   - returns the plot object invisibly for potential further manipulation

library(terra)

bias_factor_visualizer <- function(input_matrix, title = "Bias Factor (Observed/Model)", min_max_range = NULL, log_scale = FALSE){
  
  # Extract dims
  dims <- dimnames(input_matrix)
  lon_values <- as.numeric(dims[[1]])
  lat_values <- as.numeric(dims[[2]])
  
  # Convert longitude if in 0-360 format while maintaining order
  lon_values_converted <- ifelse(lon_values > 180, lon_values - 360, lon_values)
  lon_idx <- order(lon_values_converted)
  lon_values_converted <- sort(lon_values_converted)
  input_matrix_reordered <- input_matrix[lon_idx, ]
  
  # Set raster bounds
  xmin <- min(lon_values_converted)
  xmax <- max(lon_values_converted)
  ymin <- min(lat_values)
  ymax <- max(lat_values)
  
  # Create blank raster
  r <- terra::rast(ncols = length(lon_values_converted),
                   nrows = length(lat_values),
                   xmin = xmin, 
                   xmax = xmax,
                   ymin = ymin, 
                   ymax = ymax,
                   crs = "EPSG:4326")
  
  # Rotate & transpose for proper visualization
  input_matrix_rotated <- NULL
  for (i in 1:length(lon_values_converted)) { 
    input_matrix_rotated <- cbind(input_matrix_rotated, rev(input_matrix_reordered[i,])) 
  }
  
  values(r) <- input_matrix_rotated
  
  # Load shapefiles and enforce crs
  basin_shp <- terra::vect("data/bounds/basin24.shp")
  ne_ny_shp <- terra::vect("data/bounds/ne_plus_ny_indv_state_bounds.shp")
  basin_shp <- terra::project(basin_shp, "EPSG:4326")
  ne_ny_shp <- terra::project(ne_ny_shp, "EPSG:4326")
  
  # Create graticule
  grat <- terra::graticule(
    lon = seq(floor(xmin), ceiling(xmax), by = 2),
    lat = seq(floor(ymin), ceiling(ymax), by = 2),
    crs = "EPSG:4326"
  )
  
  # Use provided min/max range or calculate from data
  if (is.null(min_max_range)) {
    range_vals <- c(0, 9)
  } else {
    range_vals <- min_max_range
  }
  
  # Create diverging color palette centered on 1
  n_colors <- 100

  range_min <- range_vals[1]
  range_max <- range_vals[2]
  
  # If range includes 1, make color break at 1, otherwise use midpoint
  if (range_min <= 1 && range_max >= 1) {
    one_position <- (1 - range_min) / (range_max - range_min)
    n_below_colors <- round(one_position * n_colors)
  } else {
    n_below_colors <- 10
  }
  
  n_above_colors <- n_colors - n_below_colors
  
  below_colors <- colorRampPalette(c("red", "white"))(n_below_colors)
  above_colors <- colorRampPalette(c("white", "blue"))(n_above_colors + 1)[-1]
  pal <- c(below_colors, above_colors)
  
  terra::plot(r, 
              main = title,
              col = pal,
              xlab = "Longitude",
              ylab = "Latitude",
              legend = TRUE,
              axes = TRUE,
              range = range_vals)
  
  terra::lines(grat, col = "grey70", lwd = 0.5)
  
  terra::plot(basin_shp, col = NA, border = "black", lwd = 0.5, add = TRUE)
  terra::plot(ne_ny_shp, col = NA, border = "grey70", lwd = 1.5, add = TRUE)
  
  invisible(r)
}