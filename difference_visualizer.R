library(terra)

#' Difference Visualizer
#' 
#' @description Visualizes difference matrices using a diverging color scheme
#' @param input_matrix Matrix to visualize
#' @param title Plot title
#' @param symmetric Whether to force color scale to be symmetric around zero
#' @return The plot object (invisibly)
difference_visualizer <- function(input_matrix, title = "Difference (Observed - Model)"){
  
  # Extract dimension names
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
  
  # Range for difference
  range_vals <- c(-0.9, 1.3)
  
  # Calculate where zero falls in the range (as a proportion)
  zero_position <- abs(range_vals[1]) / (abs(range_vals[1]) + abs(range_vals[2]))
  
  # Create color breaks that place white at zero
  n_colors <- 100
  n_neg_colors <- round(zero_position * n_colors)
  n_pos_colors <- n_colors - n_neg_colors
  
  # Create the diverging palette with white at zero
  neg_colors <- colorRampPalette(c("red", "white"))(n_neg_colors)
  pos_colors <- colorRampPalette(c("white", "blue"))(n_pos_colors + 1)[-1]  # Remove first to avoid duplicate white
  pal <- c(neg_colors, pos_colors)
  
  # Plot with divergent scale
  terra::plot(r, 
              main = title,
              col = pal,
              xlab = "Longitude",
              ylab = "Latitude",
              legend = TRUE,
              axes = TRUE,
              range = range_vals)
  
  # Add graticule
  terra::lines(grat, col = "grey70", lwd = 0.5)
  
  # Overlay shapes
  terra::plot(basin_shp, col = NA, border = "black", lwd = 0.5, add = TRUE)
  terra::plot(ne_ny_shp, col = NA, border = "grey70", lwd = 1.5, add = TRUE)
  
  # Return invisibly
  invisible(r)
}
