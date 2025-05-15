# function for visualizing annual precip running mean range matrices

library(ggplot2)

rm_range_matrix_visualizer  <- function(matrix, 
                                           plot_title = "TITLE_HERE", 
                                           legend_title = "LEGEND_TITLE",
                                           color_scheme = "blue_white", 
                                           output_file = NULL) {
  # preserve dimnames info
  lon <- as.numeric(dimnames(matrix)[[1]])
  lat <- as.numeric(dimnames(matrix)[[2]])
  
  # generate a dataframe from input matrix for use in ggplot
  plotting_dataframe <- data.frame()
  
  for (i in 1:length(lon)) {
    for (j in 1:length(lat)) {
      lon_value <- lon[i]
      # Convert longitude from 0-360 to -180 to 180
      if (lon_value > 180) lon_value <- lon_value - 360
      
      plotting_dataframe <- rbind(plotting_dataframe, data.frame(
        lon = lon_value,
        lat = lat[j],
        value = matrix[i, j]
      ))
    }
  }
  
  # interpret color scheme from input param
  if (color_scheme == "blue_white") {
    low_col <- "white"
    high_col <- "blue"
    mid_col <- NULL
    mid_val <- NULL
  } else if (color_scheme == "diff") {
    # difference maps (obs - model)
    low_col <- "red"
    high_col <- "blue"
    mid_col <- "white"
    mid_val <- 0
  } else if (color_scheme == "ratio") {
    # bias maps (obs / model)
    low_col <- "orange"
    high_col <- "purple"
    mid_col <- "white"
    mid_val <- 1
  } else {
    stop("Unsupported color scheme. Use 'blue_white', 'diff', or 'ratio'.")
  }
  
  # debugging step to reduce artifacting that sometimes occurs
  lon_step <- ifelse(length(lon) > 1, min(diff(sort(unique(lon)))), 0.25)
  lat_step <- ifelse(length(lat) > 1, min(diff(sort(unique(lat)))), 0.25)
  
  # load vector data
  ne_region <- sf::st_read("data/bounds/ne_plus_ny_indv_state_bounds.shp", quiet = TRUE)
  ny_watersheds <- sf::st_read("data/bounds/basin24.shp", quiet = TRUE)
  
  # ensure consistent coordinate system
  if (sf::st_crs(ne_region) != "EPSG:4326") {
    ne_region <- sf::st_transform(ne_region, "EPSG:4326")
  }
  
  if (sf::st_crs(ny_watersheds) != "EPSG:4326") {
    ny_watersheds <- sf::st_transform(ny_watersheds, "EPSG:4326")
  }
  
  # Create the plot
  rm_range_matrix_plot <- ggplot() +
    geom_tile(data = plotting_dataframe, 
              aes(x = lon, y = lat, fill = value), 
              width = lon_step, height = lat_step) +
    geom_sf(data = ne_region,
            fill = NA,
            color = "black",
            size = 0.7) +
    geom_sf(data = ny_watersheds,
            fill = NA,
            color = "red",
            size = 0.7) +
    coord_sf(
      xlim = range(ifelse(lon > 180, lon - 360, lon)),
      ylim = range(lat),
      expand = FALSE
    ) +
    theme_minimal() +
    labs(
      title = plot_title,
      x = "Longitude",
      y = "Latitude",
      fill = legend_title
    ) +
    scale_x_continuous(
      breaks = seq(-80, -65, by = 5),
      labels = function(x) x # Now using -180 to 180 format
    ) +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 11, face = "bold"),
      legend.key.height = unit(1.5, "cm")
    )
  
  # determine the color scale
  if (!is.null(mid_col)) {
    # diverging scale (for diff or ratio)
    rm_range_matrix_plot <- rm_range_matrix_plot + scale_fill_gradient2(
      low = low_col,
      mid = mid_col,
      high = high_col,
      midpoint = mid_val,
      na.value = "transparent"
    )
  } else {
    # sequential scale (for default plots)
    rm_range_matrix_plot <- rm_range_matrix_plot + scale_fill_gradient(
      low = low_col,
      high = high_col,
      na.value = "transparent"
    )
  }
  
  # save the plot if output file is specified
  if (!is.null(output_file)) {
    ggsave(output_file, rm_range_matrix_plot, width = 7, height = 6, dpi = 300)
  }
  
  return(rm_range_matrix_plot)

}