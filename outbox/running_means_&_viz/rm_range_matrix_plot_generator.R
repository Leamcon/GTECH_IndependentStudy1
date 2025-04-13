# function for visualizing annual precip running mean range matrices

library(ggplot2)

rm_range_matrix_plot_generator <- function(matrix, 
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
      plotting_dataframe <- rbind(plotting_dataframe, data.frame(
        lon = lon[i],
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
  
  # Create the plot
  rm_range_matrix_plot <- ggplot(plotting_dataframe, aes(x = lon, y = lat)) +
    geom_tile(aes(fill = value), width = lon_step, height = lat_step) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    labs(
      title = plot_title,
      x = "Longitude",
      y = "Latitude",
      fill = legend_title
    ) +
    scale_x_continuous(
      breaks = seq(280, 295, by = 5),
      labels = function(x) ifelse(x > 180, x - 360, x) # convert to -180 to 180
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