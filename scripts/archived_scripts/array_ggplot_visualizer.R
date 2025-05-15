#' Raster Plotter using ggplot
#' 
#' visualizes 3D arrays(x, y, t)
#' takes a 3D array and a specified time index (t) as parameters
#' designed for use with data from netCDF format
#' transposes lon/lat and converts from 0-360 to -180-180
#' plots the result with a built in boundary vector overlaid
#' 
#' TODO: add a parameter for selecting different vector overlays

library(ggplot2)
library(sf)

array_ggplot_visualizer <- function(array, year_index) {
  
  # extract data and transpose
  precip_slice <- array[,,year_index]
  precip_slice <- t(precip_slice)
  lon <- attr(array, "longitude")
  lat <- attr(array, "latitude")
  
  lon_step <- mean(diff(sort(unique(lon))))
  lat_step <- mean(diff(sort(unique(lat))))
  
  # init a df for the plot 
  plotting_frame <- data.frame()
  
  for (i in 1:length(lon)) {
    for (j in 1:length(lat)) {
      lon_value <- lon[i]
      # Convert longitude from 0-360 to -180 to 180
      if (lon_value > 180) lon_value <- lon_value - 360
      
      plotting_frame <- rbind(plotting_frame, data.frame(
        lon = lon_value,
        lat = lat[j],
        precip = precip_slice[j, i]
      ))
    }
  }
  
  # load vector data
  ne_region <- sf::st_read("data/bounds/ne_plus_ny_indv_state_bounds.shp", quiet = TRUE)
  
  if (sf::st_crs(ne_region) != "EPSG:4326") {
    ne_region <- sf::st_transform(ne_region, "EPSG:4326")
  }
  
  # create the plot
  test_plot <- ggplot() +
    # add t axis data using geom_tile
    geom_tile(
      data = plotting_frame,
      aes(x = lon, y = lat, fill = precip),
      width = lon_step * 1.001,
      height = lat_step * 1.001
    ) +
    # Add color scale
    scale_fill_viridis_c(
      name = "Precipitation\n(mm/day)",
      option = "magma",
      na.value = "transparent"
    ) +
    # Add boundaries
    geom_sf(
      data = ne_region,
      fill = NA,
      color = "white",
      size = 0.7
    ) +
    # Labels and title
    labs(
      title = paste("Precipitation for year", attr(array, "years")[year_index]),
      x = "Longitude",
      y = "Latitude"
    ) +

    theme_minimal() +

    coord_sf()
  
  print(test_plot)
  
  
  return(plot = test_plot)
}