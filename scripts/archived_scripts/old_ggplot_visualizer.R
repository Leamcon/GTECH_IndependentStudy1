library(ggplot2)

# visualizer function for testing
# takes an input array and year
# transposes the lon/lat to properly display
# converts from 0-360 to -180-180 lon
# plots the result along with a boundary vector
precip_ggplot_visualizer <- function(array, year_index) {
  
  # extract data and transpose
  precip_slice <- array[,,year_index]
  precip_slice <- t(precip_slice)
  lon <- attr(array, "longitude")
  lat <- attr(array, "latitude")
  
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
    # Add raster data using geom_tile
    geom_tile(
      data = plotting_frame,
      aes(x = lon, y = lat, fill = precip)
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
      color = "gray",
      size = 0.7
    ) +
    # Labels and title
    labs(
      title = paste("Precipitation for year", attr(array, "years")[year_index]),
      x = "Longitude",
      y = "Latitude"
    ) +
    # Simple theme
    theme_minimal() +
    # Basic coordinate system setup
    coord_sf()
  
  print(test_plot)
  
  
  return(plot = test_plot)
}

