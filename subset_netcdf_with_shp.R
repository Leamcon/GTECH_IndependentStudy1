#' Spatially subset a .nc and write it to a new .nc using sf and a shapefile
#' 
#' @param input_nc Character string specifying path to input NetCDF file
#' @param output_nc Character string specifying path for output NetCDF file
#' @param shapefile_path Character string specifying path to the shapefile
#' @return Invisible NULL, creates new NetCDF file as side effect
#' @export

library(ncdf4)
library(sf)

subset_netcdf_with_shp <- function(input_nc, output_nc, shapefile_path) {
  # Input validation
  if (!file.exists(input_nc)) {
    stop("Input NetCDF file not found at specified path")
  }
  if (!file.exists(shapefile_path)) {
    stop("Shapefile not found at specified path")
  }
  
  # Read the shapefile
  boundary_sf <- st_read(shapefile_path, quiet = TRUE)
  
  # Check if all polygons are now valid
  boundary_sf <- st_make_valid(boundary_sf)
  
  if (any(!st_is_valid(boundary_sf))) {
    warning("Some polygons could not be made valid. Using bounding box instead.")
    # Fall back to using the bounding box if polygons can't be made valid
    boundary_sf <- st_as_sfc(st_bbox(boundary_sf))
  }
  
  # Open NetCDF file
  nc_in <- nc_open(input_nc)
  
  # Get coordinates from NetCDF
  lon <- ncvar_get(nc_in, "lon")
  lat <- ncvar_get(nc_in, "lat")
  
  # Check if NetCDF uses 0-360 longitude format
  is_360 <- any(lon > 180)
  
  # Transform the shapefile to match the CRS of the NetCDF if necessary
  boundary_sf_transformed <- st_transform(boundary_sf, crs = 4326) # Ensure WGS84
  bbox <- st_bbox(boundary_sf_transformed)
  
  # Handle longitude shift if needed
  if (is_360 && bbox["xmin"] < 0) {
    # For 0-360 longitude format, adjust the bbox directly
    lon_bounds <- c(
      ifelse(bbox["xmin"] < 0, bbox["xmin"] + 360, bbox["xmin"]),
      ifelse(bbox["xmax"] < 0, bbox["xmax"] + 360, bbox["xmax"])
    )
  } else {
    lon_bounds <- c(bbox["xmin"], bbox["xmax"])
  }
  
  lat_bounds <- c(bbox["ymin"], bbox["ymax"])
  
  # Find indices within bounds
  lon_idx <- which(lon >= lon_bounds[1] & lon <= lon_bounds[2])
  lat_idx <- which(lat >= lat_bounds[1] & lat <= lat_bounds[2])
  
  # Check if there are indices within bounds
  if (length(lon_idx) == 0 || length(lat_idx) == 0) {
    nc_close(nc_in)
    stop("No data found within specified shapefile bounds")
  }
  
  # Create new dimensions from input .nc
  londim <- ncdim_def("lon", "degrees_east", lon[lon_idx],
                      longname = "Longitude")
  latdim <- ncdim_def("lat", "degrees_north", lat[lat_idx],
                      longname = "Latitude")
  timedim <- nc_in$dim$time
  
  # Define the new vars and copy attributes from old .nc
  precip_orig <- nc_in$var$precip
  precip_def <- ncvar_def(name = "precip",
                          units = precip_orig$units,
                          dim = list(londim, latdim, timedim),
                          missval = precip_orig$missval,
                          longname = precip_orig$longname,
                          prec = precip_orig$prec)
  
  # Create the new .nc
  nc_out <- nc_create(output_nc, list(precip_def))
  
  # Copy global attributes
  globatts <- ncatt_get(nc_in, 0)
  for(attname in names(globatts)) {
    ncatt_put(nc_out, 0, attname, globatts[[attname]])
  }
  
  # Chunking for efficient processing
  chunk_size <- 12  # Process one year at a time
  n_times <- nc_in$dim$time$len
  n_chunks <- ceiling(n_times/chunk_size)
  
  for(i in 1:n_chunks) {
    start_idx <- (i-1)*chunk_size + 1
    end_idx <- min(i*chunk_size, n_times)
    count <- end_idx - start_idx + 1
    
    # Read chunk of data
    precip_data <- ncvar_get(nc_in, "precip",
                             start = c(min(lon_idx), min(lat_idx), start_idx),
                             count = c(length(lon_idx), length(lat_idx), count))
    
    # Write chunk to new file
    ncvar_put(nc_out, precip_def, precip_data,
              start = c(1, 1, start_idx),
              count = c(length(lon_idx), length(lat_idx), count))
  }
  
  # Close netCDFs
  nc_close(nc_in)
  nc_close(nc_out)
  
  invisible(NULL)
}