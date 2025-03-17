#' Spatially subset a .nc and write it to a new .nc
#' 
#' @param input_nc Character string specifying path to input NetCDF file
#' @param output_nc Character string specifying path for output NetCDF file
#' @param coords Matrix of coordinates with 2 columns (lon, lat) defining the bounding polygon
#' @return Invisible NULL, creates new NetCDF file as side effect
#' @export

library(ncdf4)

subset_netcdf <- function(input_nc, output_nc, coords) {
  # Input validation
  if (!file.exists(input_nc)) {
    stop("Input NetCDF file not found at specified path")
  }
  if (!is.matrix(coords) || ncol(coords) != 2) {
    stop("coords must be a matrix with 2 columns (longitude, latitude)")
  }
  
  nc_in <- nc_open(input_nc)
  
  lon <- ncvar_get(nc_in, "lon")
  lat <- ncvar_get(nc_in, "lat")
  
  # check the .nc coord format
  is_360 <- any(lon > 180)
  
  # Convert coords param to match .nc
  coords_converted <- coords
  if (is_360 && any(coords[,1] < 0)) {
    # Convert -180 to 180 coordinates to 0-360
    coords_converted[,1] <- ifelse(coords[,1] < 0, coords[,1] + 360, coords[,1])
  } else if (!is_360 && any(coords[,1] > 180)) {
    # Convert 0-360 coordinates to -180 to 180
    coords_converted[,1] <- ifelse(coords[,1] > 180, coords[,1] - 360, coords[,1])
  }
  
  # create a bounding box from coords
  lon_bounds <- range(coords_converted[,1])
  lat_bounds <- range(coords_converted[,2])
  
  # find indices within bounds
  lon_idx <- which(lon >= lon_bounds[1] & lon <= lon_bounds[2])
  lat_idx <- which(lat >= lat_bounds[1] & lat <= lat_bounds[2])
  
  # check if there are indicecs there
  if (length(lon_idx) == 0 || length(lat_idx) == 0) {
    nc_close(nc_in)
    stop("No data found within specified coordinate bounds")
  }
  
  # create new dims from input .nc
  londim <- ncdim_def("lon", "degrees_east", lon[lon_idx],
                      longname = "Longitude")
  latdim <- ncdim_def("lat", "degrees_north", lat[lat_idx],
                      longname = "Latitude")
  timedim <- nc_in$dim$time
  
  # define the new vars and copy attributes from old .nc
  precip_orig <- nc_in$var$precip
  precip_def <- ncvar_def(name = "precip",
                          units = precip_orig$units,
                          dim = list(londim, latdim, timedim),
                          missval = precip_orig$missval,
                          longname = precip_orig$longname,
                          prec = precip_orig$prec)
  
  # create the new .nc
  nc_out <- nc_create(output_nc, list(precip_def))
  
  # copy global attributes
  globatts <- ncatt_get(nc_in, 0)
  for(attname in names(globatts)) {
    ncatt_put(nc_out, 0, attname, globatts[[attname]])
  }
  
  # chunking (thanks claude)
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
  
  # close netCDFs
  nc_close(nc_in)
  nc_close(nc_out)
  
  invisible(NULL)
}