#' this function checks the continuity of time series .nc files
#' only works with data downloaded from the NASA TDS catalog

gcm_date_continuity_checker <- function(directory) {
  
  # list of files in dir
  files <- list.files(directory, pattern = "\\.nc$", full.names = FALSE)
  
  # error checks
  if (!dir.exists(directory)) {
    stop("Specified directory does not exist: ", directory)
  }
  
  if (length(files) == 0) {
    warning("No NetCDF files found in directory")
    return(NULL)
  }
  
  # use a regex to get the year from rilename
  years <- as.numeric(gsub(".*gn_(\\d{4})_v1\\.1\\.nc", "\\1", files))
  
  years <- sort(years)
  
  # get the dif between year, if 1 we're good
  year_gaps <- diff(years)
  continuous <- all(year_gaps == 1)
  
  # reprt writer
  report <- list(
    total_files = length(files),
    year_range = range(years),
    continuous = continuous,
    gaps = if(!continuous) {
      gap_positions <- which(year_gaps > 1)
      data.frame(
        missing_start = years[gap_positions] + 1,
        missing_end = years[gap_positions + 1] - 1
      )
    } else {
      NULL
    }
  )
  
  return(report)
}

# tester
#print(gcm_date_continuity_checker("data/gcm/access_cm2/historical"))
