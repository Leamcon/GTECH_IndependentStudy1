gcm_date_continuity_checker_2 <- function(directory) {
  
  
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
  
  # More robust regex that handles varying model name lengths
  # Pattern: pr_day_MODEL_SCENARIO_ENSEMBLE_gr_YEAR_v1.1.nc
  # Focusing on extracting the year that comes after '_gr_'
  years <- as.numeric(gsub(".*_gr_(\\d{4})_.*\\.nc", "\\1", files))
  
  # Filter out NA values
  valid_indices <- !is.na(years)
  valid_files <- files[valid_indices]
  years <- years[valid_indices]
  
  if (length(years) == 0) {
    warning("Could not extract valid years from filenames")
    return(NULL)
  }
  
  years <- sort(years)
  
  # get the diff between years, if 1 we're good
  year_gaps <- diff(years)
  continuous <- all(year_gaps == 1)
  
  # report writer
  report <- list(
    total_files = length(files),
    valid_files = length(years),
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