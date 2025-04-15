#' script for downloading NASA NEX-GDDP-CMIP6 data
#' 
#' leverages the mt climate office cmip6 package
#' found at https://github.com/mt-climate-office/cmip6
 
library(cmip6)
library(sf)

date_continuity_checker <- function(directory) {
  # list of files in dir
  files <- list.files(directory, pattern = "\\.nc$", full.names = FALSE)
  
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

wd <- getwd()

# AOI definition

# I define NE US for now
ne_coords <- matrix(c(
  -80.5, 37.0,  
  -66.9, 37.0,  
  -66.9, 47.5,  
  -80.5, 47.5,  
  -80.5, 37.0   
), ncol = 2, byrow = TRUE)

# create the sf
ne_region <- sf::st_sfc(st_polygon(list(ne_coords)), crs = 4326) %>%
  sf::st_as_sf()

# download CMIP6 data
cmip6::cmip6_dl(
  outdir = "data/gcm/test",
  aoi = ne_region,
  models = "ACCESS-CM2",
  scenarios = "ssp585",
  elements = "pr",
  latest = TRUE
)


date_check <- date_continuity_checker(paste(wd, "data/gcm/test", sep = "/"))

print(date_check)
