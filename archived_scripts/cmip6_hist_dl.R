#' script for downloading NASA NEX-GDDP-CMIP6 data
#' 
#' leverages the mt climate office cmip6 package
#' found at https://github.com/mt-climate-office/cmip6

library(cmip6)
library(sf)



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

# download CMIP6 historical data
cmip6::cmip6_dl(
  outdir = "data/gcm/test",
  aoi = ne_region,
  models = "ACCESS-CM2",
  scenarios = "historical",
  elements = "pr",
  latest = TRUE
)
