library(cmip6)
library(sf)

source("gcm_date_continuity_checker_2.r")

ne_region <- sf::st_read("data/bounds/ne_plus_ny_bounds.shp", quiet = TRUE)

outdir <- "data/gcm_ne_subset/gfdl_cm4_gr2/historical"
model <- "GFDL-CM4-GR2"
scenario <- "historical"

year_patch <- c(1997, 1998, 2000, 2001, 2003, 2004, 2006, 2007, 2009, 2010, 2012, 2013)

cmip6::cmip6_dl(
  outdir = outdir,
  aoi = ne_region,
  models = model,
  scenarios = scenario,
  elements = "pr",
  #years = 1990:2014,
  latest = TRUE
)

print(gcm_date_continuity_checker_2(outdir))


