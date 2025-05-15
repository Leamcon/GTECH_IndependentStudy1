# Test script for year-by-year downloading
library(cmip6)
library(sf)

# For testing, use just one model
test_model <- "CNRM-ESM2-1"
test_scenario <- "historical"

# Create historical year lookup vector
historical_year_lookup <- 1950:2014

# Load the region shape for AOI
ne_region <- sf::st_read("data/bounds/ne_plus_ny_bounds.shp", quiet = TRUE)

# Set up output directory
model_dir <- tolower(gsub("-|_", "_", test_model))
outdir <- file.path("data/gcm_ne_subset", model_dir, test_scenario)

# Create directory if it doesn't exist
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

cat(sprintf("\nTEST: Processing model: %s, scenario: %s\n", test_model, test_scenario))
cat("Using historical years (1950-2014) for testing\n")

total_years <- length(historical_year_lookup)

# Loop through each year in the historical lookup vector
for(i in seq_along(historical_year_lookup)) {
  year_val <- historical_year_lookup[i]
  cat(sprintf("\n  Processing year: %d (%d of %d)\n", year_val, i, total_years))
  
  # wrapped the dl func in a try catch for error handling
  tryCatch({
    cat(sprintf("  Starting download for %s - %s - Year %d...\n", 
                test_model, test_scenario, year_val))
    
    # Execute the cmip6_dl function for this specific year
    cmip6::cmip6_dl(
      outdir = outdir,
      aoi = ne_region,
      models = test_model,
      scenarios = test_scenario,
      elements = "pr",
      year = year_val,
      latest = TRUE
    )
    
    cat(sprintf("  Download completed for %s - %s - Year %d\n", 
                test_model, test_scenario, year_val))
    
  }, error = function(e) {
    cat(sprintf("  Error downloading %s - %s - Year %d: %s\n", 
                test_model, test_scenario, year_val, e$message))
  })
  
  # Optional: Add user prompt to continue after each year (for testing)
  cat("Completed year. Press Enter to continue to next year...")
  readline()
}

cat("\nTest download sequence completed!\n")