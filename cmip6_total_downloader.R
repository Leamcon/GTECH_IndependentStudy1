# updated CMIP6 download script
# designed to pull data from all models in the TDS CMIP6 repo
# uses a predefined .shp as aoi
# leverages the mt climate office cmip6 package
# found at https://github.com/mt-climate-office/cmip6

library(cmip6)
library(sf)

# calls my date continuity checker function
source("gcm_date_continuity_checker.r")

# model lookup vector
models <- c(
  #"ACCESS-CM2", "ACCESS-ESM1-5", "BCC-CSM2-MR", "CESM2", 
  #"CESM2-WACCM", "CMCC-CM2-SR5", 
  #"CMCC-ESM2", "CNRM-CM6-1",
  #"CNRM-ESM2-1", "CanESM5", "EC-Earth3", 
  "EC-Earth3-Veg-LR",
  "FGOALS-g3", "GFDL-CM4", "GFDL-CM4_gr2", "GFDL-ESM4",
  "GISS-E2-1-G", "HadGEM3-GC31-LL", "HadGEM3-GC31-MM",
  "IITM-ESM", "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
  "KACE-1-0-G", "KIOST-ESM", "MIROC-ES2L", "MIROC6",
  "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NESM3",
  "NorESM2-LM", "NorESM2-MM", "TaiESM1", "UKESM1-0-LL"
)

# scenario vector
scenarios <- c("historical", "ssp585")

# function convert model names to proper directory names
# uses gsub to set dashes to underscores
format_model_dir <- function(model_name) {
  tolower(gsub("-|_", "_", model_name))
}

# user prompt function
prompt_continue <- function() {
  response <- readline(prompt="Continue to next download? (Y/N): ")
  return(tolower(response) == "y")
}

# downloader function takes a model name, scenario name, and aoi sf as input
# nested loops
#  - first loops through model vector
#  - second loops through scenario
# uses model and scenario names to create dirs if none are found in the output dir
# includes user prompting
downloader <- function(models, scenarios, area_of_interest) {
  for (model in models) {
    for (scenario in scenarios) {
      # this implements the automatic directory creation
      model_dir <- format_model_dir(model)
      outdir <- file.path("data/gcm_ne_subset", model_dir, scenario)
      
      # checks for dir and creates if none found
      dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
      
      cat(sprintf("\nProcessing model: %s, scenario: %s\n", model, scenario))
      
      # wrapped the dl func in a try catch for error handling
      tryCatch({
        cmip6::cmip6_dl(
          outdir = outdir,
          aoi = area_of_interest,
          models = model,
          scenarios = scenario,
          elements = "pr",
          latest = TRUE
        )
        
        # check the time series is complete
        print(gcm_date_continuity_checker(outdir))
        
        cat(sprintf("Download completed for %s - %s\n", model, scenario))
        
      }, error = function(e) {
        cat(sprintf("Error downloading %s - %s: %s\n", 
                    model, scenario, e$message))
      })
      
      # prompt for continuation
      if (!prompt_continue()) {
        cat("Download process terminated by user.\n")
        return(invisible(NULL))
      }
    }
  }
  cat("All downloads completed successfully!\n")
}

# region shape for aoi: new england states plus ny state
ne_region <- sf::st_read("data/bounds/ne_plus_ny_bounds.shp", quiet = TRUE)

downloader(models, scenarios, ne_region)

