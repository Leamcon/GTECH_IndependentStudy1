library(abind)
library(terra)
library(jpeg)

# User Specifications:
# running mean window duration
duration <- 10

# visualizer
source("comparative_visualizer.R")

#################
# FUNCTIONS

# Main IO Function
#   - loads rdata file of modeled precip data
#   - implements rm_range_diff_calc and grid arithmetic
#   - outputs new vars
cmip6_loader_processor <- function(observed_data, duration) {
  # load modeled data
  load("rdata/modeled_precipitation_ne_subset.RData", envir = .GlobalEnv)
  
  # grepl for only the historical model data
  all_vars <- ls(envir = .GlobalEnv)
  historical_model_vars <- all_vars[grepl("_historical_", all_vars)]
  historical_model_vars <- historical_model_vars[!grepl("^cpc_", historical_model_vars)]
  
  # inst empty list for results
  results <- list()
  
  # get dimnames from obs
  lon_attr <- dimnames(observed_data)[[1]]
  lat_attr <- dimnames(observed_data)[[2]]
  
  # calc running mean range for observed data
  observed_rm_range <- running_mean_range_calculator(observed_data, duration)
  
  # apply dimnames
  dimnames(observed_rm_range) <- list(longitude = lon_attr, latitude = lat_attr)
  
  # add observed data to results
  results$observed_rm_range <- observed_rm_range
  
  # process model data
  for (model_var in historical_model_vars) {
    # get model data
    model_data <- get(model_var)
    
    # extract model name
    name_parts <- strsplit(model_var, "_")[[1]]
    noname_index <- match("historical", name_parts)
    model_name <- paste(name_parts[1:noname_index - 1], collapse = "_")
    
    # calculate running mean range
    model_rm_range_grid <- running_mean_range_calculator(model_data, duration)
    
    # apply dimnames
    dimnames(model_rm_range_grid) <- list(longitude = lon_attr, latitude = lat_attr)
    
    # perform grid arithmetic
    comparison_metrics <- grid_arithmetic_calculator(observed_rm_range, model_rm_range_grid)
    
    # create new name for var and store
    key <- paste(model_name, "historical", sep = "_")
    results[[paste(key, "rm_range", sep = "_")]] <- model_rm_range_grid
    results[[paste(key, "difference", sep = "_")]] <- comparison_metrics$difference
    results[[paste(key, "bias_factor", sep = "_")]] <- comparison_metrics$bias_factor
  }
  
  return(results)
}

# Running Mean Range Calculation Function
#   - takes a 3D array of annual precip values & num specifying window size
#   - applies a running mean calculation to each matrix of the 3D array with specified window
#   - gets the difference of max and min values for each matrix cell across 3D array
#   - returns a matrix of these differences
running_mean_range_calculator <- function(ts_array, window) {
  # create an array of running means specified thru param 2
  running_mean_array <- apply(ts_array, c(1, 2), function(ts_slice){ 
    filter(ts_slice, rep(1/window, window), sides = 1)
  })
  # reset the dims after apply() usage
  running_mean_array <- aperm(running_mean_array, c(2, 3, 1))
  
  # generate a matrix of range diffs across z axis at each x,y
  # TODO: prevent creation of inf values - see frei_read_cpc_dat
  range_matrix <- apply(running_mean_array, c(1,2), function(rm_slice) {
    if(all(is.na(rm_slice))) {
      return(NA)
    } else {
      return(diff(range(rm_slice, na.rm = TRUE)))
    }
  })
  
  return(range_matrix)
}

# Grid Arithmetic Function -- DOUBLE CHECK
#   - takes rmr diff grids for observed values and model values
#   - returns comparisons for difference and bias factor
grid_arithmetic_calculator <- function(observed_grid, model_grid) {
  
  difference_grid <- (observed_grid - model_grid) / observed_grid
  bias_factor_grid <- observed_grid / model_grid
  
  # Return both metrics as a list
  return(list(
    difference = difference_grid,
    bias_factor = bias_factor_grid
  ))
}

# Plotting Function -- Updated to ensure consistent legend for RM range plots
#   - creates plots for specific model & period
#   - ensures observed and model RM range plots use the same legend scale
create_model_plots <- function(results) {
  # create a vector of model names using grepl to get unique names
  model_keys <- names(results)[grepl("_rm_range$", names(results)) & !grepl("^observed", names(results))]
  model_names <- gsub("_historical_rm_range$", "", model_keys) # gets only unique model names
  
  # Get all RM range matrices for global min/max calculation
  all_rm_ranges <- list()
  all_rm_ranges[[1]] <- results$observed_rm_range
  
  for (model_name in model_names) {
    all_rm_ranges[[length(all_rm_ranges) + 1]] <- results[[paste0(model_name, "_historical_rm_range")]]
  }
  
  # Calculate global min and max with extra care to avoid missing extreme values
  # First flatten all matrices to ensure we don't miss any values
  all_values <- unlist(all_rm_ranges)
  
  # Remove NA, NaN, and Inf values before calculating min/max
  all_values <- all_values[is.finite(all_values)]
  
  # Calculate global min and max
  global_min <- min(all_values, na.rm = TRUE)
  global_max <- max(all_values, na.rm = TRUE)
  
  # Add a small buffer to ensure max values are included in the range (prevents white cells)
  global_max <- global_max * 1.01  # Add 1% buffer to the max value
  
  # Print the min/max values for debugging
  cat("Global min value for RM range plots:", global_min, "\n")
  cat("Global max value for RM range plots:", global_max, "\n")
  
  for (model_name in model_names) {
    # extract the data for each 2x2 plot matrix
    observed_data <- results$observed_rm_range
    model_rm_data <- results[[paste0(model_name, "_historical_rm_range")]]
    difference_data <- results[[paste0(model_name, "_historical_difference")]]
    bias_factor_data <- results[[paste0(model_name, "_historical_bias_factor")]]
    
    # Debug: Print max value in observed data for this model comparison
    obs_max <- max(observed_data, na.rm = TRUE)
    model_max <- max(model_rm_data, na.rm = TRUE)
    cat(model_name, "- Observed max:", obs_max, "Model max:", model_max, "\n")
    
    # create dir
    if (!dir.exists("figures/updated_running_mean_figures")) {
      dir.create("figures/updated_running_mean_figures", recursive = TRUE)
    }
    
    # set up the plot matrix
    output_filename <- paste("figures/updated_running_mean_figures/", model_name, "_rm_range_diff_and_bias.jpg", sep = "")
    jpeg(output_filename, width = 10, height = 8, units = "in", res = 300)
    par(mfrow = c(2, 2))
    
    # generate a plot title
    plot_title <- toupper(paste(strsplit(model_name, "_")[[1]], collapse = " "))
    
    # apply the viz function with consistent min/max values for RM range plots
    observed_plot <- multi_use_visualizer(
      observed_data, 
      paste0("Observed RM Range (", duration, "-year window)"),
      min_val = global_min,
      max_val = global_max
    )
    
    model_plot <- multi_use_visualizer(
      model_rm_data, 
      paste0(plot_title, " RM Range (", duration, "-year window)"),
      min_val = global_min,
      max_val = global_max
    )
    
    # For difference and bias plots, use default legends as they represent different metrics
    diff_plot <- multi_use_visualizer(
      difference_data, 
      paste0(plot_title, " Difference from Observed")
    )
    
    bias_plot <- multi_use_visualizer(
      bias_factor_data, 
      paste0(plot_title, " Bias Factor")
    )
    
    # clean up
    par(mfrow = c(1, 1))
    dev.off()
    cat("Saved visualization for", model_name, "to", output_filename, "\n")
  }
}

#################
# MAIN EXECUTION

load("rdata/cpc_ne_annual_mean_precipitation.RData")

rm_derived_matrices <- cmip6_loader_processor(cpc_annual_means_ne_subset, duration)

create_model_plots(rm_derived_matrices)
