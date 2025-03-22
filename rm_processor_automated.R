library(abind)
library(ggplot2)
library(gridExtra)
library(sf)

# User Specifications:
# running mean window duration
duration <- 10

# visualizer
source("rm_range_matrix_visualizer.R")

#################
# FUNCTIONS

cmip6_loader_processor <- function(observed_data, duration) {
  # load modeled data
  load("rdata/modeled_precipitation_ne_subset.RData", envir = .GlobalEnv)
  
  # get list model 3D array vars
  all_vars <- ls(envir = .GlobalEnv)
  model_vars <- all_vars[grepl("_annual_means_ne_subset$", all_vars)]
  model_vars <- model_vars[!grepl("^cpc_", model_vars)]
  
  # inst empty list for results
  results <- list()
  
  # get dimnames from obs
  lon_attr <- dimnames(observed_data)[[1]]
  lat_attr <- dimnames(observed_data)[[2]]
  
  # calc running mean range for observed data
  observed_rm_range_grid <- running_mean_range_calculator(observed_data, duration)
  
  # apply dimnames
  dimnames(observed_rm_range_grid) <- list(longitude = lon_attr, latidude = lat_attr)
  
  # add observed data to results
  results$observed <- observed_rm_range_grid
  
  # process model data
  for (model_var in model_vars) {
    # get model data
    model_data <- get(model_var, envir = .GlobalEnv)
    
    # extract model name and period inc. handling for multipart names
    name_parts <- strsplit(model_var, "_")[[1]]
    model_name <- name_parts[1]
    
    if (name_parts[2] != "annual") {
      model_name <- paste(model_name, name_parts[2], sep = "_")
      period <- name_parts[3]
    } else {
      period <- name_parts[2]
    }
    
    # period name cleanup
    if (period == "annual") {
      period <- name_parts[3]
    }
    
    # calculate running mean range
    model_rm_range_grid <- running_mean_range_calculator(model_data, duration)
    
    # apply dimnames
    dimnames(model_rm_range_grid) <- list(longitude = lon_attr, latidude = lat_attr)
    
    # perform grid arithmetic
    comparison_metrics <- grid_arithmetic_calculator(observed_rm_range_grid, model_rm_range_grid)
    
    # create new name for var and store
    key <- paste(model_name, period, sep = "_")
    results[[paste0(key, "_rm_range")]] <- model_rm_range_grid
    results[[paste0(key, "_difference")]] <- comparison_metrics$difference
    results[[paste0(key, "_bias_factor")]] <- comparison_metrics$bias_factor
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
    diff(range(rm_slice, na.rm = TRUE))
  })
  
  return(range_matrix)
}

# Grid Arithmetic Function -- DOUBLE CHECK
#   - takes rmr diff grids for observed values and model values
#   - returns comparisons for difference and bias factor
grid_arithmetic_calculator <- function(observed_grid, model_grid) {

  difference_grid <- observed_grid - model_grid
  bias_factor_grid <- observed_grid / model_grid
  
  # Return both metrics as a list
  return(list(
    difference = difference_grid,
    bias_factor = bias_factor_grid
  ))
}

# Plotting Function -- DOUBLE CHECK
#   - creates plots for specific model & period
create_model_plots <- function(results, model_name, period) {
  # Get the relevant results
  key <- paste(model_name, period, sep = "_")
  
  observed_grid <- results$observed
  model_grid <- results[[paste0(key, "_rm_range")]]
  difference_grid <- results[[paste0(key, "_difference")]]
  bias_factor_grid <- results[[paste0(key, "_bias_factor")]]
  
  # Create plots
  observed_rm_plot <- rm_range_matrix_visualizer(observed_grid,
                                                 "Running Mean Range of Observed Annual Precipitation",
                                                 "mm/day",
                                                 "blue_white")
  
  model_title <- paste("Running Mean Range of Modeled Annual Precipitation", 
                       "\nModel:", model_name, "Period:", period)
  
  modeled_rm_plot <- rm_range_matrix_visualizer(model_grid,
                                                model_title,
                                                "mm/day",
                                                "blue_white")
  
  difference_rm_plot <- rm_range_matrix_visualizer(difference_grid,
                                                   paste("Difference in Running Mean Ranges\nObserved -", model_name),
                                                   "mm/day",
                                                   "diff")
  
  factor_rm_plot <- rm_range_matrix_visualizer(bias_factor_grid,
                                               paste("Model Bias Factor\n", model_name),
                                               "Bias Factor",
                                               "ratio")
  
  # Return the plots as a list
  return(list(
    observed = observed_rm_plot,
    modeled = modeled_rm_plot,
    difference = difference_rm_plot,
    bias_factor = factor_rm_plot
  ))
}

# Function to create a comparison plot of all models
create_comparison_plot <- function(results) {
  # Get all model keys (excluding observed and metric results)
  model_keys <- names(results)[grepl("_rm_range$", names(results))]
  
  # Create a list to store plots
  plots <- list()
  
  # Add observed plot first
  plots[[1]] <- rm_range_matrix_visualizer(results$observed,
                                           "Running Mean Range of Observed Annual Precipitation",
                                           "mm/day",
                                           "blue_white")
  
  # Create plots for each model
  for (i in seq_along(model_keys)) {
    model_key <- model_keys[i]
    # Extract model name and period
    parts <- strsplit(gsub("_rm_range$", "", model_key), "_")[[1]]
    
    # Last part is the period
    period <- parts[length(parts)]
    # Everything before is the model name
    model_name <- paste(parts[-length(parts)], collapse = "_")
    
    # Create plot title
    title <- paste0(model_name, " (", period, ")")
    
    # Create plot and add to list
    plots[[i + 1]] <- rm_range_matrix_visualizer(results[[model_key]],
                                                 title,
                                                 "mm/day",
                                                 "blue_white")
  }
  
  # Determine layout
  n_plots <- length(plots)
  n_cols <- min(3, n_plots)
  n_rows <- ceiling(n_plots / n_cols)
  
  # Combine plots
  return(do.call(grid.arrange, c(plots, ncol = n_cols)))
}

#################
# MAIN EXECUTION

load("rdata/cpc_ne_annual_mean_precipitation.RData")

results <- cmip6_loader_processor(cpc_annual_means_ne_subset, duration)