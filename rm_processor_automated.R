library(abind)
library(terra)
library(jpeg)

# User Specifications:
# running mean window duration
duration <- 10

# visualizer
source("multi_use_visualizer.R")

#################
# FUNCTIONS

# Main IO Function
#   - loads rdata file of modeled precip data
#   - implements rm_range_diff_calc and grid arithmetic
#   - outputs new vars
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
create_model_plots <- function(results) {
  # grep to secure only model data, observed can be handled using $
  model_keys <- names(results)[grepl("_rm_range$", names(results)) & !grepl("^observed", names(results))]
  
  # get the model names for use later
  model_names <- gsub("_rm_range$", "", model_keys)
  
  for (model_name in model_names) {
    # extract the data for each 2x2 plot matrix
    observed_data <- results$observed
    model_rm_data <- results[[paste0(model_name, "_rm_range")]]
    difference_data <- results[[paste0(model_name, "_difference")]]
    bias_factor_data <- results[[paste0(model_name, "_bias_factor")]]
    
    # create dir
    if (!dir.exists("figures/running_mean_figures")) {
      dir.create("figures/running_mean_figures", recursive = TRUE)
    }
    
    # set up the plot matrix
    output_filename <- paste0("figures/running_mean_figures/", model_name, "_rm_range_diff_and_bias.jpg")
    jpeg(output_filename, width = 10, height = 8, units = "in", res = 300)
    par(mfrow = c(2, 2))
    
    
    # apply the viz function
    observed_plot <- multi_use_visualizer(observed_data, paste0("Observed RM Range (", duration, "-year window)"))
    model_plot <- multi_use_visualizer(model_rm_data, paste0(model_name, " RM Range (", duration, "-year window)"))
    diff_plot <- multi_use_visualizer(difference_data, paste0(model_name, " Difference from Observed"))
    bias_plot <- multi_use_visualizer(bias_factor_data, paste0(model_name, " Bias Factor"))
    
    # clean up
    par(mfrow = c(1, 1))
    dev.off()
    cat("Saved visualization for", model_name, "to", output_filename, "\n")
  }
}

#################
# MAIN EXECUTION

load("rdata/cpc_ne_annual_mean_precipitation.RData")

results <- cmip6_loader_processor(cpc_annual_means_ne_subset, duration)

create_model_plots(results)
