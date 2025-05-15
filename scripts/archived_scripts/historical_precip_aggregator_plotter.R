library(terra)
library(jpeg)

# visualizer
source("aggregate_plot_visualizer.R")

#################
# FUNCTIONS

#' Load and Process Precipitation Data
#'
#' @description Loads 3D precipitation arrays from an RData file, calculates
#'   mean precipitation across the time series, and combines with observed data.
#' @param observed_data A 3D array containing observed precipitation values
#' @return A list of 2D matrices representing mean precipitation for each model and observed data
aggregate_array_to_matrix <- function(observed_data) {
  # Get all model variables that include "_historical_" in their name
  all_vars <- ls(envir = .GlobalEnv)
  model_vars <- all_vars[grepl("_historical_", all_vars)]
  
  # Create a list to store results
  result_list <- list()
  
  # Process each model array and the observed data array
  all_arrays <- c(model_vars, "cpc_annual_means_ne_subset")
  
  for (array_name in all_arrays) {
    # Get the current array
    if (array_name == "cpc_annual_means_ne_subset") {
      current_array <- observed_data
      output_name <- "observed_mean_precipitation"
    } else {
      current_array <- get(array_name, envir = .GlobalEnv)
      
      # Extract the model name for naming the result
      name_parts <- strsplit(array_name, "_historical_")[[1]]
      model_name <- name_parts[1]
      output_name <- paste(model_name, "historical_mean_precipitation", sep = "_")
    }
    
    # Extract dimnames for later reattachment
    array_dimnames <- dimnames(current_array)
    
    # Calculate mean across time dimension (third dimension)
    # This collapses the 3D array to a 2D matrix
    mean_matrix <- apply(current_array, c(1, 2), mean, na.rm = TRUE)
    mean_matrix[is.nan(mean_matrix)] <- NA
    
    # Reattach dimnames (longitude and latitude)
    dimnames(mean_matrix) <- list(
      longitude = array_dimnames[[1]],
      latitude = array_dimnames[[2]]
    )
    
    # Add to results list
    result_list[[output_name]] <- mean_matrix
  }
  
  return(result_list)
}

#' Create Mean Precipitation Plots
#'
#' @description Creates visualization of mean precipitation for each model and
#'   observed data, using a consistent scale for comparison.
#' @param data_list A list of 2D matrices representing mean precipitation
#' @return NULL (creates and saves plots as a side effect)
create_mean_precip_plots <- function(data_list) {
  # Get model names (all except observed data)
  model_keys <- names(data_list)[grepl("_mean_precipitation$", names(data_list)) & 
                                   !grepl("^observed", names(data_list))]
  model_names <- gsub("_historical_mean_precipitation$", "", model_keys)
  
  # Find the overall range of values for consistent scaling
  all_values <- c()
  for (matrix_name in names(data_list)) {
    matrix_data <- data_list[[matrix_name]]
    all_values <- c(all_values, as.vector(matrix_data))
  }
  
  # Calculate min and max with buffer
  min_val <- min(all_values, na.rm = TRUE)
  max_val <- max(all_values, na.rm = TRUE)
  range_buffer <- (max_val - min_val) * 0.001
  
  min_val <- min_val - range_buffer
  max_val <- max_val + range_buffer
  
  # Create output directory if it doesn't exist
  output_dir <- "figures/historical_total_annual_precip_comparison"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get observed data
  observed_data <- data_list[["observed_mean_precipitation"]]
  
  # Create a plot for each model, side by side with observed data
  for (model_name in model_names) {
    model_data <- data_list[[paste0(model_name, "_historical_mean_precipitation")]]
    
    # Format plot titles with time period
    observed_title <- "Observed Mean Precipitation 1950-2014"
    
    # Format model plot title with capitalized model name
    model_title_parts <- strsplit(model_name, "_")[[1]]
    model_title_parts <- toupper(model_title_parts)
    model_title <- paste(model_title_parts, collapse = " ")
    model_title <- paste(model_title, "Mean Precipitation 1950-2014")
    
    # Create output filename
    output_filename <- paste0(output_dir, "/", model_name, "_comparison.jpg")
    
    # Create plot with 1x2 layout
    jpeg(output_filename, width = 16, height = 8, units = "in", res = 300)
    par(mfrow = c(1, 2))
    
    # Plot observed data
    aggregate_plot_visualizer(
      observed_data,
      observed_title,
      min_max_range = c(min_val, max_val)
    )
    
    # Plot model data
    aggregate_plot_visualizer(
      model_data,
      model_title,
      min_max_range = c(min_val, max_val)
    )
    
    # Reset plot parameters and close device
    par(mfrow = c(1, 1))
    dev.off()
    
    cat("Saved comparison plot for", model_name, "to", output_filename, "\n")
  }
}

#################
# MAIN EXECUTION
# Load modeled precipitation data
load("rdata/modeled_precipitation_ne_subset.RData")
load("rdata/cpc_ne_annual_mean_precipitation.RData")

precip_aggregate_list <- aggregate_array_to_matrix(cpc_annual_means_ne_subset)

create_mean_precip_plots(precip_aggregate_list)
