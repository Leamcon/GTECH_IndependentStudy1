# Mean Precipitation Analysis Script
#   - implements functions to compute mean precipitation from 3D arrays
#   - applies functions to observed and all model data read in from .RData
#   - implements a function to compute normalized difference between observed mean and model means
#   - implements a function to compute bias factor between observed mean and model means
#   - implements a function that creates 4 panel plots with inset histograms for each obs/model pair

library(terra)
library(jpeg)

# visualizers
source("scripts/visualizers/aggregate_plot_visualizer.R")
source("scripts/visualizers/difference_visualizer.R")
source("scripts/visualizers/bias_factor_visualizer.R")

#################
# FUNCTIONS

# Load and Process Precipitation Data
#  - Loads 3D precipitation arrays from an RData file
#  - calculates mean precipitation across the time series, combines with observed data
#  - returns a list of 2D matrices representing mean precipitation, differences, and bias factors
aggregate_array_to_matrix_with_metrics <- function(observed_data) {

  all_vars <- ls(envir = .GlobalEnv)
  model_vars <- all_vars[grepl("_historical_", all_vars)]
  
  # Create a list to store results
  result_list <- list()
  
  # Process observed data first
  # Extract dimnames for later reattachment
  array_dimnames <- dimnames(observed_data)
  
  # Calculate mean across time dimension (third dimension)
  observed_mean_matrix <- apply(observed_data, c(1, 2), mean, na.rm = TRUE)
  observed_mean_matrix[is.nan(observed_mean_matrix)] <- NA
  
  # Reattach dimnames (longitude and latitude)
  dimnames(observed_mean_matrix) <- list(
    longitude = array_dimnames[[1]],
    latitude = array_dimnames[[2]]
  )
  
  result_list[["observed_mean_precipitation"]] <- observed_mean_matrix
  
  # Process each model array
  for (array_name in model_vars) {

    current_array <- get(array_name, envir = .GlobalEnv)
    
    # Extract the model name for naming the result
    name_parts <- strsplit(array_name, "_")[[1]]
    noname_index <- match("historical", name_parts)
    model_name <- paste(name_parts[1:noname_index - 1], collapse = "_")
    
    # Create output name
    key <- paste(model_name, "historical", sep = "_")
    
    # Calculate mean across time dimension
    model_mean_matrix <- apply(current_array, c(1, 2), mean, na.rm = TRUE)
    model_mean_matrix[is.nan(model_mean_matrix)] <- NA
    
    # Reattach dimnames
    dimnames(model_mean_matrix) <- list(
      longitude = array_dimnames[[1]],
      latitude = array_dimnames[[2]]
    )
    
    comparison_metrics <- grid_arithmetic_calculator(observed_mean_matrix, model_mean_matrix)
    
    # Add to results list
    result_list[[paste(key, "mean_precipitation", sep = "_")]] <- model_mean_matrix
    result_list[[paste(key, "difference", sep = "_")]] <- comparison_metrics$difference
    result_list[[paste(key, "bias_factor", sep = "_")]] <- comparison_metrics$bias_factor
  }
  
  return(result_list)
}

# Grid Arithmetic Function
#   - takes mean precipitation grids for observed values and model values
#   - returns comparisons for difference and bias factor
grid_arithmetic_calculator <- function(observed_grid, model_grid) {
  difference_grid <- (model_grid - observed_grid) / observed_grid
  bias_factor_grid <- observed_grid / model_grid
  
  # Return both metrics as a list
  return(list(
    difference = difference_grid,
    bias_factor = bias_factor_grid
  ))
}

# Final plotting function with histogram insets and batch processing capabilities
#   - takes the list of matrices as input
#   - iterates through the list and creates 4 panel plots with inset histograms
create_mean_precip_plots_with_histograms <- function(results) {

  model_keys <- names(results)[grepl("_mean_precipitation$", names(results)) & !grepl("^observed", names(results))]
  model_names <- gsub("_historical_mean_precipitation$", "", model_keys)
  
  # Find the overall range for mean values to use consistent scale
  all_mean_values <- c(results$observed_mean_precipitation)
  for (model_name in model_names) {
    model_mean_data <- results[[paste0(model_name, "_historical_mean_precipitation")]]
    all_mean_values <- c(all_mean_values, model_mean_data)
  }
  min_val <- min(all_mean_values, na.rm = TRUE)
  max_val <- max(all_mean_values, na.rm = TRUE)
  range_buffer <- (max_val - min_val) * 0.001
  mean_min <- min_val - range_buffer
  mean_max <- max_val + range_buffer
  
  # Calculate appropriate scales for difference and bias factor based on actual data

  all_diff_values <- c()
  all_bias_values <- c()
  
  # Sample each model's difference and bias factor matrices
  for (model_name in model_names) {

    diff_data <- results[[paste0(model_name, "_historical_difference")]]
    diff_flat <- as.vector(diff_data)
    diff_flat <- diff_flat[!is.na(diff_flat)]
    all_diff_values <- c(all_diff_values, diff_flat)
    
    bias_data <- results[[paste0(model_name, "_historical_bias_factor")]]
    bias_flat <- as.vector(bias_data)
    bias_flat <- bias_flat[!is.na(bias_flat)]
    all_bias_values <- c(all_bias_values, bias_flat)
  }
  
  # Calculate min and max for difference with 5% buffer on each end
  diff_range <- max(all_diff_values, na.rm = TRUE) - min(all_diff_values, na.rm = TRUE)
  diff_buffer <- diff_range * 0.05
  diff_min <- min(all_diff_values, na.rm = TRUE) - diff_buffer
  diff_max <- max(all_diff_values, na.rm = TRUE) + diff_buffer
  
  # For bias factor, ensure the minimum is always >= 0
  bias_range <- max(all_bias_values, na.rm = TRUE) - min(all_bias_values, na.rm = TRUE)
  bias_buffer <- bias_range * 0.05
  bias_min <- max(0, min(all_bias_values, na.rm = TRUE) - bias_buffer)
  bias_max <- max(all_bias_values, na.rm = TRUE) + bias_buffer
  
  # Round the limits for cleaner plotting scales
  diff_min <- floor(diff_min * 10) / 10
  diff_max <- ceiling(diff_max * 10) / 10
  bias_min <- floor(bias_min * 10) / 10  
  bias_max <- ceiling(bias_max * 10) / 10
  
  if (!dir.exists("figures/final_mean_precipitation_figures")) {
    dir.create("figures/final_mean_precipitation_figures", recursive = TRUE)
  }
  
  # Helper function to create custom histogram axes using text and segments
  create_custom_histogram_axis <- function(x_min, x_max, plot_type = "mean_precip") {

    if (plot_type == "mean_precip") {
      # For mean precipitation plots (observed and model)
      tick_range <- max_val - min_val
      if (tick_range > 3) {
        tick_positions <- seq(min_val, max_val, length.out = 5)
        tick_labels <- as.character(round(tick_positions, 1))
      } else {
        tick_positions <- seq(min_val, max_val, length.out = 4)
        tick_labels <- as.character(round(tick_positions, 2))
      }
    } else if (plot_type == "difference") {
      # For normalized difference plot - create fixed number of ticks
      tick_count <- 5
      tick_positions <- seq(diff_min, diff_max, length.out = tick_count)
      tick_labels <- as.character(round(tick_positions, 2))
    } else if (plot_type == "bias") {
      tick_count <- 5
      tick_positions <- seq(bias_min, bias_max, length.out = tick_count)
      if (bias_max > 10) {
        tick_labels <- as.character(round(tick_positions, 1))
      } else {
        tick_labels <- as.character(round(tick_positions, 2))
      }
    }
    
    par(xpd = TRUE)
    
    usr <- par("usr")
    
    # Position relative to the plot height
    plot_height <- usr[4] - usr[3]
    y_bottom <- usr[3] - 0.02 * plot_height  # Adjust clearance below plot
    y_tick_length <- 0.03 * plot_height      # Length of tick marks
    
    # Use a fixed distance below the ticks rather than relative to the plot
    text_y_pos <- y_bottom - 0.08 * plot_height  
    
    # Draw segments for tick marks
    for (i in 1:length(tick_positions)) {
      segments(
        x0 = tick_positions[i], 
        y0 = y_bottom, 
        x1 = tick_positions[i], 
        y1 = y_bottom + y_tick_length, 
        lwd = 1.2
      )
    }
    
    # Add text labels
    for (i in 1:length(tick_positions)) {
      text(
        x = tick_positions[i], 
        y = text_y_pos, 
        labels = tick_labels[i],
        cex = 0.5,
        adj = c(0.5, 1),
        font = 2
      )
    }
    
    par(xpd = FALSE)
  }
  
  # Process each model
  for (model_name in model_names) {

    observed_data <- results$observed_mean_precipitation
    model_mean_data <- results[[paste0(model_name, "_historical_mean_precipitation")]]
    difference_data <- results[[paste0(model_name, "_historical_difference")]]
    bias_factor_data <- results[[paste0(model_name, "_historical_bias_factor")]]
    
    # Set up the output file
    output_filename <- paste0("figures/final_mean_precipitation_figures/", model_name, "_mean_diff_and_bias_with_hist.jpg")
    jpeg(output_filename, width = 10, height = 8, units = "in", res = 300)
    
    plot_title <- toupper(paste(strsplit(model_name, "_")[[1]], collapse = " "))
    
    # Helper function to position a grid plot in its quadrant
    position_grid_plot <- function(quadrant) {
      # Define the dimensions for each quadrant
      # Format: c(left, right, bottom, top)
      quadrant_dims <- list(
        "1" = c(0.0, 0.5, 0.5, 1.0),  # Top-left
        "2" = c(0.5, 1.0, 0.5, 1.0),  # Top-right
        "3" = c(0.0, 0.5, 0.0, 0.5),  # Bottom-left
        "4" = c(0.5, 1.0, 0.0, 0.5)   # Bottom-right
      )
      
      # Set the figure region for this quadrant
      par(fig = quadrant_dims[[as.character(quadrant)]], mar = c(4, 4, 3, 2))
      
      # Only use new=TRUE for quadrants after the first
      if (quadrant > 1) {
        par(new = TRUE)
      }
    }
    
    # Helper function to position a histogram inset in each plot
    position_histogram_inset <- function(quadrant) {
      # Define the base inset dimensions
      inset_width <- 0.165
      inset_height <- 0.32  # Increase to provide space for labels
      
      # Base coordinates for each quadrant
      base_coords <- list(
        "1" = c(0.0, 0.5, 0.5, 1.0),  # Top-left quadrant
        "2" = c(0.5, 1.0, 0.5, 1.0),  # Top-right quadrant
        "3" = c(0.0, 0.5, 0.0, 0.5),  # Bottom-left quadrant
        "4" = c(0.5, 1.0, 0.0, 0.5)   # Bottom-right quadrant
      )
      
      # Get base coordinates for this quadrant
      base <- base_coords[[as.character(quadrant)]]
      
      # Position the inset in the upper-left corner
      inset_coords <- c(
        base[1] + 0.04,                # Increase to move right
        base[1] + 0.12 + inset_width,  # Match left edge
        base[3] + (base[4] - base[3]) * 0.58,  # Lower shifts down
        base[4] - 0.08                 # Smaller values = taller plot
      )
      
      # Use minimal margins for the histogram insets
      par(fig = inset_coords, new = TRUE, mar = c(1.5, 0.1, 0.1, 0.1))
    }
    
    # 1. Observed Mean Precipitation grid (Quadrant 1: Top-left)
    position_grid_plot(1)
    aggregate_plot_visualizer(
      observed_data, 
      paste0("Observed Mean Precipitation"),
      min_max_range = c(mean_min, mean_max)
    )
    
    # Add inset histogram for observed data
    observed_flat <- as.vector(observed_data)
    observed_flat <- observed_flat[!is.na(observed_flat)]
    position_histogram_inset(1)
    
    hist(observed_flat, 
         main = "", 
         xlab = "", 
         ylab = "",
         col = "lightblue", 
         border = "black",
         xlim = c(mean_min, mean_max),
         breaks = 20,
         axes = FALSE)
    
    create_custom_histogram_axis(mean_min, mean_max, plot_type = "mean_precip")
    
    # 2. Model Mean Precipitation grid (Quadrant 2: Top-right)
    position_grid_plot(2)
    aggregate_plot_visualizer(
      model_mean_data, 
      paste0(plot_title, " Mean Precipitation"),
      min_max_range = c(mean_min, mean_max)
    )
    
    # Add inset histogram for model data
    model_flat <- as.vector(model_mean_data)
    model_flat <- model_flat[!is.na(model_flat)]
    position_histogram_inset(2)
    
    hist(model_flat, 
         main = "", 
         xlab = "", 
         ylab = "",
         col = "lightblue", 
         border = "black",
         xlim = c(mean_min, mean_max),
         breaks = 20,
         axes = FALSE)  # No axes
    
    create_custom_histogram_axis(mean_min, mean_max, plot_type = "mean_precip")
    
    # 3. Normalized difference grid (Quadrant 3: Bottom-left)
    position_grid_plot(3)
    difference_visualizer(
      difference_data, 
      "Normalized Difference from Observed",
      min_max_range = c(diff_min, diff_max)
    )
    
    # Add inset histogram for difference data
    diff_flat <- as.vector(difference_data)
    diff_flat <- diff_flat[!is.na(diff_flat)]  # Remove NA values
    position_histogram_inset(3)
    
    hist(diff_flat, 
         main = "", 
         xlab = "", 
         ylab = "",
         col = "lightblue",
         border = "black",
         xlim = c(diff_min, diff_max),
         breaks = 20,
         axes = FALSE)  # No axes
    
    create_custom_histogram_axis(diff_min, diff_max, plot_type = "difference")
    
    # 4. Bias factor grid (Quadrant 4: Bottom-right)
    position_grid_plot(4)
    bias_factor_visualizer(
      bias_factor_data, 
      "Bias Factor",
      min_max_range = c(bias_min, bias_max)
    )
    
    # Add inset histogram for bias factor data
    bias_flat <- as.vector(bias_factor_data)
    bias_flat <- bias_flat[!is.na(bias_flat)]  # Remove NA values
    position_histogram_inset(4)
    
    hist(bias_flat, 
         main = "", 
         xlab = "", 
         ylab = "",
         col = "lightblue",
         border = "black",
         xlim = c(bias_min, bias_max),
         breaks = 20,
         axes = FALSE)  # No axes
    
    create_custom_histogram_axis(bias_min, bias_max, plot_type = "bias")
    
    dev.off()
    cat("Saved visualization for", model_name, "to", output_filename, "\n")
  }
}

#################
# MAIN EXECUTION

load("rdata/modeled_precipitation_ne_subset.RData")
load("rdata/cpc_ne_annual_mean_precipitation.RData")

mean_derived_matrices <- aggregate_array_to_matrix_with_metrics(cpc_annual_means_ne_subset)

create_mean_precip_plots_with_histograms(mean_derived_matrices)

save(mean_derived_matrices, file = "rdata/total_mean_precipitation_matrices_list.RData")
