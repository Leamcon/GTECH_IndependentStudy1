# Running Mean Range Analysis Script
#   - implements a function to compute running mean range within a user specified window
#   - applies function to observed and all model data read in from .RData
#   - implements a function to compute normalized difference between observed rm range and the rm range of all models
#   - implements a function to compute bias factor between observed rm range and the rm range of all models
#   - implements a function that create 4 panel plots with inset histograms for each obs/model pair

library(abind)
library(terra)
library(jpeg)

# User Specifications:
# running mean window duration
duration <- 10

# visualizers
source("scripts/visualizers/rm_range_visualizer.R")
source("scripts/visualizers/difference_visualizer.R")
source("scripts/visualizers/bias_factor_visualizer.R")

#################
# FUNCTIONS

# Main IO Function
#   - loads rdata file of modeled precip data
#   - implements running_mean_range_calculator and grid arithmetic
#   - outputs matrices for each model + obs in list format
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
  
  # generate a matrix of range diffs across z axis at each x,y with na/inf handling
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
create_model_plots_with_histograms <- function(results) {

  model_keys <- names(results)[grepl("_rm_range$", names(results)) & !grepl("^observed", names(results))]
  model_names <- gsub("_historical_rm_range$", "", model_keys) # gets only unique model names
  
  # find the overall range for running mean values to use consistent scale
  all_rm_ranges <- c(results$observed_rm_range)
  for (model_name in model_names) {
    model_rm_data <- results[[paste0(model_name, "_historical_rm_range")]]
    all_rm_ranges <- c(all_rm_ranges, model_rm_data)
  }
  min_val <- min(all_rm_ranges, na.rm = TRUE)
  max_val <- max(all_rm_ranges, na.rm = TRUE)
  range_buffer <- (max_val - min_val) * 0.001
  rm_range_min <- min_val - range_buffer
  rm_range_max <- max_val + range_buffer
  
  # Set fixed scales for difference and bias factor
  diff_min <- -1
  diff_max <- 1.5
  bias_min <- 0
  bias_max <- 9
  
  # Create output directory if it doesn't exist
  if (!dir.exists("figures/final_running_mean_figures")) {
    dir.create("figures/final_running_mean_figures", recursive = TRUE)
  }
  
  # Helper function to create custom histogram axes using text and segments
  create_custom_histogram_axis <- function(x_min, x_max, plot_type = "rm_range") {
    # Set specific tick positions based on plot type
    if (plot_type == "rm_range") {
      # For RM Range plots (observed and model)
      tick_positions <- c(0.5, 1.0, 1.5)
      tick_labels <- c("0.5", "1.0", "1.5")
    } else if (plot_type == "difference") {
      # For normalized difference plot
      tick_positions <- c(-1, -0.5, 0, 0.5, 1.0, 1.5)
      tick_labels <- c("-1", "-0.5", "0", "0.5", "1.0", "1.5")
    } else if (plot_type == "bias") {
      # For bias factor plot
      tick_positions <- c(0, 2, 4, 6, 8)
      tick_labels <- c("0", "2", "4", "6", "8")
    }
    
    par(xpd = TRUE)
    
    usr <- par("usr")
    
    # position relative to the plot height
    plot_height <- usr[4] - usr[3]
    y_bottom <- usr[3] - 0.02 * plot_height  # adjust clearance below plot
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

    observed_data <- results$observed_rm_range
    model_rm_data <- results[[paste0(model_name, "_historical_rm_range")]]
    difference_data <- results[[paste0(model_name, "_historical_difference")]]
    bias_factor_data <- results[[paste0(model_name, "_historical_bias_factor")]]
    
    output_filename <- paste0("figures/final_running_mean_figures/", model_name, "_rm_range_diff_and_bias_with_hist.jpg")
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
      inset_height <- 0.32  # increase to provide space for labels
      
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
        base[1] + 0.04,                # increase to move right
        base[1] + 0.12 + inset_width,  # match left edge
        base[3] + (base[4] - base[3]) * 0.58,  # lower shifts down
        base[4] - 0.08                 # smaller values = taller plot
      )
      
      # Use minimal margins for the histogram insets
      par(fig = inset_coords, new = TRUE, mar = c(1.5, 0.1, 0.1, 0.1))
    }
    
    # 1. Observed RM Range grid (Quadrant 1: Top-left)
    position_grid_plot(1)
    rm_range_visualizer(
      observed_data, 
      paste0("Observed RM Range"),
      min_max_range = c(rm_range_min, rm_range_max)
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
         xlim = c(rm_range_min, rm_range_max),
         breaks = 20,
         axes = FALSE)
    
    create_custom_histogram_axis(rm_range_min, rm_range_max, plot_type = "rm_range")
    
    # 2. Model RM Range grid (Quadrant 2: Top-right)
    position_grid_plot(2)
    rm_range_visualizer(
      model_rm_data, 
      paste0(plot_title, " RM Range"),
      min_max_range = c(rm_range_min, rm_range_max)
    )
    
    # Add inset histogram for model data
    model_flat <- as.vector(model_rm_data)
    model_flat <- model_flat[!is.na(model_flat)]
    position_histogram_inset(2)
    
    hist(model_flat, 
         main = "", 
         xlab = "", 
         ylab = "",
         col = "lightblue", 
         border = "black",
         xlim = c(rm_range_min, rm_range_max),
         breaks = 20,
         axes = FALSE)
    
    # Add custom x-axis with text and segments
    create_custom_histogram_axis(rm_range_min, rm_range_max, plot_type = "rm_range")
    
    # 3. Normalized difference grid (Quadrant 3: Bottom-left)
    position_grid_plot(3)
    difference_visualizer(
      difference_data, 
      "Normalized Difference from Observed"
    )
    
    # Add inset histogram for difference data
    diff_flat <- as.vector(difference_data)
    diff_flat <- diff_flat[!is.na(diff_flat)]
    position_histogram_inset(3)
    
    hist(diff_flat, 
         main = "", 
         xlab = "", 
         ylab = "",
         col = "lightblue",
         border = "black",
         xlim = c(diff_min, diff_max),
         breaks = 20,
         axes = FALSE)
    
    create_custom_histogram_axis(diff_min, diff_max, plot_type = "difference")
    
    # 4. Bias factor grid (Quadrant 4: Bottom-right)
    position_grid_plot(4)
    bias_factor_visualizer(
      bias_factor_data, 
      "Bias Factor"
    )
    
    # Add inset histogram for bias factor data
    bias_flat <- as.vector(bias_factor_data)
    bias_flat <- bias_flat[!is.na(bias_flat)]
    position_histogram_inset(4)
    
    hist(bias_flat, 
         main = "", 
         xlab = "", 
         ylab = "",
         col = "lightblue",
         border = "black",
         xlim = c(bias_min, bias_max),
         breaks = 20,
         axes = FALSE)
    
    create_custom_histogram_axis(bias_min, bias_max, plot_type = "bias")
    
    dev.off()
    cat("Saved visualization for", model_name, "to", output_filename, "\n")
  }
}

#################
# MAIN EXECUTION

load("rdata/cpc_ne_annual_mean_precipitation.RData")

rm_derived_matrices <- cmip6_loader_processor(cpc_annual_means_ne_subset, duration)

create_model_plots_with_histograms(rm_derived_matrices)

save(rm_derived_matrices, file = "rdata/rm_derived_matrices_list.RData")

#################
# VALIDATION (uncomment when needed)
# check ranges across similar grids for use in viz standardization
# difference_keys <- names(rm_derived_matrices)[grepl("_difference$", names(rm_derived_matrices))]
# difference_matrices <- rm_derived_matrices[difference_keys]
# bias_factor_keys <- names(rm_derived_matrices)[grepl("_bias_factor$", names(rm_derived_matrices))]
# bias_factor_matrices <- rm_derived_matrices[bias_factor_keys]
# # diff min/max
# diff_min <- min(unlist(lapply(difference_matrices, min, na.rm = T)), na.rm = T)
# diff_max <- max(unlist(lapply(difference_matrices, max, na.rm = T)), na.rm = T)
# 
# # bias min/max
# bias_min <- min(unlist(lapply(bias_factor_matrices, min, na.rm = T)), na.rm = T)
# bias_max <- max(unlist(lapply(bias_factor_matrices, max, na.rm = T)), na.rm = T)
