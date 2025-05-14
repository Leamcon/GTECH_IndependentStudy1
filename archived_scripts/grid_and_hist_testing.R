
# step through vars (make sure to populate environment first)
duration <- 10
model_name <- "ACCESS_CM2"
observed_data <- rm_derived_matrices$observed_rm_range
model_rm_data <- rm_derived_matrices$access_cm2_historical_rm_range
difference_data <- rm_derived_matrices$access_cm2_historical_difference
bias_factor_data <- rm_derived_matrices$access_cm2_historical_bias_factor


# Modified plotting function that accepts individual matrices as parameters
# instead of a list of matrices to batch process
create_static_panel_plot <- function(observed_data, model_rm_data, difference_data, bias_factor_data, 
                                     model_name = "TEST_MODEL", duration = 30) {
  
  # Calculate consistent scale for RM range plots
  all_rm_ranges <- c(observed_data, model_rm_data)
  min_val <- min(all_rm_ranges, na.rm = TRUE)
  max_val <- max(all_rm_ranges, na.rm = TRUE)
  range_buffer <- (max_val - min_val) * 0.001
  rm_range_min <- min_val - range_buffer
  rm_range_max <- max_val + range_buffer
  
  # Set fixed scales for difference and bias factor
  diff_min <- -1
  diff_max <- 1
  bias_min <- 0
  bias_max <- 9
  
  # generate a plot title
  plot_title <- toupper(paste(strsplit(model_name, "_")[[1]], collapse = " "))
  
  # set up the plot matrix (2 row, 4 col)
  par(mfrow = c(2, 4))
  
  # Use specialized visualizers for each plot type
  # 1. Observed RM Range - using consistent scale across all models
  rm_range_visualizer(
    observed_data, 
    paste0("Observed RM Range (", duration, "-year window)"),
    min_max_range = c(rm_range_min, rm_range_max)
  )
  
  # 2. Model RM Range - using same scale as observed
  rm_range_visualizer(
    model_rm_data, 
    paste0(plot_title, " RM Range (", duration, "-year window)"),
    min_max_range = c(rm_range_min, rm_range_max)
  )
  
  # 3. Difference plot with diverging colors
  difference_visualizer(
    difference_data, 
    paste0(plot_title, " Normalized Difference from Observed")
  )
  
  # 4. Bias factor plot
  bias_factor_visualizer(
    bias_factor_data, 
    paste0(plot_title, " Bias Factor")
  )
  
    # 5. Observed RM Range histogram - bottom row, first column
  observed_flat <- as.vector(observed_data)
  observed_flat <- observed_flat[!is.na(observed_flat)]  # Remove NA values
  hist(observed_flat, 
       main = "Observed RM Range Histogram",
       xlab = "Range Value", 
       col = "lightblue", 
       border = "black",
       xlim = c(rm_range_min, rm_range_max),  # Use same range as grid
       breaks = 30)
  
  # 6. Model RM Range histogram - bottom row, second column
  model_flat <- as.vector(model_rm_data)
  model_flat <- model_flat[!is.na(model_flat)]  # Remove NA values
  hist(model_flat, 
       main = paste0(plot_title, " RM Range Histogram"),
       xlab = "Range Value", 
       col = "lightblue",  # Same color as observed for comparison
       border = "black",
       xlim = c(rm_range_min, rm_range_max),  # Use same range as grid
       breaks = 30)
  
  # 7. Normalized difference histogram - bottom row, third column
  diff_flat <- as.vector(difference_data)
  diff_flat <- diff_flat[!is.na(diff_flat)]  # Remove NA values
  hist(diff_flat, 
       main = "Normalized Difference Histogram",
       xlab = "Difference Value", 
       col = "lightblue",
       border = "black",
       xlim = c(diff_min, diff_max),  # Fixed range as specified
       breaks = 30)
  
  # 8. Bias factor histogram - bottom row, fourth column
  bias_flat <- as.vector(bias_factor_data)
  bias_flat <- bias_flat[!is.na(bias_flat)]  # Remove NA values
  hist(bias_flat, 
       main = "Bias Factor Histogram",
       xlab = "Bias Factor Value", 
       col = "lightblue",
       border = "black",
       xlim = c(bias_min, bias_max),  # Fixed range as specified
       breaks = 30)
  
  # reset plotting frame (comment out unless stepping through)
  par(mfrow = c(1, 1))
  
  # Return the current plot configuration (useful for testing)
  return(par("mfrow"))
}

# Modified plotting function with inset histograms
create_static_panel_plot_with_inset_histograms <- function(observed_data, model_rm_data, difference_data, bias_factor_data, 
                                                           model_name = "TEST_MODEL", duration = 30) {
  
  # Calculate consistent scale for RM range plots
  all_rm_ranges <- c(observed_data, model_rm_data)
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
  
  # Generate a plot title
  plot_title <- toupper(paste(strsplit(model_name, "_")[[1]], collapse = " "))
  
  # Clear the plot device and set standard margins
  par(mfrow = c(1, 1), mar = c(4, 4, 3, 2))
  
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
  
  # Helper function to position a histogram inset in the upper-left of its parent grid
  position_histogram_inset <- function(quadrant) {
    # Define the base inset dimensions
    inset_width <- 0.20
    inset_height <- 0.27
    
    # Base coordinates for each quadrant
    base_coords <- list(
      "1" = c(0.0, 0.5, 0.5, 1.0),  # Top-left quadrant
      "2" = c(0.5, 1.0, 0.5, 1.0),  # Top-right quadrant
      "3" = c(0.0, 0.5, 0.0, 0.5),  # Bottom-left quadrant
      "4" = c(0.5, 1.0, 0.0, 0.5)   # Bottom-right quadrant
    )
    
    # Get base coordinates for this quadrant
    base <- base_coords[[as.character(quadrant)]]
    
    # Position the inset in the upper-left corner with padding
    # Format: c(left, right, bottom, top)
    inset_coords <- c(
      base[1] + 0.12,                # > moves right & < moves left
      base[1] + 0.07 + inset_width,  # adjust inset_width for width
      base[3] + (base[4] - base[3]) * 0.69,  # > moves higher & < moves lower
      base[4] - 0.06                 # > for smaller plot & < for taller
    )
    
    par(fig = inset_coords, new = TRUE, mar = c(0.5, 0.1, 0.1, 0.1))
  }
  
  # 1. Observed RM Range grid (Quadrant 1: Top-left)
  position_grid_plot(1)
  rm_range_visualizer(
    observed_data, 
    paste0("Observed RM Range (", duration, "-year window)"),
    min_max_range = c(rm_range_min, rm_range_max)
  )
  
  # Add inset histogram for observed data
  observed_flat <- as.vector(observed_data)
  observed_flat <- observed_flat[!is.na(observed_flat)]  # Remove NA values
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
  box()
  axis(1, cex.axis = 0.5)
  
  # 2. Model RM Range grid (Quadrant 2: Top-right)
  position_grid_plot(2)
  rm_range_visualizer(
    model_rm_data, 
    paste0(plot_title, " RM Range (", duration, "-year window)"),
    min_max_range = c(rm_range_min, rm_range_max)
  )
  
  # Add inset histogram for model data
  model_flat <- as.vector(model_rm_data)
  model_flat <- model_flat[!is.na(model_flat)]  # Remove NA values
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
  box()
  axis(1, cex.axis = 0.5)
  
  # 3. Normalized difference grid (Quadrant 3: Bottom-left)
  position_grid_plot(3)
  difference_visualizer(
    difference_data, 
    paste0(plot_title, " Normalized Difference from Observed")
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
       axes = FALSE)
  box()
  axis(1, cex.axis = 0.5)
  
  # 4. Bias factor grid (Quadrant 4: Bottom-right)
  position_grid_plot(4)
  bias_factor_visualizer(
    bias_factor_data, 
    paste0(plot_title, " Bias Factor")
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
       axes = FALSE)
  box()
  axis(1, cex.axis = 0.5)
  
  # Reset plotting frame
  par(mfrow = c(1, 1), new = FALSE)
  
  # Return success indicator
  return(TRUE)
}

