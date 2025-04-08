#' running mean calculator
#' 
#' adapted from Professor Frei's mpvbc_ scripts

library(abind)
library(ggplot2)
library(gridExtra)
library(sf)

# User Specifications:
# running mean window duration
duration <- 10

# visualizer
source("rm_range_matrix_visualizer.R")

# INPUT: 3D array of annual precip values, num specifying window size
# applies a running mean calculation to each matrix of the 3D array with specified window
# gets the difference of max and min values for each matrix cell across 3D array
# returns a matrix of these differences
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

# load in the modeled and observed data
load("rdata/cesm2_waccm_historical_annual_mean_precip.RData")
load("rdata/cmcc_cm2_sr5_historical_annual_mean_precip.RData")
load("rdata/access_cm2_historical_annual_mean_precip.RData")
load("rdata/access_cm2_ssp585_annual_mean_precip.RData")
load("rdata/cpc_ne_annual_mean_precipitation.RData")

# SHELVED FOR NOW
# combine the cmip6 historical and modeled data
#access_cm2_total_annual_array <- abind(access_cm2_historical_annual_means_ne_subset, 
                                       #access_cm2_ssp585_annual_means_ne_subset, 
                                       #along = 3)

# preserve dimnames from original data
lon_attr <- dimnames(cpc_annual_means_ne_subset)[[1]]
lat_attr <- dimnames(cpc_annual_means_ne_subset)[[2]]

# create matrix of running mean ranges for observed data
observed_rm_range_grid <- running_mean_range_calculator(cpc_annual_means_ne_subset, 
                                                      duration)

# now the same for the cmip6 model data (uses just modeled historical for now)
modeled_rm_range_grid <- running_mean_range_calculator(cesm2_waccm_historical_annual_means_ne_subset, 
                                                       duration)
# modeled_rm_range_grid <- running_mean_range_calculator(cmcc_cm2_sr5_historical_annual_means_ne_subset, 
#                                                        duration)
# modeled_rm_range_grid <- running_mean_range_calculator(access_cm2_total_annual_array, 
#                                                        duration)
# modeled_rm_range_grid <- running_mean_range_calculator(access_cm2_historical_annual_means_ne_subset,
#                                                        duration)

# apply dimnames to new matrices
dimnames(observed_rm_range_grid) <- list(longitude = lon_attr, 
                                         latidude = lat_attr)
dimnames(modeled_rm_range_grid) <- list(longitude = lon_attr, 
                                         latidude = lat_attr)
#########################
# Grid Arithmetic
# difference
rm_range_difference_grid <- observed_rm_range_grid - modeled_rm_range_grid

# bias
model_bias_factor_grid <- observed_rm_range_grid / modeled_rm_range_grid

#########################
# Plotting
# plot function title and legend param accept any string
# color scheme accepts "blue_white" "diff" and "ratio"
# TODO: implement file saving

observed_rm_plot <- rm_range_matrix_visualizer(observed_rm_range_grid,
                               "Running Mean Range of Observed Annual Precipitation",
                               "mm/day",
                               "blue_white")

modeled_rm_plot <- rm_range_matrix_visualizer(modeled_rm_range_grid,
                               "Running Mean Range of Modeled Annual Precipitation",
                               "mm/day",
                               "blue_white")

difference_rm_plot <- rm_range_matrix_visualizer(rm_range_difference_grid,
                               "Difference in Running Mean Ranges, Observed - Modeled",
                               "mm/day",
                               "diff")

factor_rm_plot <- rm_range_matrix_visualizer(model_bias_factor_grid,
                               "Model Bias Factor",
                               "Bias Factor",
                               "ratio")

grid.arrange(observed_rm_plot, modeled_rm_plot, difference_rm_plot, factor_rm_plot, nrow = 2)
