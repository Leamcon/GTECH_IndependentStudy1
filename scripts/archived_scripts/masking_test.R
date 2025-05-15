library(terra)
library(ggplot2)

source("array_ggplot_visualizer.R")



load(file = "rdata/cpc_ne_annual_mean_precipitation.RData")
load(file = "rdata/access_cm2_ssp585_annual_mean_precip_ne.RData")

array_ggplot_visualizer(cpc_annual_means, 66)

# basic trimming implementation (create function if works)
# from the cmip array grab everying from x axis between [2] and -1 from end, y[2] to y[end]
xtrim <- 2:(dim(precip_3d_array)[1]-1)
ytrim <- 2:dim(precip_3d_array)[2]
cmip_ne_trimmed <- precip_3d_array[xtrim, ytrim, ]

# takes a slice of our main array to use as a basis for the mask
cpc_slice <- cpc_annual_means[,,1]

# build a matrix of the proper size then set all cells = to na in the cpc to 
cpc_mask <- matrix(ncol = 28, nrow = 51, data = 1)
cpc_mask[is.na(cpc_slice)] <- NA

masked_cmip_slice <- cpc_mask * cmip_ne_trimmed[,,1]

xtest <- 1:10
xtrimmed <- xtest[2:(length(xtest)-1)]
