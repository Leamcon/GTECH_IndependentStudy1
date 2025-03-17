# reads the cpc data processed by same in cpc_annual_mu_abinder.R
# calculates some values for the entire region, and specifically for catskills

# read in the catskills station based running means that we made
obs_fn_rm <- "catskill_sta_runmean_1960_2016.csv"
obs_dir_rm <- "data/catskill_sta_data/"
obs_rm1 <- read.csv(paste(obs_dir_rm,obs_fn_rm, sep=""))
obs_rm_yr <- obs_rm1[,1]
obs_rm_all <- obs_rm1[,-1]

# load the saved data, and get lon, lat, and yr attributes
load("cpc_ne_annual_mean_precipitation.RData")
str(cpc_annual_means_ne_subset)
lon <- attr(cpc_annual_means_ne_subset,"longitude")
lat <- attr(cpc_annual_means_ne_subset,"latitude")
yr <- attr(cpc_annual_means_ne_subset,"year")

# test image for one year
title <- paste ("min", round(min(cpc_annual_means_ne_subset, na.rm=T),3),
                "max", round(max(cpc_annual_means_ne_subset, na.rm=T),3),
                "mm/day")
image(lon, lat, cpc_annual_means_ne_subset[,,45], main=title)

#################################
# LOOK AT LONG TERM MEAN FOR NE, AND FOR CATSKILLS
# calc long term mean image
prcp_mean <- apply(cpc_annual_means_ne_subset,c(1,2),mean, na.rm=T)
title <- paste ("min", round(min(prcp_mean, na.rm=T),3),
                "max", round(max(prcp_mean, na.rm=T),3),
                "mm/day")
image(lon, lat, prcp_mean, main=title)

# draw a polygon around approximate catskills area
lon_min <- 284.75
lon_max <- 286
lat_min <- 41.8
lat_max <- 42.3
lon_cat <- c(lon_min, lon_max, lon_max, lon_min)
lat_cat <- c(lat_min, lat_min, lat_max, lat_max)
polygon(lon_cat,lat_cat)

# pick out ann time series for approximate catskills region
lon_cat_ptr <- which(lon>lon_min & lon<lon_max)
lat_cat_ptr <- which(lat>lat_min & lat<lat_max)
prcp_catskills <- cpc_annual_means_ne_subset[lon_cat_ptr,lat_cat_ptr,]

# check the points on the map, just to be sure
points(lon[lon_cat_ptr[5]], lat[lat_cat_ptr[1]])
points(lon[lon_cat_ptr[1]], lat[lat_cat_ptr[2]])

# plot mean for catskills only
prcp_catskills_mean <- apply(prcp_catskills,c(1,2),mean)
title <- paste ("min", round(min(prcp_catskills_mean, na.rm=T),3),
                "max", round(max(prcp_catskills_mean, na.rm=T),3),
                "mm/day")
image(lon[lon_cat_ptr], lat[lat_cat_ptr], prcp_catskills_mean, main=title)

########################
# LOOK AT SOME RUNNING MEANS (MOVING AVERAGES)

# moving average func
ma <- function(x,n) { as.vector(stats::filter(x, rep(1 / n, n), sides = 1)) }

# specify duration to look at
duration <- 20 # how many yrs moving average.
# for duration=1, the ma calculations should be equal to annual
# calculations above
obs_rm <- obs_rm_all[,duration]

# # test ma calculation
# x <- c(1,4,2,3,5,8,5,7,6,9,4,10,12)
# plot(x, type="b")
# lines(ma(x,5), col="red")
# lines(ma(x,10), col="blue")
# lines(ma(x,1), col="green")

# plot one of the points with the moving average 
# to see if it looks ok (drought in 60s, pluvial in 2000s)
plot(prcp_catskills[5,1,] ~ yr, type="b")
lines(ma(prcp_catskills[5,1,],duration) ~ yr, col="red")

# calc and plot ann average time series, and moving average, for regional mean
# to see if it looks ok (drought in 60s, pluvial in 2000s)
prcp_catskills_region <- apply(prcp_catskills,3,mean)
plot(prcp_catskills_region ~ yr, type="b")
lines(ma(prcp_catskills_region,duration) ~ yr, col="red")
lines(obs_rm ~ obs_rm_yr, col="black", lwd=3)

##############################################
# look at mean and variability for entire region
# calc long term mean image
prcp_mean <- apply(cpc_annual_means_ne_subset,c(1,2),mean, na.rm=T)
title <- paste ("MEAN: min", round(min(prcp_mean, na.rm=T),3),
                "max", round(max(prcp_mean, na.rm=T),3),
                "mm/day")
image(lon, lat, prcp_mean, main=title)

#####################################
# variability for annual values
# calc long term min image
prcp_min <- apply(cpc_annual_means_ne_subset,c(1,2),min) #, na.rm=T)
title <- paste ("MIN: min", round(min(prcp_min, na.rm=T),3),
                "max", round(max(prcp_min, na.rm=T),3),
                "mm/day")
image(lon, lat, prcp_min, main=title)

# calc long term max image
prcp_max <- apply(cpc_annual_means_ne_subset,c(1,2),max) #, na.rm=T)
title <- paste ("MAX: min", round(min(prcp_max, na.rm=T),3),
                "max", round(max(prcp_max, na.rm=T),3),
                "mm/day")
image(lon, lat, prcp_max, main=title)

# calc long term range (max-min) image
prcp_range <- prcp_max - prcp_min
title <- paste ("RANGE: min", round(min(prcp_range, na.rm=T),3),
                "max", round(max(prcp_range, na.rm=T),3),
                "mm/day")
image(lon, lat, prcp_range, main=title)

# draw box around approximate catskills
polygon(lon_cat,lat_cat)

###########################
# SECTION TO LOOK AT MOVING AVERAGE IMAGES FOR ENTIRE REGION

# # THIS DID NOT WORK
# # n-yr moving average func
# ma_func <- function(x,n) { 
#   if (sum(!is.na(x)) == 0) {
#     return(rep(NA,length(x)))
#   } else {
#     return(as.vector(stats::filter(x, rep(1 / n, n), sides = 1)))
#   }
# } # end ma function
# 
# prcp_ma <- apply(cpc_annual_means_ne_subset,c(1,2),ma_func,10)

# can reset duration again
duration <- 20

# SO I WAS FORCED TO DO IT WITH LOOPS
# mask out non-data (missing) grid points
mask_na <- !is.na(prcp_mean)
dim_cpc <- dim(cpc_annual_means_ne_subset)
prcp_ma <- array(NA, dim_cpc)
min_ma <- array(NA, dim_cpc[1:2])
max_ma <- array(NA, dim_cpc[1:2])
range_ma <- array(NA, dim_cpc[1:2])
for (i in 1:dim_cpc[1]) {
  for (j in 1:dim_cpc[2]) {
    if (mask_na[i,j]) {
      ts <- cpc_annual_means_ne_subset[i,j,]
      prcp_ma[i,j,] <- ma(ts,duration)
      min_ma[i,j] <- min(prcp_ma[i,j,], na.rm=T)
      max_ma[i,j] <- max(prcp_ma[i,j,], na.rm=T)
      range_ma[i,j] <- max_ma[i,j] - min_ma[i,j]
    } # end of if statement calculating ma
  } # end of j loop
} # end of i loop
  

#####################################
# variability for MA values
# min image
title <- paste ("MIN",duration, "Yr MA: min", round(min(min_ma, na.rm=T),3),
                "max", round(max(min_ma, na.rm=T),3),
                "mm/day")
image(lon, lat, min_ma, main=title)

# draw box around approximate catskills
polygon(lon_cat,lat_cat)

# max image
title <- paste ("MAX",duration, "Yr MA: min", round(min(max_ma, na.rm=T),3),
                "max", round(max(max_ma, na.rm=T),3),
                "mm/day")
image(lon, lat, max_ma, main=title)

# draw box around approximate catskills
polygon(lon_cat,lat_cat)

# (max-min) image
title <- paste ("RANGE",duration, "Yr MA: min", round(min(range_ma, na.rm=T),3),
                "max", round(max(range_ma, na.rm=T),3),
                "mm/day")
image(lon, lat, range_ma, main=title)

# draw box around approximate catskills
polygon(lon_cat,lat_cat)

  