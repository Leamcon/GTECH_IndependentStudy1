# filename: mpvbc_example.R
# MPVBC (Multiyear Precipitation Variability Bias Correction)
# Methodology to calculate and to apply MPVBC to a sample GCM
# Written by Allan Frei, 2024
# CITATION:
# Frei, Gelda, Mukundan (2024), Characterizing and Correcting for 
#       Global Climate Models’ Biases in Multiyear Extreme Precipitation Scenarios,
#       Submitted to Water Resources Research

# ADDITIONAL FILES NEEDED TO RUN THIS CODE
# mpvbc_calculate.R: user defined function to calculate the MPVBCF
# mpvbc_apply.R: user defined function to apply the MPVBC
# GCM_daily_PRCP_example.csv: a GCM daily precipitation time series (LOCA model 5)
# catskill_sta_runmean_1960_2016.csv: 
#     file with observed running mean annual precipitation for duration 1-20 yrs

############ Section for the user to change the duration
# This is the only parameter that the user should adjust 
# specifying 1-yr duration shows annual mean values
duration <- 10  # specify duration (years) from 1-20

############ other source code required for this analysis
source("mpvbc_calculate.R") # calculates the multiyear PRCP bias correction factor
source("mpvbc_apply.R") # applies the multiyear PRCP bias correction

############ Read in the GCM daily time series
# read in the GCM daily precipitation file, assumed to include 1950-2100
# (in some models the simulation ends in 2099, in others it ends in 2100)
# extract the date and the year vectors
gcm <- read.csv("GCM_daily_PRCP_example.csv")
gcm_date <- gcm$date
gcm_ts <- gcm$Model5
gcm_yr <- as.numeric(format(as.Date(gcm_date, format="%Y-%m-%d"),"%Y"))

# calculate the annual mean, and running mean, values for the full time period
gcm_ann1 <- aggregate(x=gcm_ts, by=list(gcm_yr), FUN=mean)
gcm_ann <- gcm_ann1[,-1]
gcm_ann_range <- range(gcm_ann)
gcm_ann_yr <- gcm_ann1[,1]
gcm_ann_rm <- as.vector(filter(gcm_ann, rep(1 / duration, duration), sides = 1))
gcm_ann_yr_range <- range(gcm_ann_yr)

############ Read in the observed running mean
# read in the obs running mean time precip series
# columns are duration
obs_fn_rm <- "catskill_sta_runmean_1960_2016.csv"
obs_rm1 <- read.csv(obs_fn_rm)
obs_rm_yr <- obs_rm1[,1]
obs_rm_all <- obs_rm1[,-1]
obs_rm <- obs_rm_all[,duration]

############ Specify calculation years, future years, and extract time series values
# calculation years
begyr <- 1960
endyr <- 2016
yr_vec <- begyr:endyr

# future years
begyr_future <- 2017
endyr_future <- 2099
yr_vec_future <- begyr_future:endyr_future

# extract the GCM calculation time period values
cal_ptr <- gcm_yr >= begyr & gcm_yr <= endyr
gcm_ts_cal <- gcm_ts[cal_ptr]
gcm_yr_cal <- gcm_yr[cal_ptr]
gcm_date_cal <- gcm_date[cal_ptr]

# extract the GCM future time period values
future_ptr <- gcm_yr >= begyr_future & gcm_yr <= endyr_future
gcm_ts_future <- gcm_ts[future_ptr]
gcm_yr_future <- gcm_yr[future_ptr]
gcm_date_future <- gcm_date[future_ptr]


############ Time series figure with full GCM time domain
fn <- paste("mpvbc_ts_", duration, "yr_",
            gcm_ann_yr_range[1], "_", gcm_ann_yr_range[2], ".png", sep="")
png(filename=fn, width=8, height=5,
    units="in", res=600, pointsize=16)

title <- paste("GCM and Obs PRCP ",gcm_ann_yr_range[1], "-", gcm_ann_yr_range[2], sep="")

yrange <- gcm_ann_range
yrange[1] <- yrange[1] - 0.15*diff(yrange)
plot(gcm_ann ~ gcm_ann_yr, type="l", col="lightblue", xlab="", ylab="mm/day",
     main = title, ylim=yrange)
mtext(paste(duration,"-yr duration", sep=""))
lines(gcm_ann_rm ~ gcm_ann_yr, lwd=1.5, col="blue")
lines(lowess(gcm_ann ~ gcm_ann_yr, f=0.5), col="red")
lines(obs_rm ~ obs_rm_yr, lwd=2.5)
abline(v=c(begyr, endyr+0.5, endyr_future), lty=2)
text(x=endyr+0.5, y=yrange[2] - 0.05*diff(yrange), 
     labels="calculation period", pos=2, cex=0.8)
text(x=endyr+0.5, y=yrange[2] - 0.15*diff(yrange), 
     labels=paste(begyr,"-",endyr, sep=""), pos=2, cex=0.8)
text(x=endyr+0.5, y=yrange[2] - 0.05*diff(yrange), 
     labels="future period", pos=4, cex=0.8)
text(x=endyr+0.5, y=yrange[2] - 0.15*diff(yrange), 
     labels=paste(begyr_future,"-",endyr_future, sep=""), pos=4, cex=0.8)
legend("bottom",legend=c("GCM ANN", "GCM RM", "GCM lowess", "Obs RM"), 
       col=c("lightblue", "blue", "red", "black"), pch=NA, lty=1, 
       lwd=c(1, 1.5, 1, 2.5), ncol=4, cex=0.6 ) 

# save figure 
dev.off()
par(mfrow=c(1,1)) 
############ End of Time series figure with entire GCM time domain


# calculation function
# INPUT VARIABLES
#     gcm time series: daily precipitation values during the calculation period
#     gcm yr time series: the year corresponding to each daily precipitation value
#     duration: duration of extreme event, between 1 and 20 years
#     begyr, endyr: beginning and ending years of calculation time period
#     obs_rm, obs_rm_yr: the observed running mean annual time series, and corresponding year
#     
# RETURNS
#     mpvbcf: Multiyear Precipitation Variability Bias Correction Factor
#
# FIGURES: makes one figure
mpvbcf <- mpvbc_calculate(gcm_ts_cal, gcm_yr_cal, duration, begyr, endyr,
                                      obs_rm, obs_rm_yr)

############ Apply the bias correction

# application function
# INPUT VARIABLES
#     gcm time series: daily precipitation values during the application period
#     gcm yr time series: the year corresponding to each daily precipitation value
#     duration: duration of extreme event, between 1 and 20 years
#     mpvbcf: multi-year bias correction factor
#     obs_rm, obs_rm_yr: the observed running mean annual time series, and corresponding year
#     begyr, endyr: beginning and ending years of application time period
#     
# RETURNS
#     gcm_ts_BC: data frame with corrected time series: one column for drought,
#                and one column for pluvial
#
# FIGURES: makes one figure with two panels

# apply MPVBC to the calculation time period
gcm_ts_cal_BC <- mpvbc_apply(gcm_ts_cal, gcm_yr_cal, duration, mpvbcf, 
                             obs_rm, obs_rm_yr, begyr, endyr)

# apply MPVBC to the future time period
gcm_ts_future_BC <- mpvbc_apply(gcm_ts_future, gcm_yr_future, duration, mpvbcf, 
                                obs_rm, obs_rm_yr, begyr_future, endyr_future)
