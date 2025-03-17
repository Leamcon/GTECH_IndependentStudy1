# filename: mpvbc_calculate.R
# calculation of mpvbcf

# INPUT VARIABLES
#     gcm time series: daily precipitation values during the calculation period
#     gcm yr time series: the year corresponding to each daily precipitation value
#     duration: duration of extreme event, between 1 and 20 years
#     begyr, endyr: beginning and ending years of calculation time period
#     obs_rm, obs_rm_yr: the observed running mean annual time series, and corresponding year
#     
# RETURNS
#     mpvbcf: Multi-Year Precipitation Variability Bias Correction Factor
#
# FIGURES: makes one figure

mpvbc_calculate <- function(gcm_ts_cal, gcm_yr_cal, duration, begyr, endyr,
                            obs_rm, obs_rm_yr) {
  
  # Equation 1
  # calculate GCM annual mean values
  gcm_cal_ann1 <- aggregate(x=gcm_ts_cal, by=list(gcm_yr_cal), FUN=mean)
  gcm_cal_ann <- gcm_cal_ann1[,-1]
  gcm_cal_ann_yr <- gcm_cal_ann1[,1]
  
  # Equation 2
  # calculate GCM running mean of annual mean values, for specified duration
  gcm_cal_ann_rm <- as.vector(filter(gcm_cal_ann, rep(1 / duration, duration), sides = 1))
  
  # Equation 3
  # calculate observed mean drought and pluvial values, and variability (i.e. range)
  droughtval_obs <- min(obs_rm, na.rm=T)
  pluvialval_obs <- max(obs_rm, na.rm=T)
  # Equation 5
  var_obs <- pluvialval_obs - droughtval_obs
  # Equation 4
  droughtyr_obs <- obs_rm_yr[which.min(obs_rm)]
  pluvialyr_obs <- obs_rm_yr[which.max(obs_rm)]
  
  # Equation 3
  # calculate GCM drought and pluvial values, and variability (i.e. range)
  # and also year of drought and pluvial GCM running means
  droughtval_gcm <- min(gcm_cal_ann_rm, na.rm=T)
  pluvialval_gcm <- max(gcm_cal_ann_rm, na.rm=T)
  # Equation 5
  var_gcm <- pluvialval_gcm - droughtval_gcm
  # Equation 4
  droughtyr_gcm <- gcm_cal_ann_yr[which.min(gcm_cal_ann_rm)]
  pluvialyr_gcm <- gcm_cal_ann_yr[which.max(gcm_cal_ann_rm)]
  
  # Equation 6
  # calculate multiyear precipitation bias correction factor (MPVBCF)
  mpvbcf <- var_obs / var_gcm
  
  ############ Time series figure for the calculation period
  fn_cal <- paste("mpvbc_calculation_", duration, "yr_",
                  begyr, "_", endyr, ".png", sep="")
  png(filename=fn_cal, width=8, height=5,
      units="in", res=600, pointsize=16)
  
  title <- paste("Calculation Period PRCP ",begyr,"-",endyr, sep="")
  
  yrange <- range(c(gcm_cal_ann, gcm_cal_ann_rm, obs_rm), na.rm=T)
  yrange[1] <- yrange[1] - 0.15*diff(yrange)
  plot(gcm_cal_ann ~ gcm_cal_ann_yr, type="l", col="lightblue", xlab="", ylab="mm/day",
       main = title, ylim=yrange)
  mtext(paste(duration,"-yr duration, Obs var=", round(var_obs, digits=2), 
              ", GCM var=", round(var_gcm, digits=2),
              ", MPVBCF=", round(mpvbcf, digits=2), sep=""))
  lines(gcm_cal_ann_rm ~ gcm_cal_ann_yr, lwd=1.5, col="blue")
  lines(obs_rm ~ obs_rm_yr, lwd=2)
  points(c(droughtyr_obs, pluvialyr_obs, droughtyr_gcm, pluvialyr_gcm),
         c(droughtval_obs, pluvialval_obs, droughtval_gcm, pluvialval_gcm),
         pch=c(25, 24, 25, 24), col=c("black", "black", "blue", "blue"),
         bg = c("black", "black", "blue", "blue"), cex=0.75)
  legend("bottom",legend=c("GCM ANN", "GCM RM", "Obs RM", "Min RM", "Max RM"), 
         lwd=c(1,1.5,2, NA, NA), 
         pt.bg = c(NA, NA, NA,"black", "black"),
         col=c("lightblue", "blue", "black", "black", "black"), 
         ncol=5, cex=0.6, pch=c(NA,NA,NA, 25, 24))
  
  # save figure 
  dev.off()
  par(mfrow=c(1,1)) 
  ############ End of Time series figure for the calculation period
  
  return(mpvbcf)
}
