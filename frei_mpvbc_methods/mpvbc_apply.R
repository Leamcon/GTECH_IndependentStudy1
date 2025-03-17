# filename: mpvbc_apply.R
# application of mpvbcf

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
#                and one column for pluvial. 
#
# FIGURES: makes one figure with two panels

mpvbc_apply <- function(gcm_ts, gcm_yr, duration, mpvbcf, 
                        obs_rm, obs_rm_yr, begyr, endyr) {
  
  # Equation 1
  # calculate GCM annual mean values
  gcm_ann1 <- aggregate(x=gcm_ts, by=list(gcm_yr), FUN=mean)
  gcm_ann <- gcm_ann1[,-1]
  gcm_ann_yr <- gcm_ann1[,1]
  
  # Equation 2
  # calculate GCM running mean of annual mean values, at specified duration
  gcm_ann_rm <- as.vector(filter(gcm_ann, rep(1 / duration, duration), sides = 1))
  
  # Equation 3
  # calculate GCM drought and pluvial values, and variability (i.e. range)
  # and also year of drought and pluvial GCM running means
  droughtval_gcm <- min(gcm_ann_rm, na.rm=T)
  pluvialval_gcm <- max(gcm_ann_rm, na.rm=T)
  # Equation 5
  var_gcm <- pluvialval_gcm - droughtval_gcm
  # Equation 4
  droughtyr_gcm <- gcm_ann_yr[which.min(gcm_ann_rm)]
  pluvialyr_gcm <- gcm_ann_yr[which.max(gcm_ann_rm)]
  
  # Equation 3
  # calculate observed drought and pluvial values and yrs
  droughtval_obs <- min(obs_rm, na.rm=T)
  pluvialval_obs <- max(obs_rm, na.rm=T)
  # Equation 4
  droughtyr_obs <- obs_rm_yr[which.min(obs_rm)]
  pluvialyr_obs <- obs_rm_yr[which.max(obs_rm)]
  
  # Equation 7
  ############ calculate the corrected variability, 
  # which is the product of uncorrected variability and the correction factor
  # if mpvbcf (bias correction factor) is less than or equal to 1,
  # the the gcm has greater than or equal variability compared to observations,
  # in which case we set the corrected variability equal to the uncorrected value
  if (mpvbcf >= 1) {
    var_gcm_BC <- var_gcm * mpvbcf
  } else { 
    var_gcm_BC <- var_gcm
  }
  # Equation 8
  # calculate the change in variability in units of precipitation
  var_gcm_change <- var_gcm_BC - var_gcm
  # Equation 9
  # set the change in the low and high values each to 1/2 the total change in variation
  droughtval_gcm_BC <- droughtval_gcm - (var_gcm_change/2.)
  pluvialval_gcm_BC <- pluvialval_gcm + (var_gcm_change/2.)
  
  # Equation 10
  # calculate the multipliers: values by which daily precipitation must be multiplied
  #   to correct precipitation drought and pluvial years
  drought_multiplier <- droughtval_gcm_BC / droughtval_gcm
  pluvial_multiplier <- pluvialval_gcm_BC / pluvialval_gcm
  
  # pointers to the daily time series values corresponding
  # to the minimum (drought) and max (pluvial) years
  drought_ptr <- gcm_yr <= droughtyr_gcm & gcm_yr >= (droughtyr_gcm - duration + 1)
  pluvial_ptr <- gcm_yr <= pluvialyr_gcm & gcm_yr >= (pluvialyr_gcm - duration + 1)
  
  # Equation 11
  # calculate the corrected time series
  # Separate time series for drought and pluvial results are necessary
  # because there is potential for overlap of the drought and pluvial years.
  gcm_ts_BC_drought <- gcm_ts
  gcm_ts_BC_drought[drought_ptr] <- drought_multiplier * gcm_ts[drought_ptr]
  gcm_ts_BC_pluvial <- gcm_ts
  gcm_ts_BC_pluvial[pluvial_ptr] <- pluvial_multiplier * gcm_ts[pluvial_ptr]
  
  ##################### figure with two panels: 
  # panel 1. uncorrected and corrected annual values
  # panel 2. running mean values
  # calculate the annual and running mean uncorrected and corrected values,
  gcm_ts_ann1 <- aggregate(x=gcm_ts, by=list(gcm_yr), FUN=mean)
  gcm_ts_BC_drought_ann1 <- aggregate(x=gcm_ts_BC_drought, by=list(gcm_yr), FUN=mean)
  gcm_ts_BC_pluvial_ann1 <- aggregate(x=gcm_ts_BC_pluvial, by=list(gcm_yr), FUN=mean)
  gcm_year_ann1 <- aggregate(x=gcm_yr, by=list(gcm_yr), FUN=mean)
  gcm_ts_ann <- gcm_ts_ann1$x
  gcm_ts_BC_drought_ann <- gcm_ts_BC_drought_ann1$x
  gcm_ts_BC_pluvial_ann <- gcm_ts_BC_pluvial_ann1$x
  gcm_year_ann <- gcm_year_ann1$x
  fn_ts <- paste("mpvbc_apply_", duration, "yr_",
              begyr, "_", endyr, ".png", sep="")
  # make the figure
  png(filename=fn_ts, width=8, height=10,
      units="in", res=600, pointsize=16)
  par(mfrow=c(2,1))
  
  title <- paste("Application of MPVBC, ANN Values ",begyr,"-",endyr, sep="")
  
  yrange <- c(c(min(min(gcm_ts_ann, na.rm=T), 
                    min(gcm_ts_BC_drought_ann, na.rm=T), 
                    min(gcm_ts_BC_pluvial_ann, na.rm=T))),
              c(max(max(gcm_ts_ann, na.rm=T), 
                    max(gcm_ts_BC_drought_ann, na.rm=T), 
                    max(gcm_ts_BC_pluvial_ann, na.rm=T))))
  yrange[1] <- yrange[1] - 0.15*diff(yrange)
  plot(gcm_ts_BC_drought_ann ~ gcm_year_ann, type="l", col="blue", 
       xlab="", ylab="mm/day", main = title, ylim=yrange)
  mtext(paste(duration,"-yr duration, MPVBCF=", round(mpvbcf, digits=2), sep=""))
  lines(gcm_ts_BC_pluvial_ann ~ gcm_year_ann, col="blue", lty=1) #, type="b")
  lines(gcm_ts_ann ~ gcm_year_ann, col="lightblue") #, type="b")
  mtext("(a)", adj=0, cex=1.25)
  legend("bottom",legend=c("GCM uncorr", "GCM corr"), 
         col=c("lightblue", "blue"), lty=1, # pch=1, 
         ncol=2, cex=0.6 ) 
  
  ########### end of first panel
  
  # runmean of annual values, and compare to observed runmean
  gcm_ts_ann_rm <- as.vector(filter(gcm_ts_ann, rep(1 / duration, duration), sides = 1))
  gcm_ts_BC_drought_ann_rm <- as.vector(filter(gcm_ts_BC_drought_ann, rep(1 / duration, duration), sides = 1))
  gcm_ts_BC_pluvial_ann_rm <- as.vector(filter(gcm_ts_BC_pluvial_ann, rep(1 / duration, duration), sides = 1))
  drought_gcm_BC <- min(gcm_ts_BC_drought_ann, na.rm=T)
  pluvial_gcm_BC <- max(gcm_ts_BC_pluvial_ann, na.rm=T)
  droughtyr_gcm_BC <- gcm_ann_yr[which.min(gcm_ts_BC_drought_ann_rm)]
  pluvialyr_gcm_BC <- gcm_ann_yr[which.max(gcm_ts_BC_pluvial_ann_rm)]
  
  # second panel: running mean of uncorrected, corrected, and observed
  title <- paste("Application of MPVBC, RM Values ",begyr,"-",endyr, sep="")

  yrange <- c(c(min(min(gcm_ts_ann_rm, na.rm=T), 
                    min(gcm_ts_BC_drought_ann_rm, na.rm=T), 
                    min(gcm_ts_BC_pluvial_ann_rm, na.rm=T))),
              c(max(max(gcm_ts_ann_rm, na.rm=T), 
                    max(gcm_ts_BC_drought_ann_rm, na.rm=T), 
                    max(gcm_ts_BC_pluvial_ann_rm, na.rm=T))))
  yrange[1] <- yrange[1] - 0.15*diff(yrange)
  #  plot(tt2_rm ~ ttx,  lty=2, type="l", col="blue", lwd=1.)  
  plot(gcm_ts_BC_drought_ann_rm ~ gcm_year_ann, lty=2, type="l", col="blue", lwd=1., xlab="", ylab="mm/day",
       main = title, ylim=yrange)
  mtext(paste(duration,"-yr duration, MPVBCF=", round(mpvbcf, digits=2), sep=""))
  lines(gcm_ts_BC_pluvial_ann_rm ~ gcm_year_ann, col="blue", lty=2, lwd=1.)
  lines(gcm_ts_ann_rm ~ gcm_year_ann, col="blue", lwd=1.)
  if (begyr == 1960) { lines(obs_rm ~ obs_rm_yr, lwd=2) }

  points(c(droughtyr_obs, pluvialyr_obs, droughtyr_gcm_BC, pluvialyr_gcm_BC),
         c(droughtval_obs, pluvialval_obs, droughtval_gcm_BC, pluvialval_gcm_BC),
         pch=c(25, 24, 25, 24), col=c("black", "black", "blue", "blue"),
         bg = c("black", "black", "blue", "blue"), cex=0.75)
  
  mtext("(b)", adj=0, cex=1.25)
  legend("bottom",legend=c("GCM uncorr", "GCM corr", "Obs",
         "Min", "Max"), col=c("blue", "blue", "black", "black", "black"),
         lty=c(1,2,1, NA, NA), ncol=5, cex=0.6, pch=c(NA,NA,NA, 25, 24),
         pt.bg=c(NA, NA, NA, "black", "black")) 
  
  # save figure 
  dev.off()
  par(mfrow=c(1,1)) 
  ##################### End of figure with two panels
  
  # set up data frame to return corrected time series
  gcm_ts_BC <- data.frame(gcm_ts_BC_drought, gcm_ts_BC_pluvial)
  
  return(gcm_ts_BC)
  
} # end of mpvbc_apply function
