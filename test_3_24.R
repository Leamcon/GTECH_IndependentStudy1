library(ncdf4)

# LOOKING AT CATSKILLS
# GRID POINTS FROM SAM
# 42.12 N -74.38 W. is the only grid fully in the catskills
# (42.12N -74.62W), (42.12N -74.88W), (42.12N -75.12W), 
# (42.12N -75.38W), (42.38N -74.38W), (42.38N -74.62W), 
# (42.38N -74.88W), (41.88N -74.38W), and (41.88N -74.62W) 
lon_cat2 <- c(285.625, 285.375, 285.125, 284.875, 284.625,
              285.625, 285.375, 285.125, 285.625, 285.375)
lat_cat2 <- c(42.125, 42.125, 42.125, 42.125, 42.125,
              42.375, 42.375, 42.375, 41.875, 41.875)

lon_cat2 <- lon_cat2+0.25


nc <- ncdf4::nc_open("data/gcm_ne_subset/access_cm2/historical/pr_day_ACCESS-CM2_historical_r1i1p1f1_gn_1950_v1.1.nc")

nc_dims <- nc$dim


print(nc_dims$lon$vals)
