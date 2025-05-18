# GTECH_IndependentStudy1
Fall2025 Independent Study


# PROCEDURES FOR REPRODUCTION OF RESULTS
Raw data files are contained in the ‘data’ directory. Observed data is found within ‘cpc_conus_precip’ as ‘precip.V1.0.mon.mean.nc’ while modeled data is found within the ‘gcm_ne_subset’ directory broken down by model and scenario.

The ‘scripts’ directory contains project scripts while the subdirectory ‘visualizers’ contains visualizer functions that are called from plotting scripts in the parent directory.

Figures can be found in the ‘figures’ directory. Running mean range figures can be found in ‘final_running_mean_figures’ while the summary statistic figures for the rmr matrices can be found in ‘rm_range_summary_stats_figures’ directory. Total mean figures are found in the ‘final_mean_precipitation_figures’ dir while their summary stat counterparts are located in ‘mean_precipitation_summary_stats’

A download script, ‘cmip6_total_downloader2.R’, is included that will download model data (please comment/uncomment model names within the script’s lookup vector as needed) though said data is available in the project data directory.

The ‘precip.V1.0.mon.mean.nc’ may be subset to the northeast region area of interest through use of the ‘subset_netcdf_with_shp.R’ script. A pre-subset data file is also included and this step may be skipped.

‘cpc_annual_mu_abinder.R’ and ‘cmip6_abinder_automated_ne_subset.R’ perform the extraction and initial processing of the observed and modeled data respectively. The products of these scripts will be found in the ‘rdata’ directory by default and both scripts point their output to this directory.

‘historical_mean_precip_processor.R’ is used for generating the total average precipitation figures for all models and comparisons with the observed data.

‘mean_precipitation_summary_stat_calc_plotter.R’ aggregates the model data matrices generated in the previous script. It then plots summary statistics (5 number summary + mean and sd) in the same manner with the observed data for comparison.

‘rm_processor_updated_3.R’ is used to calculate running mean ranges, rm range differences, and bias factors across all models. This script also generates figures and saves the produced matrices to a fresh rdata file. The ‘duration’ variable at the top of the script can be used to adjust the running mean window.

‘rm_summary_stat_plotter.R’ is used to generate summary statistic matrices of the observed data and the aggregate of model data. These matrices are used to generate figures.


This project incorporates code created by Dr. Allan Frei
