#################################################################
# Author: Hayley Corson-Dosch                                   #
# Email: hcorson-dosch@usgs.gov                                 #
# Content: workflow for extracting GLM output from netCDF files #
#################################################################

library(RNetCDF)
library(ncmeta) # need version 0.3.5 or higher - devtools::install_github("https://github.com/hypertidy/ncmeta.git")
library(tidyverse)
library(ncdfgeom) # need version >= v1.1.2

# Before you can run this script, make sure that you have also downloaded
# the `netCDF_extract_utils.R` script from ScienceBase 
# It contains all the functions required for this code to work. Edit the filepath
# below depending on where you saved it relative to this file.
source('src/netCDF_extract_utils.R') 

##### Read in temperature and ice predictions from a netCDF for a set of lakes #####

# Update the filepath for the NetCDF file you are extracting data from. This example 
# script assumes that you have already downloaded it from ScienceBase (item 6206d3c2d34ec05caca53071)
nc_file <- 'lake_temp_preds_GLM_GCM_MIROC5_IA.nc'

# Read in information about netCDF (variables, dates, etc.)
# read_data set to FALSE b/c netCDF is too large for all data to be read in at once
# Warning 'no altitude coordinate found' is expected
nc_info <- read_timeseries_profile_dsg(nc_file, read_data = FALSE)

# Define lake sites of interest
# Can list directly (e.g., c("nhdhr_109943476", "nhdhr_109943604")), or pull from `nc_info$timeseries_id` vector
lake_sites <- c(nc_info$timeseries_id[3:13], nc_info$timeseries_id[1], nc_info$timeseries_id[90:95])

# Pull temperature predictions (for all dates and all depths) for those lakes.
# Can be slow to run for more than a moderate # of sites at once, if
# pulling from a large netCDF file, or if pulling data for deep lakes
# Depth units = meters, temperature units = degrees Celsius
# can specify wide (long_format = FALSE) or long format (long_format = TRUE)
# if wide format, columns are named {site_id}_{depth}
temp_data <- pull_data_for_sites(nc_file, nc_info, var = 'temp', sites = lake_sites, long_format = TRUE)

# Pull boolean ice predictions (for all dates) for those lakes. Note that `ice` is not available
# for the EA-LSTM predictions and the following line will not work if you are working with those.
# Ice units: 1 = ice is present; 0 = no ice is present
# can specify wide (long_format = FALSE) or long format (long_format = TRUE)
# if wide format, columns are named {site_id}
ice_data <- pull_data_for_sites(nc_file, nc_info, var = 'ice', sites = lake_sites, long_format = TRUE)

# If you chose long format for the ice data, it is easy to add the ice flag 
#  column to the associated temperature data with a quick join:
temp_ice_data <- temp_data %>% 
  left_join(ice_data, by = c("site_id", "time"))


##### Read in GCM driver data from a GCM netCDF for a set of lakes #####

# Update the filepath for the GCM NetCDF file you are extracting data from. This example 
# script assumes that you have already downloaded it from ScienceBase (item 6206d3c2d34ec05caca53071)
gcm_nc_file <- 'GCM_MIROC5.nc'

# Read in the lake metadata. This example 
# script assumes that you have already downloaded it from ScienceBase (item 6206d3c2d34ec05caca53071)
lake_metadata <- readr::read_csv('lake_metadata.csv')

# Use the lake metadata and the previously-defined vector of lake site ids to identify
# which GCM cells the lakes fall within
gcm_cell_nos <- lake_metadata %>%
  filter(site_id %in% lake_sites) %>%
  pull(driver_gcm_cell_no) %>%
  unique()

# Pull the GCM data for those cells
# The returned dataframe includes GCM Shortwave, Longwave, AirTemp, 
# RelHum, WindSpeed, Rain, and Snow variables for all dates
# Warning 'no altitude coordinate found' is expected
gcm_data <- pull_gcm_data_for_cells(gcm_nc_file, gcm_cell_nos)
