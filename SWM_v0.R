# SWM Master file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages required:
library(lubridate)
library(ncdf4)
library(abind)
library(dplyr)
library(data.table)
library(purrr)

# User-defined inputs:
nruns <- 95 # number of total simulations

# Set tolerance for "wet" hour
# QUITE IMPORTANT PARAMETER: do sensitivity check after step 2
# histogram_visualisation_blocks.R code can help with this
rain_tol <- (1/24)/1000 # m 

# output desired start time (ts) and duration (td)
ts <- as.POSIXct("2023-01-01 00:00", tz = "UTC") # 1st Jan 2023
td <- 40 * 365 * 24 * 60 * 60 # years * days * hours * minutes * seconds of weather required - NB: min res == hrly

# Step 1: Read and compile ERA5 NetCDF Files for rain
# Before running, make sure your NetCDF files are in the data folder
source("step1_convert.R") # RAIN

# Step 2: Build weather blocks
source("step2_build_weather_blocks.R") # < 1 second
 
# Step 3: Generate stochastic weather
for (i in 1:nruns){
  source("step3_random_weather_count.R")
  assign(paste("rain.array",i, sep = ""),fake.array)
  filename_please <- paste("test_rain",i,".nc", sep = "") # output file name
  source("step4_write_rain_netcdf.R")
  }

# Step 4: Quick check of netcdf files
sim_rain <- nc_open("test_rain1.nc")
print(sim_rain)
nc_close(sim_rain)


