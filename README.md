# SWM
Stochastic Weather Model (SWM) code

Download ERA5 data across required region from https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=form
Select Total Precipitation from Wind, Pressure and Precipitation, and continous hourly data for as many years as feasible 
(recommend 40+)
Put this data in a folder called "data" in the same folder as the rest of the .R files

Open SWM_v0.R
Set number of runs required (nruns)
Set rain tolerance (rain_tol)
Set desired output start time (ts)
Set desired output duration (td)

Run all code
  step1_convert.R # Reads and processes ERA5 data in the data folder
  step2_build_weather_blocks.R # Builds blocks of wet and dry according to rain_tol
  step3_random_weather_count.R # Creates stochastic rain array (simulated data)
  step4_write_rain_netcdf.R # Writes this rain array out as a netcdf

The above produces nruns of simulated data.

Statistical checks are included in the following: 
  check1_students_t_test.R  # Looks at monthly means and variance (ANOVA) between real and simulated data
                            # Also includes Shapiro-Wilks tests for normality
                            # Also includes Bartlett (normal) and Levene (not normal) tests for ANOVA
  
  check2_tukey.R            # Looks at whether source (real or simulated) is a significant factor in the prediction of total monthly rainfall
  
  check3_acf_daily_cummulative.R   # Temporal trends on daily timescale using autocorrelation function
  check3_acf_monthly_cummulative.R # Temporal trends on monhtly timescale using autocorrelation function

  check4_bootstrap.R        # Compares empirical CDFs for simulated data with that for the real data
                            # can build 95th percentile envelopes using nruns = 95
