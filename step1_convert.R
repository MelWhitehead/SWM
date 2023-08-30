# WARNING! The ERA5 data for hourly total precipitation area actually ACCUMULATION so far for that day.
# So, 00-01 is fine, 01-02 is calculated from value at 02 MINUS value at 01, and so on.
# Resets every day and if you don't correct for this, you see dubious patterns at 24, 48 etc hours.

filenames <- list.files("data", pattern="*.nc", full.names=TRUE)
nc_list <- lapply(filenames, nc_open)

# read x,y from first file
lon <- ncvar_get(nc_list[[1]], "longitude")
lat <- ncvar_get(nc_list[[1]], "latitude")

datagrab<-function(ncs){
  t <- ncvar_get(ncs, "time")
  rain <- ncvar_get(ncs, "tp") # store the data in a 3-dimensional array
  fillvalue <- ncatt_get(ncs, "tp", "_FillValue")
  rain[rain == fillvalue$value] <- NA
  out <- list(t, rain)
  return(out)}
  
out_all <- lapply(nc_list, datagrab)
lapply(nc_list, nc_close) # prints length(t) for each file if correctly closed

tseq <- seq_len(length(out_all)*2) %% 2 # by 2 because two elements in each file: rain and time
t <- unlist(mapply(c, out_all)[tseq == 1]) # all time
t <- datetime <- as.POSIXct(t*3600, origin = '1900-01-01 00:00', tz = "UTC")

rain <- mapply(c, out_all)[tseq == 0, drop = FALSE]
rain.array <- unlist(do.call("abind", c(rain, along=3)))
dimnames(rain.array) <- list(lon, lat, t)

# SWITCH FROM ACCUMULATED TO HOURLY RAINFALL
rain.arrayn<- array(NA, dim = c(length(lon), length(lat), length(t)+1)) # Might not be required, depends on specific era5 data
rain.arrayn[,,-length(t)+1]<-rain.array
rain.array <- rain.arrayn

# for all squares
for (a in 1:length(lon)) {
  for (b in 1:length(lat)) {
    ab_cell <- rain.array[a,b,]
    ab_m <- matrix(ab_cell, nrow = ceiling(length(ab_cell)/24), ncol = 24, byrow = TRUE)
    for (i in 24:2) {
      ab_m[,i] <- ab_m[,i]-ab_m[,i-1]
    }
    ab_v <- c(t(ab_m)) # transpose first to retain order
    rain.array[a,b,]<- ab_v # replace with hourly data
  }
}

# prepare output
t[length(t)+1]<-NA
dimnames(rain.array) <-list(lon, lat, t)

day_rain<- rain.arrayn[ , ,seq(from = 24, to = length(t), by = 24)]
days <- t[seq(from = 24, to = length(t), by = 24)]
dimnames(day_rain) <-list(lon, lat, days)

rm(list=setdiff(ls(), c("rain.array", "day_rain", "t", "days", "lon", "lat", "nruns", "td", "ts", "rain_tol")))
