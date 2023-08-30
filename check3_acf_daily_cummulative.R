# written as standalong check but you
# can skip quite a few of these steps if you run check2 first

library(ncdf4)
library(abind)
library(lubridate)
library(dplyr)

# read in real data
source("step1_convert.R") # rain.array
#t <- as.POSIXct(t, origin = '1960-01-01 00:00', tz = "UTC")  # if t didn't come in as POSIXct

# Adding in rain tolerance - should match that in SWM_v0.R
rain_tol <- (1/24)/1000 # m 
rain.trunc <- rain.array
rain.trunc[rain.trunc < rain_tol] <- 0

# list of fake data files
filenames <- list.files("rain_runs", pattern="*.nc", full.names=TRUE)

# location
lon.p <- 2
lat.p <- 13

# build some functions
datagrab <- function(ncs){
  t.f <- ncvar_get(ncs, "time")
  rain.f <- ncvar_get(ncs, "rainfall") # store the data in a 3-dimensional array
  fillvalue <- ncatt_get(ncs, "rainfall", "_FillValue")
  rain.f[rain.f == fillvalue$value] <- NA
  out <- list(t.f, rain.f)
  return(out)}

real <- as.data.frame(rain.trunc[lon.p, lat.p,])
colnames(real)<- c("rain")
real$date <- t
real$day <- floor_date(real$date, "day")

daily_real <- real %>% group_by(day) %>% 
  summarise(daily = sum(rain)) %>% as.data.frame()
  
real_acf <- acf(as.numeric(daily_real$daily), na.action = na.pass, lag.max = 365, plot = FALSE)
plot(real_acf[2:100], main = "Real daily cummulative rainfall", xlab = "Lag (days)", lwd = 3, col = "red")
#abline(v = c((31+28), (31+28+31+30+31), (31+28+31+30+31+30+31+31), (31+28+31+30+31+30+31+31+30+30+30)), lty = 2, col = "red")

for (b in 1:95){
  fake <- nc_open(filenames[b])
  out.d <- datagrab(fake)
  nc_close(fake)
  # get time data
  tseq <- seq_len(length(out.d)*2) %% 2 # by 2 because two elements in each file: rain and time
  t.f <- unlist(mapply(c, out.d)[tseq == 1]) # all time
  t.f <- datetime <- as.POSIXct(t.f*3600, origin = '1970-01-01 00:00', tz = "UTC")
  rain.f <- array(unlist(out.d[tseq ==0]),dim=c(11,14,length(t.f)))
  
  fake <- as.data.frame(rain.f[lon.p, lat.p,])
  colnames(fake)<- "rain"
  fake$date <- t.f
  fake$day <- floor_date(fake$date, "day")
  
  daily_fake <- fake %>% group_by(day) %>% 
  summarise(daily = sum(rain)) %>% as.data.frame()
  
  fake_acf <- acf(as.numeric(daily_fake$daily), na.action = na.pass, lag.max = 365, plot = FALSE)
  assign(paste0("fake_run_",b), fake_acf)

}

# 1 years' worth
plot(x=NA, y=NA, xlim = c(0,365), ylim = c(-0.05, 0.05), 
     main = "ACF Fake and Real daily cummulative rainfall", 
     xlab="Lag (days)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:length(ACF$acf)], col = "grey")
}
lines(real_acf$acf[2:length(real_acf$acf)], col = "red", lwd = 2)
abline(v = seq(from =30, to = 360, by = 30), lty = 2)

# 100 days worth
plot(x=NA, y=NA, xlim = c(0,100), ylim = c(-0.05, 0.05), 
     main = "ACF Fake and Real daily cummulative rainfall", 
     xlab="Lag (days)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:length(ACF$acf)])
}
lines(real_acf$acf[2:length(real_acf$acf)], col = "red", lwd = 3)
abline(v = seq(from =30, to = 90, by = 30), lty = 2)

# 30 days worth
plot(x=NA, y=NA, xlim = c(0,30), ylim = c(-0.05, 0.05), 
     main = "ACF Fake and Real daily cummulative rainfall", 
     xlab="Lag (days)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:length(ACF$acf)], col = "grey")
}
lines(real_acf$acf[2:length(real_acf$acf)], col = "red", lwd = 3)

# 30 days worth
plot(x=NA, y=NA, xlim = c(0,30), ylim = c(-0.05, 0.05), 
     main = "ACF Fake and Real daily cummulative rainfall", 
     xlab="Lag (days)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:30], add = TRUE)
}
lines(real_acf[2:30], col = "red", lwd = 2, xlab = "Lag (days)", 
     main = "ACF Fake and Real daily cummulative rainfall")


pacf(as.numeric(real$rain), na.action = na.pass)
