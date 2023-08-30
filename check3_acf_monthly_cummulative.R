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

month_means <- function(r.array, r.time){
  months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  for (a in 1:12){
    mth <- as.data.frame(r.array[lon.p,lat.p,month(r.time)==a])
    colnames(mth) <- "rain"
    mth$yr <- (year(r.time[month(r.time) ==a]))
    mth <- mth %>% group_by(yr) %>% summarise(mean = mean(rain), var = var(rain), sum = sum(rain)) # mean hourly rainfall
    assign(months[a],mth)
  }
  out <- list(jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec)
  return(out)}

#real <- month_means(rain.array, t)
real <- month_means(rain.trunc, t) # truncated via rain threshold
for (c in 1:9){ # adds month data to year data
  real[[c]][[1]]<- paste0(real[[c]][[1]], "-0", c)
}
for (c in 10:12){ # adds month data to year data
  real[[c]][[1]]<- paste0(real[[c]][[1]], "-", c)
}
assign("real_run", real)
fseq <- seq_len(length(real)*4) %% 4 # by 2 because two elements in each file: rain and time
yr_mth <- unlist(mapply(c, real)[fseq == 1]) # all year-month data
r_mth <- unlist(mapply(c, real)[fseq == 0]) # all month summed data
yr_mth_r <- as.data.frame(t(rbind(yr_mth, r_mth)))
yr_mth_r <- yr_mth_r[order(yr_mth_r$yr_mth),] # put in chronological order
acf(as.numeric(yr_mth_r$r_mth), na.action = na.pass, lag.max = 48)
real_acf <- acf(as.numeric(yr_mth_r$r_mth), na.action = na.pass, lag.max = 48, plot = FALSE)
plot(real_acf[2:48], main = "Monthly cummulative rainfall: Real", xlab = "Lag (months)", lwd = 3, col = "red")


# get fake data from all
for (b in 1:95){
  fake <- nc_open(filenames[b])
  out.d <- datagrab(fake)
  nc_close(fake)
  # get time data
  tseq <- seq_len(length(out.d)*2) %% 2 # by 2 because two elements in each file: rain and time
  t.f <- unlist(mapply(c, out.d)[tseq == 1]) # all time
  t.f <- datetime <- as.POSIXct(t.f*3600, origin = '1970-01-01 00:00', tz = "UTC")
  rain.f <- array(unlist(out.d[tseq ==0]),dim=c(11,14,length(t.f)))
  
  fake <- month_means(rain.f, t.f)
  for (c in 1:9){ # adds month data to year data
    fake[[c]][[1]]<- paste0(fake[[c]][[1]], "-0", c)
  }
  for (c in 10:12){ # adds month data to year data
    fake[[c]][[1]]<- paste0(fake[[c]][[1]], "-", c)
  }
  assign(paste0("fake_run_",b), fake)
  fseq <- seq_len(length(fake)*4) %% 4 # by 2 because two elements in each file: rain and time
  yr_mth <- unlist(mapply(c, fake)[fseq == 1]) # all year-month data
  r_mth <- unlist(mapply(c, fake)[fseq == 0]) # all month summed data
  yr_mth_r <- as.data.frame(t(rbind(yr_mth, r_mth)))
  yr_mth_r <- yr_mth_r[order(yr_mth_r$yr_mth),] # put in chronological order
  
  fake_acf <- acf(as.numeric(yr_mth_r$r_mth), na.action = na.pass, lag.max = 480, plot = FALSE)
  assign(paste0("fake_run_",b), fake_acf)

}


# 1 years' worth
plot(x=NA, y=NA, xlim = c(0,12), ylim = c(-0.2, 0.2), 
     main = "ACF Fake and Real monthly cummulative rainfall", 
     xlab="Lag (months)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:length(ACF$acf)], col = "grey")
}
lines(real_acf$acf[2:length(real_acf$acf)], col = "red", lwd = 2)
#abline(v = seq(from =1, to = 12, by = 1), lty = 2)

# 40 years' worth
plot(x=NA, y=NA, xlim = c(0,480), ylim = c(-0.2, 0.2), 
     main = "ACF Fake and Real monthly cummulative rainfall", 
     xlab="Lag (months)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:length(ACF$acf)], col = "grey")
}
lines(real_acf$acf[2:length(real_acf$acf)], col = "red", lwd = 2)
abline(v = seq(from =12, to = 480, by = 12), lty = 2)

# 10 years' worth
plot(x=NA, y=NA, xlim = c(0,120), ylim = c(-0.05, 0.05), 
     main = "ACF Fake and Real monthly cummulative rainfall", 
     xlab="Lag (months)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:length(ACF$acf)], col = "grey")
}
lines(real_acf$acf[2:length(real_acf$acf)], col = "red", lwd = 2)
abline(v = seq(from =12, to = 120, by = 12), lty = 2)

# 4 years' worth
plot(x=NA, y=NA, xlim = c(0,48), ylim = c(-0.2, 0.2), 
     main = "ACF Fake and Real monthly cummulative rainfall", 
     xlab="Lag (months)", ylab = "ACF")
for (b in 1:95){
  ACF <- get(paste0("fake_run_",b))
  lines(ACF$acf[2:length(ACF$acf)], col = "grey")
}
lines(real_acf$acf[2:length(real_acf$acf)], col = "red", lwd = 2)
abline(v = seq(from =0, to = 48, by = 12), lty = 2)

