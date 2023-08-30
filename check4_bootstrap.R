# written as standalong check but you
# can skip quite a few of these steps if you run check2 first

library(ncdf4)
library(abind)
library(lubridate)
library(dplyr)

# read in real data
source("step1_convert.R") # rain.array
#t <- as.POSIXct(t, origin = '1960-01-01 00:00', tz = "UTC")  # if t didn't come in as POSIXct

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

all_fake <- matrix(data = NA, nrow = 1, ncol = 1)
  
# empty plot-need to manually change month name and number below
plot(x=NA, y=NA, xlim = c(0,0.35), ylim=c(0,1), main = "December", xlab="Monthly Rainfall (m)", ylab="Empirical CDF")

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
  assign(paste0("fake_run_",b), fake)
  all_fake <- c(all_fake, fake)
  lines(ecdf(fake[[12]]$sum), do.points = F) # number to match month
}

real <- month_means(rain.array, t)
lines(ecdf(real[[12]]$sum), do.points = F, col = "red", lwd = 3) # number to match month


