# Build wet- and dry-spell duration distributions from data

# -- Build wet + dry groups for all data RETAINING start date
# -- Then set sampling strategy to get date-suitable weather


#clean data
rain.array[is.na(rain.array)] <- 0
rain.array[rain.array < 0] <- 0

#inputs are rain.array
# choose a random driver cell
lon_d <- sample.int(length(lon),1)
lat_d <- sample.int(length(lat),1)

rain_A <- rain.array[lon_d,lat_d,] # ERA5 cell for driver

# group by wet (>rain_tol) and dry (<= rain_tol)
blocks_all <- as.data.frame(rain_A) %>% mutate_if(is.numeric, ~1 * (. > rain_tol)) # set to ones (wet) and zeros (dry)
#hist(blocks_all$rain_A) # visualise ratio

tab <- data.table(var = blocks_all$rain_A)
tab[, id := rleid(var)]
tab$time <- t

# Create weather blocks
play<-tab
block_nums<- unique(play$id)
block_times <- play[match(unique(block_nums), play$id),]
block_times$end <- block_times$time
block_times$end[1:length(block_times$time)-1] <- block_times$time[2:length(block_times$time)] - 60*60 # end time is start of next - one hour
block_times$end[length(block_times$time)] <- t[length(t)-1] # last data stamp

# set wet and dry blocks
block_wet <- block_times[which(block_times$var==1),]
block_dry <- block_times[which(block_times$var==0),]

rm(list=setdiff(ls(), c("rain.array", "t","nruns", "lon", "lat", "block_wet", "block_dry", "lon_d", "lat_d", "days", "day_rain", "td", "ts")))

