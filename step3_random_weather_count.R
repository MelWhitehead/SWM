# generate random weather
# ONCE

# randomly choose wet or dry start
w_x <- sample(c(1,0),1) # 1: wet, 0: dry

t_now <- ts # start time
t_total <- seq(from=ts, to = ts+td, by = "hour")
fake.array <- array(NaN, c(length(lon),length(lat),length(t_total)))# build empty array for output

n_blocks <- 0 # count for number of weather blocks used
p<-1 # starting location for fake.array

while (t_now<ts+td){
 #   print(w_x) # just to watch it working 0 > 1 > 0
  if (w_x == 1) { # wet
    block <- block_wet
    } else block <- block_dry
    
    # weather block is sampled by month
    w_b <- sample_n(block[which(month(block$time)==month(t_now)), ],1)
    # locate this time period from rain.array + pull data
    here <- c(which(t == w_b$time), which(t ==w_b$end))
    this <- rain.array[,,here[1]:here[2]]
    # supplant brick in fake.array with these data
    q <- p + (here[2]-here[1])
    if (q < length(t_total))
    { # cut off for last block
      fake.array[,,p:q] <- this
    } else fake.array[,,p:length(t_total)] <- this[,,1:length(p:length(t_total))]
      
    t_now <- t_now + 60*60 + (w_b$end-w_b$time)
    p<-q+1
    
    # need alternating blocks of wet and dry
    if (w_x == 1) {
      w_x <- 0
    } else w_x <- 1
    n_blocks <- n_blocks +1
    
}

 
