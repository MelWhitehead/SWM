
library(ncdf4)
library(abind)
library(lubridate)
library(dplyr)
library(car)

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
lon.p <- 5
lat.p <- 5

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

# some plots to visualise monthly rain distributions
hist(real[[1]]$sum*1000, main ="January", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[2]]$sum*1000, main ="February", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[3]]$sum*1000, main ="March", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[4]]$sum*1000, main ="April", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[5]]$sum*1000, main ="May", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[6]]$sum*1000, main ="June", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[7]]$sum*1000, main ="July", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[8]]$sum*1000, main ="August", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[9]]$sum*1000, main ="September", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[10]]$sum*1000, main ="October", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[11]]$sum*1000, main ="November", xlab = "Monthly rainfall (mm)", breaks = 8)
hist(real[[12]]$sum*1000, main ="December", xlab = "Monthly rainfall (mm)", breaks = 8)

shapiro.test(real[[12]]$sum)

months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
p_val <- matrix(data = NA, nrow = 95, ncol = 12)
s_val <- matrix(data = NA, nrow = 95, ncol = 12)
shap_p <- matrix(data = NA, nrow = 95, ncol = 12)
b_val <- matrix(data = NA, nrow = 95, ncol = 12)
v_val <- matrix(data = NA, nrow = 95, ncol = 12)
var <- matrix(data = NA, nrow = 95, ncol = 12)
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
  assign(paste0("fake_run_",b), fake)
  for (c in 1:12) {
    var[b,c] <- var(fake[[c]]$sum)
    
    # t-test
    l <- t.test(fake[[c]]$sum, real[[c]]$sum)
    #assign(paste0("t_", months[c], "_run_", b),l)
    s_val[b,c] <- l$statistic
    p_val[b,c] <- l$p.value
    
    # normality test
    m <- shapiro.test(fake[[c]]$sum)
    shap_p[b,c] <- m$p.value
    #assign(paste0("shapiro_", months[c], "_run_", b),m)
    # real one doesn't need to be in the outer run loop if slow.
    n <- shapiro.test(real[[c]]$sum) 
    #assign(paste0("shapiro_", months[c], "_real_", b),n)

    # equal variance if normal
    v <- data.frame(source =c(rep("real", 40), rep("fake",40)), rain = c(real[[c]]$sum, fake = fake[[c]]$sum))
    bart <- bartlett.test(v$rain,v$source)
    #assign(paste0("bart_", months[c], "_run_", b),bart)
    b_val[b,c] <- bart$p.value
    
    # equal variance if not-normal
    lev <- leveneTest(v$rain, as.factor(v$source))
    #assign(paste0("lev_", months[c], "_run_", b),lev)
    v_val[b,c] <- lev$`Pr(>F)`[1]
  }
}

# how many tests signif? (p < 0.05)?
length(p_val[p_val<0.05])
length(shap_p[shap_p<0.05])
length(b_val[b_val<0.05])
length(v_val[v_val<0.05])
# which ones are they?
ind <- which(p_val<0.05, arr.ind = TRUE)
ind <- which(shap_p<0.05, arr.ind = TRUE)
ind <- which(v_val<0.05, arr.ind = TRUE)
ind # cols are months, rows are fake run number

# various plots below here
q <- 12
hist(var[,q]*1000, main = "Variance (fake) for December", xlab = "Variance (mm)")
abline(v = var(real[[q]]$sum, na.rm = TRUE)*1000, col = "red", lwd = 3)
#abline(v = mean(var[,q])*1000, col = "black", lwd = 3, lty = 2)

summer <- c(12,1,2)
autumn <- c(3,4,5)
winter <- c(6,7,8)
spring <- c(9,10,11)
q <- spring
hist(var[,q]*1000, main = "Variance (fake) for Spring", xlab = "Variance (mm)")
abline(v = var(c(real[[q[1]]]$sum, real[[q[2]]]$sum, real[[q[3]]]$sum), na.rm = TRUE)*1000, col = "red", lwd = 3)


boxplot(cbind(fake[[1]]$sum, real[[1]]$sum), main = "January", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[2]]$sum, real[[2]]$sum), main = "February", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[3]]$sum, real[[3]]$sum), main = "March", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[4]]$sum, real[[4]]$sum), main = "April", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[5]]$sum, real[[5]]$sum), main = "May", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[6]]$sum, real[[6]]$sum), main = "June", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[7]]$sum, real[[7]]$sum), main = "July", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[8]]$sum, real[[8]]$sum), main = "August", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[9]]$sum, real[[9]]$sum), main = "September", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[10]]$sum, real[[10]]$sum), main = "October", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[11]]$sum, real[[11]]$sum), main = "November", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")
boxplot(cbind(fake[[12]]$sum, real[[12]]$sum), main = "December", names = c("Fake", "Real"), ylab = "Monthly rainfall (m)")


boxplot(cbind(fake[[1]]$sum, fake[[2]]$sum, fake[[3]]$sum, fake[[4]]$sum, fake[[5]]$sum, fake[[6]]$sum,
              fake[[7]]$sum, fake[[8]]$sum, fake[[9]]$sum, fake[[10]]$sum, fake[[11]]$sum, fake[[12]]$sum),
        main = "Fake", names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                 "Oct", "Nov", "Dec"), ylim = c(0,0.35), ylab = "Monthly rainfall (m)")

boxplot(cbind(real[[1]]$sum, real[[2]]$sum, real[[3]]$sum, real[[4]]$sum, real[[5]]$sum, real[[6]]$sum,
              real[[7]]$sum, real[[8]]$sum, real[[9]]$sum, real[[10]]$sum, real[[11]]$sum, real[[12]]$sum),
        main = "Real", names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                                 "Oct", "Nov", "Dec"), ylim = c(0,0.35), ylab = "Monthly rainfall (m)")

boxplot()


plot(p_val[,], main = "t_test results", xlab = "p-value with run number", ylab = "p-value with month", xlim = c(0,1), ylim = c(0,1))

plot(c(1:(12*95)), c(p_val), main = "t_test results", ylab = "p-value", xlab = "index") #, xlim = c(0,1), ylim = c(0,1))


plot(ecdf(p_val[,1]), main = "January", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,2]), main = "February", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,3]), main = "March", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,4]), main = "April", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,5]), main = "May", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,6]), main = "June", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,7]), main = "July", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,8]), main = "August", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,9]), main = "September", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,10]), main = "October", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,11]), main = "November", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val[,12]), main = "December", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")

# logged version as some t-test conditions not always met
{
p_val_log <- matrix(data = NA, nrow = 95, ncol = 12)
s_val_log <- matrix(data = NA, nrow = 95, ncol = 12)
shap_p_log <- matrix(data = NA, nrow = 95, ncol = 12)
b_val_log <- matrix(data = NA, nrow = 95, ncol = 12)

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
  assign(paste0("fake_run_",b), fake)
  for (c in 1:12) {
    
    # t-test
    l <- t.test(log(fake[[c]]$sum), log(real[[c]]$sum))
    assign(paste0("t_log_", months[c], "_run_", b),l)
    s_val_log[b,c] <- l$statistic
    p_val_log[b,c] <- l$p.value
    
    # normality test
    m <- shapiro.test(log(fake[[c]]$sum))
    shap_p_log[b,c] <- m$p.value
    #assign(paste0("shapiro_log_", months[c], "_run_", b),m)
    # real one doesn't need to be in the outer run loop if slow.
    n <- shapiro.test(log(real[[c]]$sum)) 
    #assign(paste0("shapiro_log_", months[c], "_real_", b),n)

    # equal variance
    v <- data.frame(source =c(rep("real", 40), rep("fake",40)), rain = c(log(real[[c]]$sum), fake = log(fake[[c]]$sum)))
    bart <- bartlett.test(v$rain,v$source)
    assign(paste0("bart_log_", months[c], "_run_", b),bart)
    b_val_log[b,c] <- bart$p.value
  }
}


} # end of logged version

# how many tests signif? (p < 0.05)?
length(p_val_log[p_val_log<0.05])
length(shap_p_log[shap_p_log<0.05])
length(b_val_log[b_val_log<0.05])
# which ones are they? (to check for e.g., consistently off months)
ind <- which(p_val_log<0.05, arr.ind = TRUE)
ind # cols are months, rows are fake run number
ind <- which(shap_p_log<0.05, arr.ind = TRUE)
ind <- which(b_val_log<0.05, arr.ind = TRUE)




plot(p_val_log[,], main = "t_test results (log)", xlab = "p-value with run number", ylab = "p-value with month", xlim = c(0,1), ylim = c(0,1))

plot(c(1:(12*95)), c(p_val_log), main = "t_test results (log)", ylab = "p-value", xlab = "index") #, xlim = c(0,1), ylim = c(0,1))


plot(ecdf(p_val_log[,1]), main = "January", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,2]), main = "February", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,3]), main = "March", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,4]), main = "April", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,5]), main = "May", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,6]), main = "June", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,7]), main = "July", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,8]), main = "August", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,9]), main = "September", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,10]), main = "October", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,11]), main = "November", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(p_val_log[,12]), main = "December", xlab = "t_test: p-value, log", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")

# remove t-tests that are not ok because of bartlett and shapiro?
# which ones are they?
ind_s <- which(shap_p<0.05, arr.ind = TRUE)
ind_b <- which(b_val<0.05, arr.ind = TRUE)

p_val_ok <- p_val
p_val_ok[ind_s]<- NA
p_val_ok[ind_b]<-NA

plot(p_val_ok[,], main = "t_test results (ok)", xlab = "p-value with run number", ylab = "p-value with month", xlim = c(0,1), ylim = c(0,1))

plot(c(1:(12*95)), c(p_val_ok), main = "t_test results (ok)", ylab = "p-value", xlab = "index") #, xlim = c(0,1), ylim = c(0,1))

plot(ecdf(c(p_val_ok)), main = "All (ok)", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(c(p_val)), main = "All", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
plot(ecdf(c(p_val_log)), main = "All (log)", xlab = "t_test: p-value", xlim = c(0,1))
abline(a = 0, b = 1, col = "red")
