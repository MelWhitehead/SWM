# write to netcdf here
# fake array is lon, lat, time

filename <- filename_please

nx <- length(lon)
ny <- length(lat)
nt <- length(t_total)

londim <- ncdim_def("longitude", "degrees_east", as.double(lon))
latdim <- ncdim_def("latitude", "degrees_north", as.double(lat))
timedim <- ncdim_def("time","hours", as.double(t_total)/3600, unlim=TRUE)

mv <- -999 #missing value to use
var_rain <- ncvar_def("rainfall", "mm", list(londim, latdim, timedim), mv) 

ncnew <- nc_create(filename, list(var_rain))
ncvar_put(ncnew, var_rain, fake.array, start=c(1,1,1), count=c(nx,ny,nt))

ncatt_put(ncnew, "longitude", "axis", "X")
ncatt_put(ncnew, "latitude", "axis", "Y")
ncatt_put(ncnew, "time", "axis", "T")

ncatt_put(ncnew,0,"title","Simulated_rainfall")
ncatt_put(ncnew,0,"code","SWM_v0")
ncatt_put(ncnew,0,"source","MGW, Massey University")
ncatt_put(ncnew,0,"references", "Whitehead and Bebbington 2023")
history <- paste("created ", date(), sep=", ")
ncatt_put(ncnew,0,"history",history)

nc_close(ncnew)


