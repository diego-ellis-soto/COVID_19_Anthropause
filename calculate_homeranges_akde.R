# Need outlier removal #####

phases = unique(DATA$baseline)

# one_ind <- DATA %>% filter(individual.local.identifier == 1153545175)

for(unique_animal in unique(DATA$individual.local.identifier)){

  one_ind <- DATA %>% filter(individual.local.identifier == unique_animal)
  
# Loop through all:

#loop through phases and estimate home-range size
for(d in 1:length(phases)){
  
  tmp <-one_ind %>% 
    dplyr::filter(baseline==phases[d])
  
  
  
  # next
  
  if(file.exists(paste("/Users/diegoellis/projects/COVID/Output/Individual_lvl_summaries/HR_results_new/",unique(tmp$study_id), '/',
                       paste(paste(unique(tmp$individual.local.identifier),
                                   phases[d], sep="_"),".RData", sep=""), sep=""))){print(paste0());next}
  
  
  
  if(
    file.exists(
      paste("/Users/diegoellis/projects/COVID/Output/Individual_lvl_summaries/HR_results_new/",
            unique(tmp$study_id), '/',
                       # paste(paste(unique(tmp$individual.local.identifier),phases[d], sep="_"),
                                   paste0(unique(tmp$individual.local.identifier),'_', phases[d],
                                   ".RData"), sep=""))){
    print(paste0(unique(tmp$individual.local.identifier),'_', phases[d], ' already calculated HR'));next}
  
  
  message(paste0('Working with ', unique(tmp$species), ' id ', unique(tmp$individual.local.identifier) ,' ', phases[d]))
  
  
  hr<-as.telemetry(tmp, timeformat="%Y-%m-%d %H:%M:%S")

  
GUESS<-tryCatch(ctmm.guess(hr, CTMM=ctmm(error=TRUE), interactive = FALSE),
                error=function(e) e)

error<-"HDOP"

#select best model
if(inherits(GUESS,"ctmm")){
  FIT<-tryCatch(
    ctmm.select(hr, GUESS, verbose=TRUE, trace=TRUE),
    error=function(e) e)
} else{
  next
}

#add and compare IID isotropic model
if(class(FIT)!="error"){
  FIT$IID<-ctmm.fit(hr)
} else{
  next
}


#home-range estimation
if(inherits(FIT,"list")){
  message("estimating home-range size")
  akde.est<-akde(hr,FIT[[1]])
} else{
  next
}

#occurence estimation
# if(inherits(FIT,"list")){
#   message("estimating occurance range")
#   occ.est<-occurrence(hr,FIT[[1]])
# } else{
#   next
# }

#if verbose=FALSE in ctmm.select(), object is top model
#if verbose=TRUE in ctmm.select(), top model is found in FIT[[1]]

#all the tryCatch() and if() below is because lots of data with issues

ctmm_mod<-tryCatch(row.names(summary(FIT))[1], error=function(e) e) 
if(inherits(ctmm_mod, "error")){
  ctmm_mod=NA
}

area95_est<-tryCatch(summary(akde.est, units=FALSE, level=0.95, level.UD=0.95)$CI[2]/1e+6, 
                     error=function(e) e) 
if(inherits(area95_est, "error")){
  area95_est=NA
}

area95_lc<-tryCatch(summary(akde.est, units=FALSE, level=0.95, level.UD=0.95)$CI[1]/1e+6, 
                    error=function(e) e) 
if(inherits(area95_lc, "error")){
  area95_lc=NA
}

area95_uc<-tryCatch(summary(akde.est, units=FALSE, level=0.95, level.UD=0.95)$CI[3]/1e+6, 
                    error=function(e) e) 
if(inherits(area95_uc, "error")){
  area95_uc=NA
}

area50_est<-tryCatch(summary(akde.est, units=FALSE, level=0.95, level.UD=0.50)$CI[2]/1e+6, 
                     error=function(e) e) 
if(inherits(area50_est, "error")){
  area50_est=NA
}

area50_lc<-tryCatch(summary(akde.est, units=FALSE, level=0.95, level.UD=0.50)$CI[1]/1e+6, 
                    error=function(e) e) 
if(inherits(area50_lc, "error")){
  area50_lc=NA
}

area50_uc<-tryCatch(summary(akde.est, units=FALSE, level=0.95, level.UD=0.50)$CI[3]/1e+6, 
                    error=function(e) e) 
if(inherits(area50_uc, "error")){
  area50_uc=NA
}

#tauP and speed will only be estimated if OUF and sampling interval are appropriate
tau_p_hat=tryCatch(
  tau_p_hat<-summary(FIT[[1]], units=FALSE)$CI[2,2]/86400, error=function(e) e) 
if(inherits(tau_p_hat, "error")){
  tau_p_hat=NA
}

tau_p_lc=tryCatch(
  tau_p_lc<-summary(FIT[[1]], units=FALSE)$CI[2,1]/86400, error=function(e) e) 
if(inherits(tau_p_lc, "error")){
  tau_p_lc=NA
}

tau_p_uc=tryCatch(
  tau_p_uc<-summary(FIT[[1]], units=FALSE)$CI[2,3]/86400, error=function(e) e) 
if(inherits(tau_p_uc, "error")){
  tau_p_uc=NA
}

speed_hat=tryCatch(
  speed_hat<-summary(FIT[[1]], units=FALSE)$CI[3,2]*86400, error=function(e) e) 
if(inherits(speed_hat, "error")){
  speed_hat=NA
}

speed_lc=tryCatch(
  speed_lc<-summary(FIT[[1]], units=FALSE)$CI[3,1]*86400, error=function(e) e) 
if(inherits(speed_lc, "error")){
  speed_lc=NA
}

speed_uc<-tryCatch(
  speed_uc<-summary(FIT[[1]], units=FALSE)$CI[3,3]*86400, error=function(e) e) 
if(inherits(speed_uc, "error")){
  speed_uc=NA
}

nEff<-tryCatch(summary(FIT[[1]])$DOF[2], error=function(e) e) 
if(inherits(nEff, "error")){
  nEff=NA
}

#make row of dataframe with hr info
dat<-data.frame(key=unique(tmp$individual.local.identifier), #species=unique(infolocs(segments)[[1]]$species), 
                phase=phases[d], #season is formatted seas_year, so no need for both seas_yr and yr below
                # yr=year[d],
                # seas_year=seas_year[d],
                seas_start=min(tmp$timestamp),
                seas_end=max(tmp$timestamp),
                ctmm_mod=ctmm_mod,
                error=error,
                area95_est=area95_est, #units must=FALSE to get comparable hr size
                area95_lc=area95_lc, 
                area95_uc=area95_uc, 
                area50_est=area50_est, #units must=FALSE to get comparable hr size
                area50_lc=area50_lc, 
                area50_uc=area50_uc,            
                tau_p_hat=tau_p_hat, 
                tau_p_lc=tau_p_lc, 
                tau_p_uc=tau_p_uc,
                speed_hat=speed_hat, 
                speed_lc=speed_lc, 
                speed_uc=speed_uc,
                nEff=nEff,
#                svf_test=svf_test, 
                nlocs=nrow(tmp),
                ndays= round(as.numeric(max(tmp$timestamp)-min(tmp$timestamp)), 2))

#area in meters, tau_p and tau_v in seconds, speed in meters/second
#converted area to km^2, tau p to days, speed to meters/day


#area in meters, tau_p and tau_v in seconds, speed in meters/second
#converted area to km^2, tau p to days, speed to meters/day

### save summary data to file (could add a write.csv if don't want to deal with RData files)


# Put loop at an end #####


print("saving data to file")


dir.create(paste0("/Users/diegoellis/projects/COVID/Output/Individual_lvl_summaries/HR_results_new/", unique(tmp$study_id), '/'))
save(dat, file=paste("/Users/diegoellis/projects/COVID/Output/Individual_lvl_summaries/HR_results_new/",unique(tmp$study_id), '/',
                     paste(paste(unique(tmp$individual.local.identifier),
                                 phases[d], sep="_"),".RData", sep=""), sep=""))

#pull out UD, calculate 50% and 95% isopleths, save to file
library(rgdal)
library(raster)

#AKDE
print("saving HR UDs/shapefiles")

UD.hr<-raster::raster(akde.est, DF="PMF")

# UD.hr<-raster::projectRaster(UD.hr, crs="+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
# 
# PROJ <- " +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
# UD.hr <- raster::projectRaster(UD.hr, crs = PROJ)

iso.95<-SpatialPolygonsDataFrame.UD(akde.est,level.UD=0.95)
# iso.95<-spTransform(iso.95, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

iso.50<-SpatialPolygonsDataFrame.UD(akde.est,level.UD=0.50)
# iso.50<-spTransform(iso.50, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))


writeRaster(UD.hr, overwrite=TRUE,
            filename=paste("/Users/diegoellis/projects/COVID/Output/Individual_lvl_summaries/HR_results_new/",
                           paste(paste(unique(tmp$individual.local.identifier), 
                                       phases[d], sep="_"),".RData", sep=""), sep=""))

writeOGR(iso.95, check_exists=TRUE, overwrite_layer=TRUE,
         dsn="/Users/diegoellis/projects/COVID/Output/Individual_lvl_summaries/HR_results_new/", 
         layer=paste(unique(tmp$individual.local.identifier), phases[d], "95", sep="_"),
         driver="ESRI Shapefile")

writeOGR(iso.50, check_exists=TRUE, overwrite_layer=TRUE, 
         dsn="/Users/diegoellis/projects/COVID/Output/Individual_lvl_summaries/HR_results_new/", 
         layer=paste(unique(tmp$individual.local.identifier), phases[d], "50", sep="_"),
         driver="ESRI Shapefile")

}
}




