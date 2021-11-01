library(move)
library(tidyverse)
library(plyr)
library(lubridate)
library(MVNH)

loginStored<-movebankLogin(username="COVID-19_IBS", password="covid19ibs")

cougar <- move('../data//Olympic Cougar Project-6479261802587306352.csv')

lockdown_start_usa <- as.POSIXct('2020-03-01 00:00:00')
lockdown_easing_new_england <- as.POSIXct('2020-06-01 00:00:00')
baseline_start <- as.POSIXct('2019-03-10 00:00:00')
baseland_end <- as.POSIXct('2019-06-20 00:00:00')
jday_of_baseline_end_2020 <- yday(baseland_end)
cougar$track_id <- cougar@trackId
summary_cougar <- ddply(  as.data.frame(cougar), 'tag.local.identifier', function(x){
  df = data.frame( who = unique(x$track_id),
                   track_start = range(x$timestamp)[1],
                   track_end =  range(x$timestamp)[2]
  )
  df[year(df$track_end) == 2020,]
})
cougar$baseline[cougar$timestamp >= baseline_start & cougar$timestamp <= baseland_end] <- as.character("baseline")
cougar$baseline[cougar$timestamp >= lockdown_start_usa & cougar$timestamp <= lockdown_easing_new_england] <- as.character("lockdown")
cougar$baseline[cougar$timestamp  >= lockdown_easing_new_england] <- as.character("after_lockdown")
table(cougar$baseline)
didi <- cougar[['Didi']]
table(didi$baseline)
# break into before during after

didi_before <- didi[na.omit(didi$baseline) == "baseline",]
didi_bef_df <- data.frame(lst = didi_before@data$MODIS.Land.Surface.Temp...Emissivity.1km.Daily.Terra.Land.Surface.Temperature.Night,
                           ndvi = didi_before@data$MODIS.Land.Vegetation.Indices.250m.16d.Terra.NDVI)
MVNH_det(na.omit(didi_bef_df))


didi_lock <- didi[na.omit(didi$baseline) == "lockdown",]
didi_lock_df <- data.frame(lst = didi_lock@data$MODIS.Land.Surface.Temp...Emissivity.1km.Daily.Terra.Land.Surface.Temperature.Night,
                          ndvi = didi_lock@data$MODIS.Land.Vegetation.Indices.250m.16d.Terra.NDVI)
MVNH_det(na.omit(didi_lock_df))

didi_aft <- didi[na.omit(didi$baseline) == "after_lockdown",]
didi_aft_df <- data.frame(lst = didi_aft@data$MODIS.Land.Surface.Temp...Emissivity.1km.Daily.Terra.Land.Surface.Temperature.Night,
                           ndvi = didi_aft@data$MODIS.Land.Vegetation.Indices.250m.16d.Terra.NDVI)


MVNH_det(na.omit(didi_aft_df))

MVNH_dissimilarity(na.omit(didi_bef_df), na.omit(didi_lock_df))
MVNH_dissimilarity(na.omit(didi_bef_df), na.omit(didi_aft_df))
