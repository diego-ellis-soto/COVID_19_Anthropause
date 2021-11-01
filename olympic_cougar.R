require(lubridate)
require(move)
library(adehabitatLT)
library(raster)
library(spatial)
library(rgdal)
library(dplyr)
library(ctmm)
library(recurse)
require(plyr)
require(tidyverse)
require(move)
require('amt')
require(MVNH)

loginStored<-movebankLogin(username="COVID-19_IBS", password="covid19ibs")

cougar <- move('/Users/diegoellis/Downloads/Olympic Cougar Project-6479261802587306352/Olympic Cougar Project-6479261802587306352.csv')

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Lockdown dates
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

lockdown_start_usa <- as.POSIXct('2020-03-01 00:00:00')
lockdown_easing_new_england <- as.POSIXct('2020-06-01 00:00:00')
baseline_start <- as.POSIXct('2019-03-10 00:00:00')
baseland_end <- as.POSIXct('2019-06-20 00:00:00')
jday_of_baseline_end_2020 <- yday(baseland_end)

cougar$baseline[cougar$timestamp >= baseline_start & cougar$timestamp <= baseland_end] <- as.character('baseline')
cougar$baseline[cougar$timestamp >= lockdown_start_usa & cougar$timestamp <= lockdown_easing_new_england] <- as.character('lockdown')
cougar$baseline[cougar$timestamp  >= lockdown_easing_new_england] <- as.character('after_lockdown')
table(cougar$baseline)

cougar$track_id <- cougar@trackId


summary_cougar <- ddply(  as.data.frame(cougar), 'tag.local.identifier', function(x){
  df = data.frame( who = unique(x$track_id),
                   track_start = range(x$timestamp)[1],
                   track_end =  range(x$timestamp)[2],
                   n_days = round(as.numeric(diff(range(x$timestamp)))),
                   n_fixes = nrow(x)
  )
  df[year(df$track_end) == 2020,]
})

summary_cougar %>% subset(track_start <= baseline_start) %>% subset(track_end >= lockdown_easing_new_england)
# Subsample to only animals that have suitable data: 
# seg_summary <- ii %>%
# group_by(migrating) %>%
#   summarise("n_records" = n(),
#             "n_days" = n_distinct(doy)) %>%
#   st_set_geometry(NULL)
#  summary_deer <- summary_deer[ summary_deer$jday_2020 > jday_of_baseline_end_2020,]



didi <- cougar[['Didi']]
didi_df <- as.data.frame(didi)


# --- --- --- --- --- --- --- --- --- ---
# Create a AMT object:
# --- --- --- --- --- --- --- --- --- ---
trk <- make_track(didi_df, .x = location.long, .y = location.lat,
.t = timestamp, id = individual.local.identifier,
species = individual.taxon.canonical.name,
movebank_study = study.name, 
crs = CRS("+init=epsg:4326"),
.phase = baseline)  %>% time_of_day() %>%
mutate(
year=year(t_),
month = month(t_),
week = week(t_),
jday = yday(t_),
hour = hour(t_),
species = species,
movebank_study = movebank_study
)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Calculate displacement code ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# gorup by id and phase if many individuals and nest by id and phase
trk_daily_nested <- trk %>% filter(.phase %in% c('baseline', 'lockdown'))  %>% nest(data = c(-'id', -'.phase')) %>%
  mutate(steps = map(data, function(x) 
    x %>%   track_resample(rate = hours(24), tolerance = minutes(60) ) %>% steps_by_burst() ))

trk_daily_nested %>% select(-data) %>% unnest(cols=steps) %>% select(id,.phase, x1_,y1_,t1_,sl_)  %>% group_by(.phase) %>%
  dplyr::summarise(mean_sl = mean(sl_), n = n(), id = unique(id))

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# AKDE ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

require(ctmm)

base <- didi[na.omit(didi$baseline) == 'baseline',]
lock <- didi[na.omit(didi$baseline) == 'lockdown',]

hr_base<-as.telemetry(base, timeformat="%Y-%m-%d %H:%M:%S")
hr_lock<-as.telemetry(lock, timeformat="%Y-%m-%d %H:%M:%S")

make_akde_homerange <- function(evt_telem){ # Input is a telemetry object:
# get initial acf guess
guess <- ctmm.guess(evt_telem, interactive = F)
# acf model selection using guess as init
fit <- ctmm.select(evt_telem, guess)
# fit 
akde <- akde(evt_telem, fit)
}

akde_base <- make_akde_homerange(hr_base)
akde_lock <- make_akde_homerange(hr_lock)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Calculate niche metrics ####
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

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



# Ocean wave observaiton networks suffer from gaps, particularly in open ocean -> Ocean currents rely on buoys and satellite telemetry -> Useufl because Jason-3 1 dat coverage and buiots ardhuin et al 2019 have spatial gaps in ocean -> Satellite restrictted only to wave height !! -> Arduin et al. 2019 -> Wave period and duraiton is not avaialble -> Need new cost efficient observaiton platdorm -> Big problem : UImportant for air sea interaciton -> 
# zero upcrossing method used to define wave height, period and direction of waves ->  use bird motion record -> most commonly used statistical measure for wave estimation -> Cmpare and validate results using a wave observation buoys mooring buoy -> 

# Seabirds AND Seals !!! -> Both papers -> Seal Science advances? -> Also Seabir -> an treat as drifting buoys -> GPS estiamtes wave chacter and Accelerometer -> Streaked shearwater -> 