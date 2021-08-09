# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# Identify recurse behavior in animals: Galapagos tortoises using watersheds as a case study
# For quesitons contact diego.ellissoto@yale.edu
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

require(viridis)
require(sp);require(raster);require(tidyverse);require(move);require(sf);require(rgeos);require(rgdal);require(maptools);require(recurse);require(mapview);require(rworldmap);require(lubridate);require(ggmap);require(recurse)

calculateTimeOfDay = function(locations, datetime)
{
  require(maptools)
  
  sunrise = sunriset(locations, dateTime = datetime, direction = "sunrise", POSIXct.out = TRUE)$time
  sunset = sunriset(locations, dateTime = datetime, direction = "sunset", POSIXct.out = TRUE)$time
  
  light = ifelse( datetime < sunrise, "night",
                  ifelse( datetime < sunset, "day", "night" ) )
  
  return(light)
}


loginStored<-movebankLogin(username="COVID-19_IBS", password="covid19ibs")

all_studies <- getMovebank(entity_type = "study", login=loginStored)

# returns a MoveStack object from the specified study
tortugas <- getMovebankData(study=2928116, animalName=c("Sir David","Roberto"),
                login=loginStored, includeExtraSensors=FALSE, 
                removeDuplicatedTimestamps=TRUE, deploymentAsIndividuals=FALSE
                ) 

roberto <- tortugas[[1]]
sir_david <- tortugas[[2]]

# Load an object that is a shape which we are interested in exploring how often and when animals use them. 
# These can be anything from roads, water holes, ponds, or a national park :)

ponds<- read.csv('/Users/diegoellis/Downloads/Master_sheet_2018_2019_2020_ponds.csv') %>% drop_na(Longitude, Latitude)

pond.spdf <- SpatialPointsDataFrame(coords= ponds[,c('Longitude', 'Latitude')], data = ponds, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # For the Points

utm_gal <- "+proj=utm +zone=15 +south +datum=WGS84 +no_defs +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
sir_david_sp <- spTransform(sir_david, utm_gal)
roberto_sp <- spTransform(roberto, utm_gal)

pond.spdf_UTM <- spTransform(pond.spdf, utm_gal)
pond.spdf_UTM_df <- data.frame(pond.spdf_UTM)


# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Plot the object of interest (waterhole) and the animal trajectory for roberto the tortoise
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

  # mapview(pond.spdf, color = 'grey40',col.regions = 'blue',layer.name=c('Ponds')) + 
  # mapview(roberto, color = 'black', 
  #         layer.name=c('Roberto La tortuga'),
  #         col.regions = 'black', cex = 0.5)


# Lets start with one:
# Interesting: If we get within 5 meters tortoises spend less time inside the ponds up to 5 hours approx 1-2 hours, is this consistent with Kyanas data? 
rec <- getRecursionsAtLocations( roberto_sp, pond.spdf_UTM_df[,c('Longitude.1', 'Latitude.1')], radius = 5 )

if(sum(rec$revisits)>1){print(paste0(unique(roberto_sp@idData$individual.local.identifier), ' has used a pond total of ', sum(rec$revisits), ' times'))}else(print(paste0('No pond used by '), unique(roberto_sp@idData$individual.local.identifier)))

plot(rec, roberto_sp, main = 'Robert pond revisits 5m radius', pch = 17, alpha = 1,
     xlim = c(790050, 790300)  )
points(pond.spdf_UTM, pch = 16, cex = 2, col = alpha('blue', 0.5)) # Plot the pond aka region of interest
points(roberto_sp, cex = 0.35, col = alpha('grey49', 0.25))   # Tracking data of roberto the tortoise
drawCircle(x = 790270, 9924500, radius = 5) # A 5m radius for reference

# How often does Roberto the tortoises revisits the ponds
hist(rec$revisits, xlab = "Revisitations", main = "", col = "gray70")
# How much time does roberto the torotise revisits the pond?
hist(rec$revisitStats$timeInside, xlab = "Time inside (h)", main = "", col = "gray70")


# And now the same but for 30 meters:

rec <- getRecursionsAtLocations( roberto_sp, pond.spdf_UTM_df[,c('Longitude.1', 'Latitude.1')], radius = 30 )

if(sum(rec$revisits)>1){print(paste0(unique(roberto_sp@idData$individual.local.identifier), ' has used a pond total of ', sum(rec$revisits), ' times'))}else(print(paste0('No pond used by '), unique(roberto_sp@idData$individual.local.identifier)))

plot(rec, roberto_sp, main = 'Robert pond revisits', pch = 17, alpha = 1,
     xlim = c(790050, 790300)  )
points(pond.spdf_UTM, pch = 16, cex = 2, col = alpha('blue', 0.5))
points(roberto_sp, cex = 0.35, col = alpha('grey49', 0.25))  
drawCircle(x = 790270, 9924500, radius = 5)
# Tortoises spend a lot of time inside the pond ! 
par(mfrow=c(1,2))
hist(rec$revisits, xlab = "Revisitations", col = "gray70", main ='Tortoise inside pond')
hist(rec$revisitStats$timeInside, xlab = "Time inside (h)", col = "gray70", main ='Roberto inside pond')

# Check ponds revisits:
roberto_sp_stats = rec$revisitStats; roberto_sp_stats = roberto_sp_stats[roberto_sp_stats$timeInside > 0.1,] # to exclude very short 

# convert to local time (timestamp is UTC time) and calculate some convience variables
roberto_sp_stats$local.entranceTime = with_tz(roberto_sp_stats$entranceTime, tz = "Pacific/Galapagos")
roberto_sp_stats$local.exitTime = with_tz(roberto_sp_stats$exitTime, tz = "Pacific/Galapagos")
roberto_sp_stats$year = year(roberto_sp_stats$entranceTime)
roberto_sp_stats$doy_enter = yday(roberto_sp_stats$local.entranceTime)
roberto_sp_stats$hour_enter = hour(roberto_sp_stats$local.entranceTime)
roberto_sp_stats$time_enter = hour(roberto_sp_stats$local.entranceTime) + minute(roberto_sp_stats$local.entranceTime) / 60 + second(roberto_sp_stats$local.entranceTime) / 60 / 60
roberto_sp_stats$time_exit = hour(roberto_sp_stats$local.exitTime) + minute(roberto_sp_stats$local.exitTime) / 60 + second(roberto_sp_stats$local.exitTime) / 60 / 60
roberto_sp_stats$overnight = factor(as.logical(yday(roberto_sp_stats$local.exitTime) - yday(roberto_sp_stats$local.entranceTime)))
levels(roberto_sp_stats$overnight) = c("no", "yes")
roberto_sp_stats$site = paste("site", roberto_sp_stats$coordIdx) # for plotting

# --- --- --- --- --- --- --- --- --- ---
# entrance/exit time of day 
# --- --- --- --- --- --- --- --- --- ---
# calculate time of day of entrace and exit (important to use local time not UTC)
clusters <-  pond.spdf_UTM_df[,c('Longitude.1', 'Latitude.1')]
names(clusters) <- c('lon', 'lat')

geoLocs = as.matrix(clusters[roberto_sp_stats$coordIdx, c("lon", "lat")])
roberto_sp_stats$light_enter = calculateTimeOfDay(geoLocs, roberto_sp_stats$local.entranceTime)
roberto_sp_stats$light_exit = calculateTimeOfDay(geoLocs, roberto_sp_stats$local.exitTime)

# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# inter- and intra-annual visit patterns across 5 sites 
# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# plot entrace time by year and day of year
ttime = ggplot(roberto_sp_stats, aes(x = doy_enter)) + 
  geom_histogram(binwidth = diff(range(roberto_sp_stats$doy_enter))/7, color = "darkgray", fill = "gray") +
  facet_grid(site ~ year) + 
  xlab("visit day of year") + ylab("revisit frequency") + 
  theme_classic()
print(ttime)


# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# visit duration by entrance time of day
# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# This should be split by season?
binhour = ggplot(roberto_sp_stats, aes(x = time_enter, y = timeInside)) +
  geom_density2d(color = "black") + ylim(0, 24) + 
  scale_color_brewer(palette = "Dark2", name = 'overnight') +
  xlab("visit entrance time") + ylab("visit duration (h)") +
  geom_point(alpha = 0.2, aes(col = overnight)) +
  theme_classic() + theme(
    axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    legend.justification = c(0, 1), legend.position = c(0, 1))+ggtitle('Evidence for overnight stays for \n Roberto La Tortuga?')
print(binhour)
# Evidence for overnight sleep in ponds? -> Is that true? Ask Freddy and trap camera

# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# does visit duration vary through breeding season?
# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
bindoy = ggplot(roberto_sp_stats, aes(x = doy_enter, y = timeInside)) +
  geom_density2d(color = "black") + ylim(0, 24) + 
  scale_color_brewer(palette = "Dark2", name = 'overnight') +
  xlab("visit entrance day of year") + ylab("visit duration (h)") +
  geom_point(alpha = 0.2, aes(col = overnight)) +
  theme_classic() + theme(
    axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    legend.justification = c(1, 1), legend.position = c(1,1)) +
  facet_grid(~ site)
print(bindoy)



# calculate approximate residence time by rounding to nearest hour (data is hourly)
begin = round(roberto_sp_stats$time_enter)
duration = round(roberto_sp_stats$timeInside)

hours = data.frame(site1 = rep(0, 24)
)

for (i in 1:nrow(roberto_sp_stats)){
  #idxs will be 0-23, so add 1 for array indexing
  # i <- 1
  idxs = (begin[i] + 0:(duration[i])) %% 24
  hours[idxs + 1, roberto_sp_stats$coordIdx[i]] = hours[idxs + 1, roberto_sp_stats$coordIdx[i]] + 1
}


# reformat data for plotting with reshape2
hours$hour = factor(row.names(hours), levels = as.character(1:24), ordered = TRUE)
hours.melt = reshape2::melt(hours, value.name="Count", variable.name="site", na.rm = TRUE)

restime = ggplot(hours.melt, aes(x = hour, y = Count)) +
  geom_bar(stat = "identity", color = "darkgray", fill = "gray") +
  xlab("hour of day") + ylab("total hours") + 
  scale_x_discrete(breaks = 1:24, 
                   labels = c("1", "", "", "", "", "6", "", "", "", "", "", "12", "", "", "", "", "", "18", "", "", "", "", "", "24")) +
  theme_classic()
restime + facet_grid(~ site) 
restime <- restime + facet_grid(~ site) 
print(restime)

# For more details on this package see: 

# https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html
