###################################################
#                                                 #
#   Parallel COVID-Movement Development Script    #
#     Scott Yanco, PhD;  scott.yanco@yale.edu     #
#      Base on dev script `olympic_cougar.r`      #
#             by Diego Ellis-Soto                 #
#                                                 #
###################################################

# Parallelized version of basic workflow script. Downloads movestack object from
# MoveBank directly. 
# COVID-19 Project credentials hardcoded into script - DO NOT SHARE!!!
# Estimates:
#   * 
#
# TODO: Once mosey_db is set up, modify to run from db rather than MoveBank.
#
# Note: test with i = 5

#==============================================================================#


#---- Inits ----#

# Required packages
suppressPackageStartupMessages({
  library(move)
  library(tidyverse)
  library(foreach)
  library(lubridate)
  library(amt)
  library(maptools)
  library(ctmm)
  library(glue)
  library(MVNH)
})

# Custom functions
# make_akde_homerange <- function(evt_telem){ # Input is a telemetry object:
#   # get initial acf guess
#   guess <- ctmm.guess(evt_telem, interactive = F)
#   # acf model selection using guess as init
#   fit <- ctmm.select(evt_telem, guess)
#   # fit 
#   akde <- akde(evt_telem, fit)
# }

# Store MoveBank login info
# loginStored<-movebankLogin(username="COVID-19_IBS", password="covid19ibs")

# Declare lockdown period dates
# TODO: check with DES - use these dates for cougars?
# TODO: 
lockdown_start_usa <- as.POSIXct('2020-03-01 00:00:00')
lockdown_easing_new_england <- as.POSIXct('2020-06-01 00:00:00')
baseline_start <- as.POSIXct('2019-03-10 00:00:00')
baseland_end <- as.POSIXct('2019-06-20 00:00:00')
jday_of_baseline_end_2020 <- yday(baseland_end)

#----     ----#


#---- Download Data ----#

# cougar0 <- getMovebankData(study = 1674593154, login = loginStored)
cougar0 <- move('data/Olympic Cougar Project-6479261802587306352.csv')

#----     ----#


#---- Process Data ----#

# Get list of individuals
inds <- unique(cougar0@trackId)

# Create modified cogar object
cougar1 <- cougar0

# Add lockdown dates to the data
cougar1$baseline[cougar1$timestamp >= baseline_start & cougar1$timestamp <= baseland_end] <- as.character('baseline')
cougar1$baseline[cougar1$timestamp >= lockdown_start_usa & cougar1$timestamp <= lockdown_easing_new_england] <- as.character('lockdown')
cougar1$baseline[cougar1$timestamp  >= lockdown_easing_new_england] <- as.character('after_lockdown')
# table(cougar1$baseline)

#----     ----#


#---- Analysis  ----#

# Init empty list for top-level output
out <- list()

# TODO:  toggle `%do%` to `%dopar%` for HPC
foreach(i = 1:length(inds)) %do% {
  
  
  #-- Per individual data prep --#
  message(glue("Processing data for individual {inds[i]}..."))
  
  # extract single individual
  dat_ind <- cougar1[[inds[i]]]
  # table(dat_ind$baseline)
  
  #make data frame version
  dat_df <- as.data.frame(dat_ind)
  
  #subset to baseline and lockdown
  tmp_move <- list(base = dat_ind[na.omit(dat_ind$baseline) == 'baseline',],
                   lock = dat_ind[na.omit(dat_ind$baseline) == 'lockdown',],
                   dat_ind[na.omit(dat_ind$baseline) == 'after_lockdown'])
  # tmp_base <- dat_ind[na.omit(dat_ind$baseline) == 'after_lockdown',]
  # tmp_lock <- dat_ind[na.omit(dat_ind$baseline) == 'lockdown',]
  # tmp_after <- dat_ind[na.omit(dat_ind$baseline) == 'after_lockdown']
  
  
  #-- Movement Metrics --#
  
  dat_df_ls <- dat_df %>% 
    group_split(baseline)
  
  amt_ls <- list()
  for(p in 1:length(dat_df_ls)){
    amt_ls[[p]] <- make_track(dat_df_ls[[p]], .x = location.long, .y = location.lat,
                              .t = timestamp, id = individual.local.identifier,
                              species = individual.taxon.canonical.name,
                              movebank_study = study.name,
                              crs = CRS("+init=epsg:4326"),
                              .phase = baseline)  %>% 
      time_of_day() %>% # add time of day
      mutate(
        # add some time vars
        year=year(t_),
        month = month(t_),
        week = week(t_),
        jday = yday(t_),
        hour = hour(t_),
        species = species) %>%
      track_resample(rate = hours(24), tolerance = minutes(60) ) %>% 
      steps_by_burst(keep_cols = "start")
    
  } # p
  
  # store output
  amt_out <- bind_rows(amt_ls)
  
  
  #-- Continuous-time Movement Models --#
  # TODO:  There is no guarantee of range residency here - need to build in a 
  #   segmentation step and a filter.
  
  #- Fit the movement models
  ctmm_ls <- list()
  
  for(p in 1:length(tmp_move)){
    # Baseline
    # check that subset results in observations
    
    if(nrow(tmp_move[[p]]) == 0){
      message("No suitable baseline records , moving to next...")
      next 
    } else{
      message(glue("{nrow(tmp_move[[s]])} records found..."))
      
      #convert to telem
      telem <- as.telemetry(tmp_base, timeformat="%Y-%m-%d %H:%M:%S")
      
      # get initial guess
      guess <- ctmm.guess(telem, interactive = F)
      
      # model selection using guess as init
      fit <- ctmm.select(telem, guess)
      
      # store outputs
      ctmm_out <- list(telem, guess, fit)
      ctmm_ls[[p]] <- ctmm_out
    } # else
  } # p
  
  
  #-- AKDEs --#
  akde_ls <- list()
  
  for(p in 1:length(ctmm_ls)){
    akde_ls[[p]] <- akde(ctmm_ls[[p]][[1]], ctmm_ls[[p]][[3]])
  } # p
  
  
  #-- RSFs --#
  #TODO:  insert RSF code from ctmm
  
  
  #-- Niche Hypervolumes --#
  
  # declare variables
  vars <- c("MODIS.Land.Surface.Temp...Emissivity.1km.Daily.Terra.Land.Surface.Temperature.Day",
            "MODIS.Land.Vegetation.Indices.250m.16d.Terra.NDVI")
  
  # Niche Breadth
  niche_dat <- list()
  labels <- c()
  niche_breadth <- list()
  #get niche MVNH-formatted objects
  for(p in 1:length(dat_df_ls)){
    dat_tmp <- dat_df_ls[[p]] %>% 
      select(vars)
    niche_dat[[p]] <- dat_tmp
    labels[p] <- dat_df_l[[p]]$baseline[1]
    niche_breadth[[p]] <- MVNH_det(na.omit(dat_tmp))
    }
  names(niche_breadth) <- labels
  names(niche_dat) <- labels
  
  # Niche Dissimilarity
  # get pariwise combos - eliminate comparisons to self
  combos <- expand_grid(comp1= labels, comp2 = labels) %>%   
  # as.data.frame() %>% 
    filter(comp1 != comp2)

  niche_dissims <- list()
  ns <- list()
  for(c in 1:nrow(combos)){
    niche_dissims[[c]] <- MVNH_dissimilarity(na.omit(niche_dat[[unlist(combos[c,1])]]), 
                                             na.omit(niche_dat[[unlist(combos[c,2])]]))
    ns[[c]] <- glue("{combos[c,1]}-{combos[c,2]}")
  }
  
  names(niche_dissims) <- ns
  
  tmp_out <- list(move_metrics = amt_out, # df of resmpled data with SL and TA across periods
                  ctmm_mods = ctmm_ls, #list with telem object, guess, and fit per period
                  home_ranges = akde_ls, # list with akde object per period
                  rsfs = NULL, # empty slot for now, placeholder for RSFs
                  niche_breadth = niche_breadth, # named list of niche breadth calcs per period
                  niche_dissims = niche_dissims # named list of niche dissimilarity comparisons
  )
  
  out[[i]] <- tmp_out               
} # foreach ((i)) - end loop through indivdiuals