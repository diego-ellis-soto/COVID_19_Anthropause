# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Outlier removal for a MoveStack
# For quesitons contact diego.ellissoto@yale.edu
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

# Both input and output is a movestack.
# Outlier functin should be run for each move object separately
animal_movestack_clean <- moveStack(unlist(lapply(move::split(animal_movestack),  function(GPS){
  
  
  telemetry <-  as.telemetry(GPS, keep=T)
  outlier_df <- outlie(telemetry, plot=T)
  
  speed_filter <- quantile(outlier_df$speed, 0.95, na.rm=T) # Remove the top 5 percentile of speeds
  speed_outliers <- outlier_df$speed > speed_filter #identify outliers based on speed
  
  telemetry_out <- telemetry[!speed_outliers,]
  GPS <- GPS[GPS@timestamps %in% telemetry_out$time]
  message('Removed a total of ', nrow(telemetry) - n.locs(GPS), ' outliers based on speed' )
  return(GPS)
}) # The input is the GPS data of a single animal
)
)

