## how many visits within tidal windows?

require(plyr)
require(lubridate)


### load high tide data
bbk <- read.csv('../Tide and weather data/Data_High-Tide-Times/Bbk_fill.csv')
bbk$gauge <- 'Bramble_bank' # add columns identifying each gauge

cam <- read.csv('../Tide and weather data/Data_High-Tide-Times/Cam_fill.csv')
cam$gauge <- 'Cambermet'

chi <- read.csv('../Tide and weather data/Data_High-Tide-Times/chi_fill.csv')
chi$gauge <- 'Chimet'

dok <- read.csv('../Tide and weather data/Data_High-Tide-Times/Dock_fill.csv')
dok$gauge <- 'Dock_head'

lym <- read.csv('../Tide and weather data/Data_High-Tide-Times/Lym_fill.csv')
lym$gauge <- 'Lymington'

pmt <- read.csv('../Tide and weather data/Data_High-Tide-Times/Pmth_obs.csv')
pmt$gauge <- 'Portsmouth'



#### join all tide data together
tides <- join_all(list(bbk, cam, chi, dok, lym, pmt), type='full')
rm(bbk, dok, lym, pmt) # clear off sub datasets

# tidy up dataframe

tides$time %<>% as.POSIXct()
tides$day <- format(tides$time, format='%Y-%m-%d')




#### Calculate tidal window

# window length (hours)
wl <- 2.5


tides$start <- tides$time - minutes(wl*60) # start and end time of tidal window
tides$end   <- tides$time + minutes(wl*60) # lubridate commands take integers only!


### go through each site day by day and 
### find out if any of the visits were conducted during the window
peng <- ddply(solent_jdata, .(Site_ID, Date), function(x) {
  
  # find the relevant tide data: same day, closest gauge
  mat <- which( (tides$day == x$Date) & (tides$gauge == x$Closest) )
  
  if (length(mat) == 0) {
    return(NA) # no matching tide data
    
  } else {
  
    # make sure loop starts from beginning
    ii <- 1
    
    # create empty vector to store results
    res <- rep(NA, length(mat))
    
    # then loop through all the tides of that day
    for (ii in 1:length(mat)) {
      # are any of the visits during that tide's window?
      res[ii] <- any(x$datetime >= tides$start[mat[ii]] & 
                     x$datetime <= tides$end[mat[ii]])
    }
    
   return(any(res)) # where any visits during any tide's window?
  }
}, .progress='text') # add progress bar as this function is really slow! ca.2 hours
# TODO - Can this function be improved/streamlined/vectorised somewhat?

## save the output so we don't have to go through all that again!
write.csv(peng, file='Data/Visit_during_tide_window.csv', row.names=F)

rm(peng, wl, mat, res)
