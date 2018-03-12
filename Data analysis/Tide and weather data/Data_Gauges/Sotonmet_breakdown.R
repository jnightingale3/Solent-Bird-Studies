# due to large file size (~1GB!), format Sotonmet dataset to be used in tidal analysis in advance
# of running those analyses!

require(magrittr)
require(readr)
require(plyr)

# read in enormous 1GB file (that I have already edited a bit to make readable)
weather <- read_fwf('~/Desktop/Weather2.csv', # fixed width format
                    fwf_widths(c(24, 9, 40, 40, 40, 15, 13, 15, 09, NA))) %>%
  as.data.frame() # suppress tidyverse's default 'tibble' class
names(weather) <- gsub(' ', '_', as.vector(weather[1,]), fixed=T)

weather %<>% extract(-1,) # drop unneeded first line

# write.csv(weather, 'Sotonmet.csv', na='', row.names = F)

# only keep date, tide and location
weather %<>% subset(select=c(EDAS_TIME, Station, Tide_Ht)) 


dockhead <- subset(weather, Station=='Dockhead', select=c(EDAS_TIME, Tide_Ht))
bramble <- subset(weather, Station=='Bramble', select=c(EDAS_TIME, Tide_Ht))




rm(weather)
gc() # garbage collection

write.csv(bramble, 'Bramble_bank.csv', na='', row.names = F)
write.csv(dockhead, 'Dockhead.csv', na='', row.names = F)
