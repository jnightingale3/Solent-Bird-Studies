# ### load packages
require(Tides)
require(plyr)
require(chron)
require(magrittr)
require(readr) # for reading large data files

# Load data from tidal gauges and calculate high tide times
# Identify gaps and fill in, where possible, with predicted heights from POLTIPS


##### Portsmouth #####

portsmouth <- read_csv("Data_Gauges/Portsmouth.zip") %>% as.data.frame
# # convert to date format
portsmouth$date <- as.POSIXct(portsmouth$Date, format='%Y/%m/%d %H:%M:%S')

# make smaller dataset
pmouth <- subset(portsmouth, select=c('date', '\"Data value\"'))
names(pmouth) <- c('time', 'h')

# remove dates with missing data
# value -99 and -1.124 seem to be used to indicate faults
pmouth[which(pmouth$h==-99),] <- NA
pmouth[which(pmouth$h==-1.124),] <- NA
# remove NA rows
pmouth %<>% na.omit


# check visually
with(tail(pmouth, 3000), plot(time, h, type='l'))

# calulate extrema of tide cycle
pmth_extma <- subset(extrema(pmouth, h0=0)$HL, HL=='H', select=c(time, h))

# write file to avoid repeating this intensive process
write.csv(pmth_extma, file='Data_High-Tide-Times/Pmth_obs.csv', na='', row.names=F)


# identify gaps to fill with POLTIPS data (missing days from 2015 onwards)
por_gaps <- gapsts(pmth_extma$time, 1, 'days') %>% subset(
  t2 >= as.POSIXct('2015-01-01', format='%F'))
por_gaps # there aren't any :)



rm(portsmouth, pmouth, por_gaps)


##### Bramble Bank (IOW) #####

bramble <- read_csv('Data_Gauges/Bramble_bank.zip') %>% as.data.frame
names(bramble) <- c('time', 'h')

bbk_extma <- extrema(bramble, h0=0)$HL %>% subset(HL=='H', select=c(time, h))

write.csv(bbk_extma, file='Data_High-Tide-Times/bbk.csv', na='', row.names=F)

# identify gaps to fill with POLTIPS data (missing days from 2015 onwards)
bbk_gaps <- gapsts(bbk_extma$time, 1, 'days') %>% subset(
  t2 >= as.POSIXct('2015-01-01', format='%F'))
bbk_gaps # there are a few, including a spell of 200 days!!


## these gaps can be plugged with POLTIPS predictions for Cowes
cowpol <- read_delim("Data_POLTIPS/cowes.txt", " ", trim_ws = TRUE) %>% 
  as.data.frame
cowpol$time <- paste(cowpol$Date, cowpol$GMT) %>% 
  as.POSIXct(format='%d/%m/%Y %H:%M:%S')

cowpol %<>% subset(select=c(time, Ht_m))
names(cowpol)[2] <- 'h'

cowpol_extma <- extrema(cowpol, h0=0)$HL %>% subset(HL=='H', select=c(time, h))

# find the gaps
bbrow <- c(which(cowpol_extma$time >= max(bbk_extma$time))) 
ii <- 1
for (ii in 1:nrow(bbk_gaps)) {
  bbrow <- c(bbrow, which(cowpol_extma$time >= bbk_gaps$t1[ii] &
                            cowpol_extma$time <= bbk_gaps$t2[ii]))
}


bbk_fill <- rbind(bbk_extma, cowpol_extma[bbrow,])
bbk_fill <- bbk_fill[order(bbk_fill$time),]

gapsts(bbk_fill$time, 1, 'days')

write.csv(bbk_extma, file='Data_High-Tide-Times/Bbk_obs.csv', na='', row.names=F)
write.csv(bbk_fill, file='Data_High-Tide-Times/Bbk_fill.csv', na='', row.names=F)


rm(list=ls())



##### Southampton Dock Head ####

dock <- read_csv('Data_Gauges/Dockhead.zip') %>% as.data.frame
names(dock) <- c('time', 'h')

dk_extma <- extrema(dock, h0=0)$HL %>% subset(HL=='H', select=c(time, h))



# identify gaps to fill with POLTIPS data (missing days from 2015 onwards)
dk_gaps <- gapsts(dk_extma$time, 1, 'days') %>% subset(
  t2 >= as.POSIXct('2015-01-01', format='%F'))
dk_gaps # One gap of 2 days....no big deal really but we'll fix it anyway


## these gaps can be plugged with POLTIPS predictions for Southampton
soupol <- read_delim("Data_POLTIPS/southampton.txt", " ", trim_ws = TRUE) %>% 
  as.data.frame
soupol$time <- paste(soupol$Date, soupol$GMT) %>% 
  as.POSIXct(format='%d/%m/%Y %H:%M:%S')

soupol %<>% subset(select=c(time, Ht_m))
names(soupol)[2] <- 'h'

soupol_extma <- extrema(soupol, h0=0)$HL %>% subset(HL=='H', select=c(time, h))

# find the gaps
dkrow <- c(which(soupol_extma$time >= max(dk_extma$time))) 
ii <- 1
for (ii in 1:nrow(dk_gaps)) {
  dkrow <- c(dkrow, which(soupol_extma$time >= dk_gaps$t1[ii] &
                            soupol_extma$time <= dk_gaps$t2[ii]))
}


dk_fill <- rbind(dk_extma, soupol_extma[dkrow,])
dk_fill <- dk_fill[order(dk_fill$time),]

gapsts(dk_fill$time, 1, 'days') # still a few short gaps before POLTIPS

write.csv(dk_extma, file='Data_High-Tide-Times/Dock_obs.csv', na='', row.names=F)
write.csv(dk_fill, file='Data_High-Tide-Times/Dock_fill.csv', na='', row.names=F)


rm(list=ls())




##### Lymington #####



### Gauge data - finishes at end of 

lym_all <- list(
  'Lym_tides2008' = read_delim('Data_Gauges/Lymington_data/Lym_tides2008.txt',
                               '\t', trim_ws=T) %>% as.data.frame,
  'Lym_tides2009' = read_delim('Data_Gauges/Lymington_data/Lym_tides2009.txt',
                               '\t', trim_ws=T) %>% as.data.frame,
  'Lym_tides2010' = read_delim('Data_Gauges/Lymington_data/Lym_tides2010.txt',
                               '\t', trim_ws=T) %>% as.data.frame,
  'Lym_tides2011' = read_delim('Data_Gauges/Lymington_data/Lym_tides2011.txt',
                               '\t', trim_ws=T) %>% as.data.frame,
  'Lym_tides2012' = read_delim('Data_Gauges/Lymington_data/Lym_tides2012.txt',
                               '\t', trim_ws=T) %>% as.data.frame,
  'Lym_tides2013' = read_delim('Data_Gauges/Lymington_data/Lym_tides2013.txt',
                               '\t', trim_ws=T) %>% as.data.frame,
  'Lym_tides2014' = read_delim('Data_Gauges/Lymington_data/Lym_tides2014.txt',
                               '\t', trim_ws=T) %>% as.data.frame,
  'Lym_tides2015' = read_delim('Data_Gauges/Lymington_data/Lym_tides2015.txt',
                               '\t', trim_ws=T) %>% as.data.frame
) %>% join_all(type='full') %>% subset(Flag == 1)

write.csv(lym_all, 'Data_Gauges/Lymington.csv', na='', row.names=F)

lym_all$time <- as.POSIXct(lym_all$`Date/Time (GMT)`, format='%d-%b-%Y %H:%M:%S')

lym <- subset(lym_all, select=c(time, Tide_CD)) # tide height from chart datum
names(lym)[2] <- 'h'

lym_extma <- extrema(lym, h0=0)$HL %>% subset(HL=='H', select=c(time, h))

# identify gaps to fill with POLTIPS data (missing days from 2015 onwards)
lym_gaps <- gapsts(lym_extma$time, 1, 'days') %>% subset(
  t2 >= as.POSIXct('2015-01-01', format='%F'))
lym_gaps


## POLTIPS data
lym_pol <- read_delim("Data_POLTIPS/lymington.txt", " ", trim_ws = TRUE) %>% 
  as.data.frame
lym_pol$time <- paste(lym_pol$Date, lym_pol$GMT) %>% 
  as.POSIXct(format='%d/%m/%Y %H:%M:%S')

lym_pol %<>% subset(select=c(time, Ht_m))
names(lym_pol)[2] <- 'h'

lym_pol_extma <- extrema(lym_pol, h0=0)$HL %>% subset(HL=='H', select=c(time, h))


# find which rows of POLTIPS data are needed 
lprow <- c(which(lym_pol_extma$time >= max(lym_extma$time))) 
ii <- 1
for (ii in 1:nrow(lym_gaps)) {
  lprow <- c(lprow, which(lym_pol_extma$time >= lym_gaps$t1[ii] &
                          lym_pol_extma$time <= lym_gaps$t2[ii]))
}
  

lym_fill <- rbind(lym_extma, lym_pol_extma[lprow,])
lym_fill <- lym_fill[order(lym_fill$time),]

gapsts(lym_fill$time, 1, 'days')

write.csv(lym_extma, file='Data_High-Tide-Times/Lym_obs.csv', na='', row.names=F)
write.csv(lym_fill, file='Data_High-Tide-Times/Lym_fill.csv', na='', row.names=F)

rm(list=ls())



##### Cambermet #####

camber <- read_csv("Data_Gauges/cam.csv") %>% as.data.frame

# remove dates with missing data
# value -7999 seems to be used to indicate faults
# plus, any value of 10m must be incorrect!!
camber[which(camber$h==-7999),] <- NA
camber[which(camber$h>=10),] <- NA
# remove NA rows
camber %<>% na.omit


# put into chronological order
camber <- camber[order(camber$time),]

# check visually
with(tail(camber, 3000), plot(time, h, type='l'))

# calulate extrema of tide cycle
cam_extma <- subset(extrema(camber, h0=0)$HL, HL=='H', select=c(time, h))




# identify gaps to fill with POLTIPS data (missing days from 2015 onwards)
cam_gaps <- gapsts(cam_extma$time, 1, 'days') %>% subset(
  t2 >= as.POSIXct('2015-01-01', format='%F') & dt <= 179)
cam_gaps 


## POLTIPS data
cam_pol <- read_delim("Data_POLTIPS/chichester.txt", " ", trim_ws = TRUE) %>% 
  as.data.frame
cam_pol$time <- paste(cam_pol$Date, cam_pol$GMT) %>% 
  as.POSIXct(format='%d/%m/%Y %H:%M:%S')

cam_pol %<>% subset(select=c(time, Ht_m))
names(cam_pol)[2] <- 'h'

cam_pol_extma <- extrema(cam_pol, h0=0)$HL %>% subset(HL=='H', select=c(time, h))


# find which rows of POLTIPS data are needed 
cprow <- c(which(cam_pol_extma$time >= max(cam_extma$time))) 
ii <- 1
for (ii in 1:nrow(cam_gaps)) {
  cprow <- c(cprow, which(cam_pol_extma$time >= cam_gaps$t1[ii] &
                            cam_pol_extma$time <= cam_gaps$t2[ii]))
}


cam_fill <- rbind(cam_extma, cam_pol_extma[cprow,], 
                  pmth_extma[which(pmth_extma$h < as.POSIXct(
                    '2015-01-01', format='%F')),])
cam_fill <- cam_fill[order(cam_fill$time),]

gapsts(cam_fill$time, 1, 'days')

write.csv(cam_extma, file='Data_High-Tide-Times/Cam_obs.csv', na='', row.names=F)
write.csv(cam_fill, file='Data_High-Tide-Times/Cam_fill.csv', na='', row.names=F)

rm(list=c("cam_extma", "cam_fill", "cam_gaps", "cam_pol", "camber", "cprow", "ii"))







##### Chimet ######


chimet <- read_csv("Data_Gauges/Chimet_all.zip", cols(TIME=col_character()),
                   col_names = T) %>% as.data.frame
# convert date and time to character strings
chimet$DATE %<>% as.character
chimet$Date %<>% as.character



# convert to date format
chimet$DATE[which(is.na(chimet$DATE))] <- chimet$Date[which(is.na(chimet$DATE))]
chimet$TIME[which(is.na(chimet$TIME))] <- chimet$Time[which(is.na(chimet$TIME))]
chimet$time <- as.POSIXct(paste(chimet$DATE, as.character(chimet$TIME)),
                          format='%d/%m/%Y %H:%M')

# remove dates with missing data
# value -7999 seems to be used to indicate faults
# plus, any value of 10m must be incorrect!!
chimet[which(chimet$DEPTH==-7999),] <- NA
chimet[which(chimet$DEPTH>=10),] <- NA

chimet %<>% subset(select=c(time, DEPTH))
names(chimet)[2] <- 'h'
# remove NA rows
chimet %<>% na.omit


# put into chronological order
chimet <- chimet[order(chimet$time),]

# check visually
with(tail(chimet, 3000), plot(time, h, type='l'))


# calulate extrema of tide cycle
chi_extma <- subset(extrema(chimet, h0=0)$HL, HL=='H', select=c(time, h))




# identify gaps to fill with POLTIPS data (missing days from 2015 onwards)
chi_gaps <- gapsts(chi_extma$time, 1, 'days') %>% subset(
  t2 >= as.POSIXct('2015-01-01', format='%F') & dt <= 179)
chi_gaps 


# find which rows of POLTIPS data are needed 
# using the same data as from Cambermet, above
# as the Chichester harbour record is closest to both
cprow <- c(which(cam_pol_extma$time >= max(chi_extma$time))) 
ii <- 1
for (ii in 1:nrow(chi_gaps)) {
  cprow <- c(cprow, which(cam_pol_extma$time >= chi_gaps$t1[ii] &
                            cam_pol_extma$time <= chi_gaps$t2[ii]))
}


chi_fill <- rbind(chi_extma, cam_pol_extma[cprow,], 
                  pmth_extma[which(pmth_extma$h < as.POSIXct(
                    '2015-01-01', format='%F')),])
chi_fill <- chi_fill[order(chi_fill$time),]

# put into chronological order
gapsts(chi_fill$time, 1, 'days')

write.csv(chi_extma, file='Data_High-Tide-Times/chi_obs.csv', na='', row.names=F)
write.csv(chi_fill, file='Data_High-Tide-Times/chi_fill.csv', na='', row.names=F)

rm(list=c("chi_extma", "chi_fill", "chi_gaps", "chi_pol", "chimet", "cprow", "ii"))
rm(chimet, chim, chm_extma, chi_gaps, pmth_extma, cam_pol_extma)
