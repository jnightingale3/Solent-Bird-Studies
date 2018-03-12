###### Tides #####
### Wader analysis only wants records from within 2.5 hours of high tide
### so flag these records
### so they can be used for Brents but not for waders
# 
# ### load packages
# require(Tides)
# require(plyr)
# require(chron)
# require(magrittr)
# 
# ### load tide from portsmouth
# ### TODO use better resolution tide data!
# portsmouth <- read.csv("../Data_Tides/portsmouth.csv", stringsAsFactors = F)
# # convert to date format
# portsmouth$date <- as.POSIXct(portsmouth$date, format='%Y/%m/%d %H:%M:%S') 
# 
# # remove dates with missing data
# # value -99 and -1.124 seem to be used to indicate faults
# portsmouth[which(portsmouth$ht==-99),] <- NA
# portsmouth[which(portsmouth$ht==-1.124),] <- NA
# 
# # remove NA rows
# pmouth <- (na.omit(portsmouth))
# names(pmouth) <- c('time', 'h')
# 
# # check visually
# # with(tail(pmouth, 1000), plot(time, h))
# 
# # calulate extrema of tide cycle
# pmth_extma <- subset(extrema(pmouth, h0=0)$HL, HL=='H') 
# pmth_extma %<>% droplevels %>% extract(,1:2)
# pmth_extma$day <- factor(as.Date(pmth_extma$time, format='%Y/%m/%d'))
# 
# 
# 
# ### find nearest high tide
# 
# tidenobs <- ddply(pmth_extma, .(day), nrow) # number of high tide obs per day
# max(tidenobs$V1) # only want 2 observations per day - good
# 
# tidediff <- ddply(pmth_extma, .(day), function(x) {
#   minht <- min(x$time)
#   maxht <- max(x$time)
#   
#   return(c(minht, maxht))
# })
# names(tidediff)[2:3] <- c('ht1', 'ht2')
# 
# 
# ### bird data
# ## create column of days
# solent$day <- factor(as.Date(solent$Date, format='%Y/%m/%d'))
# tidejoin <- join(solent, tidediff)
# 
# function to compare times without worrying about days
hourmin <- function(x) {
  as.numeric(format(x, "%H"))*60 + as.numeric(format(x, "%M"))
}
# 
# # calculate time difference from observation to each high tide
# tidejoin$diff1 <- with(tidejoin, hourmin(Time) - hourmin(ht1))
# tidejoin$diff2 <- with(tidejoin, hourmin(Time) - hourmin(ht2))
# 
# # is the closest tide within 2.5 hours (ie 150 minutes)?
# tidejoin$filter_tide <- pmin(abs(tidejoin$diff1), abs(tidejoin$diff2)) <= 150
# tidejoin$filter_tide[is.na(tidejoin$filter_tide)] <- FALSE # don't want the NA rows
# 
# 
