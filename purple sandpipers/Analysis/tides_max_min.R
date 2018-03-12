# find high and low tides for portsmouth data

portsmouth <- read.csv("../../Data analysis/Tide and weather data/Data_Gauges/Portsmouth.csv") %>% as.data.frame
# # convert to date format
portsmouth$date <- as.POSIXct(portsmouth$Date, format='%Y/%m/%d %H:%M:%S')

# make smaller dataset
pmouth <- subset(portsmouth, select=c('date', 'Data.value'))
names(pmouth) <- c('time', 'h')
rm(portsmouth)

# remove dates with missing data
# value -99 and -1.124 seem to be used to indicate faults
pmouth[which(pmouth$h==-99),] <- NA
pmouth[which(pmouth$h==-1.124),] <- NA
# remove NA rows
pmouth %<>% na.omit


# check visually
with(tail(pmouth, 3000), plot(time, h, type='l'))

# calulate extrema of tide cycle
require(Tides)
pmth_extma <- extrema(pmouth, h0=0)$HL

