#### File to extract data for county recorders
#### using administrative boundaries shapefile to clip spreadsheet
#### to appropriate county

### Load packages
require(R.utils) # for capitalize() function (for recorder names)
require(rgeos) # for gUnaryUnion() 
require(rgdal) # to convert co-ordinates to grid references
require(magrittr) # for %>% pipe operator
require(sp) # provides spatial method for rbind() function



#########################################
####### Loading & cleaning data #########
#########################################

setwd('/home/josh/Dropbox/Solent Bird Studies data/Data analysis/Data collation/Data/County Recorders/')
data <- read.csv("solentbirds_pos_2017.csv")

# see what the data looks like
str(data)

# Remove columns unwanted in final dataset
data <- subset(data, select=-c(ID, Source, Taxon.Group, BTO.code, Site.name))


### Standardise recorder names
# what is the position of the comma in the text string?
poscomma <- regexpr(',', data$Recorder)

# how long is the text string?
namelength <- nchar( as.character(data$Recorder) )

# surnames are the text before the comma 
surnames <- substr(data$Recorder, 1, poscomma-1) %>% capitalize
# firstnames come after the space after the comma
firstnames <- substr(data$Recorder, poscomma+2, namelength) %>% capitalize
# Recorders only want first initial
firstinits <- substr(firstnames, start=1, stop=1)

# new vector for fixed names
fixnames <- rep(NA, length(firstnames))
# join names together in correct order
fixnames <- paste(firstinits, '. ', surnames, sep='') 

# change nicknames etc
fixnames %<>% replace(which(fixnames=='A. P'), 'A. Pearce')
fixnames %<>% replace(which(fixnames=='M. Houari-byrne'), 'M. Houari-Byrne')
fixnames %<>% replace(which(fixnames=='L. D'), 'L. Denham')

# add to original data set
data$Recorder <- factor(fixnames)

# Breeding status
# Not using this information as a lot of it seems to be user error
# Many records of Turnstone, Brent etc. 'nesting' in middle of winter!!
# data$status <- 'M'
# data$status[datahants$Activity=='Nesting'] <- 'C'
# data$status %<>% factor

# Remove uninformative activity category
# data$Activity[data$Activity=='Other'] <- ''
# data$Activity %<>% as.character

# Remove newlines from Comments field as it messes up write.csv
# data$Comments <- gsub('\\n', '', data$Comments)


# clean up workspace
rm(firstnames, fixnames, firstinits, surnames, namelength, poscomma)


#########################################
########## Extracting Records! ##########
#########################################
#### Time ####

# Fix date and time
data$date <- format(as.POSIXct(data$Date, format='%d/%m/%Y'), '%d/%m/%Y')
# data$year <- format(as.POSIXct(data$Date, format='%d/%m/%Y'), '%Y')
data$time <- format(as.POSIXct(data$Time, format='%H:%M'), '%H:%M')


# Choose desired year
# data <- subset(data, year=='2016', select=-c(year, Date, Time)) %>% droplevels

#### Space ####

# data <- subset(data, select=-c(Longitude, Latitude)) # will use Northing/Easting
# 
# ## convert to spatial data
# # (assuming coordinates are from google maps which uses Web Mercator projection)
# spdata <- SpatialPointsDataFrame(coords=cbind(data$Easting, data$Northing),
#                                  data=data, proj4string=CRS('+init=epsg:27700'))
# plot(spdata)

## Sort out inconsistently recorded coordinates
## need to split by either comma or space

# which ones have a comma
hascomma <- grepl(',', data$Grid.Ref)
poscomma <- regexpr(',', data$Grid.Ref)

# which ones have a space
hasspace <- grepl('N ', data$Grid.Ref, fixed=T)
posspace <- regexpr('N ', data$Grid.Ref, fixed = T)
# which ones have a space but no comma
onlyspac <- hasspace & !hascomma

# how long is the text string?
grlength <- nchar( as.character(data$Grid.Ref) )

# start new vectors for latitude and longitude
lat <- lon <- rep(NA, nrow(data))

## sort all the ones with commas first
sel <- which(hascomma)
# latitude is the substring before the comma
lat[sel] <- substr(data$Grid.Ref[sel], 1, poscomma[sel]-1)
# longitude is the substring after the comma
lon[sel] <- substr(data$Grid.Ref[sel], poscomma[sel]+1, grlength[sel]+1)

# now the ones which only have a space
sel <- which(onlyspac)
# get latitude
lat[sel] <- substr(data$Grid.Ref[sel], 1, posspace[sel]-1)
# remove any Ns (to indicate North)
lat <- gsub('N', '', lat)
# get longitude
lon[sel] <- substr(data$Grid.Ref[sel], posspace[sel]+2, grlength[sel]+1)
# remove any Ws indicating West, and make those coordinates negative
lon[which(grepl('W', lon))] <- paste0('-', gsub('W', '', lon[which(grepl('W', lon))]))

# remove any extra whitespace
lat <- gsub(' ', '', lat, fixed=T)
lon <- gsub(' ', '', lon, fixed=T)

# convert to numeric format
data$Longitude <- as.numeric(lon)
data$Latitude <- as.numeric(lat)

# clean up workspace
rm(grlength, hascomma, hasspace, poscomma, posspace, lat, lon, onlyspac, sel)

# convert to spatial format
spdata <- SpatialPointsDataFrame(coords=cbind(data$Longitude, data$Latitude),
                                 data=data, proj4string=CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ')) # Google mercator projection
plot(spdata) # check


### load Hampshire shapefiles (Hants + Southampton + Portsmouth)
## load all shapefiles into a list

# filepath to folder
fp <- '../../../../Solent Bird Studies GIS Data/Administrative boundaries'

# load shapefiles of hampshire, southampton and portsmouth
hants <- rbind(
  readOGR(dsn=fp, layer='hampshire_district'),
  readOGR(dsn=fp, layer='southampton_unitary'),
  readOGR(dsn=fp, layer='portsmouth_unitary'), makeUniqueIDs = TRUE) %>%
  gUnaryUnion # and dissolve together
proj4string(hants) <- CRS('+init=epsg:27700') # set CRS (Eastings and Northings)

# reproject bird data to same coordinate system
spdata <- spTransform(spdata, CRS=hants@proj4string) 

# load shapefile for Isle of Wight
iow <- readOGR(dsn=fp, layer='iow_unitary')
proj4string(iow) <- CRS('+init=epsg:27700')

# load shapefile for West Sussex
wssx <- readOGR(dsn=fp, layer='westsussex_county')
proj4string(wssx) <- CRS('+init=epsg:27700')

# keep it tidy
rm(fp)



### trim data by overlap with Hampshire outline
datahants <- spdata[hants, ]@data %>% droplevels
datawssx <- spdata[wssx, ]@data %>% droplevels
dataiow <- spdata[iow, ]@data %>% droplevels



#########################################
####### Formatting for recorders ########
#########################################

#### Output a submittable records form

source('ogr_convert.R') # function to convert northings and eastings to OS gridrefs

#Hants Eastings Northing
hen <- spdata@coords[row.names(datahants)%>%as.numeric,]
# IOW
ien <- spdata@coords[row.names(dataiow)%>%as.numeric,]
# West Sussex
wen <- spdata@coords[row.names(datawssx)%>%as.numeric,]

### Hampshire

# based on instructions at http://www.hos.org.uk/index.php/bird-recording
# on 6 Feb 2017'
data_for_HOS_2017 <- data.frame(
  SPECIES = datahants$Species,
  LOCATION = '',
  GRID_REFERENCE = ogr_convert(hen[,1], hen[,2]),
  FIRST_DATE = datahants$Date,
  LAST_DATE = '',
  NUMBER = datahants$Number,
  MOVEMENT = '',
  STATUS = '',
  Additional_description_prompt = '',
  NOTES = paste0('Time: ', datahants$time),
  OBSERVER = datahants$Recorder
)

# Add information about images
data_for_HOS_2017$Additional_description_prompt %<>% as.character
data_for_HOS_2017$Additional_description_prompt[datahants$Images!=''] <- paste(
  'Photo', datahants$Images[datahants$Images!=''], 'available on request')

# Clean up some issues with extra/trailing dots and spaces in NOTEs column
data_for_HOS_2017$NOTES %<>% as.character
# data_for_HOS_2017$NOTES <- gsub('^[^. ]*[. ]*', '', data_for_HOS_2017$NOTES)
# data_for_HOS_2017$NOTES <- gsub('. . ', '. ', data_for_HOS_2017$NOTES, fixed=T)
# data_for_HOS_2017$NOTES <- gsub('..', '.', data_for_HOS_2017$NOTES, fixed=T)
# And a problem with messed-up time entries
data_for_HOS_2017$NOTES <- gsub('Time: NA', '', data_for_HOS_2017$NOTES, fixed=T)



### Isle of Wight

# based on form from http://www.wildonwight.co.uk/lrc/recordersforum.php

data_for_IOW_2017 <- data.frame(
  English_name = dataiow$Species,
  Scientific_name = NA,
  Site_name = NA,
  Grid_reference = ogr_convert(ien[,1], ien[,2]),
  Recorder = dataiow$Recorder,
  Determiner = NA,
  Date = dataiow$date,
  Quantity = dataiow$Number,
  Method = NA,
  Sex = NA,
  Stage = NA,
  Status = NA,
  Comment = paste(dataiow$Comments, '. Time: ', dataiow$time, sep='')
)

# Add image information to comments column
data_for_IOW_2017$Comment %<>% as.character
data_for_IOW_2017$Comment[dataiow$Images!=''] <- paste(
  data_for_IOW_2017$Comment[dataiow$Images!=''], '. Photo ', dataiow$Images[
    dataiow$Images!=''], ' available on request.',
  sep='')

# Tidy up this automatically-generated column (extra dots in wrong places)
data_for_IOW_2017$Comment <- gsub('^[^. ]*[. ]*', '', data_for_IOW_2017$Comment)
data_for_IOW_2017$Comment <- gsub('.. ', '. ', data_for_IOW_2017$Comment, fixed=T)
# And a problem with messed-up time entries
data_for_IOW_2017$Comment <- gsub('Time: NA', '', data_for_IOW_2017$Comment, fixed=T)


### West Sussex

# based on "Record Capture 2" form from http://www.sos.org.uk/general/index.php

data_for_SOS_2017 <- data.frame(
  Species = datawssx$Species,
  Number = datawssx$Number,
  Grid_Reference = ogr_convert(wen[,1], wen[,2]),
  Notes = paste(datawssx$Activity, '. ', datawssx$Comments, 
                '. Time: ', datawssx$time, sep=''),
  Observer = datawssx$Recorder
)

# Add image information to comments column
data_for_SOS_2017$Notes %<>% as.character
data_for_SOS_2017$Notes[datawssx$Images!=''] <- paste(
  data_for_SOS_2017$Notes[datawssx$Images!=''], '. Photo ', datawssx$Images[
    datawssx$Images!=''], ' available on request.',
  sep='')

# clean up extra dots and spaces in notes column
data_for_SOS_2017$Notes <- gsub('^[^. ]*[. ]*', '', data_for_SOS_2017$Notes)
data_for_SOS_2017$Notes <- gsub('. . ', '. ', data_for_SOS_2017$Notes, fixed=T)
data_for_SOS_2017$Notes <- gsub('..', '.', data_for_SOS_2017$Notes, fixed=T)
# And a problem with messed-up time entries
data_for_SOS_2017$Notes <- gsub('Time: NA', '', data_for_SOS_2017$Notes, fixed=T)

# "Please avoid commas, apostrophes and quotations as these create havoc when 
# records are moved from one programme to another."
data_for_SOS_2017$Notes <- gsub('[,\'\"\\?]{1}', '-', data_for_SOS_2017$Notes)


############################
#### Write spreadsheets ####
############################

write.csv(data_for_HOS_2017, file = 'Hants_2017.csv', 
          row.names = F, na='')
write.csv(data_for_IOW_2017, file = 'IOW_2017.csv', 
          row.names = F, na='')
write.csv(data_for_SOS_2017, file = 'West Sussex_2017.csv', 
          row.names = F, na='')
