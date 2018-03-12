# load ebird data from compressed files
# trim by location and date and re-save?
library(readr) # to read compressed tab-delimited data
library(magrittr) # %>%
library(plyr) # for revalue()


#### define function to standardise datasets on readin ####

ebird_tidy <- function(X) {
  # rename first column consistently
  names(X)[1] <- 'id'
  names(X) <- gsub(' ', '.', names(X)) # remove spaces from variable names
  
  # fix date format
  X$date <- as.POSIXct(X$OBSERVATION.DATE, format='%Y-%m-%d')
  X$month <- months(X$date)
  
  # combine observer IDs
  X$observer <- with(X, paste(`FIRST.NAME`, `LAST.NAME`, sep=' '))
  
  # return
  return(X)
}



#### read in data ####

dorset <- read_delim("dorset.tar.gz", "\t", escape_double=FALSE, trim_ws=TRUE) %>%
  ebird_tidy()
hants <- read_delim("hampshire.tar.gz", "\t", escape_double=FALSE, trim_ws=TRUE) %>%
  ebird_tidy()
iow <- read_delim("IOW.tar.gz", "\t", escape_double=FALSE, trim_ws=TRUE) %>%
  ebird_tidy()
wsx <- read_delim("west_sussex.tar.gz", "\t", escape_double=FALSE, trim_ws=TRUE) %>%
  ebird_tidy()


## combine into single dataset
ebird <- rbind(dorset, hants, iow, wsx) %>% as.data.frame
rm(list=c('dorset', 'hants', 'iow', 'wsx'))

## keep only certain months and variables
ebird %<>% subset(month %in% c('January', 'February', 'March',
                               'September', 'October', 'November', 'December'),
                  select=c('COMMON.NAME', 'OBSERVATION.COUNT', 'LOCALITY', 
                           'LATITUDE', 'LONGITUDE', 'date', 'observer',
                           "TIME.OBSERVATIONS.STARTED", 'DURATION.MINUTES', 
                           'EFFORT.DISTANCE.KM', 'EFFORT.AREA.HA', 'id',
                           'ALL.SPECIES.REPORTED','TRIP.COMMENTS','SPECIES.COMMENTS'))


## create visit ID column

ebird$visitid <- with(ebird, paste(observer, date, LATITUDE, LONGITUDE))

## Convert all counts to integer values

# how many of counts are 'X' (i.e. present but total not specified)
sum(grepl('X', ebird$OBSERVATION.COUNT)) / nrow(ebird) *100 # about a quarter

# change these to 1, which is conservative but allows calculation of totals
ebird$OBSERVATION.COUNT[which(ebird$OBSERVATION.COUNT=='X')] <- 1
ebird$OBSERVATION.COUNT %<>% as.integer

# plot(ebird$LONGITUDE, ebird$LATITUDE)



### Standardise species names

# load the WeBS list as a starting point
source('../define_species_groups.R')


# vector of other webs spp I want to keep
keep <- c(unique(ebird$COMMON.NAME)[grepl(paste(c(
  goodspp, 'Brant', 'Plover', 'Heron', 'Goose', 'Phalarope', 'Duck', 'Sandpiper',
  'Dowitcher', 'Crane', 'Eider', 'Swan', 'shorebird', 'Egret', 'Yellowlegs',
  'peep', 'Bufflehead', 'Scoter', 'Stint'), 
  collapse='|'), unique(ebird$COMMON.NAME), ignore.case=T)]) %>% get_goodspp
# keep <- goodspp(unique(ebird$COMMON.NAME))

# check others
unique(ebird$COMMON.NAME)[which(unique(ebird$COMMON.NAME) %in% keep == F)]


# keep only the desired species
ebird %<>% subset(COMMON.NAME %in% keep)


### rename names so that they are compatible with WeBS & SBS names

# which species are not in WeBS?
unique(ebird$COMMON.NAME)[which(unique(ebird$COMMON.NAME) %in% spp == F)]
fix_spp_names <- c(
  'Brant' = 'Brent Goose (Dark-bellied)',
  "Eurasian Moorhen" = "Moorhen",
  "Common Pochard" = "Pochard",
  "Common Ringed Plover" = "Ringed Plover" ,
  "European Golden-Plover" = 'Golden Plover',
  "Eurasian Coot" = 'Coot',
  'Eurasian Wigeon' = 'Wigeon',
  'Gray Heron' = 'Grey Heron',
  "Northern Shoveler" = 'Shoveler',
  "Common Snipe" = "Snipe",
  "Eurasian Oystercatcher" = "Oystercatcher" ,
  "Greater Scaup" = "Scaup",
  "Black-bellied Plover" = 'Grey Plover',
  'Common Greenshank' = 'Greenshank',
  'Common Redshank' = 'Redshank',
  "Eurasian Curlew" = 'Curlew',
  "Graylag Goose" = 'Greylag Goose (British/Irish)',
  "Northern Lapwing" = 'Lapwing',
  "Northern Pintail" = 'Pintail',
  "Pied Avocet" = 'Avocet',
  "Common Shelduck" = 'Shelduck',
  "Red Knot" = 'Knot',
  "Ruddy Turnstone" = 'Turnstone',
  'Common Goldeneye' = 'Goldeneye',
  "Eurasian Woodcock" = 'Woodcock',
  "Red Phalarope" = 'Grey Phalarope',
  "Great Bittern" = 'Bittern',
  "Common Eider" = 'Eider (except Shetland)',
  "Tundra Bean-Goose" = 'Bean Goose (Tundra)',
  "Tundra Swan" = "Bewick's Swan",
  "Greater White-fronted Goose" = 'White-fronted Goose',
  "Great Egret" = 'Great White Egret'
)

ebird$species <- revalue(x=as.character(ebird$COMMON.NAME), replace=fix_spp_names)
  