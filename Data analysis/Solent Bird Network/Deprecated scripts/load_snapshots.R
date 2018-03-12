# loads snapshot data from ESCP vantage point surveys
# and reformats it for use in the network study

# load required packages
require(rgdal)
require(rgeos)
require(magrittr)


# load merged CSV files
snap <- read.csv('Data/Snapshot_all.csv', stringsAsFactors = F)
snap$Unique.code <- gsub('Co', 'CO', snap$Unique.code)
# snap$Unique.code <- gsub('^16', 'VP16', snap$Unique.code)
snap$Northing <- snap$Easting <- NA # start empty columns



# load shapefile containing digitised records
digisnap <- readOGR(dsn='../../Solent Bird Studies GIS Data/Survey results', 
                    layer='all_snapshots')
digisnap$UniqueCode2 <- gsub('2016', '16', digisnap$UniqueCode)

# match unique codes of records in spreadsheet and shapefile
mad <- match(snap$Unique.code, digisnap$UniqueCode)
mad[is.na(mad)] <- match(snap$Unique.code[is.na(mad)], digisnap$UniqueCode2[is.na(mad)])

# add coordinates to data frame
snap$Northing <- digisnap@coords[mad,2]
snap$Easting <- digisnap@coords[mad,1]


sum(is.na(snap$Easting)) # 179 records lack coordinates still
# some have the strat code recorded - so can add polygon midpoints as coords



# load shapefile containing the strategy codes
stratcode <- readOGR(dsn='../../Solent Bird Studies GIS Data', layer='Waders & Brent Geese Sites_region')


# extract the sitecodes
sites <- unique(stratcode$SITE_ID) %>% as.character

snap$location <- NA # empty column for locations

# add the map cross refs 
snap$location[which(snap$Map.cross.ref %in% sites)] <- snap$Map.cross.ref[
  which(snap$Map.cross.ref %in% sites)]
sum(is.na(snap$Easting) & !(is.na(snap$location))) # 95 extra records :)

# get midpoints of strat sites
midpts <- gCentroid(stratcode, byid=T, id=stratcode$siteid) %>% as.data.frame

# match the record location with strat site IDs
mat <- match(snap$location, stratcode$SITE_ID)

# add coordinates to data set - overwrite digitised records where possible
# to avoid losing records through digistisation errors
snap$Easting[which(!is.na(mat))] <- midpts$x[na.omit(mat)]
snap$Northing[which(!is.na(mat))] <- midpts$y[na.omit(mat)]


# how many without coordinates?
sum(is.na(snap$Easting)) # 84 records - these all are from before protocol changed

# what are they?
subset(snap, is.na(Easting), select=c(Unique.code, Time, Species, Count, Map.cross.ref))


# clean up
rm(stratcode, digisnap, mat, mad)




# convert BTO species codes to names
# add full stop to codes too short
snap$Species[which(nchar(snap$Species)==1)] <- paste0(
  snap$Species[nchar(snap$Species)==1], '.')

# load file of BTO codes
btocode <- read.csv('../BTO_codes.csv', stringsAsFactors = F) 
# unique(snap$Species)[which(unique(snap$Species)%in%btocode$Code==F)] # check
snap$sp <- btocode$Species[match(snap$Species, btocode$Code)] # replace


rm(btocode)



# remove rows with no location information, as they are useless
snap <- subset(snap, !is.na(Easting) )
