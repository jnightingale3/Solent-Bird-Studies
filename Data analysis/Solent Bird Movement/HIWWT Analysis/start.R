library('rgdal')
require(magrittr)

###  see red book / rethink strategy


# load shapefile containing the strategy codes
stratcode <- readOGR(dsn='../../../Solent Bird Studies GIS Data', layer='Waders & Brent Geese Sites_region')
# extract the sitecodes
strat_codes <- unique(stratcode$SITE_ID)
# rm(stratcode)


# load all data
alldat <- read.csv('SW&BGS Clean and Final.csv', stringsAsFactors = F)

# split into counts and movements
mo <- subset(alldat, grepl('MO', RecordID))
co <- subset(alldat, !grepl('MO', RecordID))


# Select columns of interest from counts at strat sites
count <- subset(co, FieldID_Origin %in% strat_codes
                , select=c(UniqueID, Date, Time, FieldID_Origin,
                           Species, Count)) %>% droplevels



# for movements, find those where either origin or destination is known
origin <- subset(mo, FieldID_Origin %in% strat_codes & Oknown == 'Yes'
                 #& Species == 'DB'
                 , select=c(UniqueID, Date, Time, FieldID_Origin,
                            Species, Count)) %>% droplevels
destin <- subset(mo, Destination_code %in% strat_codes & Dknown == 'Yes'
                 #& Species == 'DB'
                 , select=c(UniqueID, Date, Time, Destination_code,
                            Species, Count)) %>% droplevels


## Join all three sets of data together
# rename columns
names(count)[4] <- 'Field'
names(origin)[4] <- 'Field'
names(destin)[4] <- 'Field'


# bind together as rows
known <- rbind(count, origin, destin)

# known sites but without zero counts
knownnz <- subset(known, Count > 0)
# which of these sites were for brent geese?
knownbg <- subset(known, Count > 0 & Species == 'DB')
