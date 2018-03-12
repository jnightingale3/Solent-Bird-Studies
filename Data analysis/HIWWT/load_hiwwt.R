### loads the dataset from HIWWT's movement surveys
### and reformats it for use in the network study

# load required packages
require(rgdal)
require(rgeos)
require(magrittr)


# load HIWWT survey data, removing one line where a surveyor recorded that it was 
# too dark to tell whether or not there were any geese in the field (!)
hiwwt <- subset( read.csv('Data/hiwwt_final.csv', stringsAsFactors = F),
                 Count != '') %>% droplevels



# load shapefile containing the strategy codes
stratcode <- readOGR(dsn='../../Solent Bird Studies GIS Data/HIWWT Strategy Layer', 
                     layer='SWBGS')
# extract the sitecodes
sites <- unique(stratcode$Site_code)
rm(stratcode)


## split into counts and movements
## only keep records where the site is a strategy coded field

# counts
count <- subset(hiwwt[grep('MO', hiwwt$RecordID, invert=T),],
                FieldID_Origin %in% sites)

# movements
# only keep those where either the origin or destination is known
moveo <- subset(hiwwt[grep('MO', hiwwt$RecordID),], 
                Oknown == 'Yes' & FieldID_Origin %in% sites)
moved <- subset(hiwwt[grep('MO', hiwwt$RecordID),], 
                Dknown == 'Yes' & Destination_code %in% sites)
# remove total dataset
rm(hiwwt)


# copy the known (destination) field into the FieldID column
# for the movements with known destination
moved$FieldID_Origin <- moved$Destination_code



## join all three back together again
hiwwt <- rbind(count, moveo, moved)
rm(count, moveo, moved)



# convert BTO species codes to names
# add full stop to codes too short
hiwwt$Species[which(nchar(hiwwt$Species)==1)] <- paste0(
  hiwwt$Species[nchar(hiwwt$Species)==1], '.')
# fix a false record
hiwwt$Species[which(hiwwt$Species=='TS')] <- 'TT'

# load file of BTO codes
btocode <- read.csv('../BTO_codes.csv', stringsAsFactors = F) 
# unique(hiwwt$Species)[which(unique(hiwwt$Species)%in%btocode$Code==F)] # check
hiwwt$sp <- btocode$Species[match(hiwwt$Species, btocode$Code)] # replace


rm(btocode)
