# load the movements from ESCP surveys where either 
# origin or destination was known

# extracted from raw data in ../Solent bird movement/Origins and destinations.R

require(magrittr)

# read in data
es <- read.csv('Data/Movements_known_sites.csv', stringsAsFactors = F)

# make Unique Code actually unique
es$Unique.Code[is.na(es$Unique.Code)] <- 'NA'
es$Unique.Code %<>% make.unique

# remove records with no location information
es <- subset(es, !is.na(Easting))



# convert BTO species codes to names
# load file of BTO codes
btocode <- read.csv('../BTO_codes.csv', stringsAsFactors = F) 
# unique(es$Species)[which(unique(es$Species)%in%btocode$Code==F)] # check
es$sp <- btocode$Species[match(es$Species, btocode$Code)] # replace


rm(btocode)

