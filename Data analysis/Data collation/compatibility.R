### script to check for compatibility between webs and Solent datasets
require(magrittr) # for %>% pipe operator 
require(sp) # for SpatialPoints class - used for converting co-ordinates

# all webs data (all.csv) should already be loaded

#### load and cleanup birds_sites_merged ####
birds <- read.csv("Data/birds_sites_merged_230614.csv")


## check species' names are equivalent between data sets
sum(birds$Taxon.Comm %in% webspplist$sp == F) 
# they aren't in the majority of cases!

# which taxa names are causing problems?
birds$Taxon.Comm[which(birds$Taxon.Comm %in% webspplist$sp == F)] %>% unique
#### Problems
# - Brent goose nomenclature
# - negative records

#### Solutions
# - rename Brent geese taxa
# - generate full species lists for each visit to get around negative record problems



## rename Brent geese taxa
# dark bellies
birds$sp <- as.character(birds$Taxon.Comm)
birds$sp <- replace(birds$sp,                                    # vector
                         birds$sp=='Brent Goose (dark-bellied)', # target
                         'Brent Goose (Dark-bellied)')           # replacement
# light bellies
birds$sp <- replace(birds$sp,                               
                    birds$sp=='Brent Goose (light-bellied)', 
                    'Brent Goose (Light-bellied)')                
# black brants
birds$sp <- replace(birds$sp,                              
                    birds$sp=='Black Brant',
                    'Brent Goose (Black Brant)')              




birds$sp <- as.factor(birds$sp)
sum(birds$sp %in% webspplist$sp == F) # still a lot...what are they?
birds$sp[which(birds$sp %in% webspplist$sp == F)] %>% unique 
# now they are all negative records


## remove spurious records of 999999 Avocets - presumably for testing app
max(birds$Number, na.rm = T) # 999999
birds2 <- birds[-which(birds$Number==999999),] # remove entire surveys
birds <- droplevels(birds2)
rm(birds2)


###########################################################################
##### DON'T NEED THE Data_analysis_29032016_data_cleaning.csv DATASET #####
############ IT'S JUST THE FIRST YEAR OF SOLENTBIRDS APP DATA #############
###########################################################################

# #### repeat this for Data_analysis_29032016_data_cleaning ####
# clean <- read.csv("Data/Data_analysis_29032016_data_cleaning.csv")
# # no negative records in this data set but need to fix some taxa names
# clean$Species[which(clean$Species %in% webspplist$sp == F)] %>% unique 
# 
# 
# ### rename taxa
# clean$sp <- as.character(clean$Species)
# 
# # dark bellies
# clean$sp <- replace(clean$sp,                               
#                     clean$sp=='Brent Goose (dark-bellied)', 
#                     'Brent Goose (Dark-bellied)')           
# # Larus fuscus
# clean$sp <- replace(clean$sp,                             
#                     clean$sp=='Lesser-black-backed Gull', 
#                     'Lesser Black-backed Gull')           
# # Larus marinus
# clean$sp <- replace(clean$sp,                               
#                     clean$sp=='Great-black-backed Gull', 
#                     'Great Black-backed Gull')           
# 
# 
# clean$sp <- as.factor(clean$sp)
# sum(clean$sp %in% webspplist$sp == F) # none
# 
# 
# ### this dataset also needs a $Visit.ID equivalent
# # create by creating a string variable of location, recorder and date - should be ok!
# clean$visitid <- factor( with(clean, sprintf('%s %s; %s; %s', Easting, Northing, Recorder, Date)) )
# 
# 
# ### Fix so that Longitude is numeric, rather than factor
# ## some level must contain non-numeric character
# 
# # convert to numeric (via character); NAs will indicate non-numeric input
# long_num <- clean$Longitude %>% as.character %>% as.numeric
# long_num %>% is.na %>% sum # 966 are non-numeric!
# clean$Longitude[is.na(long_num)] %>% head # they start with '0-'
# 
# # logical vector to select the rows with non-numeric longitudes
# sel <- grepl('0-', clean$Longitude)
# sel %>% sum      # fix only these 966 lines
# # long_fix <- rep(NA, length=nrow(clean))
# long_fix <- clean$Longitude[sel]
# long_fix <- gsub('0-', '', long_fix) %>% as.numeric # works
# long_fix <- 0 - long_fix
# # long_fix %>% hist # there are no positive values! all should be negative :)
# 
# all.equal(is.na(long_num), sel)
# long_num[sel] <- long_fix
# is.na(long_num) %>% sum # 0 - we have fixed the dataset!
# 
# clean$Longitude <- long_num



#### repeat for SW&BG Records ####

swbg_pos <- read.csv("Data/SW&BG Records 2016_positive.csv")
swbg_neg <- read.csv("Data/SW&BG Records 2016_negative.csv")
swbg <- rbind(swbg_neg, swbg_pos)
rm(swbg_neg, swbg_pos)


### rename taxa
swbg$sp <- as.character(swbg$Species)

# Brents
swbg$sp[swbg$sp=='Brent Goose'] <- 'Brent Goose (Dark-bellied)'
swbg$sp[swbg$sp=='Brent Goose ']<- 'Brent Goose (Dark-bellied)'
swbg$sp[swbg$sp=='Brent goose'] <- 'Brent Goose (Dark-bellied)'
swbg$sp[swbg$sp=='Black Brant'] <- 'Brent Goose (Black Brant)'

# check
swbg$sp <- as.factor(swbg$sp)
sum(swbg$sp %in% webspplist$sp == F) # still a lot...what are they?
swbg$sp[which(swbg$sp %in% webspplist$sp == F)] %>% unique 
# they are all negative records


### fix a sitename for compatibility with strategy document
swbg$Site[swbg$Site=='P23b'] <- 'P23B'

### visit id equivalent

swbg$visitid <- factor( with(swbg, sprintf('%s; %s %s; %s; %s %s', 
                                           Site, Easting, Northing,
                                           Recorder.Name, Date, Time.on.Site, 
                                           rownames(swbg))) )

### sites with no location data (all negative records)
sum(is.na(swbg$Easting)) # most!




#### Data from Solent Birds app #####

### positive records
sb_pos <- read.csv("Data/solentbirds-pos-21Jul2017.csv")
# remove unneeded columns for compatibility
sb_pos$BTO.code <- NULL
sb_pos$Colour.Markings <- NULL

### negative records
sb_neg <- read.csv("Data/solentbirds-neg-21Jul2017.csv")

# need to add columns for compatibility with positive record
sb_neg$Species <- 'None'
sb_neg$Number <- 0
sb_neg$Activity <- NA
sb_neg$Images <- NA

## easting and northing via conversion of coordinates

# firstly, need to convert from character to numeric
sb_neg$Latitude %<>% as.character
# then remove leading 000- and W and E characters
sb_neg$Latitude[grepl('W', sb_neg$Latitude)] <- paste0('-', sb_neg$Latitude[grepl('W', sb_neg$Latitude)])
sb_neg$Latitude <- gsub('W', '', sb_neg$Latitude)
# convert back to a number
sb_neg$Latitude %<>% as.numeric

# repeat for Longitude
sb_neg$Longitude %<>% as.character
sb_neg$Longitude <- gsub('N', '', sb_neg$Longitude)
sb_neg$Longitude %<>% as.numeric

# prepare UTM coordinates matrix
longlatcoor<-SpatialPoints(cbind(sb_neg$Latitude, sb_neg$Longitude), CRS("+proj=longlat"))
# converting to latlon
utmcoor<-spTransform(longlatcoor, CRS('+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000
+datum=OSGB36 +units=m +no_defs +ellps=airy
+towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894'))
# add to original data frame
sb_neg$Easting <- utmcoor$coords.x1
sb_neg$Northing <- utmcoor$coords.x2


## put columns into same order in both data sets
sb_neg <- sb_neg[,match(names(sb_pos), names(sb_neg))]

### join positives and negatives together
sb <- rbind(sb_neg, sb_pos)
rm(sb_pos, sb_neg, longlatcoor, utmcoor)

### fix some errors from data entry with NA in Count column
# Joel Miller put negative records in like this
sb$Number[is.na(sb$Number)&grepl('Miller', sb$Recorder)] <- 0
# Some are obviously negative records based on comments
sb$Number[is.na(sb$Number)&grepl('no |not |absent', sb$Comments, ignore.case=T)] <- 0
sb$Number[is.na(sb$Number)] <- 1 # assume at least one was present otherwise

### check compatibility of species names
# unique(sb$Species)[which(unique(sb$Species) %in% webspplist$sp == F)]
sb$sp <- as.character(sb$Species)

# dark bellies
sb$sp <- replace(sb$sp,                               
                    sb$sp=='Brent Goose (dark-bellied)', 
                    'Brent Goose (Dark-bellied)')           
# Larus fuscus
sb$sp <- replace(sb$sp,                             
                    sb$sp=='Lesser-black-backed Gull', 
                    'Lesser Black-backed Gull')           
# Larus marinus
sb$sp <- replace(sb$sp,                               
                    sb$sp=='Great-black-backed Gull', 
                    'Great Black-backed Gull')
# unique(sb$sp)[which(unique(sb$sp) %in% webspplist$sp == F)]

### create a Visit ID equivalent
sb$visitid <- factor( with(sb, sprintf('%s %s; %s; %s', Easting, Northing, Recorder, Date)) )




#### Data from HIWWT Movement Surveys ####
source('load_hiwwt.R')

# which species in this dataset don't occur in the WeBS list?
hiwwt$sp[which(hiwwt$sp %in% webspplist$sp ==F)] # none are waterbirds
# therefore we can remove those species
hiwwt <- subset(hiwwt, sp %in% webspplist$sp)

# create a visitid column
hiwwt$visitid <- factor( with(hiwwt, sprintf('%s; %s; %s', FieldID_Origin, Observer, Date)) )


#### keep the workspace tidy ####
rm(sel, long_fix, long_num)




#### Data from ESCP Snapshot Surveys ####
source('load_snapshots.R')


#### Data from ESCP Movement Surveys ####
source('load_movements_escp.R')
