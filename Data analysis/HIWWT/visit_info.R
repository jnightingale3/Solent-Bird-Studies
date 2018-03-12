#### Script to create consistently-named information about visits
#### using surveyor-recorded information.
#### First, information is extracted and named consistently.
#### Then, the format of information is standardised as much as possible.
#### This information is then joined to bird records.

### create logical vector describing which visit IDs are unique

## duplicated() function returns TRUE if the same ID occurs nearer the beginning of dataset
## therefore, we can use it to extract visit info from the first instance of each ID

require(magrittr) # for pipe operators %>% and %<>%


#### dataset: birds ####

# Row numbers containing non-duplicated Visit.ID values:

birds_visitinfo <- data.frame(
  visitid  = as.character(birds$Visit.ID),    # Visit ID
  Easting  = birds$X,           # Easting
  Northing = birds$Y,           # Northing
  Date     = birds$Sighting.D,  # Survey date
  Time     = birds$Time.Seen,   # Survey time (needs cleaning!)
  Species  = birds$sp,             # Species
  Count    = birds$Number.See,
  Describe = birds$Crop.Type,   # Surveyors' descriptions of habitat
  Protect  = birds$Crop.Prote,  # Crop protection method
  Habitat  = birds$Land_Use,    # Semi-standardised habitat descriptions
  Disturb  = birds$Disturbanc,  # Source(s) of disturbance
  Recorder = birds$Full.Name,   # Recorder's name
  Polygon  = birds$SWBGS,       # Appears to be the same name as polygons?
  Sitename = birds$Descriptio,  # Site name (as input by surveyor)
  Latitude = NA,                            # not in this dataset
  Longitude= NA,                            # not in this dataset
  Activity = NA                             # not in this dataset
)


## fix formatting to be consistent
birds_visitinfo$Date %<>% as.POSIXct(format='%A, %B %d, %Y')
birds_visitinfo$Time %<>% as.POSIXct(format='%H:%M') # note: appears some used 12h clock!

## combine factor levels

birds_visitinfo %<>% droplevels

# Crop protection
birds_visitinfo$Protect[which(birds_visitinfo$Protect=='')] <- NA

# Habitat
birds_visitinfo$Habitat[which(birds_visitinfo$Habitat=='')] <- NA

# Disturbance
birds_visitinfo$Disturb[which(birds_visitinfo$Disturb == '')] <- NA




#### dataset: clean ####

clean_visitinfo <- data.frame(
  visitid = as.character(clean$visitid),         # Visit ID
  Easting  = clean$Easting,         # Easting
  Northing = clean$Northing,        # Northing
  Date     = clean$Date,            # Survey date
  Time     = clean$Time,            # Survey time (needs cleaning!)
  Species  = clean$Species,
  Count    = clean$Number,
  Describe = NA,                                # not in this dataset
  Protect  = clean$Crop.protection, # Crop protection method
  Habitat  = clean$Habitat,         # Semi-standardised habitat descriptions
  Disturb  = clean$Disturbance,     # Source(s) of disturbance
  Recorder = clean$Recorder,        # Recorder's name
  Polygon  = NA,                                # not in this dataset
  Sitename = NA,                                # not in this dataset
  Latitude = clean$Latitude,        # Latitude
  Longitude= clean$Longitude,       # Longitude
  Activity = clean$Activity         # Birds' activity/behaviour
)

# convert to character format to manipulate text
clean_visitinfo$Date %<>% as.character
# delete string 'GMT' which is written at the end of some dates
clean_visitinfo$Date <- gsub(' GMT', '', clean_visitinfo$Date) 
# convert to POSIXct format for R analysis
clean_visitinfo$Date %<>% as.POSIXct(format='%m/%d/%Y')
clean_visitinfo$Time %<>% as.POSIXct(format='%H:%M')


## combine factor levels
# Crop protection
clean_visitinfo$Protect[which(clean_visitinfo$Protect == '')] <- NA

# Habitat
clean_visitinfo$Habitat[which(clean_visitinfo$Habitat == '')] <- NA


# Disturbance
clean_visitinfo$Disturb[which(clean_visitinfo$Disturb=='')] <- NA

# Activity
clean_visitinfo$Activity[which(clean_visitinfo$Activity=='')] <- NA
clean_visitinfo$Activity %<>% droplevels

# remove unused factor levels from whole dataset 
# (just in case any got through the purges above!)
clean_visitinfo %<>% droplevels


#### dataset: swbg ####

# Row numbers containing non-duplicated Visit.ID values:

swbg_visitinfo <- data.frame(
  visitid  = as.character(swbg$visitid),          # Visit ID
  Easting  = swbg$Easting,         # Easting
  Northing = swbg$Northing,        # Northing
  Date     = swbg$Date,            # Survey date
  Time     = swbg$Time.on.Site,    # Survey time 
  Species  = swbg$sp,
  Count    = swbg$Count,
  Describe = NA,                              # not in this dataset
  Protect  = NA,                              # not in this dataset
  Habitat  = NA,                              # not in this dataset
  Disturb  = swbg$Disturbance,     # Source(s) of disturbance
  Recorder = swbg$Recorder.Name,   # Recorder's name
  Polygon  = NA,                              # not in this dataset
  Sitename = swbg$Site,            # Strategy Site code
  Latitude = NA,                              # not in this dataset
  Longitude= NA,                              # not in this dataset
  Activity = swbg$Use              # "Feeding / Roosting"
)

# convert to character format to manipulate text
swbg_visitinfo$Date %<>% as.character
# delete string 'GMT' which is written at the end of some dates
swbg_visitinfo$Date <- gsub(' GMT', '', swbg_visitinfo$Date) 
# convert to POSIXct format for R analysis
swbg_visitinfo$Date %<>% as.POSIXct(format='%m/%d/%Y')
swbg_visitinfo$Time %<>% as.POSIXct(format='%H:%M')


## combine factor levels
# Disturbance
swbg_visitinfo$Disturb %<>% as.character
swbg_visitinfo$Disturb[which(swbg_visitinfo$Disturb=='')] <- NA
swbg_visitinfo$Disturb[which(swbg_visitinfo$Disturb=='Yes ')] <- 'Yes'
swbg_visitinfo$Disturb[which(swbg_visitinfo$Disturb=='No')] <- 'None'
swbg_visitinfo$Disturb %<>% factor



# Activity
swbg_visitinfo$Activity[which(swbg_visitinfo$Activity=='')] <- NA
swbg_visitinfo$Activity %<>% droplevels

# remove unused factor levels from whole dataset 
# (just in case any got through the purges above!)
swbg_visitinfo %<>% droplevels



#### dataset: solent birds ####

## format date and time
sb$date <- as.POSIXct(sb$Date, format='%d/%m/%Y')
sb$time <- as.POSIXct(sb$Time, format='%H:%M')

  
sb_visitinfo <- data.frame(
  visitid  = as.character(sb$visitid),         # Visit ID
  Easting  = sb$Easting,         # Easting
  Northing = sb$Northing,        # Northing
  Date     = sb$date,            # Survey date
  Time     = sb$time,            # Survey time
  Species  = sb$sp,
  Count    = sb$Number,
  Describe = NA,                 # not in this dataset
  Protect  = NA,                     # not ready yet
  Habitat  = NA,                     # not ready yet
  Disturb  = NA,                     # not ready yet
  Recorder = sb$Recorder,        # Recorder's name
  Polygon  = NA,                 # not in this dataset
  Sitename = NA,                 # not in this dataset
  Latitude = sb$Latitude,        # original coordinates
  Longitude= sb$Longitude,       # original coordinates
  Activity = NA                      # not ready yet
)



#### dataset: hiwwwt ####

## format date and time
hiwwt$date <- as.POSIXct(hiwwt$Date, format='%d/%m/%Y')
hiwwt$time <- as.POSIXct(hiwwt$Time, format='%H:%M')

hiwwt_visitinfo <- data.frame(
  visitid  = as.character(hiwwt$visitid),  # Visit ID
  Easting  = NA,                      # will be added later (swbg_record_coord.R)
  Northing = NA,                      # will be added later (swbg_record_coord.R)
  Date     = hiwwt$date,     # Survey date
  Time     = hiwwt$time,     # Survey time 
  Species  = hiwwt$sp,
  Count    = hiwwt$Count,
  Describe = NA,                      # not in this dataset
  Protect  = NA,                      # not ready yet
  Habitat  = NA,                      # not ready yet
  Disturb  = NA,                      # not ready yet
  Recorder = hiwwt$Observer, # Recorder's name
  Polygon  = NA,                      # not in this dataset
  Sitename = NA,                      # will be added later (swbg_record_coord.R)
  Latitude = NA,                      # not in this dataset
  Longitude= NA,                      # not in this dataset
  Activity = NA                       # not in this dataset
)


##### dataset: Snapshot counts #####

## format date and time
snap$date <- as.POSIXct(snap$Date, format='%d/%m/%Y')
snap$time <- as.POSIXct(snap$Time, format='%H:%M')

snap_visitinfo <- data.frame(
  visitid  = as.character(snap$Unique.code),  # Visit ID
  Easting  = snap$Easting,                # 
  Northing = snap$Northing,               #
  Date     = snap$date,                   # Survey date
  Time     = snap$time,                   # Survey time 
  Species  = snap$sp,
  Count    = snap$Count,
  Describe = NA,                      # not in this dataset
  Protect  = NA,                      # not ready yet
  Habitat  = NA,                      # not ready yet
  Disturb  = NA,                      # not ready yet
  Recorder = 'Snapshot',              # Recorder's names not included in dataset
  Polygon  = snap$location,           # only some have this
  Sitename = snap$Map.cross.ref,      # some of these are descriptive
  Latitude = NA,                      # not in this dataset
  Longitude= NA,                      # not in this dataset
  Activity = NA                       # not in this dataset
)





##### dataset: ESCP Movement Surveys #####

## format date and time
es$date <- as.POSIXct(es$Date, format='%d/%m/%Y')
es$time <- as.POSIXct(es$Time, format='%H:%M')

es_visitinfo <- data.frame(
  visitid  = as.character(es$Unique.Code),  # Visit ID
  Easting  = es$Easting,                # 
  Northing = es$Northing,               #
  Date     = es$date,                   # Survey date
  Time     = es$time,                   # Survey time 
  Species  = es$sp,
  Count    = es$Count,
  Describe = NA,                      # not in this dataset
  Protect  = NA,                      # not ready yet
  Habitat  = NA,                      # not ready yet
  Disturb  = NA,                      # not ready yet
  Recorder = NA,              # Recorder's names not included in dataset
  Polygon  = NA,           # only some have this
  Sitename = NA,      # some of these are descriptive
  Latitude = NA,                      # not in this dataset
  Longitude= NA,                      # not in this dataset
  Activity = NA                       # not in this dataset
)
