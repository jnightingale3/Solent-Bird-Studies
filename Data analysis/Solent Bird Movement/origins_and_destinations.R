# file to standardise origin and destination locatons
# TODO join shapefiles of strat sites and LTC sectors

require(rgdal)
require(rgeos)
require(magrittr)

# load tabular data
move <- read.csv('Movement_SBS_all.csv')
# fix a problem with the species codes
move$Species <- gsub(' ', '', move$Species, fixed = TRUE)


# fix NAs - equivalent to unknown
move$Oknown[is.na(move$Oknown)] <- 'N'
move$Dknown[is.na(move$Dknown)] <- 'N'


# dataset only including where origin and/or destination is known
mpartial <- subset(move, Oknown=='Y' | Dknown=='Y')




# load shapefile spatial data
shape <- readOGR(dsn='../../Solent Bird Studies GIS Data/Survey results',
                 layer='all_movements2') %>%
  spTransform(CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601272 +x_0=400000 +y_0=-100000
                  +datum=OSGB36 +units=m +no_defs +ellps=airy
                  +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894"))


# combine inconsistently named columns
shape$UniqueCode %<>% as.character
shape$UniqueCode[which(shape$UniqueCode=='0')] <-
  as.character(shape$Uniquecod[which(shape$UniqueCode==0)])


### fix some errors in the data

# what isn't matched between digitised movements and input data?
shape@data[which(!shape$UniqueCode %in% mpartial$Unique.Code),] %>% nrow  # 144
mpartial[which(!mpartial$Unique.Code %in% shape$UniqueCode),1:6] %>% nrow # 119


# one of the problem is dates - eithr 2016 or just 16: standardise to short form
shape$UniqueCode <- gsub('2016', '16', shape$UniqueCode)
shape$UniqueCode <- gsub('2017', '17', shape$UniqueCode)
mpartial$Unique.Code <- gsub('2016', '16', mpartial$Unique.Code)
mpartial$Unique.Code <- gsub('2017', '17', mpartial$Unique.Code)
shape$UniqueCode <- gsub('19011217', '190117', shape$UniqueCode)
# shape$UniqueCode <- gsub('211016', '21102016', shape$UniqueCode)
shape$UniqueCode <- gsub('212016', '211016', shape$UniqueCode)

# some codes recorded inconsistently across datasets
mpartial$Unique.Code <- gsub('7_ER_211016_D', 'VP7_ER_211016_M', mpartial$Unique.Code)
mpartial$Unique.Code <- gsub('VP', '', mpartial$Unique.Code)
shape$UniqueCode <- gsub('VP', '', shape$UniqueCode)
mpartial$Unique.Code <- gsub('M0', 'MO', mpartial$Unique.Code)
shape$UniqueCode <- gsub('M0', 'MO', shape$UniqueCode)
mpartial$Unique.Code <- gsub('M15_A', 'MO15', mpartial$Unique.Code)
mpartial$Unique.Code <- gsub('M15_B', 'MO15', mpartial$Unique.Code)
mpartial$Destination <- gsub('PO', 'P0', mpartial$Destination)

# remove whitespace-only strings
mpartial$Origin <- gsub(' ', '', mpartial$Origin)
mpartial$Destination <- gsub(' ', '', mpartial$Destination)


# write out spreadsheets of problematic codes - these are mostly fixed
# mpartial[which(!mpartial$Unique.Code %in% shape$UniqueCode),1]%>%write.csv(file='uc-move.csv')
# shape@data[which(!shape$UniqueCode %in% mpartial$Unique.Code),]%>%write.csv(file='uc-shape.csv')



mat <- match(mpartial$Unique.Code, shape$UniqueCode)


#### COORDINATES FROM DIGISTISED PATHS ####

# origin x and y coordinates - from shapefile
mpartial$ox <- NA
ii <- 1
for (ii in 1:length(mat)) {
  if (is.na(mat[ii])) {next} else {
    mpartial$ox[ii] <- unlist(shape@lines[[mat[ii]]])@Lines[[1]]@coords[1,1]
  }
}
mpartial$oy <- NA
ii <- 1
for (ii in 1:length(mat)) {
  if (is.na(mat[ii])) {next} else {
    mpartial$oy[ii] <- unlist(shape@lines[[mat[ii]]])@Lines[[1]]@coords[1,2]
  }
}



# destination coordinates from shapefile
mpartial$dx <- NA
ii <- 1
for (ii in 1:length(mat)) {
  if (is.na(mat[ii])) {next} else {
    mpartial$dx[ii] <- unlist(shape@lines[[mat[ii]]])@Lines[[1]]@coords[
      nrow(unlist(shape@lines[[mat[ii]]])@Lines[[1]]@coords),1]
  }
}

mpartial$dy <- NA
ii <- 1
for (ii in 1:length(mat)) {
  if (is.na(mat[ii])) {next} else {
    mpartial$dy[ii] <- unlist(shape@lines[[mat[ii]]])@Lines[[1]]@coords[
      nrow(unlist(shape@lines[[mat[ii]]])@Lines[[1]]@coords),2]
  }
}


# how many have missing location information?
sum( (is.na(mpartial$ox) | is.na(mpartial$dx)) ) # 78


#### COORDINATES FROM RECORDED POLYGONS ####

# load polygons
hiwwtpolys <- readOGR(dsn='../../Solent Bird Studies GIS Data/HIWWT Strategy Layer', 
                 layer='SWBGS')


#### calculate polygon midpoints

# firstly need to bodge the site ID codes so that they are all unique
hiwwtpolys$siteid <- as.character(hiwwtpolys$Site_code)
hiwwtpolys$siteid[which(duplicated(hiwwtpolys$Site_code))] <- paste(
  hiwwtpolys$siteid[which(duplicated(hiwwtpolys$Site_code))], 'Z', sep=''
)
# now calculate centroids
midpts <- gCentroid(hiwwtpolys, byid=T, id=hiwwtpolys$siteid) %>% as.data.frame



# match origins and destinations to polygon centroids
mato <- match(mpartial$Origin, rownames(midpts))
matd <- match(mpartial$Destination, rownames(midpts))


# assign origin coordinates
mpartial$ox[which(!is.na(mato))] <- 
  midpts$x[mato[!is.na(mato)]]

mpartial$oy[which(!is.na(mato))] <- 
  midpts$y[mato[!is.na(mato)]]


# assign destination coordinates
mpartial$dx[which(!is.na(matd))] <- 
  midpts$x[matd[!is.na(matd)]]

mpartial$dy[which(!is.na(matd))] <- 
  midpts$y[matd[!is.na(matd)]]


# how many have missing location information?
noloc <- (is.na(mpartial$ox) & is.na(mpartial$dx)); sum(noloc) # 46
noloc2 <- (is.na(mpartial$ox) | is.na(mpartial$dx)); sum(noloc2) # 71
mpartial[which(noloc2), 1:5]
write.csv(mpartial[which(noloc2),], file='nolocation.csv', row.names = F)


# which known movements are still not getting across, and why?
mpartial[noloc,]%>%subset(Oknown=='Y' & Dknown =='Y') # only 4
# movements are between two non-strategy sites
# therefore, these aren't really that important anyway



# Add observer information
mpartial$Observer <- NA
mpartial$Observer[grepl('DS', mpartial$Unique.Code)] <- 'Dave Stevenson'
mpartial$Observer[grepl('ER', mpartial$Unique.Code)] <- 'Ed Rowsell'
mpartial$Observer[grepl('JN', mpartial$Unique.Code)] <- 'Josh Nightingale'
mpartial$Observer[grepl('CL', mpartial$Unique.Code)] <- 'Chris Lycett'
mpartial$Observer[grepl('LM', mpartial$Unique.Code)] <- 'Louise MacCallum'
mpartial[is.na(mpartial$Observer),]%>%head # None





##### separate data set for use in site study #####

knowno <- subset(mpartial, select=c(Unique.Code, ox, oy, Date, Time, Species, Count, Observer))
knownd <- subset(mpartial,  select=c(Unique.Code, dx, dy, Date, Time, Species, Count, Observer))
                                     
# standardise column names so that datasets can be joined
names(knowno)[2:3] <- names(knownd)[2:3] <- c('Easting', 'Northing') 

# join movements with known origins or destinations together
mk_ultra <- rbind(knowno, knownd)



# write.csv(mk_ultra, file='Movements_ESCP_known_sites.csv', row.names = FALSE)




##### separate dataset for use in movement study ######

move_network <- subset(mpartial, Oknown=='Y' & Dknown=='Y', select=
                         c(Unique.Code, Date,Time, Species, Count,
                           Origin, ox, oy,
                           Destination, dx ,dy, Comments))


