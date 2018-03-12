### File to load in shapefiles


#### Setup ####

require(plyr)
require(rgdal)
require(rgeos)
require(magrittr)



#### Low Tide Counts ####


# filepath to directory containing shapefiles
fp <- '../../Solent Bird Studies GIS Data/WeBS LTC sectors/'

### Each Site has a separate shapefile which has to be loaded individually
### Then standardise them:
### Want all to have columns NWC5, X_COORD and Y_COORD and Site

bea<-readOGR(dsn=fp, layer='Beaulieu Estuary_5', stringsAsFactors = FALSE)
bea@data$COUNT <- NULL
bea@data$Site <- "Beaulieu Estuary" 

bem<-readOGR(dsn=fp, layer='bembridge_5', stringsAsFactors = FALSE)
bem@data$ID <- bem@data$AREA <- NULL
bem@data$Site <- "Bembridge Harbour"

chi<-readOGR(dsn=fp, layer='Chichester Harbour_5', stringsAsFactors = FALSE)
chi@data$COUNT <- chi@data$LABEL <- NULL
chi@data$Site <- "Chichester Harbour"

lan<-readOGR(dsn=fp, layer='Langstone Harbour_5', stringsAsFactors = FALSE)
lan@data$COUNT <- lan@data$AREA <- NULL
lan@data$Site <- "Langstone Harbour"

new<-readOGR(dsn=fp, layer='Newtown Harbour_5', stringsAsFactors = FALSE)
new@data$COUNT <- new@data$LABEL <- NULL
new@data$Site <- "Newtown Harbour" 

nws<-readOGR(dsn=fp, layer='North-west Solent_5', stringsAsFactors = FALSE)
nws@data$COUNT <- nws@data$LABEL <- NULL
nws@data$Site <- "North-west Solent"

pag<-readOGR(dsn=fp, layer='Pagham Harbour_5', stringsAsFactors = FALSE)
pag@data$COUNT <- pag@data$LABEL <- NULL
pag@data$Site <- "Pagham Harbour"

por<-readOGR(dsn=fp, layer='Portsmouth Harbour_5', stringsAsFactors = FALSE)
por@data$COUNT <- por@data$LABEL <- NULL
por@data$Site <- "Portsmouth Harbour"

sou<-readOGR(dsn=fp, layer='southampton water_5', stringsAsFactors = FALSE)
sou@data$Y_COORD <- sou@data$X_COORD <- sou@data$NWC5 <- as.numeric(NA)
sou@data$FID <- NULL
sou@data$Site <- "Southampton Water"
#### the below line is a bodge to get it to load 
#### but does not give polygons the correct names!
#### can we fix it?
sousects <- ltc$Sector.code[ltc$Site=='Southampton Water'] %>% 
  unique  %>% as.character()
sou@data$NWC5[1:length(sousects)] <- sousects

# use sp::rbind to join them all together
low_sects <- rbind(bea, bem, chi, lan, new, nws, pag, por, sou, makeUniqueIDs = TRUE)
rm(fp, sousects)
# rm(bea, bem, chi, lan, new, nws, pag, por, sou)



#### High Tide Counts ####

core_sects <- readOGR(dsn='../../Solent Bird Studies GIS Data',
                      layer='Solent_sections')

# remove Pagham sectors
core_sects %<>% subset(NAME %in% c('Pagham - Harbour', 'Pagham Lagoon', 'Pagham Sea',
                                   'North Fields','Severals', 'Sidlesham Ferry') == F)

# plot(core_sects)
head(core_sects@data); str(core_sects@data)

head(htc)

# Are all sectors accounted for in LOC_LABEL? Yes!
sum(unique(htc$sector) %in% core_sects@data$LOC_LABEL) / nlevels(htc$sector) # :)



#### combine with site names from low tide sectors ####

source('run.sectors_to_sites.R')
htc <- join(htc, htc_sites) # adds column for Site (e.g. Langstone harbour)
htc %<>% subset(Site != 'Pagham Harbour')
