####### script to assign coordinates to SW&BG records 
####### where only the site was recorded

##### It uses the old polygon layer
##### because the new one doesn't include strategy codes
##### or is there a way to join strategy codes to new dataset using some kind of overlap tool?


## sourced in visit_info_join.R - ie before the spatial join


# load required packages
require(rgdal)
require(rgeos)
require(magrittr)

## the older shapefile (with messed up dissolved polygons) contained 
## strategy codes for sites; we need this
stratcode <- readOGR(dsn='../../Solent Bird Studies GIS Data', layer='Waders & Brent Geese Sites_region')

# which columns have no coordinates?
sel <- is.na(solent$Easting)

# match sitenames to the SITE_ID column in polys
# (from which the sitenames in solent were originally created)
# to allow joining of data

# These sites have no sitename
# So assign one using regular expression to extract it from Visit.ID
solent$Sitename %<>% as.character
solent[sel,]$Sitename <- gsub(';.*$', '', solent[sel,]$visitid)

# check that all Sitecodes of sites missing data are in polygon layer
sum(solent[sel,]$Sitename %in% stratcode$SITE_ID) # 12 are not, but 2972 are :)


### data frame of polygon centroids, which will be used as site pseudo-coordinates

# firstly need to bodge the site ID codes so that they are all unique
stratcode$siteid <- as.character(stratcode$SITE_ID)
stratcode$siteid[which(duplicated(stratcode$SITE_ID))] <- paste(
  stratcode$siteid[which(duplicated(stratcode$SITE_ID))], '2', sep=''
)

midpts <- gCentroid(stratcode, byid=T, id=stratcode$siteid) %>% as.data.frame

mat <- match(solent[sel,]$Sitename, rownames(midpts))

# assign the Eastings and Northings from polys to these bird records
solent[sel,]$Easting <- midpts$x[mat]
solent[sel,]$Northing <- midpts$y[mat]

rm(sel, mat, stratcode) # remove temporary data structrures
