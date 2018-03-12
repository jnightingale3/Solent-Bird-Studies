####### script to assign coordinates to SW&BG records 
####### where only the site was recorded

## sourced in join_all_counts.R


# load required packages
require(magrittr)


# which columns have no coordinates?
sel <- is.na(solent$Easting)

# match sitenames to the Site_code column in polys
# (from which the sitenames in solent were originally created)
# to allow joining of data

# These sites have no sitename
# So assign one using regular expression to extract it from Visit.ID
# solent$Sitename <- solent$Site
solent$Sitename %<>% as.character
solent[sel,]$Sitename <- gsub(';.*$', '', solent[sel,]$visitid)

# check that all Sitecodes of sites missing data are in polygon layer
sum(solent[sel,]$Sitename %in% stratcode$Site_code) # 2216

### data frame of polygon centroids, which will be used as site pseudo-coordinates

# firstly need to bodge the site ID codes so that they are all unique
stratcode$siteid <- as.character(stratcode$Site_code) %>% make.unique

midpts <- gCentroid(stratcode, byid=T, id=stratcode$siteid) %>% as.data.frame

mat <- match(solent[sel,]$Sitename, rownames(midpts))

# assign the Eastings and Northings from polys to these bird records
solent[sel,]$Easting <- midpts$x[mat]
solent[sel,]$Northing <- midpts$y[mat]

rm(sel, mat) # remove temporary data structrures
