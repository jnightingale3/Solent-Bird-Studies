require(rgdal)
require(rgeos)
require(magrittr)



hnet <- read.csv('Movements_HIWWT_all_known.csv')

# hnet$Origin %in% rownames(midpts)

# add coordinates based on names
hnet$ox <- midpts[match(hnet$Origin, rownames(midpts)), 1]
hnet$oy <- midpts[match(hnet$Origin, rownames(midpts)), 2]
hnet$dx <- midpts[match(hnet$Destination, rownames(midpts)), 1]
hnet$dy <- midpts[match(hnet$Destination, rownames(midpts)), 2]

move_hwt <- subset(hnet, select=c('UniqueID', 'Date', 'Time', 'Species', 'Count', 'Origin', 
                                  'ox', 'oy', 'Destination', 'dx', 'dy'))
move_hwt$Comments <- NA
names(move_hwt) <- names(move_network)

move_network %<>% rbind(move_hwt)
rm(hnet, move_hwt)

# write.csv(move_network, file='Movements_network.csv', row.names = F)



### Identify the required WeBS sectors
webselo <- grepl(' Harbour', move_network$Origin) 
webseld <- grepl(' Harbour', move_network$Destination)

## extract sector ID by analysing text strings
nch <- nchar(unique(move_network$Origin[webselo])) # no. of characters
osectors <- substr(unique(move_network$Origin[webselo]), (nch-4), nch)
move_network$Destination %<>% trimws # trim whitespace from ends of string
nch <- nchar(unique(move_network$Destination[webseld]))
dsectors <- substr(unique(move_network$Destination[webseld]), (nch-4), nch)

sectors <- unique(c(osectors, dsectors)) %>% sort
rm(nch, webselo, webseld, osectors, dsectors)



# sort order of sites
# Need to sort the 2 strategy code columns, to identify reverse movements
# move_network_sort <- move_network # new dataframe to be sorted
# 
# # use a loop
# ii <- 1
# for (ii in 1:nrow(move_network_sort)) {
#   # put fields in order
#   sorted <- sort(subset(move_network_sort, select=c(Origin, Destination))[ii,])
#   # replace original values with ordered values
#   move_network_sort$Origin[ii] <- sorted[1,1]
#   move_network_sort$Destination[ii] <- sorted[1,2]
# }







# spatial files of hiwwt origins and destinations
# ho <- SpatialPointsDataFrame(coords=hnet[,24:25], data=hnet,
#                             proj4string = hiwwtpolys@proj4string)
# hd <- SpatialPointsDataFrame(coords=hnet[,26:27], data=hnet,
#                             proj4string = hiwwtpolys@proj4string)
# 
# plot(hiwwtpolys)
# points(ho, col='green')
# points(hd, col='red')
# 
# 
# # change geometries to match netwk
# 
# ho %<>% spTransform(netwk@proj4string)
# hd %<>% spTransform(netwk@proj4string)
# 
# # where do they overlap?
# holap <- over(ho, netwk) # why are there NAs?
# hdlap <- over(hd, netwk)
# ## There are NAs where the line start or endpoint is in a gap in the underlying
# ## geometry - either onland where there is no strat field, or offshore where 
# ## there are holes in the WeBS data set
# # we should probably fix this at some point
# 
# 
# 
# 
# 
# 
# # add origin and destination polygons to a dataset
# honet <- data.frame(
#   Origin = holap$Site_ID,
#   Destination = hdlap$Site_ID
# ) %>% na.omit
# 
# 
# net %<>% rbind(honet)
