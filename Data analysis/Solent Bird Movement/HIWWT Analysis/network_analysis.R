# HIWWT network visualisation and analysis

require(igraph)
require(magrittr)
require(plyr)


##### set up data for network analysis #####

# brent network

bnet <- subset(alldat, #Species=='DB' &
                 # only want brent movements 
                 grepl('MO', RecordID) & Count > 0 &
                 # with known origin and destination
                 Oknown=='Yes' & Dknown == 'Yes' &
                 # where both sites are strategy locations
                 FieldID_Origin %in% strat_codes & Destination_code %in% strat_codes
                 # and aren't movements within the same site
                 & FieldID_Origin != Destination_code) # leaving 88 observations



##### Calculate site summary data: number and size of movements #####

# Need to sort the 2 strategy code columns, to identify reverse movements
bnet_sort <- bnet # new dataframe to be sorted

# use a lopp
ii <- 1
for (ii in 1:nrow(bnet_sort)) {
  # put fields in order
  sorted <- sort(subset(bnet_sort, select=c(FieldID_Origin, Destination_code))[ii,])
  # replace original values with ordered values
  bnet_sort$FieldID_Origin[ii] <- sorted[1,1]
  bnet_sort$Destination_code[ii] <- sorted[1,2]
}

# text column combining both (ordered) strat codes as single identifier
bnet_sort$both <- with(bnet_sort, paste(FieldID_Origin, Destination_code))


# thickness of edge: number of movements recorded
movements <- ddply(bnet_sort, .(both), function(x) {
  data.frame(Thick = nrow(x))
})
bnet_sort %<>% join(movements)

# size of node: median number of birds
median_db <- ddply(bnet_sort, .(both), function(x) {
  data.frame(Size = median(x$Count))
})
bnet_sort %<>% join(median_db) # TODO: Scale thse values so they fit on graph!


write.csv(bnet_sort, file='All_known_HIWWT_movements.csv', row.names = F)

##### plot #####

# Add this site information to the dataset for plotting
bnet_edges <- subset(bnet_sort, select=c(FieldID_Origin, Destination_code,
                                    Size, Thick))

# # Create similar categorical column containing unique identifier
# bnet_edges$both <- with(bnet_edges, paste(FieldID_Origin, Destination_code))
# 
# bnet_edges %<>% extract(which(!duplicated(bnet_sort$both)),) %>% droplevels

dolphin <- graph_from_data_frame(bnet_edges, directed=F)
plot(dolphin, vertex.size=10, # basic network visualisation
     vertex.label.cex=0.65, vertex.shape='circle')



#### calculate network statistics for observations ####

# number of observations at each site
bg_nobs <- table(with(bnet, c(FieldID_Origin, Destination_code))) %>% as.data.frame


# degree = how many movements to other sites were recorded
site_deg <- data.frame(
  name = V(dolphin)$name,
  degree = degree(dolphin))


# betweenness = how many other sites each site connects together
between <- data.frame(
  name = V(dolphin)$name,
  between = betweenness(dolphin)
)

## clustering

# global = proprtion of connected triplets of sites that are a closed loop
clus_glob <- transitivity(dolphin, type='global')

# local = fraction of triplets through each site that are closed
clus_loc <- data.frame(
  name = V(dolphin)$name, # vertex (=site) names
  clus = transitivity(dolphin, type='local')
)

