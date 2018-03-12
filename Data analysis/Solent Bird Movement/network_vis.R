# visualise the network

require(igraph)
require(magrittr)

# remove movements within a single polygon
net$Origin %<>% as.character
net$Destination %<>% as.character
net <- net[(!net$Origin == net$Destination),] %>% na.omit


# remove reverse movements and duplicates?

# Need to sort the 2 strategy code columns, to identify reverse movements
net_sort <- net # new dataframe to be sorted

# use a lopp
ii <- 1
for (ii in 1:nrow(net_sort)) {
  # put fields in order
  sorted <- sort(subset(net_sort, select=c(Origin, Destination))[ii,])
  # replace original values with ordered values
  net_sort$Origin[ii] <- sorted[1,1]
  net_sort$Destination[ii] <- sorted[1,2]
}

# text column combining both (ordered) strat codes as single identifier
net_sort$both <- with(net_sort, paste(Origin, Destination))

# remove duplicates
net_sort <- net_sort[!duplicated(net_sort$both),]


### use original strategy names where possible
# # origins
# omat <- out_map$Site_code[match(net_sort$Origin, out_map@data$Site_code)]
# net_sort$Origin[which(!is.na(omat))] <- na.omit(omat)
# # destinations
# dmat <- out_map$Site_code[match(net_sort$Destination, out_map@data$Site_code)]
# net_sort$Destination[which(!is.na(dmat))] <- na.omit(dmat)


gnet <- graph_from_data_frame(net_sort, directed=F)
plot(gnet)

tkplot(gnet) # can edit layout and export




#### calculate network statistics for observations ####

### betweenness = how many other sites each site connects together
site_between <- data.frame(
  Site_code = V(gnet)$name,
  between = betweenness(gnet)
)
# top sites
head(site_between[order(site_between$between, decreasing = T),], 23)






### number of observations at each site
site_nob <- table(with(net, c(Origin, Destination))) %>% as.data.frame
names(site_nob) <- c('Site_code', 'NObs')
# top sites
head(site_nob[order(site_nob$NObs, decreasing = T),])


### degree = how many movements to other sites were recorded
site_deg <- data.frame(
  Site_code = V(gnet)$name,
  degree = degree(gnet))
# top sites
head(site_deg[order(site_deg$degree, decreasing = T),], 10)



##### clustering

### global = proprtion of connected triplets of sites that are a closed loop
clus_glob <- transitivity(gnet, type='global')


### local = fraction of triplets through each site that are closed
site_clus <- data.frame(
  Site_code = V(gnet)$name, # vertex (=site) names
  clus = transitivity(gnet, type='local')
)
# top sites
head(site_clus[order(site_clus$clus, decreasing = T),])
