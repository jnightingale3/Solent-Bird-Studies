#### TODO 
# use net from network_vis.R (processed version)
# join to out_cents by id
# then do the spatial stuff

require(magrittr)

# get centroids to standardise locations
out_cents <- gCentroid(out, byid = T)@coords %>% data.frame
out_cents$Site_code <- out$Site_code


### join centroids to move_network
# origins
omat <- match(net$Origin, out_cents$Site_code)
oc <- out_cents[omat,]
names(oc) <- paste0('o', names(oc))
# destinations
dmat <- match(net$Destination, out_cents$Site_code)
dc <- out_cents[dmat,]
names(dc) <- paste0('d', names(dc))
# join together
mc <- cbind(oc, dc)

# convert to a Lines object
ii <- 1
mclist <- list(NA)
todo <- c(which(!(is.na(mc$ox)|is.na(mc$dy))))
for (ii in  1:length(todo)) {
  mclist[[ii]] <- rbind(as.numeric(mc[todo[ii],1:2]), as.numeric(mc[todo[ii],4:5])) %>% as.matrix
}
mcline <- lapply(mclist, Line)
mclines <- Lines(mcline, ID = 'hello')
linemap <- SpatialLines(list(mclines), proj4string = out@proj4string)
mcdata <- (mc[todo,])
linemap2 <- SpatialLinesDataFrame(sl=linemap, data=mcdata[1,], match.ID = F)



# check it out with lines
plot(out)
lines(linemap2, col='red')
