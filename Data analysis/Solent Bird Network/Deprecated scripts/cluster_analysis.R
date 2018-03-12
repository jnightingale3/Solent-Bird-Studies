## have a look at clusters of important brent goose & wader sites
## not much is revealed by this approach (k-means)
## as cluster locations are determined by number of points
## and number of points is strongly affected by sampling bias in these data

## have tried to remedy this by random resampling but not very conclusive

require(ggplot2)
require(plyr)
require(maptools)
require(cluster)

#### brents ####

brent_sites <- join(brent_sites, subset(
  polys@data, select=c('Easting', 'Northing', 'Site_ID')) )

utmcoor<-SpatialPoints(cbind(brent_sites$Easting,brent_sites$Northing), proj4string=polys@proj4string)
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
brent_sites[,4:5] <- longlatcoor@coords

qq <- ggplot(subset(brent_sites),# Importance != 'No Brents'),
             aes(x=Easting, y=Northing))
qq <- qq + geom_point(aes(color=Importance))
qq <- qq + geom_density2d(aes(color=Importance, fill=Importance)) + 
  stat_density2d(h=c(0.065, 0.065))
qq <- qq + facet_wrap(~Importance, ncol=2) + coord_fixed()
print(qq)

## make a dataset for these analyses
dat <- subset(brent_sites, Importance=='A' | Importance=='B', 
              select=c('Easting', 'Northing'))

## try k-means clustering with different values of k

bg.k3 <- kmeans(dat, centers = 3)
clusplot(dat, bg.k3$cluster, color=TRUE, shade=TRUE)

bg.k4 <- kmeans(dat, centers = 4)
clusplot(dat, bg.k4$cluster, color=TRUE, shade=TRUE)

bg.k5 <- kmeans(dat, centers = 5)
clusplot(dat, bg.k5$cluster, color=TRUE, shade=TRUE)

bg.k8 <- kmeans(dat, centers = 8)
clusplot(dat, bg.k8$cluster, color=TRUE, shade=TRUE)

bg.k11 <- kmeans(dat, centers = 11)
clusplot(dat, bg.k11$cluster, color=TRUE, shade=TRUE)
         



## plot it
bgclus <- cbind(subset(brent_sites, Importance%in%c('A', 'B')), 
                factor(bg.k4$cluster))
names(bgclus)[6]<-'Cluster'
ggplot(bgclus)+geom_point(aes(x=Easting, y=Northing, color=Cluster))+coord_equal()



## optimal number of clusters
# from http://www.sthda.com/english/wiki/determining-the-optimal-number-of-clusters-3-must-known-methods-unsupervised-machine-learning
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
wss <- sapply(1:k.max, 
              function(k){kmeans(dat, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
# manually decide where is the steepest break: 3 or 5?
# abline(v = 3, lty =2)

# check against random resampling of that data
rdat <- brent_sites[sample(1:nrow(brent_sites), 
                           sum(table(brent_sites$Importance)[1:2]),
                           replace = T), 4:5]
br.k3 <- kmeans(rdat, centers = 4)
clusplot(rdat, br.k3$cluster, color=TRUE, shade=TRUE)


#### waders ####

wader_sites <- join(wader_sites, subset(
  polys@data, select=c('Easting', 'Northing', 'Site_ID')) )

utmcoor<-SpatialPoints(cbind(wader_sites$Easting,wader_sites$Northing), proj4string=polys@proj4string)
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
wader_sites[,4:5] <- longlatcoor@coords


dat <- subset(wader_sites, Importance != 'No waders', 
              select=c('Easting', 'Northing'))

wss <- sapply(1:k.max, 
              function(k){kmeans(dat, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

w.k3 <- kmeans(dat, centers = 3)
clusplot(dat, w.k3$cluster, color=TRUE, shade=TRUE)


rdat <- wader_sites[sample(1:nrow(wader_sites), 
                           sum(table(wader_sites$Importance)[1:3]),
                           replace = T), 4:5]
wr.k3 <- kmeans(rdat, centers = 3)
clusplot(rdat, wr.k3$cluster, color=TRUE, shade=TRUE)


### tidy up workspace
rm(list=ls(pattern = '.k'), dat, rdat, wss, bgclus)