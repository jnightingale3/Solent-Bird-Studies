### script to identify and plot counts over certain thresholds

require(plyr)
require(dplyr)
require(ggplot2)

### load in Solent surveys' species list
species_list <- read.csv("Species_list.csv")
head(species_list)


### single column for species included in SPA designations
species_list$Desig <- species_list$desCL == 'Y' | species_list$desPag == 'Y' |
  species_list$desPor == 'Y' | species_list$desSot == 'Y'


### new species column to match species names borked by ddply functions
species_list$sp <- gsub(pattern='[ ()-]', '.', x=species_list$Species)
## vector to specify which columns are counts of species
nsel <- which(names(solent_jdata) %in% species_list$sp)



##### bird count thresholds

### crossing the national importance threshold

# initialise empty data frame
threshgb <- data.frame(matrix(0, nrow = nrow(solentspp), ncol = ncol(solentspp) ))
colnames(threshgb) <- species_list$sp

# use for loop to compare counts with thresholds for each species
ii <- 0
for(ii in 1:length(species_list$sp)) {
  threshgb[,ii] <- solent_jdata[,(nsel[ii])] > species_list$threshGB[ii]
}
threshgb[is.na(threshgb)] <- 0

# all species with at least one nationally important
colSums(threshgb)[colSums(threshgb)>0] # 7 spp; 135 sites
# all designated SPA species
colSums(threshgb)[species_list$Desig] # BLTGO, DBBrent & Grey Plover have



### crossing the international threshold
ii <- 0
threshint <- data.frame(matrix(0, nrow = nrow(solentspp), ncol = ncol(solentspp) ))
colnames(threshint) <- species_list$sp
for(ii in 1:length(species_list$sp)) {
  threshint[,ii] <- solent_jdata[,(nsel[ii])] > species_list$threshInt[ii]
}
threshint[is.na(threshint)] <- 0

# all species with at least one internationally important
colSums(threshint)[colSums(threshint)>0] # 2 spp; 13 sites
# all designated SPA species
colSums(threshint)[species_list$Desig]  # brent & blackwit



### threshold for total size of assemblage is 20,000 indivs
### to achieve SPA designation (per JNCC website)

## use some percentage for cutoff, 
## e.g. 10% of threshold for SPA designation
pc <- 10 # percentage
solent_jdata$asstot <- rowSums(solentspp)
threshass <- which(rowSums(solentspp) > 20000 * pc/100) # 32 sites


### multifigure plot showing distribution of those counts
### overlaid onto polygon layer
### superceded by ggmap below
### due to problems with polygons TODO: fix polygons! ;)
# opar <- par()
# par(mfrow=c(2,2))
# plot(polys, main='National')
# points(solent_join[which(rowSums(threshgb)>0),], col='blue')
# plot(polys, main='International')
# points(solent_join[which(rowSums(threshint)>0),], col='red')
# plot(polys, main='Assemblage')
# points(solent_join[as.numeric(rownames(solentspp[threshass,])),], col='green')
# par(opar)

### plot on google maps ###
require(ggmap)
require(ggplot2)

# street map
mapstreet <- get_googlemap(center='Farehamn, UK', zoom=10, color='bw', maptype = 'terrain-background')
# ggmap(mapstreet)
# terrain map(s)
mapterrain3 <- get_googlemap(center='Fareham, UK', zoom=10, maptype = 'satellite',color='bw')
# ggmap(mapterrain3)

# or black-and-white high contrast map (polygons show up better)
maptoner <- get_map(location=c(lon=-1.2, lat=50.85), zoom=10, maptype = 'toner',
                          source='stamen')
# ggmap(maptoner)



### convert East/Northings to lat-long for ggmap
library(rgdal)
# prepare UTM coordinates matrix
utmcoor<-SpatialPoints(cbind(solent_jdata$Easting,solent_jdata$Northing), proj4string=polys@proj4string)
# converting to latlon
longlatcoor<-spTransform(utmcoor,CRS("+proj=longlat"))
# add to original data frame
solent_jdata <- cbind(solent_jdata, longlatcoor@coords)



#### Plot everything noteworthy on same maps ####

# plot over street map
map.s <- ggmap(mapstreet) + 
  geom_point(data=solent_jdata[which(rowSums(threshgb)>0),],
             aes(x=coords.x1, y=coords.x2), colour='yellow') +
  geom_point(data=solent_jdata[as.numeric(rownames(solentspp[threshass,])),],
             aes(x=coords.x1, y=coords.x2), colour='blue') +
  geom_point(data=solent_jdata[which(rowSums(threshint)>0),],
             aes(x=coords.x1, y=coords.x2), colour='red') 
print(map.s)


# plot over satellite map
map.t <- ggmap(mapterrain) + 
  geom_point(data=solent_jdata[which(rowSums(threshgb)>0),],
             aes(x=coords.x1, y=coords.x2), colour='yellow') +
  geom_point(data=solent_jdata[as.numeric(rownames(solentspp[threshass,])),],
             aes(x=coords.x1, y=coords.x2), colour='blue') +
  geom_point(data=solent_jdata[which(rowSums(threshint)>0),],
             aes(x=coords.x1, y=coords.x2), colour='red') 
print(map.t)




#### species-specific maps ####

colSums(threshgb)[colSums(threshgb)>0] # which species cross thresholds?

# vector to extract correct species
selmap <- which(threshint$Brent.Goose..Dark.bellied.)
# print details of sites
solent_jdata[selmap,]
# check other counts of that species for comparison
(subset(solent_jdata, Grey.Plover > 300))

# make map
map.x <- ggmap(mapterrain) + 
  geom_point(data=solent_jdata[threshass,],
             aes(x=coords.x1, y=coords.x2), size= 2, colour='blue') +
  scale_y_continuous(limits=c(50.75, 50.95)) +
ggtitle('Large waterbird assemblage (>2000 individuals)')# update title manually
print(map.x) # print map




#### summarise counts by polygon 
# (using TARGET_FID column at this stage)

### GB importance
solent_jdata$gimp <- rowSums(threshgb > 0)
gbfid <- by(solent_jdata, solent_jdata$TARGET_FID, function(x){sum(x$gimp)})
gbfid[gbfid>=5]
solent_jdata[solent_jdata$TARGET_FID==1087&solent_jdata$gimp,]



### International importance
solent_jdata$iimp <- rowSums(threshint > 0)
infid <- by(solent_jdata, solent_jdata$TARGET_FID, function(x){sum(x$iimp)})
infid[infid>0]


### Assemblage of importance
solent_jdata$assimp <- rowSums(solentspp) > 20000 * pc/100
assfid <- by(solent_jdata, solent_jdata$TARGET_FID, function(x){sum(x$assimp)})
assfid[assfid>1]
solent_jdata[solent_jdata$TARGET_FID==414&solent_jdata$assimp,] 






##### bird totals by polygon

# number of visits by polygon
fidvis <- ddply(solent_jdata, .(TARGET_FID), nrow)
head(fidvis[order(fidvis$V1, decreasing = T),], 10)
# whereis
head(solent_jdata[solent_jdata$TARGET_FID=='929',],5)


# totals by polygon
fidtots <- ddply(solent_jdata, .(TARGET_FID), function(x) {
  colSums(x[,which(names(solent_over)%in%species_list$Species)])
})

fidtots$Total <- rowSums(fidtots[,-1])
fidtots$NoSpp <- rowSums(fidtots[,-1]>1)

head(fidtots)
nrow(solent_jdata[solent_jdata$TARGET_FID==906,])


### which FIDs have totals (over all surveys) across SPA threshold
fidtots[which(fidtots$Total>20000),]

#### coordinates of FIDs
#### update this using the latlong from rgdal conversion above
coordfids <- subset(solent_jdata, duplicated(TARGET_FID)==FALSE, 
                   select=c('TARGET_FID', 'coords.x1', 'coords.x2'))

sp_fidtots <- subset(left_join(fidtots, coordfids), Total > 0)

p <- ggplot(sp_fidtots)
p1 <- p + geom_point(aes(x=coords.x1, y=coords.x2, size=log1p(Total), color=NoSpp), alpha=0.5)
print(p1)

p2 <- ggmap(mapstreet) + geom_point(data=sp_fidtots,
                                    aes(x=coords.x1, y=coords.x2, size=log1p(Total), color=NoSpp), alpha=0.5)
print(p2)

p3 <- ggmap(mapstreet) + geom_point(data=sp_fidtots,
                                    aes(x=coords.x1, y=coords.x2, size=(Total), color=NoSpp), alpha=0.5)
print(p3)





############## MONTHLY TOTALS ##########
solent_jdata$month <- factor(months(solent_jdata$Date))
with(solent_jdata[solent_jdata$asstot>0,], plot(month, Brent.Goose..Dark.bellied.))
