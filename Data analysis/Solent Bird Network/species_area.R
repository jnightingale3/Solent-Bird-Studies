#### cumulative species-area relationships
#### depends on diversity.R
require(dplyr)
require(rgeos) # for gArea - area of geometric objects


### create newspecies-area dataset


## species
solspa <- left_join(soldiv, solent)
# solspt <- SpatialPointsDataFrame(coords=cbind(solspa$Easting,solspa$Northing),
#                                  data=solent,
#                                  proj4string = polys@proj4string)


## area
areas <- data.frame(garea = gArea(polys, byid = T) / 10000) # in Ha not m^2
areas$TARGET_FID <- 0:(nrow(areas)-1)
targetfid <- subset(solent_jdata, select=c(Visit.ID, TARGET_FID))
areas2 <- left_join(targetfid, areas)
solspa2 <- left_join(solspa, areas2)
solspa <- solspa2
rm(areas2, solspa2)



#### cumulative diversity measures

# cumulative number of individuals
solspa$cumnin <- cumsum(solspa$Indivs) # cumulative sum function


# cumulative species richness - function to be re-used
cumfunc <- function(DF) {
  out <- NA # initialise empty vector
  nums2 <- names(DF) %in% names(solentspp) # only use species name columns
  
  # do 1st row separately because colSums needs 2-dimensional input
  out[1] <- sum( (DF[,nums2] > 0)[1:2,] ) 
  
  # loop to sum number of non-zero columm totals from top to each row
  ii <- 2 # index
  for (ii in 2:nrow(DF)) {
    out[ii] <- sum( colSums( DF[1:ii,(names(DF)[nums2])] > 0 ) > 0 )
  }
  
  return(out)
}

# use function to calculate cumulative richness for all data
solspa$cumric <- cumfunc(solspa)
# plot against number of individuals


### break this down by habitat

## Amenity
Amenity <- subset(solspa, Habitat=='Amenity')
# cumulative number of individuals
Amenity$cumnin <- cumsum(Amenity$Indivs) # cumulative sum function
# use function to calculate cumulative richness for all data
Amenity$cumric <- cumfunc(Amenity)


## Beach
Beach <- subset(solspa, Habitat=='Beach')
# cumulative number of individuals
Beach$cumnin <- cumsum(Beach$Indivs) # cumulative sum function
# use function to calculate cumulative richness for all data
Beach$cumric <- cumfunc(Beach)


## Arable
Arable <- subset(solspa, Habitat=='Arable')
# cumulative number of individuals
Arable$cumnin <- cumsum(Arable$Indivs) # cumulative sum function
# use function to calculate cumulative richness for all data
Arable$cumric <- cumfunc(Arable)


## Grazing
Grazing <- subset(solspa, Habitat=='Grazing')
# cumulative number of individuals
Grazing$cumnin <- cumsum(Grazing$Indivs) # cumulative sum function
# use function to calculate cumulative richness for all data
Grazing$cumric <- cumfunc(Grazing)


## Saltmarsh
Saltmarsh <- subset(solspa, Habitat=='Saltmarsh')
# cumulative number of individuals
Saltmarsh$cumnin <- cumsum(Saltmarsh$Indivs) # cumulative sum function
# use function to calculate cumulative richness for all data
Saltmarsh$cumric <- cumfunc(Saltmarsh)


### Plot - Area-Richness
opar <- par()
par(mfrow=c(1,3))

# plot species richness against cumulative area
with(solspa, plot(cumsum(log10(garea)), cumric, type='l', lty=2, xlim=c(0,4500),
                  xlab='Cumulative area counted (log Ha)',
                  ylab='Cumulative species richness'))
with(Amenity, lines(cumsum(log10(garea)), cumric, type='l'))
with(Arable, lines(cumsum(log10(garea)), cumric, type='l', col='orange'))
with(Beach, lines(cumsum(log10(garea)), cumric, type='l', col='blue'))
with(Grazing, lines(cumsum(log10(garea)), cumric, type='l', col='green'))
with(Saltmarsh, lines(cumsum(log10(garea)), cumric, type='l', col='purple'))


### Plot - Individuals-Richness
# plot species richness against number of individual
with(solspa, plot(log1p(cumnin), cumric, type='l', lty=2, xlim=c(0,12),
                  xlab='Cumulative number of individuals (Log)',
                  ylab='Cumulative species richness'))
with(Amenity, lines(log1p(cumnin), cumric, type='l'))
with(Arable, lines(log1p(cumnin), cumric, type='l', col='orange'))
with(Beach, lines(log1p(cumnin), cumric, type='l', col='blue'))
with(Grazing, lines(log1p(cumnin), cumric, type='l', col='green'))
with(Saltmarsh, lines(log1p(cumnin), cumric, type='l', col='purple'))


### Plot - Area-Individuals
# plot individuals against cumulative area
with(solspa, plot(cumsum(log10(garea)), log1p(cumnin), type='l', lty=2, xlim=c(0,4500),
                  xlab='Cumulative area counted (log Ha)', ylim=c(0,12),
                  ylab='Cumulative number individuals (log)'))
with(Amenity, lines(cumsum(log10(garea)), log1p(cumnin), type='l')) 
with(Arable, lines(cumsum(log10(garea)), log1p(cumnin), type='l', col='orange')) 
with(Beach, lines(cumsum(log10(garea)), log1p(cumnin), type='l', col='blue')) 
with(Grazing, lines(cumsum(log10(garea)), log1p(cumnin), type='l', col='green')) 
with(Saltmarsh, lines(cumsum(log10(garea)), log1p(cumnin), type='l', col='purple'))
legend('bottomright', 
       c('Amenity', 'Arable', 'Beach', 'Grazing', 'Saltmarsh', 'Total'),
       lty=c(1,1,1,1,1,2),
       col=c('black', 'orange', 'blue', 'green', 'purple', 'black'))


par(opar)


### how many visits to each polygon?
nvisits <- by(solspa, solspa$TARGET_FID, nrow)
summary(nvisits) # median is 2; mean <6
sum(nvisits < 5) / length(nvisits) * 100 # 75% fewer than 5 visits
sum(nvisits>=30) / length(nvisits) * 100 # 3% at least 30 visits
sum(nvisits < 2) / length(nvisits) * 100 # about half only 1 visit
