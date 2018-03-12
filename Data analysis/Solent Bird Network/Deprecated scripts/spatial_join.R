#### script to join bird survey results to survey polygons
#### spatial_join_strategy.R is an updated version of this code

require(rgdal)
require(magrittr)
require(tmap)
require(spatialEco)
require(maptools) # for spatial rbind technique spRbind()


## readOGR() automatically drops null geometries
## there are over 1000 in this layer...
polys <- readOGR(dsn='../../Solent Bird Studies GIS Data', layer='Polygons')
# plot(polys) # looks ok

# inspect
summary(polys)
polys@data%>%head  # lots of 0s and NAs
# plot(polys[1:10,]) # nevertheless they are real polygons


##### spatial join to solent survey data ####

head(solent)
# check all have co-ordinates
is.na(solent$Easting)%>%sum # yes

# convert to spatial dataframe (uses sp package)
solent_space <- SpatialPointsDataFrame(coords=cbind(solent$Easting, solent$Northing),
                                       data=solent,
                                       proj4string = polys@proj4string)
# plot(polys)
# points(solent_space)

# clip records from outside solent polygons area
solent_over <- solent_space[polys, ]


# spatial aggregation of bird numbers using various summary stats
# NB this section is deprecated; use confidence_importance.R
solent_agg_sum <- aggregate(x= solent_over["count"], by=polys, FUN=sum)
solent_agg_ave <- aggregate(x= solent_over["count"], by=polys, FUN=mean)
solent_agg_std <- aggregate(x= solent_over["count"], by=polys, FUN=sd)
solent_agg_N   <- aggregate(x= solent_over["count"], by=polys, FUN=length)
solent_agg_N$count %<>% divide_by(nrow(species_list))


## Quick Thematic Maps from tmap package ##

# plot data
qtm(shp=solent_agg_sum, fill='count')
qtm(shp=solent_agg_ave, fill='count')
qtm(shp=solent_agg_std, fill='count')
qtm(shp=solent_agg_N  , fill='count', style="col_blind")
hist(solent_agg_N$count[-which.max(solent_agg_N$count)], breaks=c(0, seq(from=5, to=25, by=5), 800,1000))
# would patterns be clearer using log-transformed data?


## where is the place with loads of visits
polys@data[which.max(solent_agg_N$count),]
plot(polys, col='lightgrey')
plot(polys[which.max(solent_agg_N$count),], col='red', add=T) # ca. Titchfield


# polygon attrbute data for each row in the solent_over dataset
solent_polys  <- over(solent_over, polys)

# join both attributes together
solent_join <- spCbind(solent_over, solent_polys)

# save data slot separately
solent_jdata <- solent_join@data




######## having a look at some abundance-area relationships ######
### nothing systematic
###more brents in bigger plots - density?

# with(solent_jdata, plot(Area_m, Brent.Goose..Dark.bellied.))
# with(solent_jdata, abline(lm(Brent.Goose..Dark.bellied.~Area_m)))
# lm.brentarea <- lm(Brent.Goose..Dark.bellied.~Area_m, data=solent_jdata)
# summary(lm.brentarea)
# 
# glm.brentarea <- glm(Brent.Goose..Dark.bellied.~Area_m, data=solent_jdata,
#                      family=quasipoisson())
# summary(glm.brentarea)
# 
# species=solent_jdata$Oystercatcher
# with(solent_jdata, plot(Area_m, species))
# with(solent_jdata, abline(lm(species~Area_m)))
# glm.sparea <- glm(species~Area_m, data=solent_jdata, family=quasipoisson())
# summary(glm.sparea)
# 
# bwplot(Brent.Goose..Dark.bellied.~Habitat, data=solent_jdata)
# bwplot(Brent.Goose..Dark.bellied.~Habitat, 
#        data=subset(solent_jdata, Brent.Goose..Dark.bellied.>100))
# 
# # analysis subset
# analsub <- subset(solent_jdata, Brent.Goose..Dark.bellied.>100,
#                   select=c('Brent.Goose..Dark.bellied.', 'Habitat'))
# analsub <- subset(analsub, Habitat %in% levels(analsub$Habitat)[table(analsub$Habitat)>15])
#   droplevels
# 
# pairwise.t.test(x=analsub$Brent.Goose..Dark.bellied., g=analsub$Habitat)
# 
# bwplot(analsub$Brent.Goose..Dark.bellied. ~ analsub$Habitat)
