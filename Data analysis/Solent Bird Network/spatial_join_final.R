#### script to join bird survey results to survey polygons
#### uses final, clean, undissolved polygon layer

require(rgdal)
require(magrittr)
require(maptools) # for spatial rbind technique spRbind()



# plot(polys) # looks ok

# inspect
# summary(polys)


##### spatial join to solent survey data ####


# clip records from outside solent polygons area
solent_over <- solent_space[polys, ]

# polygon attrbute data for each row in the solent_over dataset
solent_polys  <- over(solent_over, polys)


# join both attributes together
solent_join <- spCbind(solent_over, solent_polys)

# save data slot separately
solent_jdata <- solent_join@data


## clean up
rm(solent_polys, solent_join)
