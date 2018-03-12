## some temporary maps to show preliminary results and coverage

require(tmap) # for Quick Thematic Maps qtm()
require(plyr)
require(magrittr)
require(rgdal)
require(rgeos)


## Define a 'traffic light' colour scheme
tl <- c('grey20', 'green', 'yellow', 'red')

# inval <- gIsValid(polys, byid = T)
# sum(!inval) # none are invalid :)

#### Sub-scores ####

# species crossing National thresholds
qtm(polys, fill='sp_score', fill.palette=tl)


# SPA designated features
qtm(polys, fill='SPAscore', fill.palette=tl)


# closest webs sector
qtm(polys, fill='WebsScore', fill.palette=c('green', 'yellow'))


# assemblage size
qtm(polys, fill='AssScore', fill.palette=tl)

# betweenness centrality
qtm(polys, fill='BetweenScore', fill.palette=tl)


# Number of visits
qtm(polys, fill='Days', fill.palette=tl)


# # site confidence
# # qtm(polys, fill="Days", fill.textNA='Not counted', title = 'Number of visit-days')
# # very different to see patterns on the above due to outliers; bodge this
# polys$Days2 <- polys$Days
# polys$Days2[which(polys$Days2 >= 10)] <- 10
# qtm(polys, fill="Days2", fill.textNA='Not counted', title = 'Number of visit-days')
# 
# ## other interest 
# # sites that are only locallly important
# polys$interest <- NA
# polys$interest <- (polys$WebsScore == 1 & 
#   polys$sp_score == 0 & polys$SPAscore == 0 & polys$AssScore == 0)
# 
# 
# table(polys$interest); summary(polys$interest)
# 
# # are the locally important sites smaller than others?
# boxplot(rank(Area_ha) ~ (interest==F | is.na(interest)), data=polys@data)
# # no!

#### Combined scores ####
table(polys$TopScore)

qtm(polys, fill='TopScore', fill.palette=tl)

# Maps for each region
qtm(polys[polys$Closest=='Chichester.and.Langstone.Harbours',], 
    fill='TopScore', fill.palette=tl)
qtm(polys[polys$closest=='Portsmouth.Harbour',], 
    fill='TopScore', fill.palette=tl)
qtm(polys[polys$closest=='Solent.and.Southampton.Water',], 
    fill='TopScore', fill.palette=tl)


# ordered table of primary network sites
subset(polys@data, TopScore=='Primary network' & BetweenScore==2, select=c('Site_code', names(polys)[cols]))[rowSums(subset(polys@data, TopScore=='Primary network' & BetweenScore==2, select=c('Site_code', names(polys)[cols]))[,2:6], na.rm=T)%>%order(decreasing=T),]