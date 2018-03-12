
require(magrittr)
require(rgeos)
require(reshape2)

strat_to_poly <- gDistance(polys, stratcode, byid=T)
strat_to_poly %<>% t
strat_to_poly %<>% as.data.frame

names(strat_to_poly) <- stratcode$Site_code %>% as.character %>% make.unique
strat_to_poly$Site_code <- polys$Site_code

stp_long <- melt(strat_to_poly, id=.(Site_code))
names(stp_long) <- c('Site_code', 'Stratcode', 'Distance')

stp_overlap <- subset(stp_long, Distance == 0)

stp_overlap$between <- bet$between[match(stp_overlap$Stratcode, bet$Site_code)]
stp_overlap$degree <- bet$degree[match(stp_overlap$Stratcode, bet$Site_code)]


bet <- subset(stp_overlap, select=c(Site_code, between, degree))
