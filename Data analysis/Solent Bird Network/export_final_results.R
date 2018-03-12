#### Export shapefile

require(magrittr)
require(rgdal)

# select and rename columns

# choose appropriate filename and write out

final <- subset(polys, select=c('Site_code', 'TopScore',
                         'sp_score', 'SPAscore', 'Assemblage', 'AssScore',
                         'WebsScore', 'BetweenScore', 'Days'
                         ))


names(final) <- c('Site_name', 'Overall',
                  'GB.Thresh', 'SPA.Thresh', 'TotalBirds', 'Ass.Score',
                  'WeBS.Score', 'Between', 'No.Visits'
                  )

#################################
##### Export as a shapefile #####
#################################

# need to change filename, or
# remove previous file and rename afterwards - won't overwrite
# writeOGR(final, dsn='Final results layer/', layer='Asterisk_results_final',
#          driver='ESRI Shapefile')
# 
# 
# 
# ## export various tables of results
# 
# # how many in each category?
# write.csv(table(polys$TopScore), file='Tables/Overall_score_categories_asterisk.csv', row.names=F)
# 
# ii <- cols[1]
# for (ii in cols) {
#   print(names(polys)[ii])
#   print(table(polys@data[,ii]))
# }
# 
# 
# 
# ## Venn diagram
# require(eulerr)
# fit<-euler(polys@data[,cols]%>%na.omit)
# plot(fit)


##### table for appendix
# x <- subset(polys@data, select=c(Site_code, TopScore, Days, SPAscore, Assemblage, AssScore, sp_score, WebsScore, between, degree, BetweenScore))
# names(x)<-c('Site', 'Overall', 'Visits', 'SPA Feature', 'Max. Assemblage',	'SPA Assemblage',	'Species GB Thresholds',	'WeBS Sector', 'Between', 'Degree',	'Movement Surveys')
# 
# thresh_text <- function(x) {
#   x[is.na(x)] <- 0
#   x[x<=0] <- 'Below threshold'
#   x[x==1] <- 'Secondary network'
#   x[x==2] <- 'Primary network'
#   return(x)
# }
# x$`SPA Feature` %<>% thresh_text
# x$`SPA Assemblage` %<>% thresh_text
# x$`Species GB Thresholds` %<>% thresh_text
# x$`WeBS Sector` %<>% thresh_text
# x$`Movement Surveys` %<>% thresh_text
# 
# x$Between %<>% round(1)
# x$Degree %<>% round(1)
# 
# x$Between[x$Between<0] <- NA
# x$Degree[x$Degree<0] <- 0
# 
# x$`Max. Assemblage`[is.na(x$`Max. Assemblage`)] <- 'No records'
# x$Degree[is.na(x$Between)] <- 'No records' # because otherwise degree recorded as 0 if there are no records!
# x$Between[is.na(x$Between)] <- 'No records'
# 
# 
# write.csv(x, file='Tables/All_asterisk_sites_scores.csv', row.names=F)



#### Goose reserve case study ####

# p54spp <- ddply( subset(solent_jdata, Site_code=='P54' & Count > 0), 
#                  .(Species), function(x) { data.frame(
#                    No.Recs = nrow(x),
#                    Max.Count = max(x$Count)
#                  )
#                  })
# p54spp <- p54spp[order(p54spp$No.Recs, decreasing = T),]
# p54spp
# write.csv(p54spp, file='P54spp.csv', row.names=F)
# # 
# p54bg <- subset(solent_jdata, Species=='Brent Goose (Dark-bellied)' & SPAprop>=10 & Site_code=='P54')
# write.csv(p54bg, 'P54bg.csv', row.names = F)
