#### Export shapefile

require(rgdal)

# select and rename columns

# choose appropriate filename and write out
# 
# final <- subset(polys, select=c('TARGET_FID', 'Site_ID', 'Site_code', 'TopScore',
#                          'sp_score', 'SPAscore', 'Assemblage', 'AssScore',
#                          'WebsScore', 'BetweenScore', 'Days',
#                          'closest', 'closest_webs'))
# 
# 
# names(final) <- c('TARGET_FID', 'Site_ID', 'Strat_code', 'Overall',
#                   'GB.Thresh', 'SPA.Thresh', 'TotalBirds', 'Ass.Score',
#                   'WeBS.Score', 'Between', 'No.Visits', 
#                   'Nrest.SPA', 'Nrest.WeBS')

#################################
##### Export as a shapefile #####
#################################

final <- polys

final_ddneg <- final[which(final$DataDeficient & final$Assemblage == 0),]
final_ddpos <- final[which(final$DataDeficient & final$Assemblage > 0),]

final_network <- final[which(!final$DataDeficient),]

# need to change filename, or
# remove previous file and rename afterwards - won't overwrite
writeOGR(final_ddneg, dsn='Final results layer/', layer='HIWWT brent data deficient neg',
         driver='ESRI Shapefile')
writeOGR(final_ddpos, dsn='Final results layer/', layer='HIWWT brent data deficient pos',
         driver='ESRI Shapefile')
writeOGR(final_network, dsn='Final results layer/', layer='HIWWT brent network',
         driver='ESRI Shapefile')



# export final collate dataset containing every single record of every species
# writeOGR(solent_space, dsn='Final results layer/', layer='All_bird_records',
#          driver='ESRI Shapefile')
# write.csv(solent_space@data, file='Final results layer/All_bird.records.csv', row.names = F)
