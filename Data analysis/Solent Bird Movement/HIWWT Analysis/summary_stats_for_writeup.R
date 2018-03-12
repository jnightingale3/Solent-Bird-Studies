# summary stats

nrow(subset(mo, Species=='DB'))
nrow(subset(mo, Species=='DB' & Oknown=='Yes' & Dknown=='No')) / 787
nrow(subset(mo, Species=='DB' & Oknown=='No' & Dknown=='Yes')) / 787
nrow(subset(mo, Species=='DB' & Oknown=='No' & Dknown=='No')) / 787
nrow(subset(mo, Species=='DB' & FieldID_Origin == Destination_code)) / 787

nrow(subset(mo, Species=='DB' & 
              ((FieldID_Origin %in% strat_codes == F & Oknown == 'Yes')| 
                 Destination_code %in% strat_codes == F & Dknown == 'Yes'))) / 787


nrow(subset(mo, (FieldID_Origin %in% strat_codes == F & Oknown == T) ))
nrow(subset(mo, (Destination_code %in% strat_codes == F & Dknown == T) ))
     

length(unique(known$Field))
length(V(dolphin))
length(V(dolphin)) / length(unique(known$Field))


#### write information to spreadsheets
write.csv(unique(known$Field)%>%as.character%>%sort, 
          file='All_visited_sites.csv', row.names = F)
unique_edges <-read.csv('all_connections.txt', sep='-', header = F, strip.white = T,
                        stringsAsFactors = F,) %>%  unique %>% extract(,c(1,3))
names(unique_edges) <- c('From', 'To')


with(unique_edges, c(From, To)) %>% unique %>% sort %>% write.csv(
  file='All_in_networks.csv', row.names=F)


no.moves <- ddply(bnet_sort, .(FieldID_Origin, Destination_code), nrow)
names(no.moves)[3] <- 'N.Movements'
ave.geese <- ddply(bnet_sort, .(FieldID_Origin, Destination_code), function(x) {
  mean(x$Count)
})
names(ave.geese)[3] <- 'Mean.Geese'
goose.stats <- join(no.moves, ave.geese, type='full')
write.csv(goose.stats, file='Connections.csv', row.names=F)


# join network stats together
All_networks <- join(between, clus_loc)
All_networks %<>% join(site_deg)
names(All_networks) <- c('Site', 'Betweenness', 'Clustering.local', 'Degree')

write.csv(All_networks, file='All_in_networks.csv', row.names = F)

# Number of visits to each site
nvis <- table(known$Field) %>% as.data.frame
names(nvis) <- c('Site', 'N.Visits')
write.csv(nvis, file='All_visited_sites.csv', row.names=F)
