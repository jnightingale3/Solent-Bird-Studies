# temporary ebird test plots
require(ggplot2)
require(ggmap)
require(magrittr)

#### visits & recording bias ####
# how many visits
length(unique(spacedate$visitid))

# how many at each site?
table(visinfo$N.vis)


# where is visited most?
subset(visinfo, N.vis>=30)

# correlation between number of visits and max. assemblage size recorded
with(visinfo, cor.test(N.vis, Max, method='spearman'))
# correlation between number of visits and total number of species recorded
with(visinfo, cor.test(N.vis, NSpp, method='spearman'))



#### Assemblages #### 

# solent_base <- get_googlemap(center='Fareham, UK', zoom=10, color='bw', maptype = 'roadmap')

p <- ggmap(solent_base) + geom_point(data=subset(spacedate, duplicated(visitid)==FALSE), 
                                     aes(x=LONGITUDE, y=LATITUDE, 
                                                         size=Assemblage, fill=Sig.ass), 
                                     shape=21) +   theme_classic() + ylim(50.675, 50.95) + 
  theme(legend.position = 'none') + xlab('') + ylab('')
print(p)

# How many?
sum(spacedate[which(duplicated(spacedate$visitid)==F),]$Sig.ass=='TRUE')

# Top counts
sub <- subset(spacedate, Sig.ass=='TRUE'& duplicated(visitid)==FALSE,
              select=c(LOCALITY, date, Assemblage))
sub$date %<>% format('%d/%m/%Y') # fix date
names(sub) <- c('Location', 'Date', 'Total.Count')
sub[order(sub$Total.Count, decreasing=T),]  #%>% write.csv('table1.csv', row.names=F)


#### Thresholds #####

## how many across each threshold
sum(visitloc$Nat.Thresh=='TRUE', na.rm=T)
sum(visitloc$Int.Thresh=='TRUE', na.rm=T)

# Table of counts

sub2 <- subset(spacedate, Nat.Thresh, 
               select=c(LOCALITY, date, species, OBSERVATION.COUNT))
sub2$date %<>% format('%d/%m/%Y')
names(sub2) <- c('Location', 'Date', 'Species', 'Count')
sub2 %<>% join(subset(thresh, select=c(Species, threshGB, threshInt)))
sub2[order(sub2$Species, sub2$Count, sub2$Location, method='radix', decreasing=
             c(F,T,F)),] #%>% write.csv('table2.csv', row.names=F)


# which site(s) have most
table(sub2$Location)

# what spp are Farlington's counts?
table(sub2$Species[sub2$Location=='Farlington Marshes LNR'])



# plot map
q <- ggmap(solent_base) + geom_point(data=visitloc,
                                     aes(x=LONGITUDE, y=LATITUDE, fill=Nat.Thresh, 
                                         size=Int.Thresh)
                                     , shape=21) + theme_classic() + 
  ylim(50.675, 50.95) + theme(legend.position='none') + xlab('') + ylab('')
print(q)



#### number of species ####

nospp <- nospp[order(nospp$V1, decreasing=T),]

r <- ggmap(solent_base) + geom_point(data=subset(nospp, V1 >= 25), 
                                     aes(x=LONGITUDE, y=LATITUDE, size=V1, fill=V1)
                                     , shape=21) + theme_classic() + 
  ylim(50.675, 50.95) + theme(legend.position='none') + xlab('') + ylab('')
print(r)

nospp[1:10,] %>% write.csv('table3.csv', row.names=F)
