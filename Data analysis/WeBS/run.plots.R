# code for plotting

# this is temporary

#### Setup ####

require(rgdal) # for converting coordinate systems
require(plyr) # join
require(magrittr) # %>%
require(maptools) # needed for fortify() to work
require(ggplot2)# fancy plotting
require(ggmap) # for google maps
require(gridExtra) # multi-figure grob plots
require(tmap) # for qtm() maps - polygons with legend



#### Regression plot ####

ass %<>% subset(Site != 'Pagham Harbour')
ass$Label <- c('BE', 'BH', 'CH', 'LH', 'NH', 'NS', 'PH', 'SW') %>% factor


with(ass, plot(LTC, HTC))
lm.ass <- lm(HTC~LTC, data = ass)
abline(lm.ass)
summary(lm.ass)


p.ass <- ggplot(ass, aes(x=LTC, y=HTC, label=Label)) + 
  geom_abline(slope = 1, intercept = 0, linetype=2, color='grey') +
  geom_smooth(method='lm', fill='lightblue') + geom_point(color='green') +
  geom_text() + theme_classic() + xlab('No. birds low tide') + ylab('No. birds high tide')
print(p.ass)

# model predictions
pred.lm.ass <- data.frame(
  Site = ass$Site,
  LTC = round(ass$LTC),
  HTC = round(ass$HTC),
  phat = round(lm.ass$fitted.values),
  se = predict(lm.ass, se.fit = T)$se.fit
)
pred.lm.ass$ll <- pred.lm.ass$phat - (1.97*pred.lm.ass$se)
pred.lm.ass$ul <- pred.lm.ass$phat + (1.97*pred.lm.ass$se)
pred.lm.ass$HTCdiff <- pred.lm.ass$HTC - pred.lm.ass$phat
pred.lm.ass
write.csv(pred.lm.ass, file='model_table.csv', row.names = F)

#### Disturbance and body mass ####
ass_spp <- join(ass_spp, subset(ass, select=c(Site, Label)))

q <- ggplot(ass_spp, (aes(x=LTC, y=HTC, label=Label)))
q <- q + geom_abline(slope = 1, intercept = 0, linetype=2, color='grey')
q <- q + geom_smooth(method='lm', fill='lightblue')
q <- q + geom_point(color='green') +
  geom_text() + theme_classic() + xlab('% birds low tide') + ylab('% birds high tide')

q <- q + facet_wrap(~Species, scales='free')
print(q)

#### Maps of sectors with google maps underlay ####

solent_base <- get_googlemap(center='Fareham, UK', zoom=10, color='bw', maptype = 'roadmap')

# high tide counts
hicoord <- spTransform(core_sects,CRS("+proj=longlat"))
hicoord.f <- fortify(hicoord, region='LOC_LABEL')

map.cover <- subset(core_sects@data, select=c('LOC_LABEL', 'Coverage'))
names(map.cover)[1] <- 'id'

hicoord.f %<>% join(map.cover)
f1a <- ggmap(solent_base) + geom_polygon(
  data=hicoord.f, aes(x=long, y=lat, group=group, fill=Coverage), 
  colour='black', alpha=.75) +
  ylim(50.675, 50.95) + xlab('') + ylab('') +
  scale_fill_continuous("Number of\nsurveys", low='white', high='blue',
                        na.value='grey45') + theme(legend.position = 'left')

# low tide counts
locoord <- spTransform(low_sects,CRS("+proj=longlat"))
locoord.f <- fortify(locoord, region='NWC5')
f2a <- ggmap(solent_base) + geom_polygon(
  data=locoord.f, aes(x=long, y=lat, group=group),
  colour='black', fill='blue', alpha=.75) +
  ylim(50.675, 50.95) + xlab('') + ylab('')

grid.arrange(f1a, f2a, nrow=2)


#### Maps of sectors with legend ####

# current_style <- tmap_style("col_blind")
qtm(webs_sites, fill='Site', fill.textNA='Excluded') + 
  tm_legend(legend.outside=T, legend.outside.position='right')

qtm(low_sects, fill='Site', fill.textNA='Excluded') + 
  tm_legend(legend.outside=T, legend.outside.position='right')

qtm(webs_sites, fill='siteman', fill.textNA='Excluded') + 
  tm_legend(legend.outside=T, legend.outside.position='right')



##########################################
##### Thematic maps containing data ######
##########################################

#### total assemblage ####

# high tide
qtm(core_sects, fill="Assemblage", fill.textNA='Not counted', title = 'High tide')

# low tide
qtm(low_sects, fill="Assemblage", fill.textNA='Not counted', title = 'Low tide')


#### total assemblage ####

# high tide
core_sects@data <- within(core_sects@data, AssDensity[AssDensity>5000]<-5000)
qtm(core_sects, fill="AssDensity", fill.textNA='Not counted', title = 'High tide')

# low tide
low_sects@data <- within(low_sects@data, AssDensity[AssDensity>5000]<-5000)
qtm(low_sects, fill="AssDensity", fill.textNA='Not counted', title = 'Low tide') + 
  tm_layout(legend.position = c('left', 'center'), title.position = c('right', 'top'))


#### species thresholds ####

### national

# high tide
qtm(core_sects, fill='GB', fill.textNA='Not counted', title = 'High tide')

# low tide
qtm(low_sects, fill='GB', fill.textNA='Not counted', title = 'Low tide')



### international

# high tide
qtm(core_sects, fill='International', fill.textNA='Not counted', title = 'High tide')

# low tide
qtm(low_sects, fill='International', fill.textNA='Not counted', title = 'Low tide')


#### number of species ####

# high tide
qtm(core_sects, fill='No.Spp', fill.textNA='Not counted', title = 'High tide')

# low tide
qtm(low_sects, fill='No.Spp', fill.textNA='Not counted', title='Low tide')


#### species incidence ####
qtm(core_sects, fill='Incidence', fill.textNA='Not counted', title = 'High tide')

