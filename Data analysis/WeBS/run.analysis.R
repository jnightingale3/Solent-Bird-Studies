# code for summary statistics

# this is temporary

require(plyr)
require(rgeos) # for gArea function - area of polygons
require(magrittr) # ceci n'est pas un pipe

targets <- read.csv('../Target_species_list.csv')
fields <- read.csv('../terrestrial_habitats.csv')

#### Areas of harbours ####

areas <- c(gArea(bea), gArea(bem), gArea(chi), gArea(lan), gArea(new), gArea(nws),
           gArea(pag), gArea(por), gArea(sou)) /1000 /1000 # square km




#### Total assemblage size - mean ####

## Low tide count
ltc %<>% join(fields)

ltc_ass <- ddply(ltc, .(Site), function(x) {sum(x$Mean.count)})
ltc_ass_field <- ddply(ltc, .(Site, fields), function(x) {sum(x$Mean.count)})

## High tide count

htc %<>% join(fields)

htc_ass_month <- ddply(htc, .(month, year, Site), function(x) {sum(x$Count)})  
# htc_ass_year <- ddply(htc, .(Site, year), function(x) sum(x$Count)) %>% na.omit
htc_ass <- ddply(htc_ass_month, .(Site), function(x) {mean(x$V1, na.rm=T)} )

htc_ass_field_month <- ddply(htc, .(month, year, Site, fields), function(x) {sum(x$Count)})  
htc_ass_field <- ddply(htc_ass_field_month, .(Site, fields), function(x) {mean(x$V1, na.rm=T)} )


## Join
names(ltc_ass)[2] <- 'LTC'
names(htc_ass)[2] <- 'HTC'
ass <- join(ltc_ass, htc_ass)
ass$Area <- areas

names(ltc_ass_field)[3] <- 'LTC'
names(htc_ass_field)[3] <- 'HTC'
ass_field <- join(ltc_ass_field, htc_ass_field) %>% na.omit

ass2 <- subset(ass_field, fields=='yes', select=c(Site, LTC, HTC, Label))


## Convert numbers to densities
ass$D_ltc <- with(ass, LTC / Area)
ass$D_htc <- with(ass, HTC / Area)


## Standardise by dividing per-harbour numbers by totals
ass$S_ltc <- with(ass, LTC / sum(LTC)) * 100
ass$S_htc <- with(ass, HTC / sum(HTC)) * 100



#### Species of interest ####

# spp_of_interest <- c('Ringed Plover', 'Curlew', 'Grey Plover',
#                      'Dunlin', 'Black-tailed Godwit', 'Redshank',
#                      'Golden Plover', 'Lapwing',
#                      'Oystercatcher', 'Turnstone', 'Knot')
body_sizes <- c(64, 885, 240, 48, 310, 120,
                220, 230, 540, 120, 140)

spp_of_interest <-fields$Species

# create subsets
sub_h <- subset(htc, Species %in% spp_of_interest)
sub_l <- subset(ltc, Species %in% spp_of_interest)


# low tide count
ltc_spp <- ddply(sub_l, .(Site, Species), function(x) {sum(x$Mean.count)})

#h igh tide count
htc_spp_month <- ddply(sub_h, .(month, year, Site, Species), function(x) {sum(x$Count)})  
htc_spp <- ddply(htc_spp_month, .(Site, Species), function(x) {mean(x$V1, na.rm=T)} 
                 ) %>% na.omit %>% droplevels


# join
names(ltc_spp)[3] <- 'LTC'; names(htc_spp)[3] <- 'HTC'
ass_spp <- join(ltc_spp, htc_spp) %>% droplevels

# lattice plot
lattice::xyplot(HTC ~ LTC | Species, data=ass_spp, scales='free', type=c('p', 'r'))
with(subset(ass_spp, Species=='Turnstone'), plot(LTC, HTC))

spp_res <- data.frame(
  Sp = spp_of_interest,
  Mass = body_sizes,
  Rsq = rep(NA, length(spp_of_interest)),
  p = rep(NA, length(spp_of_interest))
)
for (ii in 1:length(spp_of_interest)) {
  spp_res$Sp[ii] <- spp_of_interest[ii]
  spp_res$Rsq[ii] <- summary(lm(HTC~LTC, 
                                data=subset(ass_spp, Species==spp_of_interest[ii])
  ))$adj.r.squared
  spp_res$p[ii] <- summary(lm(HTC~LTC, 
                              data=subset(ass_spp, Species==spp_of_interest[ii])
  ))$coefficients[2,4]
}
with(subset(spp_res, p<0.05), plot(log(Mass), Rsq))
summary(lm(Rsq~log(Mass), data=subset(spp_res, p<0.05)))
subset(spp_res, p<0.05)
format(spp_res, scientific=F, digits=4)
## species that use terrestrial habitats show less relationship between
## high and low tide counts

#### Total assemblage size - peak ####

## just use high tide count

#subset
badspp <- c(grep('Gull', levels(websdf$Species), value=T), 
            grep('naturalised', levels(websdf$Species), value=T),
            grep('domestic', levels(websdf$Species), value=T),
            'Ruddy Duck', 'Canada Goose', 'Red-crested Pochard', 'Muscovy Duck',
            'Mandarin Duck')
pkdat <- subset(websdf, !Species %in% badspp) %>% droplevels
pkdat <- join(pkdat, htc_sites) #%>% na.omit

pk_sp_site_visit <- ddply(pkdat, .(Species, Site, year, month), 
                         function(x) {sum(x$Count)})
pk_sp_site_year <- ddply(pk_sp_site_visit, .(Species, Site, year),
                         function(x) {max(x$V1)})
pk_site_year <- ddply(pk_sp_site_year, .(Site, year), function(x) {sum(x$V1)})
pk_site <- ddply(pk_site_year, .(Site), function(x) {mean(x$V1)})
pk_site
sum(pk_site$V1)

# Can't use peak because of unequal sample sizes

# # Low tide count
# 
# ltc_ass <- ddply(ltc, .(Site), function(x) {sum(x$Peak.count)})
# 
# ## High tide count
# 
# htc_ass_month <- ddply(htc, .(month, year, Site), function(x) {sum(x$Count)})  
# # htc_ass_year <- ddply(htc, .(Site, year), function(x) sum(x$Count)) %>% na.omit
# htc_ass <- ddply(htc_ass_month, .(Site), function(x) {max(x$V1, na.rm=T)} )
# 
# ## Join
# names(ltc_ass)[2] <- 'LTC'
# names(htc_ass)[2] <- 'HTC'
# ass <- join(ltc_ass, htc_ass)
# ass$Area <- areas


############################
##### Totals by sector #####
############################
h_w <- subset(htc, Species %in% targets$Species) # high tide wader dataset
l_w <- subset(ltc, Species %in% targets$Species) # low water wader dataset

#### total assemblage ####

## high tide: 
# for compatibility with low tide dara, take the peak count per species
# then sum all of them 
sectotvis <- ddply(h_w, .(sector, Species), function(x) {max(x$Count)})
sectot <- ddply(sectotvis, .(sector), function(x) {sum(x$V1, na.rm=T)})
names(sectot) <- c('LOC_LABEL', 'Assemblage')

core_sects$Assemblage <- NULL
core_sects@data %<>% join(sectot)
core_sects$AssDensity <- core_sects$Assemblage / (core_sects$Shape_Area /1000 /1000)


## low tide
lowtot <- ddply(l_w, .(Sector.code), function(x) {sum(x$Peak.count)})
names(lowtot) <- c('NWC5', 'Assemblage')

low_sects$Assemblage <- NULL
low_sects@data %<>% join(lowtot)
low_sects$AssDensity <- low_sects$Assemblage / (gArea(low_sects, byid=T) /1000 /1000)


#### species thresholds ####
source('../threshold_comparison.R')

### national

# high tide
secnatsp <- ddply(sectotvis, .(sector), function(x) {
  data.frame(
    Species = x$Species,
    Important = funky(spp=x$Species, maxcounts=x$V1, scale='national')
  )
})
secnat <- ddply(secnatsp, .(sector), function(x) {any(x$Important, na.rm=T)} )

names(secnat) <- c('LOC_LABEL', 'GB')
core_sects$GB <- NULL
core_sects@data %<>% join(secnat)


# low tide
lownat <- ddply(l_w, .(Sector.code), function(x) {
  any( funky(spp=x$Species, maxcounts=x$Peak.count, scale='national') )
})

names(lownat) <- c('NWC5', 'GB')
low_sects$GB <- NULL
low_sects@data %<>% join(lownat)



### international

# high tide
secintsp <- ddply(sectotvis, .(sector), function(x) {
  data.frame(
    Species = x$Species,
    Important = funky(spp=x$Species, maxcounts=x$V1, scale='international')
  )
})
secint <- ddply(secintsp, .(sector), function(x) {any(x$Important, na.rm=T)} )

names(secint) <- c('LOC_LABEL', 'International')
core_sects$International <- NULL
core_sects@data %<>% join(secint)


# low tide
lowint <- ddply(l_w, .(Sector.code), function(x) {
  any( funky(spp=x$Species, maxcounts=x$Peak.count, scale='international') )
})

names(lowint) <- c('NWC5', 'International')
low_sects$International <- NULL
low_sects@data %<>% join(lowint)


#### number of species ####

# high tide
secspp <- ddply(h_w, .(sector), function(x) {
  length(unique(x$Species[x$Count > 0]))
})

names(secspp) <- c('LOC_LABEL', 'No.Spp')
core_sects$No.Spp <- NULL
core_sects@data %<>% join(secspp)


# low tide
lowspp <- ddply(l_w, .(Sector.code), function(x) {
  length(unique(x$Species[x$Peak.count > 0]))
})

names(lowspp) <- c('NWC5', 'No.Spp')
low_sects$No.Spp <- NULL
low_sects@data %<>% join(lowspp)



#### species incidence ####

# high tide
secincsp <- ddply(h_w, .(sector, Species), function(x) {
  sum(x$Count > 0) / nrow(x)
})
secinc <- ddply(secincsp, .(sector), function(x) {max(x$V1, na.rm=T)>.75})

names(secinc) <- c('LOC_LABEL', 'Incidence')
core_sects$Incidence <- NULL
core_sects@data %<>% join(secinc)


#### Low-tide Coverage ####

coverage <- ddply(websdf, .(sector), function(x) {length(unique(x$Visit))})
names(coverage) <- c('LOC_LABEL', 'Coverage')

core_sects$Coverage <- NULL
core_sects@data %<>% join(coverage)


# low tide incidence data does not exist for sectors
# only aggregated at site level, which is not interesting



##### Median numbers per WeBS sector #####

# This is used for finding 'locally important' sites in the 
# site-based study

secavepop <- ddply(h_w, .(sector, Species), function(x) {median(x$Count)})
# rename column
names(secavepop)[3] <- 'Median'

# create a subset that includes only regularly-occurring populations
secavepop_nz <- subset(secavepop, Median >= 1) %>% droplevels

# write file
# write.csv(secavepop_nz, file='Sector_median_nonzero.csv', na='', row.names = F)
