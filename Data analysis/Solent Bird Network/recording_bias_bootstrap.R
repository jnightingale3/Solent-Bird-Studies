## Checking for evidence of recording bias in our data

## Are there more records from sites that have more birds?

## This script takes a single random visit from each site
## and the total number of visits made to that site
## to see if more visits were made to sites that (on average) have more birds

require(magrittr)
require(plyr)

# number of bootstrep replicates
Nrep <- 1000



##### Brent Geese ######


# number of visits per brent site 
brent_nvisits <- ddply(solent_jdata, .(SITE_ID), nrow)


# store names of visited sites in a vector
multivis <- levels(droplevels(solent_jdata$SITE_ID))


# initialise empty matrix for results
bias_bg <- matrix(rep(0, nrow(brent_nvisits)*Nrep), ncol=Nrep)


### use loop for bootstrapping

ii <- 1 # initialise index

# one iteration per site
for(ii in 1:length(multivis)) {
  # sample Brent Goose counts
  res <- sample( solent_jdata$Brent.Goose..Dark.bellied.[
    # from that site only
    (solent_jdata$SITE_ID==multivis[ii]) ], 
    # replacing in order to have Nrep samples from sites visited <Nrep times
    Nrep , replace=T)
  bias_bg[ii,] <- res # save results in matrix
}


# look at strength of relationship

## Spearman's correlation coefficient
cors_bg <- apply(bias_bg, 2, function(x) {
  cor(x, brent_nvisits$V1, method='spearman')
})
# plot - is it symmetrical around 0?
hist(cors_bg) # yes
abline(v=0, col='blue')

# and the associated p-value
corp_bg <- apply(bias_bg, 2, function(x) {
  cor.test(x, brent_nvisits$V1, method='spearman')$p.value
})
hist(corp_bg) # almost all are >0.05
abline(v=0.05, col='red')
sum(corp_bg < 0.05) # only 1!



##### Waders ######

# identify columns that are counts of wader species
wadercol <- names(solent_wader) %in% waderspp


# number of visits per wader site 
wader_nvisits <- ddply(solent_wader, .(SITE_ID), nrow)

# store names of visited sites in a vector
wadsite <- levels(droplevels(solent_wader$SITE_ID))


# initialise empty matrices for results
bias_wsum <- matrix(rep(0, nrow(wader_nvisits)*Nrep), ncol=Nrep)
bias_wnsp <- matrix(rep(0, nrow(wader_nvisits)*Nrep), ncol=Nrep)


### use loop for bootstrapping

iw <- 1 # initialise index for waders

# one iteration per site
for(iw in 1:length(wadsite)) {
  
  # sample wader counts from that site only
  subsamp <- subset(solent_wader, SITE_ID==wadsite[iw])
  
  # sample Nrep number of rows from that site (with replacement)
  rowno <- sample(1:nrow(subsamp), Nrep , replace=T)
  
  # include only wader columns
  subsamp <- subsamp[rowno, wadercol]
  
  res.sum <- rowSums(subsamp) # number of individuals recorded on that visit
  res.nsp <- rowSums(subsamp > 0) # number of species recorded
  
  bias_wsum[iw,] <- res.sum # save results in matrix
  bias_wnsp[iw,] <- res.nsp
}


### look at strength of relationships

## sum

## Spearman's correlation coefficient
cors_wsum <- apply(bias_wsum, 2, function(x) {
  cor(x, wader_nvisits$V1, method='spearman')
})
# plot - is it symmetrical around 0?
hist(cors_wsum) # no, around -0.17
abline(v=0, col='blue') # not on plot

# and the associated p-value
corp_wsum <- apply(bias_wsum, 2, function(x) {
  cor.test(x, wader_nvisits$V1, method='spearman')$p.value
})
hist(corp_wsum) # almost all are >0.05
abline(v=0.05, col='red')
sum(corp_wsum < 0.05) # only 1 is not!


## number of species

## Spearman's correlation coefficient
cors_wnsp <- apply(bias_wnsp, 2, function(x) {
  cor(x, wader_nvisits$V1, method='spearman')
})
# plot - is it symmetrical around 0?
hist(cors_wnsp) # no, around -0.15
abline(v=0, col='blue') # not in plot

# and the associated p-value
corp_wnsp <- apply(bias_wnsp, 2, function(x) {
  cor.test(x, wader_nvisits$V1, method='spearman')$p.value
})
hist(corp_wnsp) # almost all are <0.05
abline(v=0.05, col='red')
sum(corp_wnsp < 0.05) # only 17 aren't!




##### cleanup #####

rm(ii, iw, multivis, wadsite, res, rowno, subsamp, res.sum, res.nsp, Nrep,
   bias_bg, bias_wnsp, bias_wsum)