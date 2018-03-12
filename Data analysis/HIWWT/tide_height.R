### quick look at effects of tide height

# Wader records are filtered to be within 2.5 hours (150 min) of high tide

# tide data includes 2 maxima per day
# so we need to assign each record to either the 1st or second tide...
solent_wader$whichtide <- 0
solent_wader$whichtide[which(abs(solent_wader$diff1) <= 150)] <- 1
solent_wader$whichtide[which(abs(solent_wader$diff2) <= 150)] <- 2

# ...then get the time of that tide...
solent_wader$tidetime <- as.POSIXct(NA)
solent_wader$tidetime[solent_wader$whichtide==1] <- 
  solent_wader$ht1[solent_wader$whichtide==1]
solent_wader$tidetime[solent_wader$whichtide==2] <- 
  solent_wader$ht1[solent_wader$whichtide==2]

# ...and finally add the height of the associated tide to each bird record!
solent_wader$tideheight <- pmth_extma$h[match(solent_wader$tidetime, 
                                               pmth_extma$time)]



#### test linear relationships

### note - linear models are completely inappropriate here
### especially due to number of zeros - 
### consider:
###     negative binomial - mean and variance differ; contagious events
###     ZIP - Poisson with zeros added
###     hurdle - inc. probability to 'clear hurdle' -> non-zero count
###     birth process - rate of birds joining increases after 1st bird joins
### and ought to account for site identity using random effects

### need to construct far better models!


### total number of waders - strongly significant relationship
solent_wader$waders <- rowSums(solent_wader[,which(wadercol)])

with(solent_wader, cor.test(tideheight, log1p(waders), method='s'))

with(solent_wader, plot(tideheight, log1p(waders)))
abline(lm(log1p(waders) ~ tideheight, data=solent_wader))
summary(lm(log1p(waders) ~ tideheight, data=solent_wader))



#### particular species of interest

## Brent goose- no relationship
with(solent_wader, plot(tideheight, log1p(Brent.Goose..Dark.bellied.)))
abline(lm(log1p(Brent.Goose..Dark.bellied.)~ tideheight, data=solent_wader))
summary(lm(log1p(Brent.Goose..Dark.bellied.)~tideheight, data=solent_wader))

# Curlew - weak relationship
with(solent_wader, plot(tideheight, log1p(Curlew)))
abline(lm(log1p(Curlew) ~ tideheight, data=solent_wader))
summary(lm(log1p(Curlew) ~ tideheight, data=solent_wader))

# Oyc - none
with(solent_wader, plot(tideheight, log1p(Oystercatcher)))
abline(lm(log1p(Oystercatcher) ~ tideheight, data=solent_wader))
summary(lm(log1p(Oystercatcher) ~ tideheight, data=solent_wader))
