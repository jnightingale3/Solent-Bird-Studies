# Compare observed networks to simulated networks

require(plyr)


#### Number of observations at each site ####

# Calculate average values for random communities
r.Nobs_ave <- ddply(r.Nobs, .(Var1), function(x) {
  data.frame(
    Mean = mean(x$Freq)
  )
})

# Compare random values with observed values
t.test(r.Nobs_ave$Mean, bg_nobs$Freq)   # Random significantly less

# Join with observed values (keeping only sites with observations)
r.Nobs_join <- join(bg_nobs, r.Nobs_ave)
# Paired test
with(r.Nobs_join, t.test(Freq, Mean, paired=T))   # Random significantly less


## CONCLUSION: Observations are clustered at a smaller subset of all sites
## Follow-up: Does this still occur if zero-sites are excluded?

# Calculate averages
nzbg.Nobs_ave <- ddply(nzbg.Nobs, .(Var1), function(x) {
  data.frame(
    Mean = mean(x$Freq)
  )
})

# compare random and observed values
t.test(nzbg.Nobs_ave$Mean, bg_nobs$Freq) # Yes, random still significantly less

# Join and conduct paired test
nzbg.Nobs_join <- join(bg_nobs, nzbg.Nobs_ave)
with(nzbg.Nobs_join, t.test(Freq, Mean, paired=T)) # No difference
with(nzbg.Nobs_join, plot(Freq, Mean)); abline(0,1,col='blue')
# In most cases, random is still significantly less; however, in this small subset
# the few sites with observed frequency 1 and random freq >1 now have more influence



#### Number of sites in graph ####
t.test(r.Nsites, mu=length(V(dolphin))) # significantly more sites in random networks


## CONCLUSION: Brents use a smaller subset of avaliable sites
## Follow-up: Does this still occur if site list is based on non-zero observations
# of brent geese?
t.test(nzbg.Nsites, mu=length(V(dolphin))) # still significantly more!


#### Degree: number of connections between sites ####

# Calculate average values for random communities
r.degree_ave <- ddply(r.degree, .(name), function(x) mean(x$degree))

# Compare random and observed values
t.test(r.degree_ave$V1, site_deg$degree) # Random significantly less


# Join with observed values
r.degree_join <- join(site_deg, r.degree_ave)
# Paired test
with(r.degree_join, t.test(degree, V1, paired=T)) # Random significantly less!


## CONCLUSION: Random sites are connected to slightly fewer other sites than
# in observed networks
## QUESTION: Do sites become more attractive to brents just because they are
# closer to other used sites, or do brents select areas with many suitable sites?



#### Betweenness: bridges between communities #####

# Ranges
range(between$between) # observed sites
range(r.between$between) # random networks

# Averages for random communities
r.between_ave <- ddply(r.between, .(name), function(x) {data.frame(
  Mean = mean(x$between),
  Median = median(x$between),
  Nobs = nrow(x)
)})

# Relationship between number of observations and median betweenness in network
with(r.between_ave, plot(Nobs, Median))
# Only sites that occur in almost all  (>80%) random networks have high
# betweenness values
# As these sites are those most likely to be sampled multiple times

t.test(r.between_ave$Mean, between$between)   # Random communities have higher values
t.test(r.between_ave$Median, between$between) # No difference 
# Because skewed by a few outliers from extremely well-sampled sites

# Join with observed data
r.between_join <- join(between, r.between_ave)
with(r.between_join, t.test(between, Mean))   # Observed significantly less
with(r.between_join, t.test(between, Median)) # Observed significantly less


plot(sort(between$between))


## CONCLUSIONS: In observed networks, more sites have non-zero between values; 
# however, those non-zero values are lower than those in random networks. This is 
# because, in random networks, there are fewer, larger network sub-communities.




#### Global clustering ####

clus_glob
range(r.clus_glob) # biggest random value is half the size!

t.test(r.clus_glob, mu=clus_glob) # Obviously random nets are significantly less

## CONCLUSION: Random networks are massively less clustered - i.e., they lack
# community structure.
