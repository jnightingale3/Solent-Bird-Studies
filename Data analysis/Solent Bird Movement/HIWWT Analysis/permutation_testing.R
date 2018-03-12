# Compare obersved brent movements with randomly generated movements
# based on where observers were (inferred from distributions of counts, known
# origins and known destinations for all species)


## dataset of known Brent movements between strategy sites
obs <- subset(bnet, select=c(FieldID_Origin, Destination_code, Count))


#### Simulate  random brent movements ####

# Using a loop
N <- 1000 # how many random datasets to generate
n <- nrow(obs)
ii <- 1

# start with an empty list to put results into 
res <- list(NA)

for (ii in 1:N) {
   temp <- data.frame(
    Origin = sample(known$Field, n, replace=TRUE), # sample locations from 
    Destin = sample(known$Field, n, replace=TRUE), # total list of sites, and
    Count = sample(knownbg$Count, n, replace=TRUE)) # counts from non-zero BG counts
   # remove 'movements' within same site (as in observed data set) by 
   # shuffling all the destinations
   while(any(as.character(temp$Origin)==as.character(temp$Destin))) {
     temp$Destin <- sample(temp$Destin)
   }
   # store results in list
    res[[ii]] <- temp
  
}


## generate network graphs
r.graphs <- lapply(res, FUN = function(x) {graph_from_data_frame(x, directed=F)})

# plot some random graphs
rg <- sample(1:N, 4, replace=F) %>% sort
opar <- par()
par(mfrow=c(2,2))
ip <- 1
for (ip in rg) {plot(r.graphs[[ip]], main=paste('Random network #', ip),
                     vertex.size=10, vertex.label.cex=0.6)}
par(opar)



#### descriptive stats for random networks ####


# Number of sites in network
r.Nsites <- sapply(r.graphs, FUN=function(x) {length(V(x))})


# Number of observations at each site
r.Nobs_list <- lapply(res, FUN=function(x) {
  table(with(x, c(as.character(Origin), as.character(Destin)))) %>% as.data.frame
})
r.Nobs <- do.call('rbind', r.Nobs_list)


# Number of immediate connections each site has
r.degree_list <- lapply(r.graphs, FUN=function(x) {
  data.frame(name=V(x)$name,
             degree = degree(x))
})
r.degree <- do.call('rbind', r.degree_list)


# Number of bridges between local communities each site forms
r.between_list <- lapply(r.graphs, FUN= function(x) {
  data.frame(name = as.character( V(x)$name ),
             between = betweenness(x) )
}) 
r.between <- do.call('rbind', r.between_list)


# Global clustering
r.clus_glob <- sapply(r.graphs, FUN=function(x) {transitivity(x, type='global')})



#### Look at N.obs and Nsites using only non-zero brent sites ####
nzbg <- list(NA)
ii <- 1
for (ii in 1:N) {
  temp <- data.frame(
    Origin = sample(knownbg$Field, n, replace=TRUE),
    Destin = sample(knownbg$Field, n, replace=TRUE),
    Count = sample(knownbg$Count, n, replace=TRUE)
  )
  # remove internal movements
  while(any(as.character(temp$Origin)==as.character(temp$Destin))) {
    temp$Destin <- sample(temp$Destin)
  }
  # store results in list
  nzbg[[ii]] <- temp
}

# generate network graphs
nzbg.graphs <- lapply(nzbg, FUN = function(x) {graph_from_data_frame(x, directed=F)})


# Number of sites in network
nzbg.Nsites <- sapply(nzbg.graphs, FUN=function(x) {length(V(x))})


# Number of observations at each site
nzbg.Nobs_list <- lapply(nzbg, FUN=function(x) {
  table(with(x, c(as.character(Origin), as.character(Destin)))) %>% as.data.frame
})
nzbg.Nobs <- do.call('rbind', nzbg.Nobs_list)
