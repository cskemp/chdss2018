library(tidyverse)


speciesnames <- c("kelp", "herring", "dolphin", "tuna", "sandshark", "mako", "human")

b <- 0.1 # base rate
t <- 0.5 # transmission rate

# set up noisy-OR CPDs

zerop <- c(1-b,b)
onep  <- structure(.Data = c(1-b,(1-t)*(1-b),b,1 - (1-t)*(1-b)), .Dim = c(2,2))
twop  <- structure(.Data = c( 1-b, 
                             (1-t)*(1-b), 
                             (1-t)*(1-b), 
                             (1-t)*(1-t)*(1-b), 
                              b, 
                              1-(1-t)*(1-b), 
                              1-(1-t)*(1-b), 
                              1-(1-t)*(1-t)*(1-b)),  .Dim = c(2,2,2))
cpds <- list( kelp=zerop, 
              herring=onep, 
              dolphin=onep,
              tuna=onep,
              sandshark=onep,
              mako=twop,
              human=onep )

# compute the probability of V, which specifies a value for each species in the foodweb 

p_v <- function(v) {
  prob = cpds$kelp[v$kelp] *
    cpds$herring[v$kelp,v$herring] *
    cpds$dolphin[v$herring,v$dolphin] *
    cpds$tuna[v$herring,v$tuna] *
    cpds$sandshark[v$herring,v$sandshark] *
    cpds$mako[v$dolphin,v$tuna,v$mako] *
    cpds$human[v$mako,v$human] 

  return(prob)
}

# Compute likelihood assuming weak sampling
p_obs_given_v <- function(obs, v) {
  likelihood <- 1
  for (l in labels(obs)) {
    if (obs[l] != v[l]) {
      likelihood <-  0
    }
  }
  return(likelihood)
}

# create full hypothesis space. Unlike the number game code, here we use 1 and 2 to indicate FALSE and TRUE respectively

n <-  length(speciesnames)
hs <-  expand.grid(replicate(n, 1:2, simplify = FALSE))
colnames(hs) <- speciesnames
hs <- as.tibble(hs)
nH <- nrow(hs)

# specify observations here

obs <- list(kelp=1, mako=2)
obs <- list(kelp=1)

hs$prior = NA
hs$likelihood = NA

for (i in 1:nH) {
  hs$prior[i] <- p_v(hs[i,])
  hs$likelihood[i] <- p_obs_given_v(obs, hs[i,])
}


# compute posterior

hs$posterior <- hs$prior* hs$likelihood
# "normalise" the posterior so that it sums to 1
hs$posterior <- hs$posterior / sum( hs$posterior ) 

# compute generalization for each animal in foodweb

gen <- hs[1,]
for( animalname  in speciesnames ) {
    consistentHypotheses <- as.logical(hs[[animalname]]-1)
    gen[animalname] <- sum( hs$posterior[consistentHypotheses] ) 
}

# define plot function
makeplot <- function(gen) {
    genplot <- gather(gen)
    genplot <- genplot %>%
       mutate(species = factor(key, levels=speciesnames), gen=value) 

    pic <- genplot %>%
       ggplot(aes(x=species, y=value)) +
       scale_y_continuous(lim=c(0,1)) +  
       geom_col() + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
       xlab("species") +
       ylab("prob of having disease") 

    plot(pic)
}

# call plot function
makeplot(gen[1:n])

#-------------------------------------------------------------------------------
# 2. Inference by naive sampling


sample_v <- function() {
  tfvals <- c(1,2)
  v <- list()
  v$kelp <- sample(tfvals, 1, prob=cpds$kelp)   
  v$herring <- sample(tfvals, 1, prob=cpds$herring[v$kelp,])   
  v$dolphin <- sample(tfvals, 1, prob=cpds$dolphin[v$herring,])   
  v$tuna <- sample(tfvals, 1, prob=cpds$tuna[v$herring,])   
  v$sandshark <- sample(tfvals, 1, prob=cpds$sandshark[v$herring,])   
  v$mako <- sample(tfvals, 1, prob=cpds$mako[v$dolphin,v$tuna,])   
  v$human <- sample(tfvals, 1, prob=cpds$human[v$mako,])   

  return(v)
}


nsample <- 1000
samples <- data.frame((matrix(NA, nrow = nsample, ncol = n)))
colnames(samples) <- speciesnames
samples <- as.tibble(samples)
for (i in 1:nsample) {
  samples[i,] <- sample_v()
}

samples$consistent <- NA

# compute whether each sample is consistent with the observations
for (i in 1:nsample) {
  samples$consistent[i] <- p_obs_given_v(obs, samples[i,])
}

consistentHypotheses <- as.logical(samples$consistent)
consistentsamples <- samples[consistentHypotheses,1:n]
gen <- samples[1,]
for( animalname  in speciesnames ) {
    # for each species compute the proportion of consistent samples for which it takes value TRUE
    gen[animalname] <- mean( consistentsamples[[animalname]] ) - 1 
    # NB: we need to subtract 1 from the mean because we're using 1 and 2 for TRUE and FALSE instead of 0 and 1

}

makeplot(gen[1:n])


