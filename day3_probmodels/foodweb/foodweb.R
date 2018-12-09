# Code for foodweb problem

library(tidyverse)
library(here)

# This script can carry out inference in two ways -- by enumerating the entire
# hypothesis space, or by sampling. Set the inference method here

inferencemethod <- "enumerate"
#inferencemethod <- "sample"

# specify observations here.  Unlike the number game code, here we use 1 and 2
# to indicate FALSE and TRUE respectively

# Specify that kelp does not have the disease, but mako does
obs <- list(kelp = 1, mako = 2)
obs <- list()

#-------------------------------------------------------------------------------
# Set up the structure of the Bayes net along with functions for computing the prior and likelihood

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

# function that computes the probability of hypothesis H, which specifies a
# value for each species in the foodweb 

p_h <- function(h) {
  prob = cpds$kelp[h$kelp] *
    cpds$herring[h$kelp,h$herring] *
    cpds$dolphin[h$herring,h$dolphin] *
    cpds$tuna[h$herring,h$tuna] *
    cpds$sandshark[h$herring,h$sandshark] *
    cpds$mako[h$dolphin,h$tuna,h$mako] *
    cpds$human[h$mako,h$human] 

  return(prob)
}

# Compute likelihood p(obs|h) assuming weak sampling
p_obs_given_h <- function(obs, h) {
  likelihood <- 1
  for (l in labels(obs)) {
    if (obs[l] != h[l]) {
      likelihood <-  0
    }
  }
  return(likelihood)
}

# define function for plotting generalizations across the foodweb. For today's
# session don't worry about the details here.

makeplot <- function(gen, plotname) {
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
    ggsave(here("output",plotname), plot = pic, width=5, height=2)

}

if (inferencemethod == "enumerate") {
#-------------------------------------------------------------------------------
# 1. Inference by enumerating the entire hypothesis space.

# create full hypothesis space. Remember that 1 and 2 indicate FALSE and TRUE
# respectively

n <-  length(speciesnames)
hs <-  expand.grid(replicate(n, 1:2, simplify = FALSE))
colnames(hs) <- speciesnames
hs <- as.tibble(hs)
nH <- nrow(hs)


hs$prior = NA
hs$likelihood = NA

# set up prior and likelihood

for (i in 1:nH) {
  hs$prior[i] <- p_h(hs[i,])
  hs$likelihood[i] <- p_obs_given_h(obs, hs[i,])
}

# compute posterior
hs$posterior <- hs$prior* hs$likelihood
# "normalise" the posterior so that it sums to 1
hs$posterior <- hs$posterior / sum( hs$posterior ) 

# compute posterior predictive distribution: ie generalizations for each species in the foodweb

gen <- hs[1,]
for( animalname  in speciesnames ) {
    consistentHypotheses <- as.logical(hs[[animalname]]-1)
    gen[animalname] <- sum( hs$posterior[consistentHypotheses] ) 
}

# plot generalizations across the food web 
makeplot(gen[1:n], "foodweb_enumerate.pdf")
}

if (inferencemethod == "sample") {
#-------------------------------------------------------------------------------
# 2. Inference by naive sampling

# Sample a hypothesis from the prior

sample_h <- function() {
  tfvals <- c(1,2)
  h <- list()
  h$kelp <- sample(tfvals, 1, prob=cpds$kelp)   
  h$herring <- sample(tfvals, 1, prob=cpds$herring[h$kelp,])   
  h$dolphin <- sample(tfvals, 1, prob=cpds$dolphin[h$herring,])   
  h$tuna <- sample(tfvals, 1, prob=cpds$tuna[h$herring,])   
  h$sandshark <- sample(tfvals, 1, prob=cpds$sandshark[h$herring,])   
  h$mako <- sample(tfvals, 1, prob=cpds$mako[h$dolphin,h$tuna,])   
  h$human <- sample(tfvals, 1, prob=cpds$human[h$mako,])   

  return(h)
}


nsample <- 1000
samples <- data.frame((matrix(NA, nrow = nsample, ncol = n)))
colnames(samples) <- speciesnames
samples <- as.tibble(samples)
for (i in 1:nsample) {
  samples[i,] <- sample_h()
}

samples$consistent <- NA

# compute whether each sample is consistent with the observations
for (i in 1:nsample) {
  samples$consistent[i] <- p_obs_given_h(obs, samples[i,])
}

consistentHypotheses <- as.logical(samples$consistent)
consistentsamples <- samples[consistentHypotheses,1:n]
gen <- samples[1,]
for( animalname  in speciesnames ) {
    # for each species compute the proportion of consistent samples for which
    # it takes value TRUE
    gen[animalname] <- mean( consistentsamples[[animalname]] ) - 1 
    # NB: we need to subtract 1 from the mean because we're using 1 and 2 for TRUE and FALSE instead of 0 and 1
}

makeplot(gen[1:n], "foodweb_sample.pdf")
}


