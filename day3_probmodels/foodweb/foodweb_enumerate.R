library(tidyverse)

b <- 0.1 # base rate
t <- 0.5 # transmission rate

# set up noisy-OR CPDs

zerop <- c(1-b,b)
onep  <- structure(.Data = c(1-b,b,(1-t)*(1-b),1 - (1-t)*(1-b)), .Dim = c(2,2))
twop  <- structure(.Data = c( 1-b, 
                             (1-t)*(1-b), 
                             (1-t)*(1-b), 
                             (1-t)*(1-t)*(1-b), 
                              b, 
                              1-(1-t)*(1-b), 
                              1-(1-t)*(1-b), 
                              1-(1-t)*(1-t)*(1-b)),  .Dim = c(2,2,2))

kelp <-  1
herring <- 2
dolphin <- 3 
tuna <- 4
sandshark <- 5
mako <- 6
human <- 7

cpds <- list()
cpds[[kelp]] <- zerop
cpds[[herring]] <- onep 
cpds[[dolphin]] <- onep
cpds[[tuna]] <- onep
cpds[[sandshark]] <- onep
cpds[[mako]] <- twop
cpds[[human]] <- onep


p_vreal <- function(v) {
  prob = cpds[[kelp]][v[kelp]] *
    cpds[[herring]][v[kelp],v[herring]] *
    cpds[[dolphin]][v[herring],v[dolphin]] *
    cpds[[tuna]][v[herring],v[tuna]] *
    cpds[[sandshark]][v[herring],v[sandshark]] *
    cpds[[mako]][v[dolphin],v[tuna],v[mako]] *
    cpds[[human]][v[mako],v[human]] 

  return(prob)
}

p_v <- function(v) {
  prob = cpds[[kelp]][v[[kelp]]] 
  return(prob)
}


n = 7
hs <-  expand.grid(replicate(n, 0:1, simplify = FALSE))
colnames(hs) <- paste("V", 1:n, sep="")
hs <- as.matrix(hs)
nH <- nrow(hs)

# XXXX start here

for (i in 1:nH) {
  hs$prior <- p_v(hs[i,])
}
















