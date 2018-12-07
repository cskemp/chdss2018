library(tidyverse)

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



n <-  length(cpds)
hs <-  expand.grid(replicate(n, 1:2, simplify = FALSE))
colnames(hs) <- c("kelp", "herring", "dolphin", "tuna", "sandshark", "mako", "human")
hs <- as.tibble(hs)
nH <- nrow(hs)

hs$prior = NA

for (i in 1:nH) {
  hs$prior[i] <- p_v(hs[i,])
}


livehs <- hs %>% filter(kelp==1)
livehs$prior <- livehs$prior / sum(livehs$prior)

    
