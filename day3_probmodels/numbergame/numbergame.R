# todo: use crossing rather than expand.grid


# Code for the number game

library(ggplot2)
library(tidyverse)

# make list (PG) that includes
#   - posterior computed by combining likelihood and prior
#   - generalizations that emerge from this posterior

make_pg<- function(obs, hs, prior, likelihood, priorname, likelihoodname) {

  pg <- list()

  posterior <- prior # initialise the posterior at the prior
  for( o in obs ) { # loop over the observations
    posterior <- posterior * likelihood[,o] # update the posterior 
  }
  
  # "normalise" the posterior so that it sums to 1
  posterior <- posterior / sum( posterior ) 
  pg$p <- posterior

  # now compute the probability that a particular value will turn out to belong   # to the true concept
  n = ncol(likelihood)
  gen <- vector( length=n )
  for( v in 1:n ) {
    consistentHypotheses <- as.logical(hs[,v])
    gen[v] <- sum( posterior[consistentHypotheses] ) 
  }

  # create tibble for subsequent plotting
  x = 1:n
  g <- tibble(x=x)
  g$gen<- gen
  g$prior <- priorname
  g$likelihood <- likelihoodname 
  pg$g <- g

  return(pg)
}

numbergame <- function( obs, plot=TRUE ) {

  n <-  12

  # set up data frame with all possible hypotheses

  hs_all <-  expand.grid(replicate(n, 0:1, simplify = FALSE))
  colnames(hs_all) <- paste("V", 1:n, sep="")
  # drop the empty hypothesis
  hs_all <- hs_all[-1,]
  browser()

  # number of hypotheses
  nH <- nrow(hs_all)
  
  # specify a uniform prior distribution over the full hypothesis space
  prior <- rep.int( 1/nH, nH )
  hs_all$uprior <- prior
  
  # set up data frame with mathematical hypotheses

  hs_math_ex <- list( c(1,2,4,8), c(2,4,6,8,10,12), c(3,6,9,12), c(4,8,12), c(5,10), c(6,12), c(1,4,9), c(1,8) )
  
  nH_math = length(hs_math_ex)
  hs_math <- matrix(0, nH_math,n)
  for (i in 1:length(hs_math_ex)) {
    extension <- hs_math_ex[[i]]
    hs_math[i,extension] <- 1
  }
  hs_math <- as.data.frame(hs_math)

  prior_math <- rep.int( 1/nH_math, nH_math )

  hs_math$mprior <- prior_math

  # combine hs_all and hs_math

  hs <- merge(hs_all, hs_math, all=TRUE)
  hs[is.na(hs)] <- 0

  # relative weight for mathematical hypothesis space compared to full 
  # hypothesis space
  w_math <- 10
  hs$cprior <- hs$mprior + w_math * hs$mprior
  hs$cprior <- hs$cprior / sum(hs$cprior)

  # matrix containing likelihoods for all possible values between 1 and n, 
  # as specified by all nH hypotheses

  sizes <- rowSums(hs[,1:n])

  # strong sampling likelihood
  likelihood_strong <- hs[,1:n]/sizes
  # weak sampling likelihood
  likelihood_weak  <- hs[,1:n] 

  allpg <- list()

  allpg$u_strong <- make_pg(obs, hs[,1:n], hs$uprior, likelihood_strong, "uniform", "strong")
  allpg$u_weak <- make_pg(obs, hs[,1:n], hs$uprior, likelihood_weak, "uniform", "weak")
  allpg$m_strong <- make_pg(obs, hs[,1:n], hs$mprior, likelihood_strong, "maths", "strong")
  allpg$m_weak <- make_pg(obs, hs[,1:n], hs$mprior, likelihood_weak, "maths", "weak")

  output <- rbind(
    allpg$u_strong$g,
    allpg$u_weak$g,
    allpg$m_strong$g,
    allpg$m_weak$g
  )

  if( plot==TRUE ) {
    pic <- output  %>%
        ggplot(aes(x = x, y = gen)) +
        geom_col() +
        scale_x_continuous(breaks=1:n) +  
        facet_wrap(vars(likelihood,prior)) +
        xlab("number") +
        ylab("generalization") 

    plot(pic)
  }

  return(allpg)
}

numbergame(c(4,8,12))
