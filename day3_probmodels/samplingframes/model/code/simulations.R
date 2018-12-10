library(here)
require(rjags)

# Script for running the sampling frames model using JAGS
# The code will give you some warnings about unused variables when you run it -- don't worry about them

make <- function(bugfile, obs = list()) {
  
  # parameters
  settings <- function(obs) {
    
    # by default there are 10 categories, although the experiment only
    # asks about 7
    if(!exists("ncat",obs)) obs$ncat <- 10
    
    # add locations for the categories
    obs$test <- 1:obs$ncat
    
    # add the dummy variables for the positive observations
    # (plaxium = 1)
    obs$plaxium <- rep.int(1,obs$nobs)
    
    # by default the prior over base rates is symmetric 
    # dirichlet with concentration .35
    if(is.null(obs$alpha)) {
      obs$alpha <- rep.int(.35,obs$ncat)
    }
    
    # default parameters for the gaussian process
    if(!exists("sigma",obs)) obs$sigma <- .5
    if(!exists("tau",obs)) obs$tau <- 1.5
    if(!exists("rho",obs)) obs$rho <- .1
    # curves will have a mean of 0.5
    if(!exists("m",obs)) obs$m <- 0 
     
    return(obs)
  }
  
  # initialise
  model <- list()
  
  # simulation parameters
  model$opt <- list(
    burnin = 20000,
    its = 100000,
    nchains = 1,
    thin = 10
  )

  # data to be given to JAGS
  model$obs = settings(obs)
  
  # store the jags model specification as a string
  model$string <- paste0(
    readLines(bugfile), 
    collapse="\n"
  )
  
  # construct the jags model object
  model$jagsmod <- jags.model(
    file = textConnection(model$string),
    n.adapt = model$opt$burnin,
    n.chains = model$opt$nchains,
    data = model$obs
  )
  
  # draw samples
  model$samples <- jags.samples(
    model = model$jagsmod, 
    variable.names = c("category_means"), 
    n.iter = model$opt$its,
    thin = model$opt$thin
  )
  
  # add a convenient summary
  model$out <- data.frame(
    test = model$obs$test,
    category_means  = apply(model$samples$category_means, 1, mean)
  )
  
  return(model)
  
}

sim <- list()

# --- simulations for experiment 2 ---

sim$category_n2 <- make(
  bugfile = here("code","category.bug"),
  obs = list(nobs = 2, category = c(1,2))
)

sim$category_n6 <- make(
  bugfile = here("code","category.bug"),
  obs = list(nobs = 6, category = rep.int(c(1,2),3))
)

sim$category_n12 <- make(
  bugfile = here("code","category.bug"),
  obs = list(nobs = 12, category = rep.int(c(1,2),6))
)

sim$property_n2 <- make(
  bugfile = here("code","property.bug"),
  obs = list(nobs = 2, category = c(1,2))
)

sim$property_n6 <- make(
  bugfile = here("code","property.bug"),
  obs = list(nobs = 6, category = rep.int(c(1,2),3))
)

sim$property_n12 <- make(
  bugfile = here("code","property.bug"),
  obs = list(nobs = 12, category = rep.int(c(1,2),6))
)

# save the results
save(make, sim, file = here("output","simulations.Rdata"))

