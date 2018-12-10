library(here)
library(MASS)

# consider 7 spheres of different sizes
ncat <- 7

# suppose that the 7 spheres are equally spaced along the size dimension
test <- 1:ncat

# parameter specifying mean of the Gaussian process 
m <- 0
    
# parameters specifying covariance of the Gaussian process
sigma <- .5
tau <- 1.5
rho <- .1

mean_gp <- rep(NA, ncat)
cov_gp <- array(NA, c(ncat, ncat))

# mean and covariance matrix defining the Gaussian process
for(i in 1:ncat) {
  mean_gp[i] <- m
  cov_gp[i,i] <- (sigma^2) + (tau^2)
  if (i<7) {
    for(j in (i+1):ncat) {
      cov_gp[i,j] <- (tau^2) * exp(-rho * (test[i] - test[j])^2)
      cov_gp[j,i] <- cov_gp[i,j]
    }
  }
}

# sample functions from the Gaussian process
nsample <- 6
samples <-  mvrnorm(n=nsample,mean_gp, cov_gp)

# define logit transformation which maps real numbers to probabilities
logit <- function(x) {
 return(1/(1+exp(-x)))
}

# apply the logit transformation to the samples
category_means <- apply(samples, 1:2, logit)

# plot the samples

category_means <- as.tibble(category_means) %>%
    set_names(c("1", "2", "3", "4", "5", "6", "7"))
category_means$samplenum <- paste("sample", 1:nsample)

tidymeans <- gather(category_means, cat, value, -samplenum)

pic <- tidymeans %>%
  ggplot(aes(x = cat, y = value, group=1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~samplenum) +
  scale_y_continuous(lim=c(0,1)) +
  xlab("sizes") +
  ylab("prob of plaxium coating")
    
plot(pic)

