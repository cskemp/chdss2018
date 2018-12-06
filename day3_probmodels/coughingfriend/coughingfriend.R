# Code for the coughing friend example

library(ggplot2)
library(tidyverse)

h  <- c('cold', 'emphysema', 'stomach upset')
p_h <- c(0.46, 0.04, 0.4)
p_d_given_h <- c(0.5, 0.5, 0.05)

# update prior by multiplying by likelihood
p_h_given_d <- p_d_given_h * p_h

# "normalise" the posterior so that it sums to 1
p_h_given_d <- p_h_given_d / sum(p_h_given_d)


# plot prior, likelihood and posterior

prior <- tibble(h, val=p_h, dist='prior')
like  <- tibble(h, val=p_d_given_h, dist='like')
post  <- tibble(h, val=p_h_given_d, dist='post')

alld = rbind(prior, like, post)

alld <- alld %>%
    mutate(dist = factor(dist, levels=c("prior", "like", "post"))
)

pic <- alld %>%
  ggplot(aes(x=h, y = val)) +
  scale_y_continuous(lim=c(0,1)) +  
  geom_col() +
  facet_grid(dist ~ .)  +
  xlab("hypothesis")

plot(pic)
