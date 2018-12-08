# Code for the coughing friend example

library(ggplot2)
library(tidyverse)
library(here)

h  <- c('cold', 'emphysema', 'stomach upset')
p_h <- c(0.46, 0.04, 0.4)
p_d_given_h <- c(0.4, 0.4, 0.05)

# update prior by multiplying by likelihood
p_h_given_d <- p_d_given_h * p_h

# "normalise" the posterior so that it sums to 1
p_h_given_d <- p_h_given_d / sum(p_h_given_d)


# plot prior, likelihood and posterior
prior <- tibble(h, val=p_h, dist='prior P(h)')
like  <- tibble(h, val=p_d_given_h, dist='like P(D|h)')
post  <- tibble(h, val=p_h_given_d, dist='post P(h|D)')

alld = rbind(prior, like, post)

alld <- alld %>%
    mutate(dist = factor(dist, levels=c("prior P(h)", "like P(D|h)", "post P(h|D)"))
)

pic <- alld %>%
  ggplot(aes(x=h, y = val)) +
  scale_y_continuous(lim=c(0,1)) +  
  geom_col() +
  facet_grid(dist ~ .)  +
  xlab("hypothesis")

plot(pic)

ggsave(here("output","coughingfriend.pdf"), plot = pic, width=4, height=4)

