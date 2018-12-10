library(here)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(readr)
library(purrr)
library(stringr)

load(here("output","simulations.Rdata"))

get_out <- function(mod, cond, samp) {
  out <- mod$out
  names(out)[2] <- "prediction" 
  out$sample_size<- samp
  out$condition <- cond
  return(out)
}

output <- rbind(
  get_out(sim$category_n2, "category", "small"),
  get_out(sim$property_n2, "property", "small"),  
  get_out(sim$category_n6, "category", "medium"),
  get_out(sim$property_n6, "property", "medium"),  
  get_out(sim$category_n12, "category", "large"),
  get_out(sim$property_n12, "property", "large") 
)

output<- output%>%
  mutate(sample_size = factor(sample_size, levels = c("small","medium","large"))) %>%
  # plot results only for the first seven categories along the size dimension
  filter(test <= 7)


pic2 <- output  %>%
  ggplot(aes(x = test, y = prediction, colour = condition)) +
  geom_line() +
  geom_point() +
  facet_wrap(~sample_size)
plot(pic2)

ggsave(here("output","fits_line.pdf"), plot = pic2, width = 10, height = 6)

