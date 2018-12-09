library('rjags')
library('tidybayes')

# specify observations here
obs <- list(kelp = 1, mako = 2)
 
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

foodwebdata<- c(obs, list(
      p.kelp = zerop,
      p.herring = onep,
      p.dolphin = onep,
      p.tuna = onep,
      p.sandshark = onep,
      p.mako = twop, 
      p.human = onep))

# set up the model in JAGS
jags <- jags.model('foodweb.bug', data = foodwebdata,
                   n.chains = 4,
                   n.adapt = 100)

# actually run the model in JAGS (ie sample from the posterior P(h|obs) )
samples <- coda.samples(jags,
             c('kelp', 'herring', 'dolphin', 'tuna', 'sandshark', 'mako', 'human'),
             10000)

# for each species extract the proportion of samples for which it is TRUE. For today's session don't worry about the details of this function.

genplot <- samples %>% 
        gather_draws(kelp, herring, dolphin, tuna, sandshark, mako, human) %>%
        rename(species=.variable, gen=.value) %>%
        ungroup() %>%
        select(species, gen) %>%
        mutate(species= factor(species, levels=speciesnames)) %>%
        group_by(species) %>% 
        summarize(gen=mean(gen)-1)

pic <- genplot %>% 
      ggplot(aes(x=species, y=gen)) +
      scale_y_continuous(lim=c(0,1)) +  
      geom_col() + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      xlab("species") +
      ylab("prob of having disease") 

plot(pic)
ggsave(here("output","foodweb_jags.pdf"), plot = pic, width=5, height=2)


