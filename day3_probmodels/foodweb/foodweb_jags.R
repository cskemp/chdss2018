library('rjags')
library('tidybayes')
 
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

# specify observations here

obs <- list(kelp=1)

foodwebdata<- c(obs, list(
      p.kelp = zerop,
      p.herring = onep,
      p.dolphin = onep,
      p.tuna = onep,
      p.sandshark = onep,
      p.mako = twop, 
      p.human = onep))


jags <- jags.model('foodweb.bug',data = foodwebdata,
                   n.chains = 4,
                   n.adapt = 100)

samples <- coda.samples(jags,
             c('kelp', 'herring', 'dolphin', 'tuna', 'sandshark', 'mako', 'human'),
             10000)


tidysamples <- samples %>% gather_draws(kelp, herring, dolphin, tuna, sandshark, mako, human)


# XXX: make barplot

pic <- tidysamples %>% 
    ggplot(aes(.value)) +
    geom_histogram(aes(y=stat(count)), bins=2) +
    facet_wrap(~.variable) + 
    xlab("variable value") +
    ylab("probability")  
    #scale_y_continuous(lim=c(0,1))   
plot(pic)


# reorder columns for plotting
# samples <- samples[,c(4,2,1,7,6,5,3)]
# png('plot_1.png')
# plot(trace=FALSE,samples, xlim=c(0,2))
# dev.off()


