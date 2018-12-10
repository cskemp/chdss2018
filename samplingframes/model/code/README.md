# Model for Experiment 2 (Spheres of Sodor) from Hayes, Banner, Forrester & Navarro (in preparation). *Sampling frames and inductive inference.* Manuscript in preparation

The files in this directory are based in large part on a project repository created by Danielle Navarro:

https://github.com/djnavarro/samplingframes

- samplefromprior.R : sample category means from Gaussian process prior on category means 
- `category.bug` and `property.bug` are the JAGS model files that implement the Gaussian process model with censored sampling
- `simulations.R` runs the JAGS simulations for the experiment and saves the results to the `output` directory
- `plotsims.R` reads the results of the simulations and plots model predictions  

