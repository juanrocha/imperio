library(tictoc)
library(deSolve)
library(tidyverse)
library(foreach)
library(doParallel)
library(fs)
library(here)
library(furrr)

# library(igraph)
# library(intergraph)
# if working outside RStudio, set working directory
setwd("/Users/juanrocha/Documents/Projects/imperio")

load("data_processed/230819_exp_design.Rda")
exp_design 
#### Pollution model ####

# This event function avoids negative levels of pollutants
# Add this directly above your call to ode()
posfun <- function(t, y, parms){
    with(as.list(y), {
        y[which(y<0)] <- 0
        return(y)
    })
}

nutrients_df <- tibble(
    time = seq(0,200,1),
    nutrients = c(seq(from = 5,to = -5, by = -0.05)) #, rep(0,100)
)

u_pollution <- approxfun(nutrients_df)

## Pollution model:
pollution <- function(t, y, params,...){
    with(as.list(c(y, params)), {
        x = y
        n = length(x) # capturing again n_size
        ## If controlling node use u_pollution, else a constant
        u <- ifelse(1:n %in% controlling_set, u_pollution(t), 0.5)
        pollutant <- u - s*x + v * (x^alpha/(z^alpha + x^alpha))
        outflow <-  (A_ij * delta_ij) %*% x
        inflow <-  t(A_ij * delta_ij) %*% x
        
        dy <- pollutant + (inflow - outflow)
        
        return(list(c(dy)))
    })
}

times <- seq(from = 0, to = 200, by = 0.01)

## run simulations and save results in parallel: First option not working
# Error in { : task 1 failed - "could not find function "tic""
cl <- makeCluster(10)
registerDoParallel(cl)

tic()
foreach(i=206:nrow(exp_design)) %dopar% {
    tictoc::tic()
    out <- deSolve::ode(
        y = exp_design$yini[[i]], times = times, func = pollution,
        parms = exp_design$params[[i]],
        method = "bdf", events = list(func = posfun, time = times)
    )
    save(out,
         file = paste0(
             "simulations/pollution_expid", exp_design$exp_id[i], # experiment id (96 experiments)
             "_rep", exp_design$exp_rep[i], "_file", # replication id (100 replicates)
             exp_design$id[i], ".Rda")) # file id is the row number, so if missing i can find it quickly
    cat("Experiment", i, "took:\n")
    tictoc::toc()
}
toc()


stopCluster(cl)


## Second option with furrr