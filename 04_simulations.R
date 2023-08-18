library(tictoc)
library(deSolve)
library(tidyverse)
library(doParallel)

# library(igraph)
# library(intergraph)

load("data_processed/pollution_exp_design.Rda")
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

## run simulations and save results in parallel:
registerDoParallel(cores = 8)

tic()
for (i in 1:nrow(exp_design)) {
    tic()
    out <- ode(
        y = exp_design$yini[[i]], times = times, func = pollution,
        parms = exp_design$params[[i]],
        method = "bdf", events = list(func = posfun, time = times)
    )
    save(out, file = paste0("simulations/pollution", exp_design$id[i], ".Rda"))
    cat("Experiment", i, "took:\n")
    toc()
}
toc()