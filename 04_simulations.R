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
foreach(i=4355:nrow(exp_design)) %dopar% {
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
toc() # 505700.343 sec elapsed; 5.85 days for half batch


stopCluster(cl)


## Second option with furrr



## check errors in delta
(exp_design$net[[1]] |> igraph::as_adjacency_matrix(sparse = FALSE) *
exp_design$delta_ij[[1]]) |> colSums()

check_mat <- function(A, delta){
    A = A |> igraph::as_adjacency_matrix(sparse = FALSE)
    any(
        colSums(A*delta) > 1,
        rowSums(A*delta) > 1
    )
}

# test
check_mat(exp_design$net[[1]], exp_design$delta_ij[[1]])

resp <- list()
tic()
resp <- map2_lgl(.x = exp_design$net, .y = exp_design$delta_ij, .f = check_mat)
toc()

sum(resp) # mumber of matrixes with delta > 1

outdegree <- map(exp_design$net, degree, mode = "out")
indegree <- map(exp_design$net, degree, mode = "in")
head(outdegree)

delta_ij <- pmap(
    list(exp_design$delta_ij,outdegree, indegree),
    function(net, out, ind){
        # I'm assuming from i to j
        net[upper.tri(net)] <- (net/out)[upper.tri(m)]
        net[lower.tri(net)] <- (net/ind)[lower.tri(m)]
        net[is.infinite(net)] <- 0
        return(net)
    })

## reset the parameters and update the delta_ij

exp_design <- exp_design |> select(-params) |> 
    #mutate(net = nets) |> # no need to repeat
    mutate(delta_ij = list(delta_ij)) |> 
    rowwise() |>
    ## seting parameters now inside the dataframe
    mutate(params = list(list(
        s = rep(2.2, n_size),    # internal loss rate (sedimentation)
        v = rep(10, n_size),     # max level of internal nutrient release
        z = rep(2.2, n_size),    # threshold
        alpha = 4,  # sharpness of the shift
        delta_ij = delta_ij,
        A_ij = net |> as_adjacency_matrix() |> as.matrix(),
        # remove NAs, they come when fvs is empty (dags)
        controlling_set = c(mds, fvs) |> na.omit() |> as.vector()
    ))
    ) |> ungroup()

base::save(exp_design, file = "data_processed/240903_exp_design.Rda")
