library(tictoc)
library(deSolve)
library(tidyverse)
library(foreach)
library(doParallel)
library(fs)
library(here)

# library(igraph)
# library(intergraph)
# if working outside RStudio, set working directory
#setwd("/Users/juanrocha/Documents/Projects/imperio")

load("data_processed/240903_exp_design-resource.Rda")
exp_design 

# update parameters for resource model
exp_design <- exp_design |>
    select(-params, -yini) |>
    rowwise() |>
    ## seting parameters now inside the dataframe
    mutate(params = list(list(
        r = rep(1.5, n_size),    # Instrinsic growth rate
        K = rep(10, n_size),     # Carrying capacity
        q = rep(2.2, n_size),    # threshold
        beta = 4,  # sharpness of the shift
        delta_ij = delta_ij,
        A_ij = net |> igraph::as_adjacency_matrix() |> as.matrix(),
        # remove NAs, they come when fvs is empty (dags)
        controlling_set = c(mds, fvs) |> na.omit() |> as.vector()
    ))
    ) |> ungroup()

exp_design <- exp_design |>
    rowwise() |>
    mutate(yini = list(runif(n_size, 1,4.5))) # I need some systems in low regime, K = 10

times <- seq(from = 0, to = 200, by = 0.01)

# base::save(exp_design, file = "data_processed/240903_exp_design-resource.Rda")

# This event function avoids negative levels of pollutants
# Add this directly above your call to ode()
posfun <- function(t, y, parms){
    with(as.list(y), {
        y[which(y<0)] <- 0
        return(y)
    })
}

harvest_df <- tibble(
    time = seq(0,200,1),
    harvest = c(seq(from = 10,to = 0, by = -0.05)) #, rep(0,100)
)

c_harvest <- approxfun(harvest_df)


## Resource model:
resource <- function(t, y, params){
    with(as.list(c(y, params)), {
        
        n = length(y) # capturing again n_size
        ## If controlling node use u_pollution, else a constant
        c <- ifelse(1:n %in% controlling_set, c_harvest(t), 0.5)
        
        growth <- r * y * (1- y/K)  ## This should recover Lodka-Volterra
        consumption <- c *((y^beta) / ((q^beta) + (y^beta) ))
        
        immigration <-  t(A_ij *delta_ij) %*% y
        emmigration <-  (A_ij * delta_ij) %*% y
        
        dx <- growth - consumption + (immigration - emmigration)
        
        return(list(c(dx)))
    })
}

#### Test it works ####

## run simulations and save results in parallel: First option not working
cl <- makeCluster(48) # Gunvor has 48
registerDoParallel(cl)

tic()
foreach(i=1:nrow(exp_design)) %dopar% {
    out <- deSolve::ode(
        y = exp_design$yini[[i]], times = times, func = resource,
        parms = exp_design$params[[i]],
        method = "bdf", events = list(func = posfun, time = times)
    )
    
    save(out,
         file = paste0(
             "simulations/resource_expid", exp_design$exp_id[i], # experiment id (96 experiments)
             "_rep", exp_design$exp_rep[i], "_file", # replication id (100 replicates)
             exp_design$id[i], ".Rda")) # file id is the row number, so if missing i can find it quickly

}
toc()
stopCluster(cl)

# 
# df_sim <- out %>% as_tibble() %>%
#     gather(key = "patches", value = "population", 2:last_col()) |>
#     mutate(time = as.numeric(time), population = as.numeric(population))

# df_sim %>%
#     #filter(time < 5) |>
#     ggplot(aes(x=time, y=population)) +
#     geom_line(aes(color = patches), size = 0.25, show.legend = T) +
#     #labs(tag = "B") + ylim(c(0,20)) +
#     theme_light()
