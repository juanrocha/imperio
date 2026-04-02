library(tidyverse)
library(igraph)
library(tictoc)

load("data_processed/250225_exp_design.Rda") # when working with pollution
# load("data_processed/250225_exp_design-resource.Rda") # when working with resource

# this dataset has already computed the non-normality statistic
exp_design

# exp_design |> 
#     slice(1) |> 
#     pull(params)

## Modify the controlling set, replace for the top degree nodes but same number of nodes

size_controlling_set <- exp_design$params |> 
    map(function(x) length(x$controlling_set)) |> 
    unlist()

tic()
top_degree_set <- map2(
    .x = exp_design$net, # networks
    .y = size_controlling_set,
    .f = function(x,y){
    # x is a network
    d <- degree(x) 
    z <- order(d, decreasing = TRUE)[1:y] # select only the top up to the size of controlling set
    return(z)
})
toc() # 3.5s

### Create new sets of parameters for the different simulation rounds
### params: was created in the Rmd document 04-simulations for the original set
### params2: is created now for the degree rank idea by reviewer X
### params3: is created now to make a null set of simulations where we can control everything

tic()
exp_design <- exp_design |> 
    mutate(top_degree_set = top_degree_set) |> 
    rowwise() |>
    ## seting parameters now inside the dataframe
    mutate(params = list(list(
        s = rep(2.2, n_size),    # internal loss rate (sedimentation)
        v = rep(10, n_size),     # max level of internal nutrient release
        z = rep(2.2, n_size),    # threshold
        alpha = 4,  # sharpness of the shift
        delta_ij = delta_ij,
        A_ij = net |> as_adjacency_matrix() |> as.matrix(),
        # top nodes by degree, size == original controlling set:
        controlling_set = top_degree_set
    )) ) |> # just re-write the params according with the simulation run
    # mutate(params = list(list(
    #     s = rep(2.2, n_size),    # internal loss rate (sedimentation)
    #     v = rep(10, n_size),     # max level of internal nutrient release
    #     z = rep(2.2, n_size),    # threshold
    #     alpha = 4,  # sharpness of the shift
    #     delta_ij = delta_ij,
    #     A_ij = net |> as_adjacency_matrix() |> as.matrix(),
    #     # top nodes by degree, size == original controlling set:
    #     controlling_set = 1:n_size))
    # ) |> 
    ungroup()
toc() # 23s

rm(size_controlling_set, top_degree_set)
detach("package:igraph")


