library(tidyverse)
library(tictoc)
library(future)
library(furrr)

load("data_processed/240903_exp_design.Rda")
load("data_processed/240903_exp_design-resource.Rda")
exp_design 

Henrici <- function(net){
    # Henrici departure from normality, expects the adjacency matrix
    # From Asllani et al 2018
    M_f <- net |> 
        igraph::as_adjacency_matrix() |> 
        as.matrix() |> 
        matrixcalc::frobenius.norm()
    eig_val <- net |> 
        igraph::as_adjacency_matrix() |> 
        as.matrix() |>
        eigen(only.values = TRUE) |> 
        unlist() 
    # according to help, forbenious.norm is already with exponent 2, no need to square here
    sqrt(((M_f^2) - (sum(abs(eig_val)^2))))
}

Henrici(exp_design$net[[100]])

safe_henrici <- safely(Henrici)

plan(multisession, workers = 10)

tic()
out <- future_map(
    .x = exp_design$net,
    .f = safe_henrici,
    .progress = TRUE
)
toc() # 21s, 10 workers parallel

out <- transpose(out)
is_ok <- map_lgl(out$error, is.null)
sum(is_ok) # 9600 / 9600, success!

## add the results to the experimental design, so I can retrieve them when plotting in 05-

exp_design$henrici <- unlist(out$result)

exp_design |> 
    ggplot(aes(henrici)) + 
    geom_density() +
    geom_vline(xintercept = 1)

base::save(exp_design, file = "data_processed/250225_exp_design-resource.Rda")

#### example with one network #####
## only testing to create the function
# M_f <- exp_design$net[[100]] |>
#     igraph::as_adjacency_matrix() |>
#     as.matrix() |>
#     matrixcalc::frobenius.norm()
# 
# eig_val <- exp_design$net[[100]] |>
#     igraph::as_adjacency_matrix() |>
#     as.matrix() |>
#     eigen(only.values = TRUE) |>
#     unlist()
# 
# sqrt(((M_f) - (sum(abs(eig_val^2)))))
# 
# ## zero value expected on symmetric network
# x <- matrix(c(0,1,1,0), 2,2)
# sqrt(
#     matrixcalc::frobenius.norm(x)^2 -
#         sum(abs(unlist(eigen(x, only.values = TRUE)))^2)
# )
# indeed, very close to 0, 2.10e-08
# squared is needed on first term
