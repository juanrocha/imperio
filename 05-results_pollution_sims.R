library(tidyverse)
library(fs)
library(tictoc)
library(deSolve)
library(ggnetwork)

load("data_processed/pollution_exp_design.Rda")
exp_design

fls <- dir_ls("simulations/")

## a function for plotting quick the networks:
net_plot <- function(x) { #x is the net object
    ggplot(ggnetwork::ggnetwork(x, arrow.gap = 0.005) ,
           aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_edges(
            size = 0.35, alpha = 1,
            arrow = arrow(length = unit(2, "pt"), type = "closed")
        ) +
        geom_nodes(color = "orange", alpha = 0.4) +
        theme_blank(base_size = 10) 
}

load(fls[1])

out %>% 
    as_tibble() |> 
    pivot_longer(`1`:last_col(), names_to = "system", values_to = "nutrients") |> 
    mutate(time = as.numeric(time), nutrients = as.numeric(nutrients)) |>  
    ggplot(aes(x=time, y=nutrients)) +
    geom_line(aes(color = system), size = 0.25, show.legend = T) + 
    theme_light()

exp_design$net[[3]] |> 
    igraph::plot.igraph(
        vertex.size = 10, vertex.color = "orange", vertex.frame.color = NA,
        edge.color = "grey75", edge.width = 0.5, edge.arrow.size = 0.5)



## extract key results ##
## 1. % of systems recovered
## 2. are they the same as the control_set?
## 3. when do they recovered? is the control_set / n related to faster recovery?
tic()
recovered_systems <- map(
    fls,
    function(x) {
        load(x)
        rec_sys <- out |> 
            as_tibble() |> 
            mutate(across(time:last_col(), as.numeric)) |> 
            pivot_longer(
                `1`:last_col(), names_to = "system", values_to = "nutrients") |> 
            group_by(system) |> 
            mutate(low_nutrient = nutrients <=0.01) |> 
            mutate(
                lag1 = slider::slide_dbl(
                    nutrients, diff, .before = 1, .complete = TRUE)) |>
            # make sure you're not picking the first dynamics towards equilibrium
            filter(time > 5, any(low_nutrient)) |>
            filter(lag1 == min(lag1)) |> 
            ungroup() |> select(-low_nutrient, lag1)
        
        return(rec_sys)
    }
)
toc() #1073.472

head(recovered_systems)

## because the sims were on parallel, the files were created asynchronously and they are not in order. Thus the object does not follow the id order of exp_design. 

recovered_systems <- map2(recovered_systems, fls, function(x,y) {
    x$id <- y |> str_remove("simulations/pollution") |> 
        str_remove(".Rda")
    return(x)}) |> 
    bind_rows()

recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |> 
    group_by(id) |> 
    summarize(recovered = n(), mean_time = mean(time)) |> 
    right_join(exp_design) |> 
    mutate(prop_recovered = recovered / n_size) |> 
    ggplot(aes(as_factor(dens), prop_recovered)) +
    geom_boxplot(aes(color = delta, fill = delta), alpha = 0.3) +
    geom_point(aes(color = delta, shape = dag), alpha = 0.5) +
    facet_wrap(~class, scales = "free_x")



## Random notes:
## while climate is important, do not underestimate other coupling mechanisms
## structural control does not provide full power to recover 100% of the connected
## system