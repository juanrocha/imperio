library(tidyverse)
library(fs)
library(tictoc)
library(deSolve)
library(patchwork)
library(furrr)

load("data_processed/250225_exp_design.Rda")
load("data_processed/250225_exp_design.Rda")
exp_design

#fls <- dir_ls("simulations/") |> str_subset(pattern = ".Rda")

## a function for plotting quick the networks:
# net_plot <- function(x) { #x is the net object
#     ggplot(ggnetwork::ggnetwork(x, arrow.gap = 0.005) ,
#            aes(x = x, y = y, xend = xend, yend = yend)) +
#         geom_edges(
#             size = 0.35, alpha = 1,
#             arrow = arrow(length = unit(2, "pt"), type = "closed")
#         ) +
#         geom_nodes(color = "orange", alpha = 0.4) +
#         theme_blank(base_size = 10) 
# }

# load(fls[1])
# 
# out %>% 
#     as_tibble() |> 
#     pivot_longer(`1`:last_col(), names_to = "system", values_to = "nutrients") |> 
#     mutate(time = as.numeric(time), nutrients = as.numeric(nutrients)) |>  
#     ggplot(aes(x=time, y=nutrients)) +
#     geom_line(aes(color = system), size = 0.25, show.legend = T) + 
#     theme_light()

# exp_design$net[[3]] |> 
#     igraph::plot.igraph(
#         vertex.size = 10, vertex.color = "orange", vertex.frame.color = NA,
#         edge.color = "grey75", edge.width = 0.5, edge.arrow.size = 0.5)
# 


## extract key results ##
## 1. % of systems recovered
## 2. are they the same as the control_set?
## 3. when do they recovered? is the control_set / n related to faster recovery?
# 
# plan(multisession, workers = 10)
# 
# tic()
# recovered_systems <- future_map(
#     fls,
#     function(x) {
#         load(x)
#         rec_sys <- out |> 
#             as_tibble() |> 
#             mutate(across(time:last_col(), as.numeric)) |> 
#             pivot_longer(
#                 `1`:last_col(), names_to = "system", values_to = "nutrients") |> 
#             group_by(system) |> 
#             mutate(low_nutrient = nutrients <=0.01) |> 
#             mutate(
#                 lag1 = slider::slide_dbl(
#                     nutrients, diff, .before = 1, .complete = TRUE)) |>
#             # make sure you're not picking the first dynamics towards equilibrium
#             filter(time > 5, any(low_nutrient)) |>
#             filter(lag1 == min(lag1)) |> 
#             ungroup() |> select(-low_nutrient, lag1) |> 
#             mutate(file = as.character(x))
#         
#         return(rec_sys)
#     }
# )
# toc() #1073.472 for 96 files, 51s for 6 files, 11s in parallel. | 15275.571 sec elapsed in parallel for 9600 files
# 
# head(recovered_systems)
# 
# ## because the sims were on parallel, the files were created asynchronously and they are not in order. Thus the object does not follow the id order of exp_design. 
# 
# recovered_systems <- recovered_systems |> 
#     bind_rows()
load("data_processed/pollution_processed_results.Rda")

tic()
recovered_systems <- recovered_systems |>
    separate(file, into = c("root", "exp_id", "exp_rep", "id"), sep = "_") |>
    select(-root) |>
    mutate(id = str_remove_all(id, "file|Rda"),
           exp_id = str_remove(exp_id, "expid"),
           exp_rep = str_remove(exp_rep, "rep"),
           id = str_remove(id, "[:punct:]"))
toc() # 9s

## improve labels for graphics
tic()
exp_design <-  exp_design |> 
    mutate(dag = case_when(dag == FALSE ~ "DCG", dag == TRUE ~ "DAG")) |> 
    mutate(class = case_when(
        class == "sample_gnp" ~ "random",
        class == "sample_pa" ~ "preferential attachment",
        class == "sample_ws" ~ "small-world"
    ))
toc() # 7s

## 1. % of systems recovered
a <- recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |> 
    group_by(id) |> 
    summarize(recovered = n(), mean_time = mean(time)) |> 
    right_join(exp_design |> 
                   mutate(class = as_factor(class))) |> 
    mutate(prop_recovered = recovered / n_size) |> 
    ggplot(aes(as_factor(dens), prop_recovered)) +
    geom_boxplot(aes(color = delta, fill = delta), alpha = 0.5, linewidth = 0.25, outlier.size = 1) +
    #geom_jitter(aes(color = delta, shape = dag), alpha = 0.15) +
    scale_fill_brewer(name = expression(delta), palette = "Set1") +
    scale_color_brewer(name = expression(delta), palette = "Set1") +
    labs(tag = "A", x = "Network density", y = "Proportion of nodes recovered") +
    facet_wrap(~class, scales = "free_x") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.93,0.2), legend.key.size = unit(2, "mm"))
    #facet_grid(dag~class, scales = "free_x")


# 2. are they the same as the control_set?
# extract first the control set: removes the original lists of mds and fvs, combined in cs

# plan(multisession, workers = 10)
# tic()
# exp_design <- exp_design |> #head() |>
#         unnest(mds) |> unnest(fvs) |>
#         group_by(id) |>
#         pivot_longer(
#             cols = c(mds,fvs), names_to = "set_class", values_to = "control_set") |>
#         filter(!is.na(control_set)) |> select(-set_class) |>
#         unique() |>
#         nest(cs = control_set)
# toc() #900 s ~15min

b <- recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |>
    group_by(id) |> 
    select(system) |> 
    mutate(system = as.numeric(system)) |> 
    nest(system = system) |> 
    right_join(exp_design |> 
                   mutate(class = as_factor(class))) |> 
    #head() |> 
    mutate(match = map2_dbl(
        .x = system,
        .y = cs,
        .f = function(x,y){sum(x$system %in% y$control_set)/length(x$system )}
    )) |> 
    ggplot(aes(as_factor(dens), match)) +
    geom_boxplot(aes(color = delta, fill = delta), alpha = 0.5, linewidth = 0.25, outlier.size = 1) +
    #geom_jitter(aes(color = delta, shape = dag), alpha = 0.15) +
    scale_fill_brewer(name = expression(delta), palette = "Set1") +
    scale_color_brewer(name = expression(delta), palette = "Set1") +
    labs(tag = "B", x = "Network density", y = "Proportion of nodes recovered\n that belong to control set") +
    facet_wrap(~class, scales = "free_x") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.93,0.2), legend.key.size = unit(2, "mm"))


## 3. when do they recovered? is the control_set / n related to faster recovery?
c <- recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |> 
    group_by(id) |> 
    summarize(recovered = n(), mean_time = mean(time)) |> 
    right_join(exp_design |> 
                   mutate(class = as_factor(class))) |> 
    mutate(prop_recovered = recovered / n_size) |> 
    ggplot(aes(prop_recovered, mean_time)) +
    geom_point(aes(color = delta, shape = dag), alpha = 0.5, size = 0.5) +
    facet_wrap(~class, scales = "free_x") +
    labs(tag = "C", x = "Proportion of nodes recovered", y = "Mean recovery time") +
    scale_color_brewer(name = expression(delta), palette = "Set1") +
    scale_shape(name = "Type") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.05, 0.3), legend.direction = 'vertical',
          legend.key.size = unit(2, "mm"), #legend.text = element_text(size = 4),
          legend.spacing.y = unit(0.5, "mm"), legend.background = element_rect(fill = "transparent"))
    # facet_grid(dag~class, scales = "free_x")

a/b/c

ggsave(
    plot = a/b/c,
    filename = "pollution_results.png", path = "figures/", device = "png",
    width = 3.5, height = 5, dpi = 500, bg = "white"
)

d <- recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |> 
    group_by(id) |> 
    summarize(recovered = n(), mean_time = mean(time)) |> 
    right_join(exp_design |> 
                   mutate(class = as_factor(class))) |> 
    mutate(prop_recovered = recovered / n_size) |> 
    ggplot(aes(prop_recovered, mean_time)) +
    geom_point(aes(color = henrici, shape = dag), alpha = 0.5, size = 0.5) +
    facet_grid(delta~class, scales = "free_x") +
    labs(tag = "C", x = "Proportion of nodes recovered", y = "Mean recovery time") +
    scale_color_viridis_c(name = "Henrici") +
    scale_shape(name = "Type") +
    theme_light(base_size = 10) +
    theme(legend.position = c(0.05, 0.2), legend.direction = 'vertical',
          legend.key.size = unit(2, "mm"), #legend.text = element_text(size = 4),
          legend.spacing.y = unit(0.5, "mm"), legend.background = element_rect(fill = "transparent"))
    
d
# save(recovered_systems, file = "data_processed/pollution_processed_results.Rda")

## Random notes:
## while climate is important, do not underestimate other coupling mechanisms
## structural control does not provide full power to recover 100% of the connected
## system