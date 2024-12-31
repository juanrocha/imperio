library(tidyverse)
library(fs)
library(tictoc)
library(deSolve)
library(patchwork)
library(furrr)

load("data_processed/240903_exp_design-resource.Rda")
exp_design

load("data_processed/resource_processed_results.Rda")

# run in gunvor
# fls <- dir_ls("/home/juan/imperio/simulations/") |> str_subset(pattern = ".Rda")
fls <- dir_ls("simulations/resource/") |> str_subset(pattern = '.Rda')

## extract key results ##
## 1. % of systems recovered
## 2. are they the same as the control_set?
## 3. when do they recovered? is the control_set / n related to faster recovery?

## Run in gunvor
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
#                 `1`:last_col(), names_to = "system", values_to = "resources") |>
#             group_by(system) |>
#             mutate(high_resource = resources >=5) |>
#             mutate(
#                 lag1 = slider::slide_dbl(
#                     resources, diff, .before = 1, .complete = TRUE)) |>
#             # make sure you're not picking the first dynamics towards equilibrium
#             filter(time > 5, any(high_resource)) |>
#             filter(lag1 == max(lag1)) |>
#             ungroup() |> select(-high_resource, lag1) |>
#             mutate(file = as.character(x))
# 
#         return(rec_sys)
#     }
# )
# toc() # 56795.827 sec elapsed,15h

head(recovered_systems)

## because the sims were on parallel, the files were created asynchronously and they are not in order. Thus the object does not follow the id order of exp_design. 

# recovered_systems <- recovered_systems |>
#     bind_rows()
# 
# tic()
# recovered_systems <- recovered_systems |>
#     separate(file, into = c("root", "exp_id", "exp_rep", "id"), sep = "_") |>
#     select(-root) |>
#     mutate(id = str_remove_all(id, "file|Rda"),
#            exp_id = str_remove(exp_id, "expid"),
#            exp_rep = str_remove(exp_rep, "rep"),
#            id = str_remove(id, "[:punct:]"))
# toc() # 9s

exp_design

tic()
exp_design <-  exp_design |> 
    mutate(dag = case_when(dag == FALSE ~ "DCG", dag == TRUE ~ "DAG")) |> 
    mutate(class = case_when(
        class == "sample_gnp" ~ "random",
        class == "sample_pa" ~ "preferential attachment",
        class == "sample_ws" ~ "small-world"
    ))
toc() # 7s

## problematic cases: there are a few cases where the summary stats are not unique
## the reason is that max lag1 is zero (they are on equilibrium) for the whole
## simulation time.
probs <- recovered_systems |> 
    group_by(id, system) |> 
    summarize(n=n()) |> 
    filter(n>1) 

recovered_systems |> 
    filter(id == "4828", system == "13") |> 
    unique() |> 
    filter(lag1 > 0) # this removes the observation and solves the problem, but it is a bit too much.
# one can solve it by selecting only the first time step when the system reaches lag1 == 0
    ggplot(aes(time, resources)) + geom_line()

## 1. % of systems recovered
recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |> 
    group_by(id) |> 
    filter(lag1 > 0) |> # because I used max as selector, for some systems it is the eq.
    summarize(recovered = n(), mean_time = mean(time)) |> 
    right_join(exp_design) |> 
    mutate(prop_recovered = recovered / n_size) |>
    #filter(prop_recovered > 1)
    ggplot(aes(as_factor(dens),prop_recovered)) +
    geom_boxplot(aes(color = delta, fill = delta), alpha = 0.3) +
    geom_point(aes(color = delta, shape = dag), alpha = 0.5) +
    facet_wrap(~class, scales = "free_x")
    #facet_grid(dag~class, scales = "free_x")


a <- recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |> 
    group_by(id) |> 
    filter(lag1 > 0) |>
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
# # extract first the control set: removes the original lists of mds and fvs, combined in cs

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
    filter(lag1 > 0) |>
    select(system) |> 
    mutate(system = as.numeric(system)) |> #summarize(n = n()) |> 
    nest(system = system) |> 
    right_join(exp_design |> 
                   mutate(class = as_factor(class))) |> #ggplot(aes(n, n_size)) + geom_point()
    #head() |> 
    mutate(match = map2_dbl(
        .x = system,
        .y = cs,
        .f = function(x,y){sum(x$system %in% y$control_set)/length(x$system)}
    )) |> 
    ggplot(aes(as_factor(dens), match)) +
    geom_boxplot(aes(color = delta, fill = delta), alpha = 0.5, linewidth = 0.25, outlier.size = 1) +
    #geom_jitter(aes(color = delta, shape = dag), alpha = 0.15) +
    scale_fill_brewer(name = expression(delta), palette = "Set1") +
    scale_color_brewer(name = expression(delta), palette = "Set1") +
    labs(tag = "B", x = "Network density", y = "Proportion of nodes recovered\n that belongs to control set") +
    facet_wrap(~class, scales = "free_x") +
    theme_light(base_size = 6) +
    theme(legend.position = c(0.93,0.2), legend.key.size = unit(2, "mm"))

## 3. when do they recovered? is the control_set / n related to faster recovery?
c <- recovered_systems |> 
    mutate(id = as.numeric(id)) |> 
    arrange(id) |> 
    group_by(id) |> 
    filter(lag1 > 0) |> 
    summarize(recovered = n(), mean_time = mean(time)) |> 
    right_join(exp_design |> 
                   mutate(class = as_factor(class))) |> 
    mutate(prop_recovered = recovered / n_size) |> 
    ggplot(aes(prop_recovered, mean_time)) +
    geom_point(aes(color = delta, shape = dag), alpha = 0.5, size = 0.5) +
    facet_wrap(~class, scales = "free_x") +
    labs(tag = "C", x = "Proportion of nodes recovered", y = "Mean recovery time") +
    scale_color_brewer(name = expression(delta), palette = "Set1",
                       guide = guide_legend(title.position = "top")) +
    scale_shape(name = "Type",
                guide = guide_legend(title.position = "top")) +
    theme_light(base_size = 6) +
    theme(legend.position = "bottom", legend.direction = 'horizontal',
          legend.key.size = unit(2, "mm"), #legend.spacing = ,
          legend.spacing = unit(0.15, "mm"), legend.background = element_rect(fill = "transparent"))
# facet_grid(dag~class, scales = "free_x")
a/b/c

ggsave(
    plot = a/b/c,
    filename = "resource_results.png", path = "figures/", device = "png",
    width = 3.75, height = 5.25, dpi = 500, bg = "white"
)


exp_design |> 
    ggplot(aes(dag)) +
    geom_bar() +
    facet_wrap(~class)

## Random notes:
## while climate is important, do not underestimate other coupling mechanisms
## structural control does not provide full power to recover 100% of the connected
## system