library(tidyverse)
library(patchwork)

#### Figure 1: controllability of individual RS ####
load("data_processed/individualRS_control.RData")

g1 <- df_comb %>% 
    ungroup() %>% 
    group_by(regime_shift, type) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% group_by(regime_shift) %>% 
    mutate(total = sum(n),
           regime_shift = as_factor(regime_shift)) %>% 
    mutate(regime_shift = fct_rev(regime_shift)) %>% #%>% pull(regime_shift) %>% levels()
    ggplot(aes(regime_shift, n)) +
    geom_col(aes(fill = type), position = "stack") +
    coord_flip() +
    scico::scale_fill_scico_d(
        palette = "berlin", name = "Controllability", 
        labels = c("Feedback vertex set", "Minimum vertex set"),
        guide = guide_legend(#nrow = 2,
                             title.position = 'top', 
                             keywidth = unit(2.5, "mm"),
                             keyheight = unit(2.5, "mm"))) +
    labs(y = "Number of driving nodes", x = "Regime shifts", tag = "A") +
    theme_light(base_size = 5) + theme(legend.position = "bottom")


g2 <- df_comb %>% 
    ungroup() %>% 
    group_by(regime_shift, type) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    left_join(df_stats) %>% 
    group_by(regime_shift) %>% 
    mutate(total = sum(n),
           regime_shift = as_factor(regime_shift),
           prop = n / nodes) %>% 
    mutate(regime_shift = fct_rev(regime_shift)) %>%   
    # select (regime_shift, prop) %>% summarise(prop = sum(prop)) %>%  arrange(desc(prop))
    ggplot(aes(prop, regime_shift )) +
    geom_col(aes(fill = type), position = "stack") +
    scico::scale_fill_scico_d(
        palette = "berlin", name = "Controllability", 
        labels = c("Feedback vertex set", "Minimum vertex set"),
        guide = guide_legend(#nrow = 2,
                             title.position = 'top', 
                             keywidth = unit(2.5, "mm"),
                             keyheight = unit(2.5, "mm"))) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = "Proportion of nodes", y = "", tag = "B") +
    theme_light(base_size = 5) + 
    theme(legend.position = "bottom", axis.text.y = element_blank())


## J210826: The order of factors is not working, not sure what am doing wrong. I manage to organize them but not to show the higher values on the top (it used to work with fct_reorder)
g3 <- df_comb %>% 
    ungroup() %>% 
    group_by(node, type) %>% 
    summarize(n = n(), .groups = "keep") %>% 
    pivot_wider(names_from = type, values_from = n) %>% 
    mutate(total = sum(fvs, mds, na.rm = TRUE)) %>% 
    arrange((total)) %>% 
    filter(total > 3) %>% 
    mutate(node = as_factor(node)) %>% 
    mutate(node = fct_reorder(.f = node, .x = total, .desc = FALSE) %>% fct_rev()) %>%
    #pull(node) %>% fct_inorder()
    pivot_longer(cols = c("fvs", "mds"), names_to = "type", values_to = "n") %>% 
    ggplot(aes(n, fct_inorder(node))) +
    geom_col(aes(fill = type), position = "stack") +
    scico::scale_fill_scico_d(
        palette = "berlin", name = "Controllability", 
        labels = c("Feedback vertex set", "Minimum vertex set"),
        guide = guide_legend(#nrow = 2,
                             title.position = 'top', 
                             keywidth = unit(2.5, "mm"),
                             keyheight = unit(2.5, "mm"))) +
    labs(x = "Number of regime shifts", y = "Control variables", tag = "C") +
    theme_light(base_size = 5) + theme(legend.position = "bottom")

(g1 + g2 + g3) / guide_area() + plot_layout(guides = "collect", heights = c(3,0.5)) & theme(
    legend.text = element_text(size = 6, hjust = 0.5), 
    legend.title = element_text(size = 8, hjust = 0.5),
    plot.tag = element_text(size =6))


ggsave(
    filename = "fig1_control-individua-rs.png",
    path = "paper/figures/",
    plot = last_plot(),
    device = "png",
    width = 7, height = 3,
    units = "in", dpi = 450,
    bg = "white"
)


#### Figure 2: controllability in coupled RS ####
load("data_processed/cascading_control.RData")

p1 <- g2$data %>% 
    ggplot(aes(prop, regime_shift )) +
    geom_col(aes(fill = type), position = "stack") +
    scico::scale_fill_scico_d(
        palette = "berlin", name = "Controllability", 
        labels = c("Feedback vertex set", "Minimum vertex set"),
        guide = guide_legend(nrow = 2,
            title.position = 'top', 
            keywidth = unit(2.5, "mm"),
            keyheight = unit(2.5, "mm"))) +
    scale_x_reverse(labels = scales::percent, limits = c(0.9,NA)) +
    scale_y_discrete(position = "right") +
    labs(x = "", y = "", tag = "A") +
    theme_light(base_size = 5) + 
    theme(legend.position = "inside",
        legend.position.inside = c(0.1, 0.8), 
          axis.text.y.right = element_text(hjust = 1))

# this graph has 
p2 <- df_comb %>%
    select(-n) %>%
    group_by(couple) %>%
    add_count() %>% ungroup() %>%
    separate(couple, into = c("rs1", "rs2"), sep = "_")  %>%
    nest(cols = c(node, type)) %>%
    arrange(rs1, rs2) %>%
    mutate(rs1 = as_factor(rs1)  ,
           rs2 = as_factor(rs2) %>% fct_rev() ) %>%
    ggplot(aes(rs1, rs2)) +
    geom_tile(aes(fill = n)) +
    scico::scale_fill_scico(
        name = "Number of nodes", palette = "roma", direction = -1,
        guide = guide_colorbar(
            title.hjust = 0.5, direction = "horizontal",
            title.position = "top", barheight = unit(2.5, "mm"),
            barwidth = unit(2, "cm"))) +
    labs(x= "", y = "", tag = "B") +
    theme_light(base_size = 5) +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5),
          axis.text.y = element_blank(),
          legend.position = "inside", legend.position.inside = c(0.75,0.9))

## this is proportion, p2 is number of nodes
p3 <- df_stats %>% 
    mutate(rs1 = fct_rev(rs1)) %>% 
    ggplot(aes(rs1, rs2)) +
    geom_tile(aes(fill = prop)) +
    scico::scale_fill_scico(
        name = "Ratio of nodes", palette = "vikO", direction = 1,
        guide = guide_colorbar(
            title.hjust = 0.5, direction = "horizontal",
            title.position = "top", barheight = unit(2.5, "mm"), 
            barwidth = unit(2, "cm"))) +
    labs(x= "", y = "", tag = "B") +
    theme_light(base_size = 5) +
    theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0.5),
          axis.text.y = element_blank(), 
          legend.position = "inside", legend.position.inside = c(0.75,0.9))


p4 <- df_comb %>% 
    ungroup() %>% group_by(node) %>% 
    select(node) %>% 
    summarise(n = n()) %>% 
    arrange(desc(n)) %>% 
    #filter(n>100) %>% 
    slice_max(order_by = n, n = 30) %>% 
    mutate(node = as_factor(node),
           node = fct_reorder(node, n, sort)) %>% 
    ggplot(aes(node, n)) +
    geom_col() +
    coord_flip() +
    labs(x = "Most common driver nodes", y = "Regime shifts pair-wise\n combinations", tag = "C")+
    theme_light(base_size = 5)


layout <- "
ABBCCD
ABBCCD
ABBCCD
ABBCCD
#####D
"
# layout <- "
# ABBC
# ABBC
# ABBC
# ABBC
# "

p1 + p2 + p3 + p4 + 
    plot_layout(design = layout, heights = c(2,2,2,2, 0.1))

ggsave(
    filename = "draft_combined-control.png",
    path = "paper/figures/",
    plot = last_plot(),
    device = "png",
    width = 7, height = 3.5,
    dpi = 500, units = "in",
    bg = "white"
)
    