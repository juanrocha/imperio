library(tidyverse)
library(fs)

fls <- dir_ls("simulations/")
load("data_processed/230819_exp_design.Rda")
exp_design 

## Aim: from each file get the following statistics
## - 