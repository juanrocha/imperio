## Install the necessary packages when working remotely
install.packages("pak")

pak::pak(c("dplyr", "igraph", "tictoc", "deSolve", "foreach", "furrr", "future",
           "doParallel", "fs", "here"))