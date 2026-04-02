## Install the necessary packages when working remotely
install.packages("pak")

pak::pak_install(c("tidyverse", "igraph", "tictoc", "deSolve", "foreach", "furrr", "futures", "doParallel",
                   "fs", "here", ""))