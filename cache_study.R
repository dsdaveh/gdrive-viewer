library(tidyverse)
library(googledrive)
library(tictoc)

find_times <- tibble( n_max = integer(), time = numeric())

n_test <- 10 ^ (3:5)
for (n_max in n_test) {
    tic()
    files <- drive_find(n_max = n_max)
    rec <- toc()
    find_times <- find_times %>% rbind(tibble( n_max = n_nax, time = rec$toc - rec$tic))
    print(find_times)
}
