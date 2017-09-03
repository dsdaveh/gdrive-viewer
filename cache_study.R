library(tidyverse)
library(googledrive)
library(tictoc)
library(ggplot2)

find_times <- tibble( n_max = integer(), time = numeric())

n_test <- 10 ^ (1:5)
n_test <- c(n_test, n_test[2:5] / 2) %>% sort()
for (n_max in n_test) { 
    tic()
    files <- drive_find(n_max = n_max)
    rec <- toc()
    find_times <- find_times %>% rbind(tibble( n_max = n_max, time = rec$toc - rec$tic))

    #replace last n_max with actual max
    if (n_max > nrow(files)) {
        find_times[nrow(find_times), 'n_max'] <- nrow(files)
        break
    }
    print(find_times)
}

saveRDS(find_times, file = 'find_times.rds')

find_times %>% ggplot(aes(n_max, time)) + geom_line() + geom_smooth(se = FALSE, span=1.2) + geom_point() 

cat(sprintf('Find rate is ~ %d sec / 1000 files\n', round(1000 * max(find_times$time) / max(find_times$n_max))))
    