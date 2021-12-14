library(nflfastR)
library(tidyr)
#num_seasons lets you decide how many years worth of data you would like to use.
#maximum is 20 and only takes integers.
num_seasons=3
start_season=2022-num_seasons
future::plan("multisession")
parallelPBP <-nflfastR::load_pbp(start_season:2021)

#Removes all plays that aren't from the regular season
data <- parallelPBP %>%dplyr::filter(parallelPBP$season_type %in% c("REG"))

#Writes out the usable data to a csv
write.csv(data, "./data/regular_season_pbp.csv")
