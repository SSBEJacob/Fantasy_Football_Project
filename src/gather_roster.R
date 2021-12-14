library(nflfastR)
#num_seasons lets you decide how many years worth of data you would like to use.
#maximum is 20 and only takes integers.
num_seasons=3
start_season=2022-num_seasons
future::plan('multisession')
rosters <- nflfastR::fast_scraper_roster(start_season:2021)
write.csv(rosters, "./data/full_roster.csv")
