library(tidyr)

#loads in the roster created by gether_roster and the data genereated by aggregate_pbp
stats <- read.csv("./data/summary_statistics.csv")
roster <- read.csv("./data/full_roster.csv")

#Calculates the players age given their birthday and the year of the currebt season
roster$birth_year <-sapply(strsplit(as.character(roster$birth_date),"-"),'[',1)
roster$birth_year <- as.numeric(roster$birth_year)
roster$age <- (roster$season-roster$birth_year)

smaller_roster <- roster[c('position','season','team','full_name','gsis_id','age','height','weight')]

smaller_roster <- dplyr::rename(smaller_roster,PlayerID=gsis_id )

#The summary statistics were all organized by PlayerID. This code looks up the PlayerID's and adds the appropriate name, team, height, weight, etc
filled_stats <- merge(smaller_roster,stats, by=c("PlayerID","season"))
filled_stats <- filled_stats[,!(names(filled_stats) %in% c("X"))]

#We then write out the more in-depth data to another sheet
write.csv(filled_stats, "./data/filled_summary_statistics.csv")