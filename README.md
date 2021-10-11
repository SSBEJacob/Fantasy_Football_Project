# Fantasy_Football_Project
Welcome to my Fantasy Football Project. It utilizes the nflfastr package for accessing historical data and then parses it into clean and readable summary statistics which are then fed into a fixed effects model to predict how age and position effect fantasy point production (given a ruleset).

Because of the size of the datasets being handled in this project, the data folder was not uploaded. Data will be added to the data folder while running the code in the src folder.

# File Formats
All code that needs to be run for this project can be found in the src folder, is of the format Rmd and should be run in the following order:
1) gather_pbp.Rmd
2) gather_roster.Rmd
3) aggregate_pbp.Rmd
4) combine_stats_and_roster.Rmd
5) calculate_FP.Rmd
6) model_fantasy_points.Rmd

# Most Time Consuming Tasks
There are two processes that will take longer than any others. The first is aggragating the pbp data. The second is calculating the fantasy points. 
