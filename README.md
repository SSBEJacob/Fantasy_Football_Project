# Fantasy_Football_Project
Welcome to my Fantasy Football Project. It utilizes the nflfastr package for accessing historical data and then parses it into clean and readable summary statistics which are then fed into a fixed effects model to predict how age and position effect fantasy point production (given a ruleset).

Because of the size of the datasets being handled in this project, the data folder was not uploaded. Data will be added to the data folder while running the code in the Fantasy_Football_Report.Rmd.

# File Formats
All code for this project can be found in the src folder in the form of R scripts,. Each can be run within the Fantasy_Football_Report.Rmd file. The R scripts:
1) gather_pbp.R
2) gather_roster.R
3) aggregate_pbp.R
4) combine_stats_and_roster.R
5) calculate_FP.R

# Most Time Consuming Tasks
Some processes such as gather_pbp and aggregate_pbp will take a few minutes to run because of the amount of data being downloaded and transformed. The longest code to run is the fixed effect model in the Fantasy_Football_Report.Rmd file
