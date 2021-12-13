#Test to see if aggregate pbp gets expected results
#Do not run this test until after you have run aggregate_pbp as this test compares the csv from that file to statistics obtained from NFL.com



library(testthat)
df <- read.csv("../data/summary_statistics.csv")



TomBrady<-df[which(df$PlayerID=="00-0019596"),]
TomBrady2021<-TomBrady[which(TomBrady$season==2021),]
TomBrady2021W1<-TomBrady2021[which(TomBrady2021$week==1),]



test_that('integer',{expect_equal(TomBrady2021W1[1,'PassTD'],4)})