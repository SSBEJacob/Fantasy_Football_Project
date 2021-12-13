#These first commands load in data that we will use to calculate each teams fantasy points.
weekly_sums_df <- read.csv("./data/filled_summary_statistics.csv")
weekly_sums_df$FPoints <-0
points <- read.csv("./data/points.csv")
value_stats<-colnames(points)
val_cols=c()

#This for loop calculates how many points each player gets for each stat
for (col in value_stats){
  val=points[1,col]
  new_col=paste(col,"_value")
  weekly_sums_df[new_col]=weekly_sums_df[col]*val
  val_cols<-append(val_cols,new_col)
}
#Finally, we sum up the points into one column and export the results
weekly_sums_df$FPoints=rowSums(weekly_sums_df[,val_cols],na.rm=TRUE)
write.csv(weekly_sums_df, "./data/weekly_fantasy_points.csv")