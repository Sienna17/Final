load_dataset<-function(){
  d<-read.csv("../Data/nfl_stats/QBStats_all.csv",header=T,stringsAsFactors = FALSE)
}
#install.packages("dummies")
library(dummies)
library(ggplot2)


