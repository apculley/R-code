#load data, in this case using example data from R
air<-airquality

air$temp_bin<-cut(air$Temp, breaks=c(-1,60,65,75, 10000), labels=c('Less than 60 degrees','60-65 degrees','65-70 degrees','More than 75 degrees'))
table(air$temp_bin)
