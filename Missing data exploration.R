#load data, in this case using example data from R
air<-airquality

na_counts<-apply(air, 2,function(x) sum(is.na(x)))
percent_na<-apply(air,2,function(x) sum(is.na(x))/length(x))
OnArrivalNA<-cbind(na_counts, percent_na)

#Get mean and range percent missing:
mean(OnArrivalNA[,2])
min(OnArrivalNA[,2])
max(OnArrivalNA[,2])

library(VIM)
image(is.na(air), main = "Missing Values", xlab = "Observation", ylab = "n", xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(air)), 1:nrow(air), col = "white")
axis(2,seq(0, 1, length.out = ncol(air)),names(air), col = "white", las = 2 )

#Looking at patterns in other variable for missing data, looking for mcar vs mar.  This section is optional.

can<-air[which(is.na(air$Ozone)),]
not<- air[which(!is.na(air$Ozone)),]

summary(can$Solar.R)
summary(not$Solar.R)

hist(can$Solar.R)
hist(not$Solar.R)

summary(can$Wind)
summary(not$Wind)

hist(can$Wind)
hist(not$Wind)

