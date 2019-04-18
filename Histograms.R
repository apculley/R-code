library(ggplot2)
#load data, in this case using example data from R
cars<-chickwts

# look at data structure
str(cars)

# Basic histograms using Rbase
hist(cars$weight)
hist(cars$weight,breaks=100)

# Basic histograms using ggplot
ggplot(cars, aes(x=weight)) + geom_histogram()
# Change the width of bins
ggplot(cars, aes(x=weight)) +  geom_histogram(binwidth=1)
# Change colors
ggplot(cars, aes(x=weight)) +  geom_histogram(color="black", fill="white")
#add a verticle line at mean
ggplot(cars, aes(x=weight)) + geom_histogram(color="black", fill="white")+
  geom_vline(aes(xintercept=mean(cars$weight, na.rm = T)),color="blue", linetype="dashed", size=1)

# Overlaid groups histogram
ggplot(cars, aes(x=weight, color=feed ,y = ..ncount..)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")

# facetwrapped, color-coded groups histograms

ggplot(cars, aes(x=weight, color=feed)) +  geom_histogram(fill="white")+facet_grid(feed~.)
ggplot(cars, aes(x=weight, color=feed)) +  geom_histogram( fill="white")+facet_grid(.~feed)
