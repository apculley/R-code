#load data, in this case using example data from R
air<-airquality
#add another categorical variable to the data
air$Temp_cat<-air$Temp<75

#most basic boxplot by group
boxplot(air$Ozone~air$Month)
#basic boxplot with labels
boxplot(air$Ozone~air$Month, main='Boxplot Title', xlab='Month', ylab='Ozone')



library(ggplot2)

# ggplot needs all factors to be defined as factors
air$Month<-as.factor(air$Month)

# most basic ggplot boxplot by group
ggplot(data=air, aes(y=Ozone, x=Month)) + geom_boxplot()
# make outines thicker
ggplot(air, aes(x = Month, y = Ozone))+geom_boxplot(size = .75)
# plot jittered points on top of boxplots
ggplot(qhest, aes(x = Q10_6, y = age))+geom_boxplot(size = .75) +geom_jitter(alpha = .5)
# color coded
ggplot(data=air, aes(y=Ozone, x=Month, fill=Month)) + geom_boxplot()
# choose your own colors
ggplot(data=air, aes(y=Ozone, x=Month, fill=Month)) + geom_boxplot()+
  scale_fill_manual(values=c('#f6546a','#c0d6e4','#088da5','#fa8072','#40e0d0'))
# add or change labels
ggplot(data=air, aes(y=Ozone, x=Month, fill=Month)) + geom_boxplot()+
  labs(title="Plot of Ozone across Months", y = "Ozone (ppb)") 
# change legend location
ggplot(data=air, aes(y=Ozone, x=Month, fill=Month)) + geom_boxplot()+
  theme(legend.position="bottom")
# delete legend
ggplot(data=air, aes(y=Ozone, x=Month, fill=Month)) + geom_boxplot()+
  theme(legend.position="none")
# change size of all font
ggplot(data=air, aes(y=Ozone, x=Month, fill=Month)) + geom_boxplot()+
  theme(legend.position="none",text = element_text(size = 24))
# change angle of font
ggplot(data=air, aes(y=Ozone, x=Month, fill=Month)) + geom_boxplot()+
  theme(legend.position="none",text = element_text(size = 24, angle=45))


# add in another factor
ggplot(data=air, aes(y=Ozone, x=Month, fill=Temp_cat)) + geom_boxplot()
# to change factor labels, you have to change the factor levels in the dataset
str(air$Temp_cat)
air$Temp_cat<-factor(air$Temp_cat,c('TRUE','FALSE'))
levels(air$Temp_cat)<-c('Low','High')
ggplot(data=air, aes(y=Ozone, x=Month, fill=Temp_cat)) + geom_boxplot()

# add in the number of observations per group
cts <- merge(aggregate(Ozone ~ Temp_cat + Month, air, length), 
             aggregate(Ozone ~ Temp_cat+ Month, air,median), 
             by=c("Temp_cat", "Month"))
names(cts) <- c("Temp_cat", "Month", 'count',"Ozone")
cts$count<-paste('n =',as.character(cts$count))
ggplot(data=air, aes(y=Ozone, x=Month, fill=Temp_cat)) + geom_boxplot()+
  geom_text(data = cts, aes(label=count), position=position_dodge(width=.75), vjust=-.65, colour='Black', size=3)
# vjust changes the vertical position of the text

