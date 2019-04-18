library(ggplot2)
#load data, in this case using example data from R
air<-airquality
#add two more categorical variable to the data
air$Temp_cat<-air$Temp<85
air$Temp_cat<-factor(air$Temp_cat,c('TRUE','FALSE'))
levels(air$Temp_cat)<-c('Cold','Hot')
mean(air$Ozone, na.rm=T)
air$Ozone_cat<-air$Ozone<42
air$Ozone_cat<-factor(air$Ozone_cat,c('TRUE','FALSE'))
levels(air$Ozone_cat)<-c('Low','High')

# most basic barplot, with labels
temp<-table(air$Month)
barplot(temp, main='Barplot',ylab='Frequency',xlab='Month')

# base R grouped bar plot
counts <- table(air$Ozone_cat, air$Month)
barplot(counts, main="Ozone levels over Month",xlab="Month", col=c("darkblue","red"), legend = rownames(counts), beside=TRUE)

# base R groups with percents across months
counts <- table(air$Ozone_cat, air$Month)
temp<-round(prop.table(counts,1)*100)
temp
barplot(temp, main="Ozone levels over Month",xlab="Month", ylab='Percent', col=c("darkblue","red"), legend = rownames(counts), beside=TRUE)

# base R groups with percent high/low within each month
temp<-table(air$Ozone_cat, air$Month)
temp<-round(prop.table(temp,2)*100)
temp
barplot(temp, main='Barplot',ylab='Percents',xlab='Month',legend = rownames(temp))

### THREE WAY BARCHART AND BARCHART OPTIONS USING GGPLOT ###
# create precent high ozone percents table
temp<-ftable(air$Temp_cat,air$Month, air$Ozone_cat)
temp<-round(prop.table(temp,1)*100)
temp
temp<-as.data.frame(temp)
temp
temp<-temp[which(temp$Var3=='High'),]
temp

# three categorical variable comparison with x and y labels, but no legend title
ggplot(data=temp, aes(x=Var2, y=Freq, fill=Var1)) +geom_bar(stat="identity", position=position_dodge())+
  labs(y='Percent high ozone over time for high and low temperatures',fill=NULL,x='Month')

# add the text percent on top of bars
ggplot(data=temp, aes(x=Var2, y=Freq, fill=Var1)) +geom_bar(stat="identity", position=position_dodge())+
  labs(y='Percent high ozone over time for high and low temperatures',fill=NULL,x='Month')+
  geom_text(data = temp, aes(label=paste0(as.character(temp$Freq),'%')), position=position_dodge(width=.9), vjust=-.5)

# expand x axis to range from 0 to 100 (since percents)
ggplot(data=temp, aes(x=Var2, y=Freq, fill=Var1)) +geom_bar(stat="identity", position=position_dodge())+
  labs(y='Percent high ozone over time for high and low temperatures',fill=NULL,x='Month')+
  expand_limits(y=c(0,100))

# move legend location and change text size of all text
ggplot(data=temp, aes(x=Var2, y=Freq, fill=Var1)) +geom_bar(stat="identity", position=position_dodge())+
  labs(y='Percent high ozone over time for high and low temperatures',fill=NULL,x='Month')+
  theme(legend.position="bottom",text = element_text(size = 24))

# move legend location and change text size of all text
ggplot(data=temp, aes(x=Var2, y=Freq, fill=Var1)) +geom_bar(stat="identity", position=position_dodge())+
  labs(y='Percent high ozone over time for high and low temperatures',fill=NULL,x='Month')+
  scale_fill_manual(values=c('#088da5','#fa8072'))

