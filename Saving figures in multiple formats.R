#load data, in this case using example data from R
air<-airquality

# height and width are pixels, 
png('/home/thyme/Consulting/temp.PNG',
    height=1540, width=3080,res=300,
    bg = "white")
#basic boxplot with labels
boxplot(air$Ozone~air$Month, main='Boxplot', xlab='Month', ylab='Ozone')
dev.off()

# can do other file formats
jpeg('/home/thyme/Consulting/temp.jpeg',
    height=1540, width=3080,res=300,
    bg = "white")
#basic boxplot with labels
boxplot(air$Ozone~air$Month, main='Boxplot', xlab='Month', ylab='Ozone')
dev.off()
