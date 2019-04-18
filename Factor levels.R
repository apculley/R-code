chick<-chickwts

levels(chick$feed)
table(chick$feed)

# change order of levels
chick$feed<-factor(chick$feed,levels=c("sunflower","linseed", "casein","horsebean","soybean","meatmeal" ))

levels(chick$feed)
table(chick$feed)

# change name of one factor level
levels(chick$feed)[levels(chick$feed)=="horsebean"] <- "broadbean"

levels(chick$feed)
table(chick$feed)

# change name of all factor levels

levels(chick$feed)<-c('Treatment 1','Treatment 2','Treatment 3','Treatment 4','Treatment 5','Treatment 6')

levels(chick$feed)
table(chick$feed)

# drop unused factor levels (factor levels that no observations were under)
chick<-droplevels(chick)