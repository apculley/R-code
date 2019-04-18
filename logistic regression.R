library(pscl)
library(ResourceSelection)
library(ggplot2)
library(broom)
library(car)
candy<-read.csv('/home/thyme/Consulting/Resources/R-Code/From Helen Reach Project/candy-data.csv')
#look at variable names
colnames(candy)
#look at data structure
str(candy)

# fix data structure issues
candy$nougat<-as.factor(candy$nougat)
candy$caramel<-as.factor(candy$caramel)
candy$pluribus<-as.factor(candy$pluribus)

#fit model
model<-glm(chocolate~nougat+caramel+sugarpercent+pricepercent+pluribus,family=binomial(link='logit'),data=candy)

### CHECKING MODEL ###
# pseudo R2
pR2(model)
# if significant, does not fit log reg function
hoslem.test(as.numeric(candy$chocolate), fitted(model), g=10)

probabilities <- predict(model, type = "response")
hist(probabilities)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
table(predicted.classes)

candy$logit<-log(probabilities/(1-probabilities))

#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

# check for a more or less linear relationship between logit and continuous variables
ggplot(candy, aes(logit, sugarpercent))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(candy, aes(logit, pricepercent))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

#check for outliers
plot(model, which = 4, id.n = 3)

model.data<-augment(model, data=candy)
ggplot(model.data, aes(rownames(candy), .std.resid)) + 
  geom_point(aes(color = chocolate), alpha = .5) +
  theme_bw()

# check for multicollinearity
vif(model)

### RESULTS ###

# p-values, coefficients, and odds ratios
summary(model)
anova(model, test='Chisq')
as.data.frame(round(cbind(exp(cbind(OR = coef(model), confint(model))),p_value=summary(model)$coefficients[,4]),3))

# I don't love this option, but it is a fast way to plot probabilities for each variable at mean of all other variables
library(effects)
plot(allEffects(model), ylab= 'Probability of Hot Temperature')

# looking at and plotting probabilities

# at mean of all continuous vars
newdata1 <- with(candy,data.frame(pricepercent = mean(pricepercent),sugarpercent=mean(sugarpercent),pluribus=c(rep('0',4),rep('1',4)),nougat=rep(c('0','1','0','1'),2),caramel=rep(c('0','0','1','1'),2)))
newdata1$rankP <- predict(model, newdata = newdata1, type = "response")
newdata1

ggplot(data=newdata1, aes(x=pluribus, y=rankP, fill=nougat)) +
  geom_bar(stat="identity", position=position_dodge())+scale_y_continuous(breaks=seq(0, 1, 0.1))+expand_limits(y=c(0,1))+  
  labs(y='Probability of containing chocolate') +facet_grid(caramel ~ .)+
  theme(legend.position="bottom")+ ggtitle("Probability of candy containing chocolate at average price ans sugar percent")


# varying one continuous var 

z=100 #number of breaks in price percent 
newdata2 <- with(candy,data.frame(pricepercent = rep(seq(from=min(candy$pricepercent),max(candy$pricepercent), length.out=z), each=8),sugarpercent=mean(sugarpercent),pluribus=c(rep('0',4),rep('1',4)),nougat=rep(c('0','1','0','1'),4),caramel=rep(c('0','0','1','1'),4)))
newdata2$rankP <- predict(model, newdata = newdata2, type = "response")
newdata2

levels(newdata2$nougat)<-c('No Nougat', 'Nougat')
levels(newdata2$caramel)<-c('No Caramel', 'Caramel')
newdata2$contents<-factor(paste(newdata2$nougat,',',newdata2$caramel))

# two plot formats
ggplot(newdata2, aes(x = pricepercent, y = rankP))+ geom_line(aes(colour = pluribus), size = 1)+facet_grid(caramel~nougat)+
  scale_y_continuous(breaks=seq(0, 1, 0.1))+expand_limits(y=c(0,1))+
  labs(y='Probability of chocolate in candy',x='Price percent')

ggplot(newdata2, aes(x = pricepercent, y = rankP))+ geom_line(aes(colour = contents), size = 1)+facet_grid(pluribus~.)+
  scale_y_continuous(breaks=seq(0, 1, 0.1))+expand_limits(y=c(0,1))+
  labs(y='Probability of chocolate in candy',x='Price percent') + 
  theme(legend.position="bottom")

