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

air$Month<-factor(air$Month, ordered=T)


air=air[c('Temp_cat','Month','Ozone_cat','Solar.R','Wind')]
# this deletes any rows with missing values (which R does automatically in the glm statement) In general, you'll want to impute instead
air=air[complete.cases(air),]
model<-glm(Temp_cat~Month+Ozone_cat+Solar.R+Wind,family=binomial(link='logit'),data=air)

### CHECKING MODEL ###
pR2(model)
air$Temp_cat<-as.numeric(air$Temp_cat)-1
hoslem.test(as.numeric(air$Temp_cat), fitted(model), g=10)

probabilities <- predict(model, type = "response")
hist(probabilities)
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
table(predicted.classes)

air$logit<-log(probabilities/(1-probabilities))

#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/

# check for a more or less linear relationship between logit and continuous variables
ggplot(air, aes(logit, Solar.R))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

ggplot(air, aes(logit, Wind))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw()

#check for outliers, which exist in this case
plot(model, which = 4, id.n = 3)

model.data<-augment(model, data=air)
ggplot(model.data, aes(rownames(air), .std.resid)) + 
  geom_point(aes(color = Temp_cat), alpha = .5) +
  theme_bw()

# check fgr multicollinearity
vif(model)

### RESULTS ###

# p-values, coefficients, and odds ratios
summary(model)
anova(model, test='Chisq')

as.data.frame(round(cbind(exp(cbind(OR = coef(model), confint(model))),p_value=summary(model)$coefficients[,4]),3))

# I don't love this option, but it is a fast way to plot probabilities for each variable at mean of all other variables
library(effects)
plot(allEffects(model), ylab= 'Probability of Hot Temperature')

air=air[c('Temp_cat','Month','Ozone_cat','Solar.R','Wind')]
newdata1 <- with(air,data.frame(age = mean(Solar.R),Year= mean(Wind),each=20), Q27=rep(levels(air$Month),10),Q26_col=rep(c('American Indian','non-Indian'),20))
newdata1$rankP <- predict(model, newdata = newdata1, type = "response")
