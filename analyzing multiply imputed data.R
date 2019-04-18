library(mice)
library(car)
# load data, and run basic imputation, just to creat example imputed dataset.  
# Can save image as .RData file so can load instead of re-running imputation every time.
air<-airquality
imp<-mice(air, m=20, maxit = 20, seed = 196652)
colnames(air)

# runs linear regression on all imputed data sets and combines results
model1 <- with(imp,lm(Ozone~Month + Solar.R+ Wind+Temp))
# most output similar to lm output, differences are: nmis = number of obs missing, fmi = 'proportion of variability that is attributable
# to the uncertainty caused by the missing data' (MICE in R doc), lambda = 'proportion of total variance attributable to the missing data'
round(summary(pool(model1)),3)
s<-as.data.frame(summary(pool(model1)))
round(cbind(exp(s[c('est','lo 95','hi 95')]),s['lambda'],s['t'],s['Pr(>|t|)']),3)

# pull out a sample of individual models and check assumptions (I just did the first imputed data set as an example)
imp1<-complete(imp,1)
model<-lm(Ozone~Month + Solar.R+ Wind+Temp, data=imp1)
summary(model)
plot(model)
vif(model)
anova(model)


#compare imputed results to non-imputed (listwise deletion) results.  Were there dramatic differences? May want to explore further.
model<-lm(Ozone~Month + Solar.R+ Wind+Temp, data=air)
summary(model)
