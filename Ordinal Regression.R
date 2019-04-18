air<-airquality

# taken from https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/

library(ordinal)

str(air)
#add a categorical variable to the data
air$Temp_cat<-air$Temp<85
air$Temp_cat<-factor(air$Temp_cat,c('TRUE','FALSE'))
levels(air$Temp_cat)<-c('Cold','Hot')
# set month as an ordinal factor
air$Month<-factor(air$Month,ordered = T)

### Method 1, using ordinal package ###
colnames(air)
fit<-clm(Month~Temp_cat+Ozone+Solar.R+Wind,data=air, link='logit')

convergence(fit)

# nominal test = for ordered independent vars, if sig, odds not proportional bw levels
nominal_test(fit)
# scale test = for unordered independent vars, if sig, odds not proportional bw levels (this model fails, unsurprisingly)
scale_test(fit)

levels(air$Month)
sf <- function(y) {
  c('Y>=5' = qlogis(mean(y >= 1)),
    'Y>=6' = qlogis(mean(y >= 2)),
    'Y>=7' = qlogis(mean(y >= 3)),
    'Y>=8' = qlogis(mean(y >= 4)),
    'Y>=9' = qlogis(mean(y >= 5)))
}
require(Hmisc)
# if proportional, diff bw levels should be equal across levels
(s <- with(air, summary(as.numeric(Month) ~ Temp_cat+Ozone+Solar.R+Wind, fun=sf)))
# distances bw points should be same for all levels of each factor (clearly not the case for this model)
plot(s, which=1:5, pch=1:5, xlab='logit', main=' ',xlim=c(-5,5))

# check predictions from model
predict(fit, type='class')

### Method 2, using 
library(MASS)
m <- polr(Month~Temp_cat+Ozone+Solar.R+Wind,data=air, Hess=TRUE)

# summary doesn't output p-values, so need to calculate them separately
summary(m)
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(m)) 
# convert to odds ratios
exp(coef(m))
exp(cbind(OR = coef(m), ci))

levels(air$Month)
sf <- function(y) {
  c('Y>=5' = qlogis(mean(y >= 1)),
    'Y>=6' = qlogis(mean(y >= 2)),
    'Y>=7' = qlogis(mean(y >= 3)),
    'Y>=8' = qlogis(mean(y >= 4)),
    'Y>=9' = qlogis(mean(y >= 5)))
}
require(Hmisc)
# if proportional, diff bw levels should be equal across levels
(s <- with(air, summary(as.numeric(Month) ~ Temp_cat+Ozone+Solar.R+Wind, fun=sf)))
# distances bw points should be same for all levels of each factor (clearly not the case for this model)
plot(s, which=1:5, pch=1:5, xlab='logit', main=' ',xlim=c(-5,5))

# check predictions from model
predict(m, type='class')
