library(mice)

#load data, in this case using example data from R
air<-airquality

# prediction matrix, predictors decided by proportion of useable values (if most are mising concurrently, then variable is not a 
# good predictor), also selected based on correlation, so should only be used in cases where correlation is a valid measure.
auto_predmatrix<-quickpred(air)

# minpuc = minumum proportion of useable values
auto_predmatrix<-quickpred(air,minpuc = 0.75)

# always include Wind as a predictor
auto_predmatrix<-quickpred(air,include='Wind')

#run example imputation to check methods
ini=mice(air, m=5, maxit = 0, seed = 196652,predictorMatrix=auto_predmatrix)
method_imp<-ini$method
method_imp

# change imputation method for Ozone
# posssible methods and their applicability listed in 'Mice in R' file.
method_imp["Ozone"]<-"rf"
method_imp

# set Solar.R not to be imputed
method_imp["Solar.R"]<-""

#set Ozone to use all other vars as predictors
auto_predmatrix["Ozone",]<- 1
auto_predmatrix['Ozone',"Ozone"]<- 0
auto_predmatrix[,"Ozone"]

# set Solar.R not to be used as a predictor
auto_predmatrix[,"Solar.R"]<- 0


# default is to include all variables as predictors, but in this case I used quickpred
# there is debate around appropriate number of imputed datasets (m) https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3701793/
# in this case, I used the max percent missing amongst the variables to be imputed
# set maxit to lowest number that still provides convergence, I usually start at 20
# resetting quickpred to default
imp<-mice(air, m=20, maxit = 20, seed = 196652,predictorMatrix=auto_predmatrix,imputationMethod =method_imp )

# check convergence.  You want good mixing, no upwards or downwards trend at end of iterations, see 'Mice in R' file for more details.
plot(imp)

### EXAMINING MULTIPLE IMPUTATION DISTRIBUTIONS ###
# Mostly, you should just focus on the convergence plots, only need to check these if your results are really odd.
library(lattice)
library(ggplot2)

# this creates a grid of plots looking at the distribution of missing/non values for each imputation of the numeric or ordered variables
stripplot(imp)

# this puts all imputed datasets into one long single dataset.
long <- complete(imp,"long")

# for comparing distribution of imputed/original values of numeric variables
long$Ozone.na<-is.na(imp$data$Ozone)
densityplot(~Ozone|.imp, data=long, group=Ozone.na, plot.points=FALSE,ref=TRUE,scales=list(y=list(draw=F)),
            par.settings=simpleTheme(col.line=rep(c("blue","red"))),auto.key = list(columns=2,text=c("Observed","Imputed")))

# to demonstrate categorical imputation:

# create missing categorical values and run default imputation
chick<-chickwts
chick$feed[sample(1:length(chick$feed),15)]<-NA
imp<-mice(chick, m=20, maxit = 20, seed = 196652 )

long <- complete(imp,"long")

# look at missing and non distributions for each imputation
long$var.na<-is.na(imp$data$feed)
counts<-ftable(long$.imp,long$var.na, long$feed)
counts
counts<-prop.table(counts,1)
counts
temp<-as.data.frame(counts)
temp
ggplot(data=temp, aes(x=Var3, y=Freq, fill=Var2)) +geom_bar(stat="identity", position=position_dodge())+facet_wrap(~Var1)

# look at missing and non distributions for combined imputations
long$var.na<-is.na(imp$data$feed)
counts<-table(long$var.na, long$feed)
counts
temp=as.data.frame(prop.table(counts,1))
temp
ggplot(temp, aes(x=Var2, y=Freq,fill=Var1)) + geom_bar(stat="identity", position=position_dodge())

