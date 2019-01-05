mydata <- read.csv2("dataset_Facebook.csv",header=TRUE)
myType <- as.numeric(factor(mydata$Type))
#myType is my new variable, video is 4, photo is 2, status is 3, link is 1
#I convert them into numeric values for easier understanding of the data.
is.numeric(myType)
is.numeric(mydata$Category)
is.numeric(mydata$Paid)
summary(mydata)
#variables.
x1 <- (mydata$Page.total.likes)
x2 <- (myType)
x3 <- (mydata$Category)
x4 <- (mydata$Post.Month)
x5 <- (mydata$Post.Weekday)
x6 <- (mydata$Post.Hour)
x7 <- (mydata$Paid)
#x8 <- (mydata$Lifetime.Post.Total.Reach)
#x9 <- (mydata$Lifetime.Post.Total.Impressions)
#x10 <- (mydata$Lifetime.Engaged.Users)
#x11 <- (mydata$Lifetime.Post.Consumers)
#x12 <- (mydata$Lifetime.Post.Consumptions)
#x13 <- (mydata$Lifetime.Post.Impressions.by.people.who.have.liked.your.Page)
#x14 <- (mydata$Lifetime.Post.reach.by.people.who.like.your.Page)
#x15 <- (mydata$Lifetime.People.who.have.liked.your.Page.and.engaged.with.your.post)
# these are not asked in the project requirement, but i leave them here anyways.
Y <- (mydata$Total.Interactions)
Firstmodel <- lm(Y~x1+x2+x3+x4+x5+x6+x7)
summary(Firstmodel)
plot(Firstmodel)
# So this is our first model. 
# One can easily notice we need to transform this data.
Y2 <- log(mydata$Total.Interactions + 1)
# The + 1 is very important, without + 1 you cant log transformation
# Since some of the data has interaction value = 0, and log(0) is N/A.
# Please mention this is paper.
SecondModel <- lm(Y2~x1+x2+x3+x4+x5+x6+x7)
library(car)
outlierTest(SecondModel)
mydata <- mydata[-c(101, 442, 77,418), ]
myType <- as.numeric(factor(mydata$Type))
x1 <- (mydata$Page.total.likes)
x2 <- (myType)
x3 <- (mydata$Category)
x4 <- (mydata$Post.Month)
x5 <- (mydata$Post.Weekday)
x6 <- (mydata$Post.Hour)
x7 <- (mydata$Paid)
Y2 <- log(mydata$Total.Interactions + 1)
SecondModel <- lm(Y2~x1+x2+x3+x4+x5+x6+x7)
summary(SecondModel)
plot(SecondModel)
# QQ plot here indicate heavy tail. 
# residual plot shows further transformation is needed (perhaps)
# i will leave this part to you.

#install.packages("alr3")
library(alr3)
library(leaps)
is.na(x7)
#the last data is missing a "paid" value.
#I assumed it to be 0 to proceed the data. 
#If you have any better way to ignore/input the missing data, do it.
x7[is.na(x7)] <- 0
X <- cbind(x1,x2,x3,x4,x5,x6,x7)
cor(X)
b <- regsubsets(as.matrix(X),Y2)
rs <- summary(b)
par(mfrow=c(1,1))
subsets(b,statistic=c("adjr2"),legend=FALSE)
rs$adjr2
om1 <- lm(Y2~x3)
om2 <- lm(Y2~x3+x2)
om3 <- lm(Y2~x3+x2+x7)
om4 <- lm(Y2~x3+x2+x5+x7)
om5 <- lm(Y2~x3+x2+x5+x6+x7)
om6 <- lm(Y2~x1+x2+x7+x3+x5+x6)
om7 <- lm(Y2~x1+x2+x3+x4+x5+x6+x7)
#Subset size=1
n <- length(om1$residuals)
npar <- length(om1$coefficients) +1
#Calculate AIC
extractAIC(om1,k=2)
#Calculate AICc
extractAIC(om1,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om1,k=log(n))
#Subset size=2
npar <- length(om2$coefficients) +1
#Calculate AIC
extractAIC(om2,k=2)
#Calculate AICc
extractAIC(om2,k=2)+2*npar*(npar+1)/(n-npar-1)
#Calculate BIC
extractAIC(om2,k=log(n))
npar <- length(om3$coefficients) +1
extractAIC(om3,k=2)
extractAIC(om3,k=2)+2*npar*(npar+1)/(n-npar-1)
extractAIC(om3,k=log(n))
npar <- length(om4$coefficients) +1
extractAIC(om4,k=2)
extractAIC(om4,k=2)+2*npar*(npar+1)/(n-npar-1)
extractAIC(om4,k=log(n))
npar <- length(om5$coefficients) +1
extractAIC(om5,k=2)
extractAIC(om5,k=2)+2*npar*(npar+1)/(n-npar-1)
extractAIC(om5,k=log(n))
npar <- length(om6$coefficients) +1
extractAIC(om6,k=2)
extractAIC(om6,k=2)+2*npar*(npar+1)/(n-npar-1)
extractAIC(om6,k=log(n))
npar <- length(om7$coefficients) +1
extractAIC(om7,k=2)
extractAIC(om7,k=2)+2*npar*(npar+1)/(n-npar-1)
extractAIC(om7,k=log(n))

#Backward elimination based on AIC.
backAIC <- step(om7,direction="backward", data=Hald)

#Backward elimination based on BIC.
backBIC <- step(om7,direction="backward", data=Hald, k=log(n))
mint <- lm(Y2~1,data=mydata)

#Forward selection based on AIC
forwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~x1+x2+x3+x4+x5+x6+x7),
                   direction="forward", data=mydata)

#Forward selection based on BIC
forwardBIC <- step(mint,scope=list(lower=~1, 
                                   upper=~x1+x2+x3+x4+x5+x6+x7),direction="forward", data=mydata,k=log(n))

#You can notice BIC perfer less variables. As we already know.
#I will leave the 'choice and why' part to you, good luck M'lady.




#bootrap
#Not necessery useful, but im throwing it here anyways.

library(boot)

bs <- function(formula, data, indices) {
  d <- data[indices,]
  fit <- lm(formula, data=d)
  return(coef(fit)) 
} 
# bootstrapping with 1000 replications 
results <- boot(data=mydata, statistic=bs, 
                R=1000, formula=Y2~x2+x3+x5+x6+x7)

#For here i choose the equation of log(y)~x2+x3+x5+x6+x7
#but you are welcome to change the equation, im just putting it here to go further

results
plot(results, index=1) # intercept 
plot(results, index=2) # wt 
plot(results, index=3) # disp 

#I cant seem to make these code work. Please fix if you can. 
#They should yield 95% CI if works.
#boot.ci(results, type="bca", index=1) # intercept 
#boot.ci(results, type="bca", index=2) # wt 
#boot.ci(results, type="bca", index=3) # disp 


#Lasso and Ridge

# Form predictor matrix and response vector
x <- model.matrix(Y2~x2+x3+x5+x6+x7)[,-1]
lambda <- 10^seq(20, -2, length = 100)

library(glmnet)

set.seed(489)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = Y2[test]

#OLS
m.ols <- lm(Y2~x2+x3+x5+x6+x7)
summary(m.ols)
coef(m.ols)
# Cross validation
datasetlm <- lm(Y2~x2+x3+x5+x6+x7, subset = train)
ridge.mod <- glmnet(x[train,], Y2[train], alpha = 0, lambda = lambda)

# Find the best lambda cross-validation with ridge regression
cv.out <- cv.glmnet(x[train,], Y2[train], alpha = 0)

#best lambda
bestlamridge <- cv.out$lambda.min

#predictions on test data
ridge.pred <- predict(ridge.mod, s = bestlamridge, newx = x[test,])
ols.pred <- predict(datasetlm, newdata = mydata[test,])
summary(ridge.pred)

#MSE
mean((ols.pred-ytest)^2)
mean((ridge.pred-ytest)^2)

out = glmnet(x[train,],Y2[train],alpha =0)
predict(ridge.mod, type = "coefficients", s = bestlamridge)[1:6,]

# Lasso
lasso.mod <- glmnet(x[train,], Y2[train], alpha = 1, lambda = lambda)

# Lasso Cross Validation
cv.out <- cv.glmnet(x[train,], Y2[train], alpha = 1)
bestlamlasso <- cv.out$lambda.min

# Lasso MSE
lasso.pred <- predict(lasso.mod, s = bestlamridge, newx = x[test,])
mean((lasso.pred-ytest)^2)

# Lasso coefficients
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlamlasso)[1:6,]

# OLS
coef(m.ols)

# Ridge
predict(ridge.mod, type = "coefficients", s = bestlamridge)[1:6,]

# Lasso
lasso.coef
summary(m.ols)

#If we choose data using 4 independent variable.
datasetlm <- lm(Y2~x2+x3+x5+x7, subset = train)
ridge.mod <- glmnet(x[train,], Y2[train], alpha = 0, lambda = lambda)

# Find the best lambda cross-validation with ridge regression
cv.out <- cv.glmnet(x[train,], Y2[train], alpha = 0)

#best lambda
bestlamridge <- cv.out$lambda.min

#predictions on test data
ridge.pred <- predict(ridge.mod, s = bestlamridge, newx = x[test,])
ols.pred <- predict(datasetlm, newdata = mydata[test,])
summary(ridge.pred)

#MSE
mean((ols.pred-ytest)^2)
mean((ridge.pred-ytest)^2)

out = glmnet(x[train,],Y2[train],alpha =0)
predict(ridge.mod, type = "coefficients", s = bestlamridge)[1:6,]

# Lasso
lasso.mod <- glmnet(x[train,], Y2[train], alpha = 1, lambda = lambda)

# Lasso Cross Validation
cv.out <- cv.glmnet(x[train,], Y2[train], alpha = 1)
bestlamlasso <- cv.out$lambda.min

# Lasso MSE
lasso.pred <- predict(lasso.mod, s = bestlamridge, newx = x[test,])
mean((lasso.pred-ytest)^2)

# Lasso coefficients
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlamlasso)[1:6,]

# OLS
coef(m.ols)

# Ridge
predict(ridge.mod, type = "coefficients", s = bestlamridge)[1:6,]

# Lasso
lasso.coef
summary(m.ols)



#This is the end of my coding. For now. Please leave advice and edit the code.
#This is NOT the complete version, many parts still require improvement from you.
#If you have trouble understand part of my code please let me know asap.
