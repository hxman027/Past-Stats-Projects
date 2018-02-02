###Lag1 Data:
#LSdata <- read.csv("Data1.csv", header = T)
###Lag2 Data:
LSdata <- read.csv("Data2mon.csv", header = T)

#find the correlation with all quantitative variables
b <- cbind(LSdata[,2:9], LSdata[,12:18])
cor(b)
summary(LSdata[,2:17])

attach(LSdata)

###Check Residual Plots
par(mfrow=c(4,3))
plot(num.sold, num.trans, xlab = "num.sold" , ylab = "num.trans")
plot(num.sold, log(num.trans), xlab = "num.sold" , ylab = "log(num.trans)")
plot(factor(int.rate), num.trans, xlab = "int.rate" , ylab = "num.trans")

plot(apart, num.trans, xlab = "apart" , ylab = "num.trans")
plot(apart, log(num.trans), xlab = "apart" , ylab = "log(num.trans)")
plot(primeTime, num.trans, xlab = "primeTime" , ylab = "num.trans")

plot(ave.pri, num.trans, xlab = "ave.pri($)" , ylab = "num.trans")
plot(ave.pri, log(num.trans), xlab = "ave.pri($)" , ylab = "log(num.trans)")
plot(timeNumOrder, num.trans, xlab = "timeNumOrder (t=1:34, from Apr 2014 - Jan 2017)" , ylab = "num.trans")

plot(tot.perm, num.trans, xlab = "tot.perm" , ylab = "num.trans")
plot(tot.perm, log(num.trans), xlab = "tot.perm" , ylab = "log(num.trans)")
plot(season, num.trans, xlab = "season" , ylab = "num.trans")


#Data Transformation:
num.trans = num.trans/1000
num.sold = num.sold/1000
ave.pri = ave.pri/1000
HPI = HPI /100
tot.perm = tot.perm/1000000
apart = apart/1000

# ANOVA testing
anovatest = aov(log(num.trans) ~ season)
TukeyHSD(anovatest)



###Variables with all explanatory variables: 

fit1 <- lm(log(num.trans) ~ detached + apart + num.sold + ave.pri + monthNum + ave.pri + HPI + tot.perm + season + primeTime + newHomes + X5yr.mortg + X1yr.mortg + int.rate + GIC + timeNumOrder, data = LSdata)
sum1 <- summary(fit1)
sum1

###Variable Selection:
library(leaps)
s1 <- regsubsets(log(num.trans)~ detached + apart + tHouse + ave.pri + HPI + tot.perm + primeTime + season + monthNum + newHomes + int.rate + GIC + timeNumOrder, data = LSdata, method = "exhaustive")
ss1 <- summary(s1)
ss1
ss1$cp


###fit2 has 7 explan. variables
fit2 <- lm(log(num.trans) ~ primeTime + season + monthNum + int.rate + ave.pri + tot.perm + apart)
sum2 <- summary(fit2)
sum2

###fit3 has 5 explan. variables
fit3 <- lm(log(num.trans) ~ apart + HPI + tot.perm + season + primeTime)
sum3 <- summary(fit3)
sum3

###fit4 has 5 explan. variables with "spring" baseline.
season <- relevel(factor(season), ref = "Spring")
fit4 <- lm(log(num.trans) ~ apart + HPI + tot.perm + season + primeTime)
sum4 <- summary(fit4)
sum4

ls.diag(fit4)

###Check Residual Plots

residSally <- resid(fit3)
predSally <- predict(fit3)
par(mfrow=c(2,3))
qqnorm(residSally)
plot(predSally, residSally, xlab = "Predicted/1000", ylab = "Residuals")
abline(h=0)
plot(HPI, residSally, xlab = "HPI", ylab = "Residuals")
abline(h=0)
plot(tot.perm, residSally, xlab = "tot.perm/100000", ylab = "Residuals")
abline(h=0)
plot(apart, residSally, xlab = "apart/1000", ylab = "Residuals")
abline(h=0)

###Check for Quadratic Terms 
modelquad <- lm(log(num.trans) ~ (HPI + tot.perm)^2 + I(HPI^2) + I(tot.perm^2) + apart + season1 + primeTime)
summary(modelquad)


###Check for DW statistics
library(lmtest)
dw1 <- dwtest(log(num.trans) ~ primeTime + season + monthNum + int.rate + ave.pri + tot.perm + apart, alternative="two.sided")
dw1
dw2 <- dwtest(log(num.trans) ~ apart + HPI + tot.perm + season + primeTime, alternative="two.sided")
dw2

###Function to calculate the leave-one-out cross validation error.
ls.cvrmse <- function(ls.out)
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}

fit2.cvrmse <- ls.cvrmse(fit2)
fit2.cvrmse

fit3.cvrmse <- ls.cvrmse(fit3)
fit3.cvrmse
###should be same with 4 beause only basaeline was changed
fit4.cvrmse <- ls.cvrmse(fit4)
fit4.cvrmse

##Randomly select half of the data as training set and the rest as holdout set.
##Do the regression using the training set and predict on the holdout set to get the rmse.
##We summarize the comparisons of 3 models int the table.
#cross-validation
#cross-validation
n = 34
id.subset1 <- sort(sample(1:n, round(n/2), replace = FALSE))
#subset1 is training and subset2 is holdout
LSdata.subset1 <- LSdata[id.subset1,]
LSdata.subset2 <- LSdata[-id.subset1,] 

# 7 var
fit2 <- lm(log(num.trans) ~ primeTime + season + monthNum + int.rate + ave.pri + tot.perm + apart, data=LSdata.subset1)
pred2 <- predict(fit2,LSdata.subset2)
err2 <- sqrt(mean((log(LSdata.subset2$num.trans)-pred2)^2))
err2

# 5 var
fit3 <- lm(log(num.trans) ~ apart + HPI + tot.perm + season + primeTime, data=LSdata.subset1)
pred3 <- predict(fit3,LSdata.subset2)
err3 <- sqrt(mean((log(LSdata.subset2$num.trans)-pred3)^2))
err3
