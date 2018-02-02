#Model checking?

setwd("~/Downloads")
set.seed(406)
alldata <- read.csv("all data.csv", header = T)
alldata <- alldata[,-1] #delete date

alldata$Total.Transit.Ridership <- alldata$Total.Transit.Ridership/100000
alldata$Total.Labour.Force <- alldata$Total.Labour.Force/100000
alldata$Licensed.Vehicles <- alldata$Licensed.Vehicles/100000
alldata$Immigration.Population <- alldata$Immigration.Population/1000
alldata$HPI <- alldata$HPI/10000
alldata$AvgSoldPrice.... <- alldata$AvgSoldPrice..../10000

N <- nrow(alldata)
selected <- sample(N, 30, replace = FALSE, prob = NULL)
test <- alldata[selected,]
train <- alldata[-selected,]

###Ridge regression
library(glmnet)
y <- as.matrix(train$HPI)
xm <- as.matrix(train[, -c(13,17)]) #get rid of Season and HPI
lambdas <- exp( seq(-5, 10, length=50))
a <- glmnet(x=xm, y=y, lambda=rev(lambdas), family='gaussian', alpha=0)
plot(a, xvar='lambda', label=TRUE, lwd=6, cex.axis=1.5, cex.lab=1.2, ylim=c(-10, 20))
set.seed(406)
tmp <- cv.glmnet(x=xm, y=y, lambda=lambdas, nfolds=5, alpha=0, family='gaussian')
plot(tmp, lwd=6, cex.axis=1.5, cex.lab=1.2)

set.seed(406)
op.la <- 0
for(j in 1:20) {
  tmp <- cv.glmnet(x=xm, y=y, lambda=lambdas, nfolds=5, alpha=0, family='gaussian')
  op.la <- op.la + tmp$lambda.min # tmp$lambda.1se
}
(op.la <- op.la / 20)
log(op.la)

#Lasso
la <- glmnet(x=xm, y=y, lambda=rev(lambdas), family='gaussian', alpha=1, intercept=TRUE)
plot(la, xvar='lambda', label=TRUE, lwd=6, cex.axis=1.5, cex.lab=1.2)

#CV：
set.seed(406)
tmp2 <- cv.glmnet(x=xm, y=y, lambda=lambdas, nfolds=5, alpha=1, family='gaussian', intercept=TRUE)
plot(tmp2, lwd=6, cex.axis=1.5, cex.lab=1.2)
tmp2$lambda.min
coef(tmp2, s=tmp2$lambda.min)

