#Model checking?

setwd("~/Downloads")
set.seed(406)
dat <- read.csv("all data.csv", header = T)
dat <- dat[-sample(1:106,1), -1] #delete date, randomly delete one row so its a multiple of 5
###scaling
dat$Total.Transit.Ridership <- dat$Total.Transit.Ridership/100000
dat$Total.Labour.Force <- dat$Total.Labour.Force/100000
dat$Licensed.Vehicles <- dat$Licensed.Vehicles/100000
dat$Immigration.Population <- dat$Immigration.Population/1000
dat$HPI <- dat$HPI/10000
dat$AvgSoldPrice.... <- dat$AvgSoldPrice..../10000

attach(dat)
library(glmnet)
y <- as.matrix(dat$HPI)
xm <- as.matrix(dat[, -c(13,17)]) #get rid of Season and HPI
lambdas <- exp( seq(-5, 10, length=50))


library(MASS)
n <- nrow(xm)
k <- 5
ii <- (1:n)%%k + 1
set.seed(406)
N <- 5
mspe.08 <- mspe.la <- mspe.st <- mspe.ri <- mspe.f <- rep(0, N)

for (i in 1:N) {
  ii <- sample(ii)
  pr.la <- pr.f <- pr.ri <- pr.st <- rep(0, n)
  pr.08 <- rep(0,n)
  for (j in 1:k) {
    tmp.ri <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas, 
                        nfolds = 5, alpha = 0, family = "gaussian")
    tmp.la <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas, 
                        nfolds = 5, alpha = 1, family = "gaussian")
    tmp.08 <- cv.glmnet(x = xm[ii != j, ], y = y[ii != j], lambda = lambdas, nfolds = 5, alpha = 0.8, family = "gaussian")
    
    null <- lm(HPI ~ 1, data = dat[ii != j, -17])
    full <- lm(HPI ~ ., data = dat[ii != j, -17])
    tmp.st <- stepAIC(null, scope = list(lower = null, upper = full), trace = 0)
    pr.ri[ii == j] <- predict(tmp.ri, s = "lambda.min", newx = xm[ii == 
                                                                    j, ])
    pr.la[ii == j] <- predict(tmp.la, s = "lambda.min", newx = xm[ii == 
                                                                    j, ])
    pr.08[ii == j] <- predict(tmp.08, s = "lambda.min", newx = xm[ii ==  j, ])
    pr.st[ii == j] <- predict(tmp.st, newdata = dat[ii == j, -17])
    pr.f[ii == j] <- predict(full, newdata = dat[ii == j, -17])
  }
  mspe.ri[i] <- mean((dat$HPI - pr.ri)^2)
  mspe.la[i] <- mean((dat$HPI - pr.la)^2)
  mspe.st[i] <- mean((dat$HPI - pr.st)^2)
  mspe.f[i] <- mean((dat$HPI - pr.f)^2)
  mspe.08[i] <- mean((dat$HPI - pr.08)^2)
  
}

boxplot(mspe.08,mspe.la, mspe.ri, mspe.st, mspe.f, names = c("EN","LASSO", "Ridge", "Stepwise",
                                                     "Full"), col = c("white","steelblue", "gray80", "tomato", "springgreen"), cex.axis = 1,
        cex.lab = 1, cex.main = 2)
mtext(expression(hat(MSPE)), side = 2, line = 2.5))

# boxplot(mspe.la, mspe.ri, mspe.st, mspe.f, names = c("LASSO", "Ridge", "Stepwise", 
#                                                      "Full"), col = c("steelblue", "gray80", "tomato", "springgreen"), cex.axis = 1, 
#         cex.lab = 1, cex.main = 2)
# mtext(expression(hat(MSPE)), side = 2, line = 2.5)











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

