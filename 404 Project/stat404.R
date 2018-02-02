setwd("~/Downloads")
dat=read.csv("stat404 data.csv",header=T)

names(dat)=c("Building","Position","Level","Company","System","Time","Reception")
library(MASS)
library(faraway)

boxcox(Reception~Building+Position+Level+Company+System+Time+Building:Position+Building:Level+Building:Company+Position:Level+Position:Company+Level:Company,data=dat,lambda=seq(-1,2,len=21),ylab="log likelihood")
model=aov(Reception~Building+Position+Level+Company+System+Time+Building:Position+Building:Level+Building:Company+Position:Level+Position:Company+Level:Company,data=dat)

summary(model)
plot(model$fitted.values,model$residuals)
qqnorm(model$residuals)
interaction.plot(dat$Position,dat$Level,dat$Reception,xlab="Position",trace.label = "Level",ylab="Reception")
?interaction.plot
model.tables(model,type="mean")

modellm=lm(Reception~Building+Position+Level+Company+System+Time+Building:Position+Building:Level+Building:Company+Position:Level+Position:Company+Level:Company,data=dat)
summary(modellm)
halfnorm(modellm$coef[-1], nlab= length(modellm$coef[-1])/3-1, labs= names(modellm$coef[-1]),
         ylab= "Absolute estimated effect",
         main= "Half Normal Plot")






 a <- 3.18*0.12
 -0.05625 - a
 -0.05625 +a 
 
 