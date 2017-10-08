
plot(mtcars$wt, mtcars$mpg, main="Car weight v.s. Fuel efficiency",
     xlab = "Car Weight ", ylab = "Miles Per Gallon ", pch = 18)

fit.line <- lm(mtcars$mpg~mtcars$wt)
abline(fit.line, col="red")

cf <- round(coef(fit.line), 2)
eq <- paste0("mpg = ", cf[1],
             ifelse(sign(cf[2]) ==1, " + ", " - "), abs(cf[2]), " car weight")
mtext(eq, 3, line=2)







#############################################################################
#personel income ?????ϴ? linear regression

setwd("C:/Users/ephik/Desktop/Coding Scripts/R/Data Science Intro")
load("C:/Users/ephik/Desktop/Coding Scripts/R/Data Science Intro/psub.RData")
install.packages(("ggplot2"))

summary(psub$AGEP)
summary(psub$SEX)
summary(psub$COW)
View(psub)

dtrain <- subset(psub, ORIGRANDGROUP>=500)
dtest <- subset(psub, ORIGRANDGROUP < 500)
dTrain
model <- lm(log(PINCP, base=10) ~ AGEP + SEX + COW + SCHL, data=dtrain)


dtest$predLogPINCP <- predict(model, newdata=dtest)
dtrain$predLogPINCP <- predict(model, newdata=dtrain)

head(dtest$predLogPINCP)
head(log(dtest$PINCP, base=10))

#rmse ???ϱ?
#evaluation with graph
??

#Y
library('ggplot2')
ggplot(data=dtest,aes(x=predLogPINCP,y=log(PINCP,base=10))) + 
  geom_point(alpha=0.2,color="black") + 
  geom_smooth(aes(x=predLogPINCP, y=log(PINCP,base=10)), color="black") +
  geom_line(aes(x=log(PINCP,base=10), y=log(PINCP,base=10)), color="blue",linetype=2) +
  scale_x_continuous(limits=c(4,5)) +
  scale_y_continuous(limits=c(3.5,5.5))



#????
ggplot(data=dtest,aes(x=predLogPINCP, y=predLogPINCP-log(PINCP,base=10))) +
  geom_point(alpha=0.2,color="black") + geom_smooth(aes(x=predLogPINCP,
                                                        y=predLogPINCP-log(PINCP,base=10)),
                                                    color="black")

#RMSE
rmse <- function(y, f) { sqrt(mean( (y-f)^2 )) }

rmse(log(dtrain$PINCP,base=10),predict(model,newdata=dtrain))
rmse(log(dtest$PINCP,base=10),predict(model,newdata=dtest))



