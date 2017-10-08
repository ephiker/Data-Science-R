setwd("C:/Users/ande/Desktop/Rcode")
d <- read.table('student-grade.csv', header=T, sep=',', na.strings=c('NA',''))
library('ggplot2')


stuTrain <- d[1:700,]
stuTest <- d[701:1044,]
colnames(stuTrain)
dim(d)

#회귀모델만들기
model1 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
          + Fjob + reason + guardian + traveltime + studytime + failures + schoolsup
          + famsup + paid + activities + nursery + higher + internet + romantic + famrel
          + freetime + goout + Dalc + Walc + health + absences + class, data = stuTrain)

summary(model1)

stuTest$predG3 <- predict(model1, newdata = stuTest)
stuTrain$predG3 <- predict(model1, newdata = stuTrain)

head(stuTest$predG3)
head(stuTrain$predG3)


#그래프 그리기
ggplot(data=stuTest,aes(x=predG3,y=G3)) + 
  geom_point(alpha=0.2,color="red") + 
  geom_smooth(aes(x=predG3,y=G3), color="black") +
  geom_line(aes(x=G3,y=G3), color="blue",linetype=2) +
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(limits=c(0,21))


#성능예측
rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(stuTrain$G3, predict(model1,newdata=stuTrain))
rmse(stuTest$G3, predict(model1,newdata=stuTest))


#################################################################
#어떤 영향을 미치는지 알아보기 위해 따로 



#Fjobteacher
train.teacher <- subset(stuTrain, Fjob == "teacher")

model6 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + reason + guardian + traveltime + studytime + failures + schoolsup
             + famsup + paid + activities + nursery + higher + internet + romantic + famrel
             + freetime + goout + Dalc + Walc + health + absences + class, data = train.teacher)

train.teacher$predG3 <- predict(model6, newdata = train.teacher)
mean(train.teacher$predG3)


rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(train.teacher$G3, predict(model6,newdata=train.teacher))


#schoolsupyes
train.supyes <- subset(stuTrain, schoolsup == "yes")

model7 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + Fjob + reason + guardian + traveltime + studytime + failures
             + famsup + paid + activities + nursery + higher + internet + romantic + famrel
             + freetime + goout + Dalc + Walc + health + absences + class, data = train.supyes)

train.supyes$predG3 <- predict(model7, newdata = train.supyes)

rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(train.supyes$G3, predict(model7,newdata=train.supyes))



#romanticyes
train.romanyes <- subset(stuTrain, romantic == "yes")
test.romanyes <- subset(stuTest, romantic == "yes")

model8 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + Fjob + reason + guardian + traveltime + studytime + failures + schoolsup
             + famsup + paid + activities + nursery + higher + internet + famrel
             + freetime + goout + Dalc + Walc + health + absences + class, data = train.romanyes)

View(train.romanyes)
summary(model8)

train.romanyes$predG3 <- predict(model8, newdata = train.romanyes)
mean(train.romanyes$predG3)



#성능예측
rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(train.romanyes$G3, predict(model8,newdata=train.romanyes))
rmse(test.romanyes$G3, predict(model8,newdata=test.romanyes))






#classport
train.classport <- subset(stuTrain, class == "port")

model9 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + Fjob + reason + guardian + traveltime + studytime + failures + schoolsup
             + famsup + paid + activities + nursery + higher + internet + romantic + famrel
             + freetime + goout + Dalc + Walc + health + absences, data = train.classport)

train.classport$predG3 <- predict(model9, newdata = train.classport)
mean(train.classport$predG3)



rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(train.classport$G3, predict(model9,newdata=train.classport))






#higheryes
train.higheryes <- subset(stuTrain, higher == "yes")
test.higheryes <- subset(stuTest, higher == "yes")

model10 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + Fjob + reason + guardian + traveltime + studytime + failures + schoolsup
             + famsup + paid + activities + nursery + internet + romantic + famrel
             + freetime + goout + Dalc + Walc + health + absences + class, data = train.higheryes)

View(train.higheryes)
summary(model10)

test.higheryes$predG3 <- predict(model10, newdata = test.higheryes)
train.higheryes$predG3 <- predict(model10, newdata = train.higheryes)


#성능예측
rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(train.higheryes$G3, predict(model10,newdata=train.higheryes))
rmse(test.higheryes$G3, predict(model10,newdata=test.higheryes))




mean(stuTrain$predG3)
mean(train.teacher$predG3)
mean(train.supyes$predG3)





#####################################################################
#성능올리기 - 코이피션트가높은경우에대해서모델을구성하여성능측정####

#Fjob이 teacher인경우
train.teacher <- subset(stuTrain, Fjob == "teacher")

model6 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + reason + guardian + traveltime + studytime + failures + schoolsup
             + famsup + paid + activities + nursery + higher + internet + romantic + famrel
             + freetime + goout + Dalc + Walc + health + absences + class, data = train.teacher)

train.teacher$predG3 <- predict(model6, newdata = train.teacher)
mean(train.teacher$predG3)


rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(train.teacher$G3, predict(model6,newdata=train.teacher))







#teacher
train.teacher <- subset(stuTrain, Fjob == "teacher")

model6 <- lm(G3 ~ school + sex + age + address + famsize + Pstatus + Medu + Fedu + Mjob
             + reason + guardian + traveltime + studytime + failures + schoolsup
             + famsup + paid + activities + nursery + higher + internet + romantic + famrel
             + freetime + goout + Dalc + Walc + health + absences + class, data = train.teacher)

train.teacher$predG3 <- predict(model6, newdata = train.teacher)
mean(train.teacher$predG3)


rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(train.teacher$G3, predict(model6,newdata=train.teacher))



#########################################################
#성능올리기 : 상호작용 도입

anova(model11, test = "Chisq")

#회귀모델만들기
model11 <- lm(G3 ~ failures*(schoolsup + class + higher) + school + sex + age
             + address + famsize + Pstatus + Fedu + Mjob + Medu + Fjob + reason
             + guardian + traveltime + studytime + famsup + paid + activities
             + nursery  + internet + romantic + famrel + freetime + goout + Dalc
             + Walc + health + absences, data = stuTrain)


stuTest$predG3 <- predict(model11, newdata = stuTest)

rmse <- function(y, f) {
  sqrt(mean( (y-f)^2 )) }

rmse(stuTest$G3, predict(model11,newdata=stuTest))

summary(model11)




#그래프 그리기
ggplot(data=stuTest,aes(x=predG3,y=G3)) + 
  geom_point(alpha=0.2,color="red") + 
  geom_smooth(aes(x=predG3,y=G3), color="black") +
  geom_line(aes(x=G3,y=G3), color="blue",linetype=2) +
  scale_x_continuous(limits=c(0,20)) +
  scale_y_continuous(limits=c(0,21))


