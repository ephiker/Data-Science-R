##############################################################################################
setwd("C:/Users/andre/Desktop/Rcode")
install.packages("doBy")
library("doBy")
install.packages("ggplot2")
library(ggplot2)
##############################################################################################
#Loading data into R

student_m <- read.csv("student-math.csv", header=T)
student_p <- read.csv("student-port.csv", header=T)

class_m<-data.frame(class=c("math"))
new_math<-cbind(class_m, student_m)
class_p<-data.frame(class=c("port"))
new_port<-cbind(class_p, student_p)

studata<-rbind(new_math, new_port)
View(studata)


##############################################################################################
#Data Partitioning ???Training and Test dataset
#d에서dTrainAll과 dTest로 분리
set.seed(2100034)
studata$rgroup <- runif(dim(studata)[[1]])
stuTrainAll <- subset(studata,rgroup<=0.9) #stuTrainAll 생성
stuTest <- subset(studata,rgroup>0.9) #stuTest 생성
nrow((stuTrainAll))
nrow((stuTest))

##############################################################################################
#Separating variables from target variable 1(train test)
vars <- setdiff(colnames(stuTrainAll), c('rgroup'))
#stuTrainAll에서 뽑아낸 Variable들 이름들

catVars <- vars[sapply(stuTrainAll[,vars],class) %in% c('factor','character')]
#dTrainAll에서 뽑아낸 카테고리컬 Variable 이름들

numericVars <- vars[sapply(stuTrainAll[,vars],class) %in% c('numeric','integer')]
#dTrainAll에서 뽑아낸 누메릭 Variable 이름들

catVars #변수를 카테고리컬과 누메릭 두 가지로 나눔
numericVars #변수를 카테고리컬과 누메릭 두 가지로 나눔
remove<-c("G1", "G2", "G3", "scoremean")
numVars<-setdiff(numericVars,remove)

####################################################################################333

#Separating variables from target variable 2(calibratiion)
#train으로부터 calibration 분리 90%, 10%
useForCal <- rbinom(n=dim(stuTrainAll)[[1]],size=1,prob=0.1)>0
stuCal <- subset(stuTrainAll,useForCal)
stuTrain <- subset(stuTrainAll,!useForCal)

nrow((stuTrainAll))
nrow((stuTrain))
nrow((stuCal))
nrow((stuTest))


levels(studata$Fjob)

varTab <- table(varTab = stuTrain[,catVars[9]], stuTrain$G3, useNA='ifany')

###############################################################################

#Function for Categorical Variables
makePregG3cat <- function(trainset, calset, testset,v){
  varTab <- table(varTab = trainset[,catVars[v]], trainset$G3, useNA='ifany')
  scoreVector <- as.numeric(colnames(varTab))
  rownum <- dim(varTab)[1]
  #total <- sum(varTab[,]*scoreVector)
  #predG3 <- total/sum(varTab[,]) #모든 점수 합 나누기 모든 인원수 

  MSEsum <- 0
  #Train RMSE
  for (i in 1:rownum){
    total <- sum(varTab[i,]*scoreVector)
    predG3 <- total/sum(varTab[i,]) #모든 점수 합 나누기 모든 인원수
    MSE <- sum((predG3 - trainset$G3[trainset[catVars[v]] == rownames(varTab)[i]])^2)
    MSEsum <- MSEsum + MSE 
  }
  RMSE1 <- sqrt(MSEsum/dim(trainset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", catVars[v], "Train", RMSE1))


  MSEsum <- 0
  #Cal RMSE
  for (i in 1:rownum){
    total <- sum(varTab[i,]*scoreVector)
    predG3 <- total/sum(varTab[i,]) #모든 점수 합 나누기 모든 인원수
    MSE <- sum((predG3 - calset$G3[calset[catVars[v]] == rownames(varTab)[i]])^2)
    MSEsum <- MSEsum + MSE
  }
  RMSE2 <- sqrt(MSEsum/dim(calset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", catVars[v], "Cali", RMSE2))
  
  
  MSEsum <- 0
  #Test RMSE
  for (i in 1:rownum){
    total <- sum(varTab[i,]*scoreVector)
    predG3 <- total/sum(varTab[i,]) #모든 점수 합 나누기 모든 인원수
    MSE <- sum((predG3 - testset$G3[testset[catVars[v]] == rownames(varTab)[i]])^2)
    MSEsum <- MSEsum + MSE
  }
  RMSE3 <- sqrt(MSEsum/dim(testset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", catVars[v], "Test", RMSE3))
}


summary(stuTrain$failures)
###############################################################################

#Function for Numeric Variables
makePregG3num <- function(trainset, calset, testset,v2){
  rownum1 <- length(tapply(trainset$G3, trainset[,numVars[v2]], mean))
  rownum2 <- length(tapply(calset$G3, calset[,numVars[v2]], mean))
  rownum3 <- length(tapply(testset$G3, testset[,numVars[v2]], mean))
  
  MSEsum <- 0
  #Train RMSE
  for (i in 1:rownum1){
    predG3<-tapply(trainset$G3, trainset[,numVars[v2]], mean)[i]
    MSE <- sum((predG3 - trainset$G3[trainset[,numVars[v2]]
                                     == rownames(tapply(trainset$G3, trainset[,numVars[v2]], mean))[i]])^2)
    MSEsum <- MSEsum + MSE
  }
  RMSE1 <- sqrt(MSEsum/dim(trainset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", numVars[v2], "Train", RMSE1))
  
  
  MSEsum <- 0
  #Cal RMSE
  for (i in 1:rownum2){
    predG3<-tapply(calset$G3, calset[,numVars[v2]], mean)[i]
    MSE <- sum((predG3 - calset$G3[calset[,numVars[v2]]
                                     == rownames(tapply(calset$G3, calset[,numVars[v2]], mean))[i]])^2)
    MSEsum <- MSEsum + MSE
  }
  RMSE2 <- sqrt(MSEsum/dim(calset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", numVars[v2], "Cali", RMSE2))
  
  
  MSEsum <- 0
  #Test RMSE
  for (i in 1:rownum3){
    predG3<-tapply(testset$G3, testset[,numVars[v2]], mean)[i]
    MSE <- sum((predG3 - testset$G3[testset[,numVars[v2]]
                                     == rownames(tapply(testset$G3, testset[,numVars[v2]], mean))[i]])^2)
    MSEsum <- MSEsum + MSE
  }
  RMSE3 <- sqrt(MSEsum/dim(testset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", numVars[v2], "Test", RMSE3))
}


#################################################################################################
#Q.1-1) 모든 변수들에 대해서 모델을 만들고 calibration set에 대해 성능을 측정하여라.

#각 

print("RMSE of Categorical Variables")
scope <- length(catVars)
for (v in 1:scope){
  #v는 각 변수의 넘버
  makePregG3cat(stuTrain, stuCal, stuTest, v)
}


print("RMSE of Numaric Variables")
scope <- length(numVars)
for (v2 in 1:scope){
  #v는 각 변수의 넘버
  makePregG3num(stuTrain, stuCal, stuTest, v2)
}









#Q.1-2) 가장 좋은 모델은 어떤 변수를 어떻게 사용해서 만들었는지를 설명하라.

#위의 모델로 측정한 성능이 가장 좋은 변수는
#Categorical 에서는는 paid, RMSE : 0.220000" 이다.
#Numeric 에서는 "Variable Name : studytime, RMSE : 0.272656"이다.


#어떻게 만들었는지는 똑같은 모델이므로 한 번만 설명하겠다.
#makePregG3cat 함수의 내용을 보면
makePregG3cat <- function(dataset, v)
#아규먼트로 데이터셋(여기서는 stuCal)과 변수의 순번 v(여기서는 paid의 순번인 13)을를 주면
#성능을 출력해주는 함수이다.
  
  
varTab <- table(varTab = stuCal[,catVars[13]], stuCal[,outcome],useNA='ifany')
#해당 변수에 대해 테이블을 만든다. outcome으로는 G1과 G2 값의 평균인 scoremean을 할당한다.
#각 변수에 대한 밸류 마다의 예측값과 실제값을 RMSE로 측정할 것이다.
scoreVector <- as.numeric(colnames(varTab))
#해당 변수가 획득한 outcome 점수에 대한 목록을 만든다
rownum <- dim(varTab)[1]
#반복할 행의 갯수 
MSEsum <- 0
#계산에 사용될 sum 변수인데 전역변수로 선언해준다

for (i in 1:rownum){
  total <- sum(varTab[i,]*scoreVector)
  predG3 <- total/sum(varTab[i,])
  realG3 <- mean(stuCal$G3[stuCal[catVars[13]] == rownames(varTab)[i]])
  MSE <- (predG3 - realG3)^2
  #outcome으로 예측한 G3 값과 실제 G3 값의 차이
  MSEsum <- MSEsum + MSE
  #각 행마다의 MSE를 모두 더하여준다
}

RMSE <- sqrt(MSEsum/rownum)
#모두 더한 MSE 값에 루트를 씌워 RMSE를 구한다.
print(sprintf("Variable Name : %s, RMSE : %f", catVars[13], RMSE))
#결과를 출력한다.





#Q.1-3) 성능이 좋지 않게 나온 모델들에 대해서 왜 성능이 좋지 않았는지를 설명하라





#Q.1-4) 2)에서 얻은 모델을 test set에 대해서 성능을 측정하여 보라.


#1 Test Set에서 Categorical Variable들에 대한 성능 측정


#2 Test Set에서 Numeric Variable들에 대한 성능 측정




Q.2-1) 최종 모델에 대해서 어떻게 성능을 향상 시켰는지, 그렇게 한 이유는 무엇인지, 최종 성능은 어떤지를 설명하세요
Q.2-2) 최종 모델을 그림으로 나타내세요.




