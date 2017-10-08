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
#d����dTrainAll�� dTest�� �и�
set.seed(2100034)
studata$rgroup <- runif(dim(studata)[[1]])
stuTrainAll <- subset(studata,rgroup<=0.9) #stuTrainAll ����
stuTest <- subset(studata,rgroup>0.9) #stuTest ����
nrow((stuTrainAll))
nrow((stuTest))

##############################################################################################
#Separating variables from target variable 1(train test)
vars <- setdiff(colnames(stuTrainAll), c('rgroup'))
#stuTrainAll���� �̾Ƴ� Variable�� �̸���

catVars <- vars[sapply(stuTrainAll[,vars],class) %in% c('factor','character')]
#dTrainAll���� �̾Ƴ� ī�װ����� Variable �̸���

numericVars <- vars[sapply(stuTrainAll[,vars],class) %in% c('numeric','integer')]
#dTrainAll���� �̾Ƴ� ���޸� Variable �̸���

catVars #������ ī�װ����ð� ���޸� �� ������ ����
numericVars #������ ī�װ����ð� ���޸� �� ������ ����
remove<-c("G1", "G2", "G3", "scoremean")
numVars<-setdiff(numericVars,remove)

####################################################################################333

#Separating variables from target variable 2(calibratiion)
#train���κ��� calibration �и� 90%, 10%
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
  #predG3 <- total/sum(varTab[,]) #��� ���� �� ������ ��� �ο��� 

  MSEsum <- 0
  #Train RMSE
  for (i in 1:rownum){
    total <- sum(varTab[i,]*scoreVector)
    predG3 <- total/sum(varTab[i,]) #��� ���� �� ������ ��� �ο���
    MSE <- sum((predG3 - trainset$G3[trainset[catVars[v]] == rownames(varTab)[i]])^2)
    MSEsum <- MSEsum + MSE 
  }
  RMSE1 <- sqrt(MSEsum/dim(trainset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", catVars[v], "Train", RMSE1))


  MSEsum <- 0
  #Cal RMSE
  for (i in 1:rownum){
    total <- sum(varTab[i,]*scoreVector)
    predG3 <- total/sum(varTab[i,]) #��� ���� �� ������ ��� �ο���
    MSE <- sum((predG3 - calset$G3[calset[catVars[v]] == rownames(varTab)[i]])^2)
    MSEsum <- MSEsum + MSE
  }
  RMSE2 <- sqrt(MSEsum/dim(calset)[1])
  print(sprintf("%-10s %-10s RMSE = %f", catVars[v], "Cali", RMSE2))
  
  
  MSEsum <- 0
  #Test RMSE
  for (i in 1:rownum){
    total <- sum(varTab[i,]*scoreVector)
    predG3 <- total/sum(varTab[i,]) #��� ���� �� ������ ��� �ο���
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
#Q.1-1) ��� �����鿡 ���ؼ� ���� ����� calibration set�� ���� ������ �����Ͽ���.

#�� 

print("RMSE of Categorical Variables")
scope <- length(catVars)
for (v in 1:scope){
  #v�� �� ������ �ѹ�
  makePregG3cat(stuTrain, stuCal, stuTest, v)
}


print("RMSE of Numaric Variables")
scope <- length(numVars)
for (v2 in 1:scope){
  #v�� �� ������ �ѹ�
  makePregG3num(stuTrain, stuCal, stuTest, v2)
}









#Q.1-2) ���� ���� ���� � ������ ��� ����ؼ� ����������� �����϶�.

#���� �𵨷� ������ ������ ���� ���� ������
#Categorical �����´� paid, RMSE : 0.220000" �̴�.
#Numeric ������ "Variable Name : studytime, RMSE : 0.272656"�̴�.


#��� ����������� �Ȱ��� ���̹Ƿ� �� ���� �����ϰڴ�.
#makePregG3cat �Լ��� ������ ����
makePregG3cat <- function(dataset, v)
#�ƱԸ�Ʈ�� �����ͼ�(���⼭�� stuCal)�� ������ ���� v(���⼭�� paid�� ������ 13)���� �ָ�
#������ ������ִ� �Լ��̴�.
  
  
varTab <- table(varTab = stuCal[,catVars[13]], stuCal[,outcome],useNA='ifany')
#�ش� ������ ���� ���̺��� �����. outcome���δ� G1�� G2 ���� ����� scoremean�� �Ҵ��Ѵ�.
#�� ������ ���� ��� ������ �������� �������� RMSE�� ������ ���̴�.
scoreVector <- as.numeric(colnames(varTab))
#�ش� ������ ȹ���� outcome ������ ���� ����� �����
rownum <- dim(varTab)[1]
#�ݺ��� ���� ���� 
MSEsum <- 0
#��꿡 ���� sum �����ε� ���������� �������ش�

for (i in 1:rownum){
  total <- sum(varTab[i,]*scoreVector)
  predG3 <- total/sum(varTab[i,])
  realG3 <- mean(stuCal$G3[stuCal[catVars[13]] == rownames(varTab)[i]])
  MSE <- (predG3 - realG3)^2
  #outcome���� ������ G3 ���� ���� G3 ���� ����
  MSEsum <- MSEsum + MSE
  #�� �ึ���� MSE�� ��� ���Ͽ��ش�
}

RMSE <- sqrt(MSEsum/rownum)
#��� ���� MSE ���� ��Ʈ�� ���� RMSE�� ���Ѵ�.
print(sprintf("Variable Name : %s, RMSE : %f", catVars[13], RMSE))
#����� ����Ѵ�.





#Q.1-3) ������ ���� �ʰ� ���� �𵨵鿡 ���ؼ� �� ������ ���� �ʾҴ����� �����϶�





#Q.1-4) 2)���� ���� ���� test set�� ���ؼ� ������ �����Ͽ� ����.


#1 Test Set���� Categorical Variable�鿡 ���� ���� ����


#2 Test Set���� Numeric Variable�鿡 ���� ���� ����




Q.2-1) ���� �𵨿� ���ؼ� ��� ������ ��� ���״���, �׷��� �� ������ ��������, ���� ������ ����� �����ϼ���
Q.2-2) ���� ���� �׸����� ��Ÿ������.



