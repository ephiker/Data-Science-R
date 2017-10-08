
setwd("C:/Users/andre/Desktop/Rcode")
d <- read.table('student-grade.csv', header=T, sep=',', na.strings=c('NA',''))
View(d)

#K nearest neighbor로 성적 예측
install.packages("gmodels")
install.packages("class")
library(gmodels)
library(class)

stuTrain <- d[1:700,]
stuTest <- d[701:1044,]

#나누기
numVars<-names(d)[sapply(stuTrain, class) %in% c('numeric', 'integer')]


train.data<-stuTrain[numVars][,-14]
test.data<-stuTest[numVars][,-14]


#노멀라이즈 안함
grade_pred<- knn(train=train.data, test=test.data, cl=stuTrain$G3, k=25)
grade_pred
#test set에 있는 학생들의 예상점수. 이 각각의 점수를 train set g3 평균과 비교하여 rmse 추출
grade_real<-stuTest$G3
rmse<-sqrt(mean((as.numeric(grade_real)-as.numeric(grade_pred))^2))
rmse

####################################################################
#노멀라이즈#
####################################################################
#노멀라이즈1
normalize.1<-function(x){return((x-min(x))/(max(x)-min(x)))}
d.n1 <- as.data.frame(lapply(d[numVars], normalize.1))

stuTrain.n1 <- as.data.frame(lapply(stuTrain[numVars], normalize.1))
stuTest.n1 <- as.data.frame(lapply(stuTest[numVars], normalize.1))

train.data.n1 <- stuTrain.n1[,-14]
test.data.n1 <- stuTest.n1[,-14]

#K nearest neighbor로 성적 예측
grade_pred.n1<- knn(train=train.data.n1, test=test.data.n1, cl=stuTrain.n1$G3, k=25)
head(grade_pred.n1)


grade_pred.n1.matrix<- as.matrix(grade_pred.n1)
#test set에 있는 학생들의 예상점수. 이 각각의 점수를 train set g3 평균과 비교하여 rmse 추출
grade_pred.n1.matrix
grade_real.n1<-stuTest.n1$G3

rmse.n1<-sqrt(mean((as.numeric(grade_real.n1)-as.numeric(grade_pred.n1.matrix))^2))
rmse.n1



##########################################################################################
#노멀라이즈2 


normalize.2<-function(x){return(x/median(x))}
d.n2 <- as.data.frame(lapply(d[numVars], normalize.2))

stuTrain.n2 <- as.data.frame(lapply(stuTrain[numVars], normalize.2))
stuTest.n2 <- as.data.frame(lapply(stuTest[numVars], normalize.2))
stuTrain.n2$failures <- stuTrain$failures
stuTest.n2$failures <- stuTest$failures


train.data.n2 <- stuTrain.n2[,-14]
test.data.n2 <- stuTest.n2[,-14]



#K nearest neighbor로 성적 예측
grade_pred.n2<- knn(train=train.data.n2, test=test.data.n2, cl=stuTrain.n2$G3, k=25)
head(grade_pred.n2)


grade_pred.n2.matrix<- as.matrix(grade_pred.n2)
#test set에 있는 학생들의 예상점수. 이 각각의 점수를 train set g3 평균과 비교하여 rmse 추출
grade_pred.n2.matrix

grade_pred.n2.matrix<- as.matrix(grade_pred.n2)
grade_real.n2<-stuTest.n2$G3

rmse.n2<-sqrt(mean((as.numeric(grade_real.n2)-as.numeric(grade_pred.n2.matrix))^2))
rmse.n2







#########################################################################


normalize.3<-function(x){return((x-mean(x))/(sd(x)))}
d.n3 <- as.data.frame(lapply(d[numVars], normalize.3))
stuTrain.n3 <- as.data.frame(lapply(stuTrain[numVars], normalize.3))
stuTest.n3 <- as.data.frame(lapply(stuTest[numVars], normalize.3))


train.data.n3 <- stuTrain.n3[,-14]
test.data.n3 <- stuTest.n3[,-14]

#K nearest neighbor로 성적 예측
grade_pred.n3<- knn(train=train.data.n3, test=test.data.n3, cl=stuTrain.n3$G3, k=25)
head(grade_pred.n3)


grade_pred.n3.matrix<- as.matrix(grade_pred.n3)
#test set에 있는 학생들의 예상점수. 이 각각의 점수를 train set g3 평균과 비교하여 rmse 추출
grade_pred.n3.matrix

grade_pred.n3.matrix<- as.matrix(grade_pred.n3)
grade_real.n3<-stuTest.n3$G3

rmse.n3<-sqrt(mean((as.numeric(grade_real.n3)-as.numeric(grade_pred.n3.matrix))^2))
rmse.n3





################################################################

#K 값 다르게

for (k in 3:12){
  grade_pred.n1<- knn(train=train.data.n1, test=test.data.n1, cl=stuTrain.n1$G3, k*5)
  grade_pred.n1.matrix<- as.matrix(grade_pred.n1)
  grade_real.n1<-stuTest.n1$G3
  rmse.n1<-sqrt(mean((as.numeric(grade_real.n1)-as.numeric(grade_pred.n1.matrix))^2))
  print(rmse.n1)
  print(sprintf("k = %-10s, RMSE = %-10f ", k*5, rmse.n1))
  
}


#그래프로 나타내기(노멀라이즈 1)
knnG3<-data.frame(K=seq(15, 60, 5), RMSE=c(0.215761, 0.209470, 0.211298, 0.206399, 0.206223,
                                           0.206336, 0.205339, 0.199551, 0.207081, 0.200853))

plot(RMSE ~ K, data = knnG3, type = 'b')




####################categorical
#knn 예측에서 사용하는 변수는 numeric이어야 한다.
#각각의 categorical 밸류들을 숫자로 바꿔주면 된다.
#숫자로 바꾼 뒤 모든 variable을 범위로 하여 KNN 예측을 해보고 성능을 측정해본다.
#정규화




#categorical Variable Calling
catVars<-names(d)[sapply(stuTrain, class) %in% c('factor','character')]
catVars



#각 Variable의 levels 파악하기
levels(d[catVars][,1])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,1])<-c(0,1)

#각 Variable의 levels 파악하기
levels(d[catVars][,2])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,2])<-c(0,1)

#각 Variable의 levels 파악하기
levels(d[catVars][,3])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,3])<-c(0,1)

#각 Variable의 levels 파악하기
levels(d[catVars][,4])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,4])<-c(0,1)

#각 Variable의 levels 파악하기
levels(d[catVars][,5])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,5])<-c(0,1)

#각 Variable의 levels 파악하기
levels(d[catVars][,6])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,6])<-c(0, 1/4, 1/2, 3/4, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,7])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,7])<-c(0, 1/4, 1/2, 3/4, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,8])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,8])<-c(0, 1/3, 2/3, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,9])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,9])<-c(0, 1/2, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,10])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,10])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,11])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,11])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,12])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,12])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,13])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,13])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,14])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,14])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,15])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,15])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,16])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,16])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,17])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,17])<-c(0, 1)

#각 Variable의 levels 파악하기
levels(d[catVars][,18])
#각 Variable의 levels 정규화된 숫자로 바꾸기
levels(d[catVars][,18])<-c(0, 1)


#나누기

stuTrain <- d[1:700,]
stuTest <- d[701:1044,]

train.data.all<-stuTrain[,-31]
test.data.all<-stuTest[,-31]


grade_pred<- knn(train=train.data.all, test=test.data.all, cl=stuTrain$G3, k=25)
grade_real<-stuTest$G3
rmse<-sqrt(mean((as.numeric(grade_real)-as.numeric(grade_pred))^2))
rmse





