install.packages("gmodels")
library(gmodels)
install.packages("class")
library(class)

#AUC 평가 라이브러리 설치
install.packages('ROCR')
library('ROCR')

install.packages('ggplot2')
library('ggplot2')

#데이터프레임만드는방법

#AUC 평가 함수 작성
calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}






#Loading data into R
setwd("C:/Users/andre/Desktop/Rcode")
train<-read.csv("trainData.csv")
test<-read.csv("testData.csv")

#making tree model with rpart
install.packages("rpart")
library('rpart')
target <- "default.payment.next.month"
f<-paste(target, '>0 ~ ', paste(colnames(train[-24]), collapse = ' + '), sep = '')
tmodel <- rpart(f, data=train)

par(cex=0.5)
plot(tmodel)
text(tmodel)


#making prediction
pred.default<-predict(tmodel, newdata=train)
summary(predict(tmodel, newdata=train))

#installing package
install.packages("gmodels")
library(gmodels)
install.packages('ROCR')
library('ROCR')



target <- "default.payment.next.month"

#function call


######성능개선시도
improving <- function(train, pred.default, threshold){
  target <- "default.payment.next.month"
  for (i in 1:25000){
    if (pred.default[i] >= threshold){
      train$pred.default[i] <- "TRUE"
    }
    if (pred.default[i] < threshold){
      train$pred.default[i] <- "FALSE"
    }
  }
  
  defaultTable<-CrossTable(train$default.payment.next.month, train$pred.default,
                           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
  
  print(sprintf("threshold : %4.3f", threshold))
  printF1Score(defaultTable, 'reverse')
}

improving(train, pred.default, threshold = 0.3)
#  print(sprintf("AUC : %4.3f", auctrain <- calcAUC(predict(tmodel, newdata = train), train[,target])))

#성능향상방법 3 : 컨트롤 한 트리 사용


tmodel <- rpart(f, data=train,
                control=rpart.control(cp=0.001,
                                      minsplit=1000,
                                      minbucket=1000,
                                      maxdepth=5))



improving(train, pred.default, threshold = 0.3)









#성능평가 F1score (도출함수는 맨 밑에 첨부)
printF1Score(defaultTable, 'reverse')

threshold = 0.5

for (i in 1:25000){
  if (pred.default[i] >= threshold){
    train$pred.default[i] <- "TRUE"
  }
  if (pred.default[i] < threshold){
    train$pred.default[i] <- "FALSE"
  }
}

#evaluating performance

#F1 Score 도출
#크로스테이블 작성


defaultTable<-CrossTable(train$default.payment.next.month, train$pred.default,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('actual default', 'predicted default'))

#성능평가 F1score (도출함수는 맨 밑에 첨부)
printF1Score(defaultTable, 'reverse')


#성능평가 AUC
print(auctrain <- calcAUC(predict(tmodel, newdata = train), train[,target]))




#####################


#성능향상방법 1 : Threshold 바꾸기
#Threshold 0.5일 때 성능 "F1Score : 0.452"
#Threshold 0.3일 때 성능 "F1Score : 0.523"

#성능향상방법 2 : 명목 변수들을 수치 형태로 변환한 트리 사용\
tVars <- paste('pred', c(catVars, numericVars),sep = '')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')
tmodel <- rpart(fV2,data=dTrain)


#성능향상방법 3 : 컨트롤 한 트리 사용
tmodel <- rpart(f, data=train)
tmodel <- rpart(f,data=dTrain,
                control=rpart.control(cp=0.00007,
                                      minsplit=1000,
                                      minbucket=1000,
                                      maxdepth=13))

coefficients(train)

#성능향상방법 4 : 선택된 변수만 사용




#최종 모델 명세
#기법 : decision tree with rpart package
#사용 변수 : 모든 변수
#default 확률에 대한 threshhold : 0.3
#training set에서의 성능 : 0.523





#####
#만든 모델 사용하여 test data 예측

View(test)

#making prediction and final data frame
final<-data.frame(prob=1:5000, pred=1:5000)
str(final)


pred.default2 <- predict(tmodel, newdata=test)
summary(pred.default2)



threshold = 0.3

for (i in 1:5000){
  #make prob column
  test$prob[i] <- pred.default2[i]
  
  #make pred column
  if (pred.default2[i] >= threshold){
    test$pred.default[i] <- "TRUE"
  }
  if (pred.default2[i] < threshold){
    test$pred.default[i] <- "FALSE"
  }
  
  #making final data frame
  final$prob[i]<-test$prob[i]
  final$pred[i]<-test$pred.default[i]
}


#making csv file
View(final)

write.csv(final, "FINAL_21000348서안드레.csv",row.names = T)
print(head(read.csv("FINAL_21000348서안드레.csv", header=T, sep=',')))

read




################################################################################
################################################################################


#direction은 크로스테이블에 대한 방향(순방향:1, 역방향:-1)
#make dafa frame
#F1 Score 도출함수


printF1Score<-function(ctable, direction){
  dframe<-as.data.frame(ctable)
  
  #extract a,b,c,d
  a<-dframe[1,3]
  b<-dframe[3,3]
  c<-dframe[2,3]
  d<-dframe[4,3]
  
  #precision and recall and f1 score
  precisionF <- a/(a+c)
  precisionR <- d/(b+d)
  recallF<- a/(a+b)
  recallR<- d/(c+d)
  scoreF <- 2*(precisionF*recallF)/(precisionF+recallF)
  scoreR <- 2*(precisionR*recallR)/(precisionR+recallR)
  
  #direction selecting
  if (direction == 'forward'){
    real.score <- scoreF
  }
  
  if (direction == 'reverse'){
    real.score <- scoreR
  }
  
  #print
  print(sprintf("F1 Score : %4.3f", real.score))
}




#AUC 평가 함수 작성

install.packages('ROCR')
library('ROCR')

calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(predcol,outcol==1),'auc')
  as.numeric(perf@y.values)
}















vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
#dTrainAll에서 Variable들 이름만 남김

catVars <- vars[sapply(dTrainAll[,vars],class) %in% c('factor','character')]
#dTrainAll에서 카테고리컬 Variable 이름만 남김

numericVars <- vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]
#dTrainAll에서 누메릭 Variable 이름만 남김


