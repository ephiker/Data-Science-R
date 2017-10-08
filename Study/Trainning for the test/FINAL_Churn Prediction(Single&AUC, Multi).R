
#churn = 해당 고객이 서비스를 끊을 예정인 상태
#churn을 미리 예측한다면 서비스, 비즈니스에 좋
#주어진 변수만으로 churn을 예측하는 모델 만들어야 한다
library(gmodels)


#Loading data into R
setwd("C:/Users/andre/Desktop/Rcode")
d <- read.table('orange_small_train.data.gz', header=T, sep='\t', na.strings=c('NA',''))

churn <- read.table('orange_small_train_churn.labels.txt', header=F, sep='\t')
d$churn <- churn$V1

appetency <- read.table('orange_small_train_appetency.labels.txt', header=F,sep='\t')
d$appetency <- appetency$V1

upselling <- read.table('orange_small_train_upselling.labels.txt', header=F,sep='\t')
d$upselling <- upselling$V1



# variables in the data
names(d)




#Data Partitioning ???Training and Test dataset
#d에서dTrainAll과 dTest로 분리
set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9) #dTrainAll 생성
dTest <- subset(d,rgroup>0.9) #dTest 생성

nrow((dTrainAll))
nrow((dTest))



#Separating variables from target variable 1(train test)
outcomes=c('churn','appetency','upselling') 
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
#dTrainAll에서 Variable들 이름만 남김

catVars <- vars[sapply(dTrainAll[,vars],class) %in% c('factor','character')]
#dTrainAll에서 카테고리컬 Variable 이름만 남김

numericVars <- vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]
#dTrainAll에서 누메릭 Variable 이름만 남김

rm(list=c('d','churn','appetency','upselling'))
#d에서dTrainAll과 dTest로 분리했으므로 d 삭제 

outcome <- 'churn'
pos <- '1'


outcome #뽑아낼 아웃컴은 churn
catVars #변수를 카테고리컬과 누메릭 두 가지로 나눔
numericVars #변수를 카테고리컬과 누메릭 두 가지로 나눔



#Separating variables from target variable 2(calibratiion)
#train으로부터 calibration 분리
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)

nrow((dTest))
nrow((dTrainAll))
nrow((dTrain))
nrow((dCal))






#########################################
#######Real Single Variable Model########
#########################################

#Create Pivot table : churn을 예측하기 위해서 Train Data 사용, 변수 Var218 사용

table218 <- table(Var218 = dTrain[,'Var218'], churn=dTrain[,outcome],useNA='ifany')
#테이블 만들기 : table(테이블 이름 = rowname이 될 항목, colname이 될 항목, NA처리)
#표 안의 내용은 churn col의 내용이며 1, -1의 숫자가 있음. 1이 pos 라고 위에서 정의내림
print(table218)

print(table218[,2]/(table218[,1]+table218[,2]))
#분자가 되는 2번째 col이 예측하려고 하는 churn이고 분모는 모든 값임. 각 row별로 확률 계산.




#Predict based on Pivot table
pPos <- sum(dTrain[,outcome] == pos) / nrow(dTrain)
#train 전체에서 churn이 1인 비율


naTab <- table(as.factor(dTrain[is.na(dTrain$Var218),outcome]))
#
pPosWna <- (naTab / sum(naTab))[pos]
#train 중 value가 NA일 때 churn이 1인 비율


vTab <- table(as.factor(dTrain[,outcome]), dTrain$Var218)
pPosWv <- vTab[pos, ] / colSums(vTab)
#train 중 Value 별로 churn이 1인 비율


pred <- pPosWv[dCal$Var218]
pred[is.na(dCal$Var218)] <- pPosWna
#dCal 내부 NA값에 Train 한 비율 대입
pred



#For repetitive job, make prediction function

mkPredC <- function(outCol,varCol,appCol) {
  #outCol, varCol: training 용 label과 variable
  #appCol: 실제 예측 을수행할 데이터의 variable
  
  pPos <- sum(outCol==pos)/length(outCol)
  #train 전체에서 churn이 1인 비율
  
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  #train 중 value가 NA일 때 churn이 1인 비율
  

  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  #train 중 Value 별로 churn이 1인 비율
  
  
  pred <- pPosWv[appCol]
  #실제 예측 수행항 데이터 appCol에 적용
  pred[is.na(appCol)] <- pPosWna
  #실제 예측 수행한 데이터 내부 NA값에 Train 한 비율 대입
  
  
  pred[is.na(pred)] <- pPos
  #???training 때없었던값이appCol에나타나는경우NA <-pPos

  pred
}





###########################
###성능측정################
###########################

#성능측정 방법1 : F1 score

#성능 측정을 위한 크로스테이블 만들기!

#쓰레시홀드설정
threshold <- 0.06

#새로운 예측열 만들어 채우기
for (i in 1:4510){
  if (pred[i] > threshold){
    dCal$pred.churn[i] <- 1
  }
  else {
    dCal$pred.churn[i] <- -1
  }
}

#크로스테이블
ctable1<-CrossTable(dCal$churn, dCal$pred.churn, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual churn', 'predicted churn'))


f1score(ctable1, -1)

f1score<-function(ctable, direction){
  #make dafa frame
  cdf<-as.data.frame(ctable)
  View(cdf)
  
  #extract
  a<-cdf[1,3]
  c<-cdf[2,3]
  b<-cdf[3,3]
  d<-cdf[4,3]
  
  #precision and recall and score
  precision1 <- a/(a+c)
  precision2 <- d/(b+d)
  recall1<- a/(a+b)
  recall2<- d/(c+d)
  score1 <- 2*(precision1*recall1)/(precision1+recall1)
  score2 <- 2*(precision2*recall2)/(precision2+recall2)
  
  #print
  if (direction == 1){
    real.score<-score1
    print(sprintf("F1Score : %4.3f", real.score))
    
  }
  if (direction == -1){
    real.score<-score2
    print(sprintf("F1Score : %4.3f", real.score))
    
  }
}





#Precision (1이라고 예측한 것 중에 실제 1의 비율)
#1이라고 예측함 = 0.452 + 0.040
#예측도 1이고 실제도 1임 = 0.040
precision1 <- 0.040/0.452+0.040


#Recall (실제 1 중에 1이라고 예측한 것의 비율)
#예측도 1이고 실제도 1임 = 0.040
#실제 1임 = 0.033 + 0.040
recall1 <- 0.040/0.040+0.033



f1score <- function(pre, rec){
  f1<-2*(pre*rec)/(pre+rec)
  print(sprintf("F1SCORE : %4.3f ",f1))
} 

f1score(precision1, recall1)





###성능측정 방법 2 : AUC
###Test with all categorical variables(AUC)

#데이터로 예측 수행
for(v in catVars) {
  pi <- paste('pred',v,sep='') #pi는 variable들의 이름
  #mkPredC(a, b, c) a=트레이닝에서 churn // b=트레이닝에서 변수들 // c=예측수행할 데이터 
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}


#AUC 평가 라이브러리 설치
install.packages('ROCR')
library('ROCR')


#AUC 평가 함수 작성
calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}




#Train AUC 순서로 정렬
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f", pi,aucTrain,aucCal))
  }
}




length(unique(dTrain$Var218))
#지금까지 사용한 변수인 var218의 level 수는 3개이기 때문에 좋은 정확성을 가진다.
#레벨이 적을수록 좋은 정확성을 가진다. 




#####################Single#########################
################Numeric Variable####################
####################################################

#discretize numeric variables
#구간나누기
mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol,probs=seq(0, 1, 0.1),na.rm=T)))
  #varCol을 10분위수로 나눔
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
}


for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.55) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f", pi, aucTrain,aucCal))
  }
}


#변수126에 대한 density plot
install.packages('ggplot2')
library('ggplot2')
ggplot(data=dCal) + geom_density(aes(x=predVar126, color=as.factor(churn)))






####################################################
#############Multi Variable Model###################
####################################################
#############Variable Selelction####################


logLikelyhood <- function(outCol,predCol) {
  sum(ifelse(outCol==pos,log(predCol),log(1-predCol)))
}

selVars <- c()
minStep <- 5
baseRateCheck <- logLikelyhood(dCal[,outcome],sum(dCal[,outcome]==pos)/length(dCal[,outcome]))


#카테고리컬 밸류 스코어
for(v in catVars){
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi])-baseRateCheck))
  if(liCheck>minStep){
    print(sprintf("%s. calibrationScore: %g",pi,liCheck))
    selVars <- c(selVars,pi)
  }
}

#누메릭 밸류 스코어
for(v in numericVars) { 
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
                   baseRateCheck))
  if(liCheck>=minStep) {
    print(sprintf("%s, calibrationScore: %g",
                  pi,liCheck))
    selVars <- c(selVars,pi)
  }
}






############의사결정나무(Rpart)
install.packages("rpart")
library('rpart')

class(c(catVars, numericVars))
#방법1 : 기본트리
fv1<-paste(outcome, '>0 ~ ', paste(c(catVars, numericVars), collapse = ' + '), sep = '')
#fv는formula
tmodel <- rpart(fv1, data=dTrain)

#print(tmodel)
print(calcAUC(predict(tmodel, newdata=dTrain), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dTest), dTest[,outcome]))
print(calcAUC(predict(tmodel, newdata=dCal), dCal[,outcome]))



#방법2 : 명목 변수들을 수치 형태로 변환한 트리
tVars <- paste('pred', c(catVars, numericVars),sep = '')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')
tmodel <- rpart(fV2,data=dTrain)
#print(tmodel)

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))





#방법3 : 컨트롤 한 트리
fv3 <- paste(outcome,'>0 ~ ',paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(fv3,data=dTrain,
                control=rpart.control(cp=0.00007,
                                      minsplit=1000,
                                      minbucket=1000,
                                      maxdepth=13))

#tmodel <- rpart(fV2,data=dTrain, control=rpart.control
#                (cp=0.001,minsplit=1000,minbucket=1000,maxdepth=5))

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


par(cex=0.5)
plot(tmodel)
text(tmodel)




#방법 4 : 선택된 변수만 사용
fv4 <- paste(outcome,'>0 ~ ', paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(fv4,data=dTrain, control=rpart.control(cp=0.001,minsplit=1000,
                                                     minbucket=1000,maxdepth=5))

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome])) 
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))




#####################################################################


###########################
###성능측정################
###########################

#성능측정 방법1 : F1 score

#성능 측정을 위한 크로스테이블 만들기!
library(gmodels)

pred.churn2<-predict(tmodel, newdata=dCal)
#predict 함수(중요)
threshold2<-0.06
length(pred.churn2)

for (i in 1:4510){
  if (pred.churn2[i] > threshold){
    dCal$pred.churn2[i] <- 1
  }
  else {
    dCal$pred.churn2[i] <- -1
  }
}


#예측값
dCal$pred.churn2

#실제값
dCal$churn

#크로스테이블
CrossTable(dCal$churn, dCal$pred.churn2, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual churn', 'predicted churn'))


#Precision (1이라고 예측한 것 중에 실제 1의 비율)
#1이라고 예측함 = 0.091 + 0.008
#예측도 1이고 실제도 1임 = 0.008
precision2 <- 0.008/(0.091+0.008)


#Recall (실제 1 중에 1이라고 예측한 것의 비율)
#예측도 1이고 실제도 1임 = 0.040
#실제 1임 = 0.033 + 0.065
recall2 <- 0.008/(0.008+0.065)



f1score <- function(pre, rec){
  f1<-2*(pre*rec)/(pre+rec)
  print(sprintf("F1SCORE : %4.3f ",f1))
} 

f1score(precision2, recall2)




#성능측정 방법2 : AUC

#AUC 평가 라이브러리 설치
install.packages('ROCR')
library('ROCR')


pred.churn2<-predict(tmodel, newdata=dCal)

performance(prediction(tmodel, newdata=dCal), 'auc')

#AUC 평가 함수 작성
calcAUC <- function(predcol, outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}

perfor<-performance(prediction(pred.churn2, ),'auc')







###############################
########C50####################

#model training
install.packages("C50")
library(C50)


colnames(dCal)
#making tree with C5.0
churn_model <- C5.0(dCal[-231], dCal$churn)
#C5.0(DEFAULT 빼고 남은 데이터프레임 전부 다, 분류기준이 되는 변수)
#로지컬을 팩터로 혹은 로지컬 변수들을 확인 후에 다 지운다!


logicVars <- vars[sapply(dTrainAll[,vars],class) %in% c('logical')]
head(d$Var8)
head(d$Var15)
head(d$Var20)
head(d$Var31)
head(d$Var32)
head(d$Var39)
head(d$Var42)
head(d$Var48)


###크로스테이블그리기

credit_pred<-predict(credit_model, credit_test)
#예측값 추출 = 예측함수 predict(트레이닝 한 트리모델, 테스트 셋 대입)

credit_pred
#예측한 default 값

credit_test$default
#테스트 셋의 실제 default 값


library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq =  FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#cross table까지 만들어야 진짜 예측 가능
#x = 실제 테스트 셋의 default 값, y = 예측 default 값











