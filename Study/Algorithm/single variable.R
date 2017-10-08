
#churn = 해당 고객이 서비스를 끊을 예정인 상태
#churn을 미리 예측한다면 서비스, 비즈니스에 좋
#주어진 변수만으로 churn을 예측하는 모델 만들어야 한다


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



dTrainAll

#Separating variables from target variable 2(calibratiion)
#train으로부터 calibration 분리
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)
nrow((dTrainAll))
nrow((dTrain))
nrow((dCal))
nrow((dTest))
names(dTrain)
names(dTest)


#####################218

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

summary(pred)






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
calcAUC <- function(predcol,outcol) {
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


cuts <- unique(as.numeric(quantile(dTrain$age,probs=seq(0, 1, 0.1),na.rm=T)))
varC <- cut(dTrain$numericVars,cuts)

dTrain$Var218
numericVars


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


x = c(1,2,3,4,5)
quantile(x, probs=seq(0,1,0.5), na.rm = FALSE)

dTrain$Var218













#########################################################################
###########  Variable Selelction

logLikelyhood <- function(outCol,predCol) {
  sum(ifelse(outCol==pos,log(predCol),log(1-predCol)))
} 
selVars <- c()
minStep <- 5
baseRateCheck <- logLikelyhood(dCal[,outcome],
                               sum(dCal[,outcome]==pos)/length(dCal[,outcome]))
for(v in catVars) { 
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi]) -
                   baseRateCheck))
  if(liCheck>minStep) {
    print(sprintf("%s, calibrationScore: %g",
                  pi,liCheck))
    selVars <- c(selVars,pi)
  }
}


#Decision tree
library('rpart')
fv<-paste(outcome, '>0 ~ ', paste(c(catVars, numericVars), collapse = ' + '), sep = '')
#fv는formula
tmodel <- rpart(fv, data=dTrain)
#트리모델은 포뮬라, 데이터 셋 두 가지를 아규먼트로 받
print(tmodel)
print(calcAUC(predict(tmodel, newdata=dTrain), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dTest), dTest[,outcome]))
print(calcAUC(predict(tmodel, newdata=dCal), dCal[,outcome]))






#명목 변수들을 수치 형태로 변환
tVars <- paste('pred', c(catVars, numericVars),sep = '')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')
tmodel <- rpart(fV2,data=dTrain)
print(tmodel)

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#rpart 컨트롤 인자 사용
tmodel <- rpart(fV2,data=dTrain, control=rpart.control
                (cp=0.001,minsplit=1000,minbucket=1000,maxdepth=5))

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#선택된 변수만 사용########다시!
f <- paste(outcome,'>0 ~ ', paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(f,data=dTrain, control=rpart.control(cp=0.001,minsplit=1000,
                                                     minbucket=1000,maxdepth=5))
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome])) 
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))




