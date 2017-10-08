#######################multi variable



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





#######################################
#Decision tree
library('rpart')
fv<-paste(outcome, '>0 ~ ', paste(c(catVars, numericVars), collapse = ' + '), sep = '')
tmodel <- rpart(fv, data=dTrain)
print(tmodel)
print(calcAUC(predict(tmodel, newdata=dTrain), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dTest), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dCal), dTrain[,outcome]))






#명목 변수들을 수치 형태로 변환
tVars <- paste('pred', c(catVars, numericVars),sep = '')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')

tmodel <- rpart(fV2,data=dTrain)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#rpart 컨트롤 인자 사용
tmodel <- rpart(fV2,data=dTrain, control=rpart.control
                (cp=0.001,minsplit=1000,minbucket=1000,maxdepth=5))

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#선택된 변수만 사용
f <- paste(outcome,'>0 ~ ', paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(f,data=dTrain, control=rpart.control(cp=0.001,minsplit=1000,
                                                     minbucket=1000,maxdepth=5))
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome])) 
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


