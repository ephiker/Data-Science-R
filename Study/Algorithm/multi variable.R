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
#d����dTrainAll�� dTest�� �и�
set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9) #dTrainAll ����
dTest <- subset(d,rgroup>0.9) #dTest ����
nrow((dTrainAll))
nrow((dTest))




#Separating variables from target variable 1(train test)
outcomes=c('churn','appetency','upselling') 
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
#dTrainAll���� Variable�� �̸��� ����

catVars <- vars[sapply(dTrainAll[,vars],class) %in% c('factor','character')]
#dTrainAll���� ī�װ����� Variable �̸��� ����

numericVars <- vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]
#dTrainAll���� ���޸� Variable �̸��� ����

rm(list=c('d','churn','appetency','upselling'))
#d����dTrainAll�� dTest�� �и������Ƿ� d ���� 

outcome <- 'churn'
pos <- '1'


outcome #�̾Ƴ� �ƿ����� churn
catVars #������ ī�װ����ð� ���޸� �� ������ ����
numericVars #������ ī�װ����ð� ���޸� �� ������ ����



dTrainAll

#Separating variables from target variable 2(calibratiion)
#train���κ��� calibration �и�
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






#���� �������� ��ġ ���·� ��ȯ
tVars <- paste('pred', c(catVars, numericVars),sep = '')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')

tmodel <- rpart(fV2,data=dTrain)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#rpart ��Ʈ�� ���� ���
tmodel <- rpart(fV2,data=dTrain, control=rpart.control
                (cp=0.001,minsplit=1000,minbucket=1000,maxdepth=5))

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#���õ� ������ ���
f <- paste(outcome,'>0 ~ ', paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(f,data=dTrain, control=rpart.control(cp=0.001,minsplit=1000,
                                                     minbucket=1000,maxdepth=5))
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome])) 
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))

