
#churn = �ش� ������ ���񽺸� ���� ������ ����
#churn�� �̸� �����Ѵٸ� ����, ����Ͻ��� ��
#�־��� ���������� churn�� �����ϴ� �� ������ �Ѵ�


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


#####################218

#Create Pivot table : churn�� �����ϱ� ���ؼ� Train Data ���, ���� Var218 ���

table218 <- table(Var218 = dTrain[,'Var218'], churn=dTrain[,outcome],useNA='ifany')
#���̺� ����� : table(���̺� �̸� = rowname�� �� �׸�, colname�� �� �׸�, NAó��)
#ǥ ���� ������ churn col�� �����̸� 1, -1�� ���ڰ� ����. 1�� pos ��� ������ ���ǳ���
print(table218)

print(table218[,2]/(table218[,1]+table218[,2]))
#���ڰ� �Ǵ� 2��° col�� �����Ϸ��� �ϴ� churn�̰� �и�� ��� ����. �� row���� Ȯ�� ���.




#Predict based on Pivot table
pPos <- sum(dTrain[,outcome] == pos) / nrow(dTrain)
#train ��ü���� churn�� 1�� ����


naTab <- table(as.factor(dTrain[is.na(dTrain$Var218),outcome]))
#
pPosWna <- (naTab / sum(naTab))[pos]

#train �� value�� NA�� �� churn�� 1�� ����


vTab <- table(as.factor(dTrain[,outcome]), dTrain$Var218)
pPosWv <- vTab[pos, ] / colSums(vTab)
#train �� Value ���� churn�� 1�� ����


pred <- pPosWv[dCal$Var218]
pred[is.na(dCal$Var218)] <- pPosWna
#dCal ���� NA���� Train �� ���� ����

summary(pred)






#For repetitive job, make prediction function

mkPredC <- function(outCol,varCol,appCol) {
  #outCol, varCol: training �� label�� variable
  #appCol: ���� ���� �������� �������� variable
  
  pPos <- sum(outCol==pos)/length(outCol)
  #train ��ü���� churn�� 1�� ����
  
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  #train �� value�� NA�� �� churn�� 1�� ����
  

  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  #train �� Value ���� churn�� 1�� ����
  
  
  pred <- pPosWv[appCol]
  #���� ���� ������ ������ appCol�� ����
  pred[is.na(appCol)] <- pPosWna
  #���� ���� ������ ������ ���� NA���� Train �� ���� ����
  
  
  pred[is.na(pred)] <- pPos
  #???training ������������appCol����Ÿ���°��NA <-pPos

  pred
}





###Test with all categorical variables(AUC)

#�����ͷ� ���� ����
for(v in catVars) {
  pi <- paste('pred',v,sep='') #pi�� variable���� �̸�
  #mkPredC(a, b, c) a=Ʈ���̴׿��� churn // b=Ʈ���̴׿��� ������ // c=���������� ������ 
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}


#AUC �� ���̺귯�� ��ġ
install.packages('ROCR')
library('ROCR')


#AUC �� �Լ� �ۼ�
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}



#Train AUC ������ ����
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f", pi,aucTrain,aucCal))
  }
}




length(unique(dTrain$Var218))
#���ݱ��� ����� ������ var218�� level ���� 3���̱� ������ ���� ��Ȯ���� ������.
#������ �������� ���� ��Ȯ���� ������. 


cuts <- unique(as.numeric(quantile(dTrain$age,probs=seq(0, 1, 0.1),na.rm=T)))
varC <- cut(dTrain$numericVars,cuts)

dTrain$Var218
numericVars


#discretize numeric variables
#����������
mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol,probs=seq(0, 1, 0.1),na.rm=T)))
  #varCol�� 10�������� ����
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
#fv��formula
tmodel <- rpart(fv, data=dTrain)
#Ʈ������ ���Ķ�, ������ �� �� ������ �ƱԸ�Ʈ�� ��
print(tmodel)
print(calcAUC(predict(tmodel, newdata=dTrain), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dTest), dTest[,outcome]))
print(calcAUC(predict(tmodel, newdata=dCal), dCal[,outcome]))






#���� �������� ��ġ ���·� ��ȯ
tVars <- paste('pred', c(catVars, numericVars),sep = '')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')
tmodel <- rpart(fV2,data=dTrain)
print(tmodel)

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#rpart ��Ʈ�� ���� ���
tmodel <- rpart(fV2,data=dTrain, control=rpart.control
                (cp=0.001,minsplit=1000,minbucket=1000,maxdepth=5))

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))


#���õ� ������ ���########�ٽ�!
f <- paste(outcome,'>0 ~ ', paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(f,data=dTrain, control=rpart.control(cp=0.001,minsplit=1000,
                                                     minbucket=1000,maxdepth=5))
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome])) 
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))



