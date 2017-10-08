
#churn = �ش� ������ ���񽺸� ���� ������ ����
#churn�� �̸� �����Ѵٸ� ����, ����Ͻ��� ��
#�־��� ���������� churn�� �����ϴ� �� ������ �Ѵ�
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



#Separating variables from target variable 2(calibratiion)
#train���κ��� calibration �и�
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
pred



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





###########################
###��������################
###########################

#�������� ���1 : F1 score

#���� ������ ���� ũ�ν����̺� �����!

#������Ȧ�弳��
threshold <- 0.06

#���ο� ������ ����� ä���
for (i in 1:4510){
  if (pred[i] > threshold){
    dCal$pred.churn[i] <- 1
  }
  else {
    dCal$pred.churn[i] <- -1
  }
}

#ũ�ν����̺�
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





#Precision (1�̶�� ������ �� �߿� ���� 1�� ����)
#1�̶�� ������ = 0.452 + 0.040
#������ 1�̰� ������ 1�� = 0.040
precision1 <- 0.040/0.452+0.040


#Recall (���� 1 �߿� 1�̶�� ������ ���� ����)
#������ 1�̰� ������ 1�� = 0.040
#���� 1�� = 0.033 + 0.040
recall1 <- 0.040/0.040+0.033



f1score <- function(pre, rec){
  f1<-2*(pre*rec)/(pre+rec)
  print(sprintf("F1SCORE : %4.3f ",f1))
} 

f1score(precision1, recall1)





###�������� ��� 2 : AUC
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
calcAUC <- function(predcol, outcol) {
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




#####################Single#########################
################Numeric Variable####################
####################################################

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


#����126�� ���� density plot
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


#ī�װ����� ��� ���ھ�
for(v in catVars){
  pi <- paste('pred',v,sep='')
  liCheck <- 2*((logLikelyhood(dCal[,outcome],dCal[,pi])-baseRateCheck))
  if(liCheck>minStep){
    print(sprintf("%s. calibrationScore: %g",pi,liCheck))
    selVars <- c(selVars,pi)
  }
}

#���޸� ��� ���ھ�
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






############�ǻ��������(Rpart)
install.packages("rpart")
library('rpart')

class(c(catVars, numericVars))
#���1 : �⺻Ʈ��
fv1<-paste(outcome, '>0 ~ ', paste(c(catVars, numericVars), collapse = ' + '), sep = '')
#fv��formula
tmodel <- rpart(fv1, data=dTrain)

#print(tmodel)
print(calcAUC(predict(tmodel, newdata=dTrain), dTrain[,outcome]))
print(calcAUC(predict(tmodel, newdata=dTest), dTest[,outcome]))
print(calcAUC(predict(tmodel, newdata=dCal), dCal[,outcome]))



#���2 : ���� �������� ��ġ ���·� ��ȯ�� Ʈ��
tVars <- paste('pred', c(catVars, numericVars),sep = '')
fV2 <- paste(outcome,'>0 ~ ',paste(tVars,collapse=' + '),sep='')
tmodel <- rpart(fV2,data=dTrain)
#print(tmodel)

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))





#���3 : ��Ʈ�� �� Ʈ��
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




#��� 4 : ���õ� ������ ���
fv4 <- paste(outcome,'>0 ~ ', paste(selVars,collapse=' + '),sep='')
tmodel <- rpart(fv4,data=dTrain, control=rpart.control(cp=0.001,minsplit=1000,
                                                     minbucket=1000,maxdepth=5))

print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome])) 
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))




#####################################################################


###########################
###��������################
###########################

#�������� ���1 : F1 score

#���� ������ ���� ũ�ν����̺� �����!
library(gmodels)

pred.churn2<-predict(tmodel, newdata=dCal)
#predict �Լ�(�߿�)
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


#������
dCal$pred.churn2

#������
dCal$churn

#ũ�ν����̺�
CrossTable(dCal$churn, dCal$pred.churn2, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual churn', 'predicted churn'))


#Precision (1�̶�� ������ �� �߿� ���� 1�� ����)
#1�̶�� ������ = 0.091 + 0.008
#������ 1�̰� ������ 1�� = 0.008
precision2 <- 0.008/(0.091+0.008)


#Recall (���� 1 �߿� 1�̶�� ������ ���� ����)
#������ 1�̰� ������ 1�� = 0.040
#���� 1�� = 0.033 + 0.065
recall2 <- 0.008/(0.008+0.065)



f1score <- function(pre, rec){
  f1<-2*(pre*rec)/(pre+rec)
  print(sprintf("F1SCORE : %4.3f ",f1))
} 

f1score(precision2, recall2)




#�������� ���2 : AUC

#AUC �� ���̺귯�� ��ġ
install.packages('ROCR')
library('ROCR')


pred.churn2<-predict(tmodel, newdata=dCal)

performance(prediction(tmodel, newdata=dCal), 'auc')

#AUC �� �Լ� �ۼ�
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
#C5.0(DEFAULT ���� ���� ������������ ���� ��, �з������� �Ǵ� ����)
#�������� ���ͷ� Ȥ�� ������ �������� Ȯ�� �Ŀ� �� �����!


logicVars <- vars[sapply(dTrainAll[,vars],class) %in% c('logical')]
head(d$Var8)
head(d$Var15)
head(d$Var20)
head(d$Var31)
head(d$Var32)
head(d$Var39)
head(d$Var42)
head(d$Var48)


###ũ�ν����̺��׸���

credit_pred<-predict(credit_model, credit_test)
#������ ���� = �����Լ� predict(Ʈ���̴� �� Ʈ����, �׽�Ʈ �� ����)

credit_pred
#������ default ��

credit_test$default
#�׽�Ʈ ���� ���� default ��


library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq =  FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#cross table���� ������ ��¥ ���� ����
#x = ���� �׽�Ʈ ���� default ��, y = ���� default ��










