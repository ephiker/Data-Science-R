setwd("C:/Users/andre/Desktop/Rcode")
d <- read.table('orange_small_train.data.gz', header=T, sep='\t', na.strings=c('NA',''))
head(d)
View(appetency)

churn <- read.table('orange_small_train_churn.labels.txt', header=F, sep='\t')
d$churn <- churn$V1
appetency <- read.table('orange_small_train_appetency.labels.txt', header=F,sep='\t')
d$appetency <- appetency$V1
upselling <- read.table('orange_small_train_upselling.labels.txt', header=F,sep='\t')
d$upselling <- upselling$V1
colnames(d)



dim(d)[[1]]
runif(dim(d)[[1]])[10]

set.seed(729375)
d$rgroup <- runif(dim(d)[[1]])
dTrainAll <- subset(d,rgroup<=0.9)
dTest <- subset(d,rgroup>0.9)
nrow((dTrainAll))
nrow((dTest))



d$churn
vars = 
?setdiff
colnames(dTrainAll)
colnames(dTest)
colnames(d)
names(vars)
View(vars)
summary(vars)
dTrainAll[,vars]


outcomes=c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in% c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]
rm(list=c('d','churn','appetency','upselling'))
outcome <- 'churn'
pos <- '1'




dim(dTrainAll[1])
dim(dCal[1])
dim(dTrain[1])



rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)

View(dTrain)
dTrain$Var6



head(dTrainAll)
useForCal <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
useForCal
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)

dTrain[,'Var6']
dTrain$Var6
dTrain[,outcome]
head(dTrain)

table218 <- table(Var218=dTrain[,'Var218'], churn=dTrain[,outcome],useNA='ifany')
print(table218)
print(table218[,2]/(table218[,1]+table218[,2]))

table218[,1]
table218[,2]
table218[,2]/(table218[,1]+table218[,2])


table217 <- table(Var217=dTrain[,'Var217'], churn=dTrain[,outcome],useNA='ifany')



dTrain$Var217
dTrain$churn

Var218
View


dTrain[,outcome]
dTrain[,outcome] == pos
sum(dTrain[,outcome] == pos)

is.na(dTrain$Var218)
dTrain$outcome

dTrain[outcome]

pPos <- sum(dTrain[,outcome] == pos) / nrow(dTrain)

naTab <- table(as.factor(dTrain[is.na(dTrain$Var218),outcome]))
pPosWna <- (naTab / sum(naTab))[pos]

vTab <- table(as.factor(dTrain[,outcome]), dTrain$Var218)
pPosWv <- vTab[pos, ] / colSums(vTab)

pred <- pPosWv[dCal$Var218]
pred[is.na(dCal$Var218)] <- pPosWna





mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])

mkPredC <- function(outCol,varCol,appCol) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  
  pred
}



dTrauncatVars


dTrain[,pred Var191]

#######AUC

for(v in catVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}


install.packages('ROCR')
library('ROCR')
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}


for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.6) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f", pi,aucTrain,aucCal))
  }
}





























###Decision Tree


library('rpart')
fV <- paste(outcome,'>0 ~ ', + paste(c(catVars,numericVars),collapse=' + '),sep='')
tmodel <- rpart(fV,data=dTrain)
print(calcAUC(predict(tmodel,newdata=dTrain),dTrain[,outcome]))
print(calcAUC(predict(tmodel,newdata=dTest),dTest[,outcome]))
print(calcAUC(predict(tmodel,newdata=dCal),dCal[,outcome]))










### K-neighbor

setwd("C:/Users/andre/Desktop/Rcode")
wbcd <-read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)
View(wbcd)
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

normalize<-function(x){return((x-min(x))/(max(x)-min(x)))}
wbcd <- wbcd[,-1]
wbcd_n<-as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])


##split test and trainig
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]
wbcd_train_labels <-wbcd[1:469,1]
wbcd_test_labels <-wbcd[470:569,1]
dim(wbcd_train)
dim(wbcd_test)
length(wbcd_train_labels)
length(wbcd_test_labels)

##crosstable
library(gmodels)
library(class)
wbcd_test_pred<- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=21)
wbcd_test_pred
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)




wbcd_test_pred<- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=21, prob=T)
wbcd_test_pred_prob <- ifelse(wbcd_test_pred=="Malignant",
                              attributes(wbcd_test_pred)$prob,
                              1-attributes(wbcd_test_pred)$prob)

print(calcAUC(wbcd_test_pred_prob, wbcd_test_labels))








###########################################################################
###Naive
sms_raw<-read.csv("sms_spam.csv", stringsAsFactors = FALSE)
View(sms_raw)
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

install.packages("tm")
library(tm)

sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2])

corpus_clean<-tm_map(sms_corpus, content_transformer(tolower))
corpus_clean<-tm_map(corpus_clean, content_transformer(removeNumbers))
corpus_clean<-tm_map(corpus_clean, content_transformer(removeWords),stopwords())
corpus_clean<-tm_map(corpus_clean, content_transformer(removePunctuation))
corpus_clean<-tm_map(corpus_clean, content_transformer(stripWhitespace))

sms_corpus[[1]]$content
corpus_clean[[1]]$content
sms_corpus[[1234]]$content
corpus_clean[[1234]]$content


##Data Preparation

sms_dtm <- DocumentTermMatrix(corpus_clean)
inspect(sms_dtm[1:10, 1000:1010])
dim(sms_dtm)


#Spliting Training and Test set
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5558, ]

sms_dtm_train<-sms_dtm[1:4169, ]
sms_dtm_test<-sms_dtm[4170:5558, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5558]

prop.table(table(sms_raw_train$type))

prop.table(table(sms_raw_test$type))

##visualization wordcloud

install.packages(("wordcloud"))
library(wordcloud)
wordcloud(sms_corpus_train, min.freq=40, random.order=FALSE)



spam<-subset(sms_raw_train, type=="spam")
ham<-subset(sms_raw_train, type=="ham")
wordcloud(spam$text, min.freq=20, max.words=40, scale=c(3,0.5))
wordcloud(ham$text, max.words=40, scale=c(3,0.5))



###Filterting frequent terms
findFreqTerms(sms_dtm_train,5)
sms_dict<-findFreqTerms(sms_dtm_train,5)
sms_train<-DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test<-DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

convert_counts <- function(x){
  x<-ifelse(x>0, 1, 0)
  x<-factor(x, levels = c(0,1), labels=c("No", "Yes"))
}

sms_train<-apply(sms_train, MARGIN = 2, convert_counts)
sms_test<-apply(sms_test, MARGIN = 2, convert_counts)





###selvars

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





########################################KNN
library('class')
nK <- 200
knnTrain <- dTrain[,selVars]
knnCl <- dTrain[,outcome]==pos
knnPred <- function(df) {
  knnDecision <- knn(knnTrain,df,knnCl,k=nK,prob=T)
  ifelse(knnDecision==TRUE,
         attributes(knnDecision)$prob,
         1-(attributes(knnDecision)$prob))}
