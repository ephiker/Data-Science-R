setwd("C:/Users/andre/Desktop/Rcode")
d<- read.table('orange_small_train.data.gz',
               header=T, sep='\t', na.strings=c('NA',''))
churn <- read.table('orange_small_train_churn.labels.txt',
                    header=F, sep='\t')

View(d)
View(churn)

d$churn <- churn$V1

appetency <- read.table('orange_small_train_appetency.labels.txt', 
                        header=F, sep='\t')
d$appetency <-appetency$V1

upselling <- read.table('orange_small_train_upselling.labels.txt', header=F, sep='\t')
d$upselling <- upselling$V1

#variables in the data
names(d)
d$rgroup <- runif(dim(d)[[1]])
d$rgroup

dTrainAll<-subset(d,rgroup<=0.9)
dTest<-subset(d,rgroup>0.9)
class(dTrainAll)
head(dTrainAll)
head(dTest)
nrow((dTrainAll))
nrow((dTest))

data(d)

outcomes=c('churn','appetency','upselling')
vars <- setdiff(colnames(dTrainAll), c(outcomes,'rgroup'))
catVars <- vars[sapply(dTrainAll[,vars],class) %in% c('factor','character')]
numericVars <- vars[sapply(dTrainAll[,vars],class) %in% c('numeric','integer')]
rm(list=c('d','churn','appetency','upselling'))
outcome <- 'churn'
pos <- '1'


useForCal <- rbinom(n=dim(dTrainAll)[[1]],
                    size=1,prob=0.1)>0
dCal <- subset(dTrainAll,useForCal)
dTrain <- subset(dTrainAll,!useForCal)




table218 <- table(
  Var218=dTrain[,'Var218'],
  churn=dTrain[,outcome],
  useNA='ifany')

print(table218[,2]/(table218[,1]+table218[,2]))

class(dTrain)

dTrain[,outcome]
dTrain[,'Var218']





pPos <- sum(dTrain[,outcome] == pos) / nrow(dTrain)
naTab <- table(as.factor(dTrain[is.na(dTrain$Var218),outcome]))
pPosWna <- (naTab / sum(naTab))[pos]
vTab <- table(as.factor(dTrain[,outcome]), dTrain$Var218)
pPosWv <- vTab[pos, ] / colSums(vTab)
pred <- pPosWv[dCal$Var218]
pred[is.na(dCal$Var218)] <- pPosWna


View(churn)
View(d)




