##Decision Tree
#����� ���� ��� �ɰ��� ���� ������ ����
setwd("C:/Users/andre/Desktop/Rcode")


#data loading
credit<-read.csv("credit.csv")
str(credit)
summary(credit)
View(credit)

#data explorations
#categorical features
table(credit$checking_balance)
table(credit$savings_balance)


#numeric features
summary(credit$months_loan_duration)
#target variable
summary(credit$default)


#data partitioning
set.seed(12345)
rand_vector <- runif(nrow(credit))
#ũ���� �� ������ ���� ���� ����. ���� ���� 0<n<1 
credit_train <- credit[rand_vector < 0.9,]
#0.9���� ������ T, ũ�� F. T,F ���ͷ� credit���� ��� �̾Ƴ��� ������ ������ ����.

credit_test <- credit[rand_vector >= 0.9,]
#credit �����͸� Ʈ���ΰ� �׽�Ʈ�� �и�


prop.table(table(credit_train$default))
#����Ʈ ���͸� ���̺��� ���, prop.table�� �� ���̺� ����鰣�� ������ ����

prop.table(table(credit_test$default))




###############################################################################
#�� ����� ��� 1. C5.0 �Լ��� default �����ϱ�


#model training
install.packages("C50")
library(C50)

View(credit_train)
#making tree with C5.0
credit_model <- C5.0(credit_train[-17], credit_train$default)
#C5.0(DEFAULT ���� ���� ������������ ���� ��, �з������� �Ǵ� ����)
credit_model
summary(credit_model)




#performance evaluation
#���� �� ��� : ���� ���� ���� ���� ��


credit_pred<-predict(credit_model, credit_test)
#������ ���� = �����Լ� predict(Ʈ���̴� �� Ʈ����, �׽�Ʈ �� ����)

credit_pred
#������ default ��

credit_test$default
#�׽�Ʈ ���� ���� default ��

?CrossTable

library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq =  FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#cross table���� ������ ��¥ ���� ����
#x = ���� �׽�Ʈ ���� default ��, y = ���� default ��




#######################################################
# ���� �ø���
#Improving Performance

#Adaptive Boosting

credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq =  FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Giving different weights according to cost type
error_cost <- matrix(c(0,1,4,0), nrow = 2)
error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq =  FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


##########################################################################
#Ȯ�� ��Ÿ����

#probability output
library('ROCR')
calcAUC <- function(predCol, outCol){
  perf <- performance(prediction(predCol, outCol), 'auc')
  as.numeric(perf@y.values)
}

?predict

credit_pred_prob<-predict(credit_model, credit_train, type = 'prob')[,2]
#train �𵨿��� ������ yes�� ���� ����

auc_train <- calcAUC(credit_pred_prob, credit_train$default == 'yes')
#train �𵨿��� ������ yes�� ���� ����, ���� yes�� ���� ���� ����

credit_pred_prob<-predict(credit_model, credit_test, type = 'prob')[,2]
auc_test <- calcAUC(credit_pred_prob, credit_test$default == 'yes')
#test set���� ����

print(sprintf("train AUC : %4.3f test AUC : %4.3f", auc_train, auc_test))��

��
