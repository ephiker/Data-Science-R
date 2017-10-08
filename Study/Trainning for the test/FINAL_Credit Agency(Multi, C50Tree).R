##Decision Tree
#경우의 수를 계속 쪼개어 예측 성능을 높임
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
#크레딧 행 갯수에 맞춰 난수 생성. 난수 범위 0<n<1 
credit_train <- credit[rand_vector < 0.9,]
#0.9보다 작으면 T, 크면 F. T,F 벡터로 credit에서 요소 뽑아내어 데이터 프레임 생성.

credit_test <- credit[rand_vector >= 0.9,]
#credit 데이터를 트레인과 테스트로 분리


prop.table(table(credit_train$default))
#디폴트 벡터를 테이블에 담고, prop.table은 그 데이블 밸류들간의 비율을 구함

prop.table(table(credit_test$default))




###############################################################################
#모델 만든는 방법 1. C5.0 함수로 default 예측하기


#model training
install.packages("C50")
library(C50)

View(credit_train)
#making tree with C5.0
credit_model <- C5.0(credit_train[-17], credit_train$default)
#C5.0(DEFAULT 빼고 남은 데이터프레임 전부 다, 분류기준이 되는 변수)
credit_model
summary(credit_model)




#performance evaluation
#성능 평가 방법 : 실제 값과 예측 값을 비교


credit_pred<-predict(credit_model, credit_test)
#예측값 추출 = 예측함수 predict(트레이닝 한 트리모델, 테스트 셋 대입)

credit_pred
#예측한 default 값

credit_test$default
#테스트 셋의 실제 default 값

?CrossTable

library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq =  FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))
#cross table까지 만들어야 진짜 예측 가능
#x = 실제 테스트 셋의 default 값, y = 예측 default 값




#######################################################
# 성능 올리기
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
#확률 나타내기

#probability output
library('ROCR')
calcAUC <- function(predCol, outCol){
  perf <- performance(prediction(predCol, outCol), 'auc')
  as.numeric(perf@y.values)
}

?predict

credit_pred_prob<-predict(credit_model, credit_train, type = 'prob')[,2]
#train 모델에서 예측한 yes에 대한 벡터

auc_train <- calcAUC(credit_pred_prob, credit_train$default == 'yes')
#train 모델에서 예측한 yes에 대한 벡터, 실제 yes에 대한 벡터 대입

credit_pred_prob<-predict(credit_model, credit_test, type = 'prob')[,2]
auc_test <- calcAUC(credit_pred_prob, credit_test$default == 'yes')
#test set에도 적ㅇ

print(sprintf("train AUC : %4.3f test AUC : %4.3f", auc_train, auc_test))용

ㅇ

