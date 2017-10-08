##21000348 Andre Seo
##Assignment 1

##
head(mtcars)
install.packages("doBy")
require("doBy")
##


##Problem1

#환산기준
kpl <- 1.609344/3.785412 

#단위환산
mtcars$mpg<-c(mtcars$mpg*kpl)

#행이름변경
colnames(mtcars)[1]<-"kpl"

head(mtcars)



##Problem2

#연비가 좋은 3개의 모델
rownames(mtcars)[which.maxn(mtcars$kpl,3)]

#연비가 나쁜 3개의 모델
rownames(mtcars)[which.minn(mtcars$kpl,3)]



##Problem3

#실린더가 4개인 그룹 연비의 평균
mean(subset(mtcars, cyl=='4')[,1])

#실린더가 6개인 그룹 연비의 평균
mean(subset(mtcars, cyl=='6')[,1])

#실린더가 8개인 그룹 연비의 평균
mean(subset(mtcars, cyl=='8')[,1])



##Problem4

#Genesis 벡터 생성
gen<-c(8.5, 6, 200, 100, 3.50, 3.000, 15.00, 1, 1, 3, 3)


#바인딩
rbind(mtcars,gen)
mtcars<-rbind(mtcars, gen)
rownames(mtcars)[33]<-"Genesis"

mtcars



##Problem5

#100마력이 넘는 차종의 연비의 평균값
mean(subset(mtcars,hp>100)[,1])

#100마력이 넘지 않는 차종의 연비의 평균값
mean(subset(mtcars,hp<=100)[,1])



##Problem6

#연비낮은순 정렬
rownames(mtcars)[order(mtcars$kpl)]

#마력높은순 정렬
rownames(mtcars)[order(mtcars$hp, decreasing = T)]



##Problem7

#단위변환
mtcars$wt<-c(mtcars$wt*1000*0.453592)

head(mtcars)

#무거운 5개 분리
rownames(mtcars)[which.maxn(mtcars$wt,5)]

#가벼운 5개 분ㄹ
rownames(mtcars)[which.minn(mtcars$wt,5)]


##Problem8

#무거운 쪽 평균연비
mean(mtcars$kpl[c(which.maxn(mtcars$wt,5))])

#무거운 쪽 평균마력 
mean(mtcars$hp[c(which.maxn(mtcars$wt,5))])

#가벼운쪽 평균연비
mean(mtcars$kpl[c(which.minn(mtcars$wt,5))])

#가벼운쪽 평균마력
mean(mtcars$hp[c(which.minn(mtcars$wt,5))])



##Problem9

#price 벡터 생성
price <- seq(50000, 82000, 1000)
price <- sample(price, 33)

#바인딩
cbind(mtcars,price)
mtcars<-cbind(mtcars, price)

head(mtcars)


##Problem10

#CSV파일로 저장
write.csv(mtcars, "mtcars_test.csv")



##Problem11
#자료 저장
save(list = ls(), file = "mtcars_test.RData")






