##21000348 Andre Seo
##Assignment 1

##
data(mtcars)
head(mtcars)
install.packages("doBy")
require("doBy")
##


##Problem1

#ȯ�����
kpl <- 1.609344/3.785412 

#����ȯ��
mtcars$mpg<-c(mtcars$mpg*kpl)

#���̸�����
colnames(mtcars)[1]<-"kpl"

#Ȯ��
head(mtcars)



##Problem2

#���� ���� 3���� ��
rownames(mtcars)[which.maxn(mtcars$kpl,3)]

#���� ���� 3���� ��
rownames(mtcars)[which.minn(mtcars$kpl,3)]



##Problem3

#�Ǹ����� 4���� �׷� ������ ���
mean(subset(mtcars, cyl=='4')[,1])

#�Ǹ����� 6���� �׷� ������ ���
mean(subset(mtcars, cyl=='6')[,1])

#�Ǹ����� 8���� �׷� ������ ���
mean(subset(mtcars, cyl=='8')[,1])


##Problem4

#Genesis ���� ����
gen<-c(8.5, 6, 200, 100, 3.50, 3.000, 15.00, 1, 1, 3, 3)

#���ε�
#rbind(mtcars,gen)
mtcars<-rbind(mtcars, gen)
rownames(mtcars)[33]<-"Genesis"
mtcars



##Problem5

#100������ �Ѵ� ������ ������ ��հ�
mean(subset(mtcars,hp>100)[,1])

#100������ ���� �ʴ� ������ ������ ��հ�
mean(subset(mtcars,hp<=100)[,1])



##Problem6

#�������� ����
rownames(mtcars)[order(mtcars$kpl)]

#���³����� ����
rownames(mtcars)[order(mtcars$hp, decreasing = T)]



##Problem7

#������ȯ
mtcars$wt<-c(mtcars$wt*1000*0.453592)
colnames(mtcars)[6]<-"kg"


head(mtcars)

#���ſ� 5�� �и�
rownames(mtcars)[which.maxn(mtcars$kg,5)]

#������ 5�� ��fl
rownames(mtcars)[which.minn(mtcars$kg,5)]


##Problem8

#���ſ� �� ��տ���
mean(mtcars$kpl[c(which.maxn(mtcars$kg,5))])

#���ſ� �� ��ո��� 
mean(mtcars$hp[c(which.maxn(mtcars$kg,5))])

#�������� ��տ���
mean(mtcars$kpl[c(which.minn(mtcars$kg,5))])

#�������� ��ո���
mean(mtcars$hp[c(which.minn(mtcars$kg,5))])



##Problem9

#price ���� ����
price <- seq(50000, 82000, 1000)
price <- sample(price, 33)

#���ε�
cbind(mtcars,price)
mtcars<-cbind(mtcars, price)

head(mtcars)


##Problem10

#CSV���Ϸ� ����
write.csv(mtcars, "mtcars_test.csv")



##Problem11
#�ڷ� ����
save(list = ls(), file = "mtcars_test.RData")





