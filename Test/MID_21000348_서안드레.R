##############################################################################################
setwd("C:/Users/andre/Desktop/Rcode")
install.packages("doBy")
library("doBy")
install.packages("ggplot2")
library(ggplot2)
##############################################################################################
student_m <- read.csv("student-math.csv", header=T)
student_p <- read.csv("student-port.csv", header=T)
##############################################################################################
head(data1)
View(student_p)
dim(mer)
str(student_m)
summary(data1)
##############################################################################################








####1
student_m <- read.csv("student-math.csv", header=T)
dim(student_m)

student_p <- read.csv("student-port.csv", header=T)
dim(student_p)


####2
length(colnames(student_m))
length(colnames(student_p))
colnames(student_m) == colnames(student_p)


####3
Male.math<-sum(student_m$sex == 'M')
Female.math<-sum(student_m$sex == 'F')
Male.math/(Male.math+Female.math)*100
Female.math/(Male.math+Female.math)*100

Male.port<-sum(student_p$sex == 'M')
Female.port<-sum(student_p$sex == 'F')
Male.port/(Male.port+Female.port)*100
Female.port/(Male.port+Female.port)*100




####4
mean(student_m$G3)
mean(student_p$G3)




####5
mean.math.G3 <- mean(student_m$G3)
student_m$G3.normal <- student_m$G3/mean.math.G3

mean.port.G3 <- mean(student_p$G3)
student_p$G3.normal <- student_p$G3/mean.port.G3


library(ggplot2)
plot(density(student_m$G3))
plot(density(student_m$G3.normal))

plot(density(student_p$G3))
plot(density(student_p$G3.normal))






####6
mean(student_m$G3[student_m$Dalc >=3 & student_m$Walc >=3])
mean(student_m$G3[student_m$Dalc <=2 & student_m$Walc <=2])

mean(student_p$G3[student_p$Dalc >=3 & student_p$Walc >=3])
mean(student_p$G3[student_p$Dalc <=2 & student_p$Walc <=2])




####7
Medu.fix.math <- ordered(as.factor(student_m$Medu))
new.level <- c("none",
               "primary education",
               "5th to 9th grade",
               "secondary education",
               "higher education")
levels(Medu.fix.math)<-new.level
student_m$Medu.fix.math <- Medu.fix.math


Fedu.fix.math <- ordered(as.factor(student_m$Fedu))
new.level <- c("none",
               "primary education",
               "5th to 9th grade",
               "secondary education",
               "higher education")
levels(Fedu.fix.math)<-new.level
student_m$Fedu.fix.math <- Fedu.fix.math




Medu.fix.port <- ordered(as.factor(student_p$Medu))
new.level <- c("none",
               "primary education",
               "5th to 9th grade",
               "secondary education",
               "higher education")
levels(Medu.fix.port)<-new.level
student_p$Medu.fix.port <- Medu.fix.port


Fedu.fix.port <- ordered(as.factor(student_p$Fedu))
new.level <- c("none",
               "primary education",
               "5th to 9th grade",
               "secondary education",
               "higher education")
levels(Fedu.fix.port)<-new.level
student_p$Fedu.fix.port <- Fedu.fix.port







