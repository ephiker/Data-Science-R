
########################################
##########Data Exploration##############
########################################
###################21000348 Andre Seo###



#Working directory setting
setwd("C:/Users/andre/Desktop/Rcode")

#Data reading(Reset data)###############
data<-read.csv("keat4j.csv")############
class(data)

#Overviewing
dim(data)
head(data)


###1 Feature Expolation


#A:TOTKIDS, CHILDNO
summary(as.factor(data$TOTKIDS))
summary(as.factor(data$CHILDNO))
(sum(data$TOTKIDS)-sum(data$CHILDNO))/sum(data$TOTKIDS)*100

#B:CHIDEAL, CH_PREG
summary(as.factor(data$CHIDEAL))
summary(as.factor(data$CH_PREG))

#C:CONTRNOW, EDN, MARR_AGE
summary(as.factor(data$CONTRNOW))
summary(as.factor(data$EDN))
summary(as.factor(data$MARR_AGE))

#D:WATERCH, MILKCH, JUICECH
(summary(data$WATERCH)[2])/(summary(data$WATERCH)[1] + summary(data$WATERCH)[2])*100
(summary(data$MILKCH)[2])/(summary(data$MILKCH)[1] + summary(data$MILKCH)[2])*100
(summary(data$JUICECH)[2])/(summary(data$JUICECH)[1] + summary(data$JUICECH)[2])*100




#########################################
#####Data Cleaning and Preprocessing#####
#########################################


###1 Missing Value

#A:MARR_AGE
summary(data$MARR_AGE)
brks <- c(0, 15, 20, 25, 30, Inf)
data$MARR_AGE_CUT <- cut(data$MARR_AGE, breaks=brks, include.lowest=T)
summary(data$MARR_AGE_CUT)

data$MARR_AGE_CLEAN <- ifelse(is.na(as.character(data$MARR_AGE_CUT)),
                              "Never married", as.character(data$MARR_AGE_CUT))
summary(as.factor(data$MARR_AGE_CLEAN))


#B:CHIDEAL
summary(data$CHIDEAL)
meanCHIDEAL <- mean(data$CHIDEAL, na.rm=T)
data$CHIDEAL_CLEAN <- ifelse(is.na(data$CHIDEAL), meanCHIDEAL, data$CHIDEAL)
summary(data$CHIDEAL_CLEAN)

#C:HTSTAND
summary(as.factor(data$HTSTAND))
data$HTSTAND <- ifelse(is.na(as.character(data$HTSTAND)),
                       ifelse(is.na(as.character(data$HEIGHT2)), "UNCERTAIN", "NOT MESUARED"),
                       as.character(data$HTSTAND))
summary(as.factor(data$HTSTAND))

#D:HEIGHT2
summary(data$HEIGHT2)
meanHEIGHT2 <- mean(data$HEIGHT2, na.rm=T)
data$HEIGHT2 <- ifelse(is.na(data$HEIGHT2), meanHEIGHT2, data$HEIGHT2)
summary(data$HEIGHT2)

#E:INCLEVEL
summary(as.factor(data$INCLEVEL))
data$INCLEVEL <- ifelse(is.na(as.character(data$INCLEVEL)), "NOT SURE", as.character(data$INCLEVEL))
summary(as.factor(data$INCLEVEL))



###2 Invalid Value

summary(as.factor(data$CHIDEAL_CLEAN))
data$CHIDEAL_CLEAN2<-ifelse(as.numeric(data$CHIDEAL_CLEAN > 90),
                            meanCHIDEAL, as.numeric(data$CHIDEAL_CLEAN))
summary(as.factor(data$CHIDEAL_CLEAN2))
summary(as.numeric(data$CHIDEAL_CLEAN2))


###3 Proposing
(sum(data$TOTKIDS)-sum(data$CHILDNO))/sum(data$TOTKIDS)*100

(summary(data$WATERCH)[2])/(summary(data$WATERCH)[1] + summary(data$WATERCH)[2])*100
(summary(data$MILKCH)[2])/(summary(data$MILKCH)[1] + summary(data$MILKCH)[2])*100
(summary(data$JUICECH)[2])/(summary(data$JUICECH)[1] + summary(data$JUICECH)[2])*100

summary(as.factor(data$CONTRNOW))

mean(data$CHIDEAL_CLEAN2)
mean(data$CH_PREG)

summary(as.factor(data$EDN))
summary(as.factor(data$MARR_AGE2))



########################################
##########Data Exploration##############
########################################
###################21000348 Andre Seo###