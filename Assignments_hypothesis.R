#Here we have two sample and we have to compare diameters of two uints cutlet
# We will perform two sample t test
#Ho= Cutlet diameter are equal ie mu1=mu2
#Ha=Cutlet diameter are not equal ie mu1=!m2
cutlet <- read.csv("D:\\DATA_SCIENCE\\ASSIGNMENTS\\2Hypothesi.testing/Cutlets.csv")
View(cutlet)
attach(cutlet)
colnames(cutlet)
shapiro.test(cutlet$Unit.A)
# Since p-value = 0.32 It follows normality.
shapiro.test(cutlet$Unit.B)
# p-value =0.5225 it follows normality.

#two sample t-test
t.test(cutlet$Unit.A,cutlet$Unit.B,alternative = "two.sided",mu=0,var.equal = FALSE,conf.level = 0.95,correct=TRUE)
# p-value =0.4723 > 0.05 ,muo =!mu1 accept  alternate hypothesis 
#Hence Ha alternate hypothesis accepted outlet diameters are not equal

##--------------------------------####
## Here we have to compare Average Turn Around Time of four labs
##Ho= Average TAT is same ie mu1=mu2=mu3=mu4
##Ha= Average TAT is not same ie atleast one AV TAT Differs
labtab <- read.csv("D:\\DATA_SCIENCE\\RLearning\\ASSIGNMENTS\\2Hypothesi.testing/LabTAT.csv")
View(labtab)
stackedlbtt <- stack(labtab)
View(stackedlbtt)

##normality test
shapiro.test(labtab$Laboratory.1)
shapiro.test(labtab$Laboratory.2)
shapiro.test(labtab$Laboratory.3)
shapiro.test(labtab$Laboratory.4)
# since p-value is greater than 0.05 data is normally distributed
boxplot(labtab$Laboratory.1,labtab$Laboratory.2,labtab$Laboratory.3,labtab$Laboratory.4)
# mean of lab1 and lab two are similar but lab3,lab4 differs

result <- aov(values~ind,data = stackedlbtt)
summary(result)

## Clearly The value of F-test= 118.7 and p-value is nearly 0 ie P<0.05
## Hence Reject null hypothesis ie the Average TAT Differs

##--------------********************--------------###
## Given that
## Ho=All proportions are equal.
## Ha=Not all propotions are equal
sop <- read.csv(file.choose())
View(sop)
attach(sop)


table(sop$East,sop$East+sop$West+sop$North+sop$South)
prop.test(x=c(50,435),n=c(393,1523),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
## p=1.206e^-10 < alpha=0.05 reject null hupothesis
## equal propotions

table(sop$West,sop$East+sop$West+sop$North+sop$South)
prop.test(x=c(142,1523),n=c(393,1523),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
## p=2.2e^-10 < alpha=0.05 reject null hupothesis
## equal propotions

table(sop$North,sop$East+sop$West+sop$North+sop$South)
prop.test(x=c(131,1356),n=c(393,4064),conf.level=0.95,correct=FALSE,alternative="two.sided")
## p=0.9895 > alpha=0.05 Accept null hypothesis
## unequal propotions

table(sop$South,sop$East+sop$West+sop$North+sop$South)
prop.test(x=c(70,750),n=c(393,4064),conf.level = 0.95,correct = TRUE,alternative ="two.sided")
## p=0.7534 > alpha=0.05 accept null hypothesis
## unequal propotions


###------------**********************-----------####

##Ho=no form defective
## H1=formis defective
cof <- read.csv(file.choose())
View(cof)
cof1 <- stack(cof)


###------------**********************-----------####

##Ho=%of males versus female do not differ on weekdays  
## H1=%of males versus female differ on weekdays

Fantloons <- read.csv(file.choose())
View(Fantloons)
attach(Fantloons)
table(Fantloons$Weekday)

prop.test(x=c(287,113),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative="two.sided")
# p=2.2e^-16 < 0.05 accept null hypothesis
table(Fantloons$Weekend)
prop.test(x=c(233,167),n=c(400,400),conf.level=0.95,correct=FALSE,alternative="two.sided")
# p=3.05e^-16 < 0.05 accept null hypothesis
# Thus %of males versus female do not differ on weekdays and weekends###