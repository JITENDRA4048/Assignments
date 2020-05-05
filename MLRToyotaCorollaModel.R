Corolla <- read.csv(file.choose())
windows()
View(Corolla)
tc = Corolla[,c(3,4,7,9,13,14,16,17,18)]
library(lattice)
plot(tc)
dotplot(tc$Price)
boxplot(tc$Price)
dotplot(tc$Age_08_04)
boxplot(tc$Age_08_04)
dotplot(tc$KM)
boxplot(tc$KM)
dotplot(tc$HP)
boxplot(tc$HP)
dotplot(tc$cc)
boxplot(tc$cc)
dotplot(tc$Doors)
boxplot(tc$Doors)
dotplot(tc$Gears)
boxplot(tc$Gears)
dotplot(tc$Quarterly_Tax)
boxplot(tc$Quarterly_Tax)
dotplot(tc$Weight)
boxplot(tc$Weight)

##correlation matrix
cor(tc)
colnames(tc)
attach(tc)
tcreg <- lm(Price~Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(tcreg)

cor(cc,Doors)
install.packages("car")
install.packages("corpcor")
library(car)
library(corpcor)
vif(tcreg)
###vif <2
library(MASS)
stepAIC(tcreg)

finaltcreg <- lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
                   Weight,data = tc)
summary(finaltcreg)
### Diagnosis & Deletion
plot(finaltcreg)
residualPlots(finaltcreg)
influence.measures(finaltcreg)
influenceIndexPlot(finaltcreg)

#### Regression after Deletion 
tc1 <- tc[-c(602,655,992,110),]
tcreg1 <- lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
                   Weight,data = tc1)
summary(creg1)
### Diagnosis & Deletion
plot(finaltcreg1)
residualPlots(tcreg1)
influence.measures(tcreg1)
influenceIndexPlot(tcreg1)

## On deleting AdjustedR^2 is slightly improved but it is making 
##Quarterly_tax value Insignificant
###our final MLR model is 
finaltcreg <- lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
                   Weight,data = tc)
summary(finaltcreg)