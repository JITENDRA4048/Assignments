Computer_Data <- read.csv(file.choose(),stringsAsFactors = FALSE,header = TRUE)
View(Computer_Data)

class(Computer_Data$cd)
Computer_Data$cd[Computer_Data$cd=="yes"] <- 1
Computer_Data$cd[Computer_Data$cd=="no"] <- 0
Computer_Data$cd <- as.numeric(as.character(Computer_Data$cd))
class(Computer_Data$cd)

Computer_Data$multi[Computer_Data$multi=="yes"] <- 1
Computer_Data$multi[Computer_Data$multi=="no"] <- 0
Computer_Data$multi <- as.numeric(as.character(Computer_Data$multi))
class(Computer_Data$multi)

Computer_Data$premium[Computer_Data$premium=="yes"] <- 1
Computer_Data$premium[Computer_Data$premium=="no"] <- 0
Computer_Data$premium <- as.numeric(as.character(Computer_Data$premium))
class(Computer_Data$premium)

comdata <- Computer_Data[,-1]
comdata
attach(comdata)
pairs(comdata)
cor(comdata)
colnames(comdata)
library(lattice)
dotplot(price)
dotplot(speed)
dotplot(hd)
dotplot(ram)
dotplot(screen)
dotplot(cd)
dotplot(multi)
dotplot(premium)
dotplot(ads)
dotplot(trend)

boxplot(price)
boxplot(speed)
boxplot(hd)
boxplot(ram)
boxplot(screen)
boxplot(cd)
boxplot(multi)
boxplot(premium)
boxplot(ads)
boxplot(trend)

compreg <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=comdata)
summary(compreg)

### Every variable has a significant p-value
library(MASS)
stepAIC(compreg)
###AIC also suggest the same model

## Deletion & Diagonsis
plot(compreg)
residualPlots(compreg)
install.packages("car")
library(car)
installed.packages(corpcor)
library(corpcor)
influence.measures(compreg)
influenceIndexPlot(compreg)

compdata1 <- comdata[-c(20,721,982,994,1043,1049,1123,1177,1441,1689,1701,1785,3784,4478,5961),]
compreg1 <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,data=compdata1)
summary(compreg1)

plot(compreg1)
residualPlots(compreg1)
influence.measures(compreg)
influencePlot(compreg1)
influenceIndexPlot(compreg1)
