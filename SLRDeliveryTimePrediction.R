dlvrtm1 <- read.csv(file.choose())
dlvrtm <- dlvrtm1[-c(5,9,21),] 

View(dlvrtm)
attach(dlvrtm)

dotplot(Sorting.Time)
##data is evenly spread from 2-10

dotplot(Delivery.Time)
## data constant b/w 8-28

boxplot(Sorting.Time,col="dodgerblue")
boxplot(Delivery.Time,col = "dodgerblue4")

## boxplot shows no outliers

plot(Delivery.Time,Sorting.Time,col="dodgerblue",pch=20)


cor(Sorting.Time,Delivery.Time)
##Input output data shows strong correlation

dlvrtmreg <- lm(Delivery.Time~Sorting.Time,data = dlvrtm)
summary(dlvrtmreg)
##Residuals shows min to max error values
## P value << 0.05 thus Ho regected ie X!=0 at any point
## R^2 is smaller away from 1 ie explained variation is small


test <- data.frame(Sorting.Time=2)
pred <- predict(dlvrtmreg,newdata = test)
pred

pred <- predict(dlvrtmreg)
View(pred)

##Error cecking
chkerror <- data.frame(dlvrtm,pred,"Errors"=Delivery.Time-pred)
chkerror
View(chkerror)

install.packages("car")
library(car)
plot(dlvrtmreg)
influenceIndexPlot(dlvrtmreg)

