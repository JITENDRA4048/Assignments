##Add csv file
calories_consumed <- read.csv(file.choose())
View(calories_consumed)
library(lattice)
attach(calories_consumed)
summary(calories_consumed)
dotchart(Weight.gained..grams.,pch = 20,color = "dodgerblue")
dotchart(Calories.Consumed,pch = 20,color = "dodgerblue")

##boxplot 
boxplot(Weight.gained..grams.,main="wtgaind",col = "dodgerblue")
boxplot(Calories.Consumed,main="Calories",col = "dodgerblue4")
## No outliers present

plot(Calories.Consumed,Weight.gained..grams.,pch=20,col="dodgerblue4")
##plot shows moderate to strong relationship

##check correlation coefficient
cor(Weight.gained..grams.,Calories.Consumed)
## correlation coefficient is 0.95 ie strong linear relation

##simple linear regression
clcmdreg <- lm(Weight.gained..grams.~Calories.Consumed,data = calories_consumed)

summary(clcmdreg)
## co-relation coefficient r= 0.946991 suggest that input & output is strongly correlated 
## here p-values are significantly low ie nearly Zero,so regect Ho ie X!=0   
## r squared values is above R^2=0.85 hence our model is as acceptable
test <- data.frame(Calories.Consumed=1500)
pred <- predict(clcmdreg,newdata = test)
pred
predwt <- predict(clcmdreg,data.frame(Calories.Consumed=c(1700,1800,1900)))
predwt
predwt1 <- predict(clcmdreg)
View(predwt1)
ER <-calories_consumed$Weight.gained..grams.-predwt1 

##Error chk
finaldata <- data.frame(calories_consumed,predwt1,"ERROR"=calories_consumed$Weight.gained..grams.-predwt1)
View(finaldata)
window()
plot(clcmdreg)
write.csv(finaldata)

