slhk <- read.csv(file.choose())
View(slhk)
attach(slhk)


dotplot(YearsExperience)
dotplot(Salary)
##Evenly distributed data
boxplot(YearsExperience)
boxplot(Salary)
## No outliers

plot(YearsExperience,Salary,col="dodgerblue",
     col.main="dodgerblue4",col.lab="dodgerblue4",
     main = "linearity plot",
     xlab = "YearsExperience",ylab = "Salary_hike")
cor(YearsExperience,Salary)
###Strong correlation

slhkreg <- lm(Salary~YearsExperience,data = slhk)
summary(slhkreg)

### Excelent ^2 value 
slhkpred <- predict(slhkreg,data.frame(YearsExperience=1.1))
slhkpred
pred <- predict(slhkreg)
pred

#Errors comparison

Errors <- data.frame(slhk,pred,"Errors"= Salary-pred)
View(Errors)
