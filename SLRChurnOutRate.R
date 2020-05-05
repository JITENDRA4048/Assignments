emp <- read.csv(file.choose())
library(lattice)
View(emp)
attach(emp)

dotplot(Salary_hike,col="dodgerblue",pch=20)
## Most of the data lies B/w 1590-1750
dotplot(Churn_out_rate,col="dodgerblue",pch=20)
## Data evenly spreads from 60-100

boxplot(Salary_hike,col = "dodgerblue4")
boxplot(Churn_out_rate,col="dodgerblue4")
## No outliers Present

plot(Churn_out_rate,Salary_hike,col="dodgerblue",pch=25)
## data shows negative correlation

cor(Salary_hike,Churn_out_rate)
## Strong negative corelation

empreg <- lm(Churn_out_rate~Salary_hike,data = emp)
summary(empreg)
###R^2 value is high so a well explained model can be set up

test <-data.frame(Salary_hike=c(1580,1690,1700))
predict(empreg,newdata = test)


## Errors comparison table

Errtable <- data.frame(emp,predict(empreg),"Errtable"=Churn_out_rate-predict(empreg))
Errtable
View(Errtable)
plot(empreg)

### implementing Cooks distance & removing values above 0.5

emp1 <- emp[-c(1,9,10),]
emp1
empreg1 <- lm(Churn_out_rate~Salary_hike,data = emp1)
summary(empreg1)
plot(empreg1)
### R^2 and adjusted R^2 improved significant
test1 <-data.frame(Salary_hike=c(1580,1690,1700))
predict(empreg1,newdata = test1)

