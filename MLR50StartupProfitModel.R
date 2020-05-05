###Coose File
startup <- read.csv(file.choose())
library(lattice)
###Data Analysis
View(startup)
startup1=startup[-4]
View(startup1)
attach(startup1)
summary(startup1)
colnames(startup1)

dotchart(R.D.Spend,color = "dodgerblue",pch = 20,main ="R.D.Spend" )
boxplot(R.D.Spend,col = "dodgerblue",pch = 20,main ="R.D.Spend" )
dotchart(Administration,color = "dodgerblue",pch = 20,main ="Administration")
boxplot(Administration,col = "dodgerblue",pch = 20,main ="Administration")
dotchart(Marketing.Spend,color = "dodgerblue",pch = 20,main ="Marketing.Spend")
boxplot(Marketing.Spend,col = "dodgerblue",pch = 20,main ="Marketing.Spend")
dotchart(Profit,color = "dodgerblue",pch = 20,main ="Profit")
boxplot(Profit,col = "dodgerblue",pch = 20,main ="Profit")
dotplot(Profit,color = "dodgerblue",pch = 20,main ="Profit")

pairs(startup1)

panel.cor <- function(x,y,digits=2,prefix="",cex.cor)
{
  usr <- par("usr");on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt <- format(c(r,0.123456789),digits=digits)[1]
  txt <- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(startup1,upper.panel = panel.cor,main="scatterplot matrix with correlation coefficients")

startupreg <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup1)
summary(startupreg)
### Variation Inflation Factor
vif(startupreg)
##Administration & Marketing Doesnot show p-value significance on Profit
##checking each individually
regadm <- lm(Profit~Administration,data = startup1)
summary(regadm)
regmkt <- lm(Profit~Marketing.Spend,data = startup1)
summary(regmkt)
residualPlots(startupreg)


##Administration is sill insignificant & Marketing is signigicance

## Now droping Administration
startupreg1 <- lm(Profit~R.D.Spend+Marketing.Spend,data = startup1)
summary(startupreg1)

##Checking with AIC
library(MASS)
stepAIC(startupreg) 


## marketing is nearly significant 
install.packages("car")
library(car)
install.packages("corpcor")
library(corpcor)
plot(startupreg1)
influenceIndexPlot(startupreg1)
## Model Deletion & Diagonistics
startup2 <- startup1[-c(47,48,49,50),]
View(startup2)
startupreg2 <- lm(Profit~R.D.Spend+Marketing.Spend,data = startup2)
summary(startupreg2)
plot(startupreg2)
influenceIndexPlot(startupreg2)
residualPlot(startupreg2)

###Added Variable Plots
avPlot(startupreg,id.n=3,id.cex=0.8,col=red)###Unaple to execute

      
       
       
       
       
