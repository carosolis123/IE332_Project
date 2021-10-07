data = USArrests

#ID is what we would have the players' name would be
states = rownames(dat)

#these will be our future variables
Murder = data$Murder
Assault = data$Assault
UrbanPop = data$UrbanPop
Rape = data$Rape

Y_test <- 0
data$Y_test <- 0

#install.packages('ggplot2')
library('ggplot2')

#Empirical Earnings Cumulative Density Function
ggplot(data.frame(ID6, Earnings), aes(Earnings)) + stat_ecdf(geom = "point")+
  labs(y = "F(Earnings)", x="ID")+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("Empirical Earnings Cumulative Density Function")

#Geom Earnings Density Plot
ggplot(data.frame(ID6, Earnings), aes(x = Earnings)) + 
  geom_density(color = 'darkblue', fill = 'lightblue') + 
  ggtitle("Geom Earnings Density Plot") +
  theme(plot.title = element_text(hjust = 0.5))

#The Shapiro Wilks test rejects the hypothesis of normality when the p-value is less than or equal to 0.05.
shapiro.test(Murder) 
#W = 0.95703, p-value = 0.06674 > 0.05 --> normal
shapiro.test(Assault) 
#W = 0.95181, p-value = 0.04052 < 0.05 -->  not normal
shapiro.test(UrbanPop) 
#W = 0.97714, p-value = 0.4385 > 0.05 -->  normal
shapiro.test(Rape) 
#W = 0.94674, p-value = 0.0251 < 0.05 --> not normal


#build model with Y_test as dependent on each variable
model = lm(Y_test ~ Murder + Assault + UrbanPop + Rape)
summary(model)

resid1 = model$residuals
plot(model)

stepmod = step(model, direction='both', trace=1)

library(car)
qqPlot(resid1, ylab='Residuals', main='Normal Probability Plot',
       pch=19,col='blue',xlab='Theoretical Quantiles')

hist(resid1, prob = T, xlab = "Residuals", 
     main = "Histogram of Residual Values")
lines(density(resid1), col = "red", lwd = 3)

n = length(resid1)

plot(c(1:n), resid1, type = 'b', xlab = "Observation Order", ylab = "Residuals", 
     main = "Residuals vs. Order Plot")
abline(h=0, lty = 2, col = 'red')

vif(model)
outlierTest(model)

outlierPlot = cooks.distance(model)
plot(outlierPlot, ylab = "Cook's Distance", 
     main = "Plot of Cooks Distance Values")

cooks = cooks.distance(model)
print(cooks)
#View(cooks)
plot(cooks, )

cooks_list = cooks[c(66,542,551,694)]
print(cooks_list) 
#^^cook's distances of the outliers from the Residuals vs Leverage graph

modelTransform = lm(log(Earnings) ~ Main + HighDeg + Control + Admits + 
                      EngrPercent + FullTime + FamilyIncome)
summary(modelTransform)

resid2 = modelTransform$residuals
plot(modelTransform)

plot(model)

model = lm(FamilyIncome ~ Admits + EngrPercent + FullTime + Earnings)
summary(model)
