install.packages('car')
install.packages('dplyr')
install.packages('ggplot2')

library(car)
library(dplyr)
library(ggplot2)


data1 = read.csv('Data2014_3.csv')

colnames(data1) = c("Country", "Status", "Life.expectancy", "Adult.Mortality", "Infant.Deaths", "Alcohol", "percentage.expenditure" , "HepB" ,"Measles" , "BMI" , "under.five.deaths" , "Polio", "Total.expenditure" , "Diphtheria", "HIV.AIDS", "thinness.5.9.years", "Income.composition.of.resources", "Schooling", "Population", "GDP")
data1 = data1[-c(21)]

sample <- sample(c(TRUE, FALSE), nrow(data1), replace=TRUE, prob=c(0.9,0.1))
train  <- data1[sample, ]
test   <- data1[!sample, ]

model1 = lm(Life.expectancy ~ . - Country, data = train)

summary(model1)

vif(model1)

#removed Infant.Deaths and percentage.expenditure

model2 = lm(Life.expectancy ~ . - Country - Infant.Deaths - percentage.expenditure, data = train)
summary(model2)

vif(model2)

#removed Income composition of resources
model3 = lm(Life.expectancy ~ . - Country - Infant.Deaths - percentage.expenditure - 
            Income.composition.of.resources, data = train)
summary(model3)

vif(model3)

#removed Measles because of high corr with Population
model4 = lm(Life.expectancy ~ . - Country - Infant.Deaths - percentage.expenditure - 
            Income.composition.of.resources - Measles , data = train)
summary(model4)

vif(model4)

#residual analysis on complete model
options(repr.plot.width = 5, repr.plot.height = 5)

mean(resid(model4))
plot(fitted(model4), resid(model4), xlab = 'fitted values', ylab = 'residuals')
plot(fitted(model4), rstandard(model4), xlab = 'fitted values', ylab = 'standardised residuals')
plot(cooks.distance(model4), ylab = 'Cooks Distance')
qqnorm(resid(model4))
qqline(resid(model4))

hist(resid(model4),breaks=20, xlab = 'Residuals')
plot(train$BMI[!is.na(train$BMI)], model4$residuals[!is.na(train$BMI)], xlab = 'BMI', ylab = 'residuals')
plot(train$Adult.Mortality[!is.na(train$Adult.Mortality)], model4$residuals[!is.na(train$Adult.Mortality)], xlab = 'Adult Mortality', ylab = 'residuals')
plot(train$Alcohol[!is.na(train$Alcohol)], model4$residuals[!is.na(train$Alcohol)], xlab = 'Alcohol', ylab = 'residuals')
plot(train$HepB[!is.na(train$HepB)], model4$residuals[!is.na(train$HepB)], xlab = 'HepB', ylab = 'residuals')
plot(train$BMI[!is.na(train$BMI)], model4$residuals[!is.na(train$BMI)], xlab = 'BMI', ylab = 'residuals')
plot(train$under.five.deaths[!is.na(train$under.five.deaths)], model4$residuals[!is.na(train$under.five.deaths)], xlab = 'Under 5 Deaths', ylab = 'residuals')
plot(train$Polio[!is.na(train$Polio)], model4$residuals[!is.na(train$Polio)], xlab = 'Polio', ylab = 'residuals')
plot(train$Total.expenditure[!is.na(train$Total.expenditure)], model4$residuals[!is.na(train$Total.expenditure)], xlab = 'Total Expenditure', ylab = 'residuals')
plot(train$Diphtheria[!is.na(train$Diphtheria)], model4$residuals[!is.na(train$Diphtheria)], xlab = 'Diphtheria', ylab = 'residuals')
plot(train$HIV.AIDS[!is.na(train$HIV.AIDS)], model4$residuals[!is.na(train$HIV.AIDS)], xlab = 'HIV/AIDS', ylab = 'residuals')
plot(train$thinness.5.9.years[!is.na(train$thinness.5.9.years)], model4$residuals[!is.na(train$thinness.5.9.years)], xlab = 'Thinness 5-9 years', ylab = 'residuals')
plot(train$Schooling[!is.na(train$Schooling)], model4$residuals[!is.na(train$Schooling)], xlab = 'Schooling', ylab = 'residuals')
plot(train$Population[!is.na(train$Population)], model4$residuals[!is.na(train$Population)], xlab = 'Population', ylab = 'residuals')
plot(train$GDP[!is.na(train$GDP)], model4$residuals[!is.na(train$GDP)], xlab = 'GDP', ylab = 'residuals')

options(repr.plot.width = 6, repr.plot.height = 6)

train[cooks.distance(model4)>4/183,]
outs = as.numeric(names(cooks.distance(model4))[(cooks.distance(model4) > (4/nrow(train)))])
train_wo_outs  = train[-outs, ]

#data without country column, had trouble testing data without this
model4_noc = lm(Life.expectancy ~ . - Infant.Deaths - percentage.expenditure - 
            Income.composition.of.resources - Measles , data = subset(train, select = -c(Country)))
summary(model4_noc)

test2 = subset(test, select = -c(Country, Life.expectancy))

#prediction on complete model
pred1 = predict(model4_noc, newdata = test, interval="prediction", level=0.90)
plot(test$Life.expectancy[!is.na(pred1[,1])], pred1[,1][!is.na(pred1[,1])], ylim = c(30, 100), xlab = 'Actual Life Expectancy', ylab = 'Predicted Life Expectancy')
title('Test Results with Complete Model')
abline(0, 1)
points(test$Life.expectancy[!is.na(pred1[,1])], pred1[ ,2][!is.na(pred1[,1])], col = 'red', pch = '-')
points(test$Life.expectancy[!is.na(pred1[,1])], pred1[ ,3][!is.na(pred1[,1])], col = 'blue', pch = '-')
RMSE1 = sqrt(mean((pred1[,1] - test$Life.expectancy)^2, na.rm = TRUE))

#reduced model
model5_noc = lm(Life.expectancy ~ . - Infant.Deaths - percentage.expenditure - Income.composition.of.resources - Measles - 
            - GDP - BMI - Polio - under.five.deaths - Population - Total.expenditure - Alcohol, 
                data = subset(train, select = -c(Country)))

#prediction on reduced model
pred2 = predict(model5_noc, newdata = test, interval="prediction", level=0.90)
plot(test$Life.expectancy[!is.na(pred2[,1])], pred2[,1][!is.na(pred2[,1])], ylim = c(30, 100), xlab = 'Actual Life Expectancy', ylab = 'Predicted Life Expectancy')
title('Test Results with Reduced Model')
abline(0, 1)
points(test$Life.expectancy[!is.na(pred2[,1])], pred2[ ,2][!is.na(pred2[,1])], col = 'red', pch = '-')
points(test$Life.expectancy[!is.na(pred2[,1])], pred2[ ,3][!is.na(pred2[,1])], col = 'blue', pch = '-')
RMSE2 = sqrt(mean((pred2[,1] - test$Life.expectancy)^2, na.rm = TRUE))

#residual analysis on reduced model
options(repr.plot.width = 6, repr.plot.height = 6)

mean(resid(model5_noc))
plot(fitted(model5_noc), resid(model5_noc), xlab = 'Fitted Values', ylab = 'residuals')
plot(fitted(model5_noc), rstandard(model5_noc), xlab = 'Fitted Values', ylab = 'standardised residuals')
plot(cooks.distance(model5_noc), ylab = 'Cooks Distance')
qqnorm(resid(model5_noc))
qqline(resid(model5_noc))
hist(resid(model5_noc),breaks=20, ylab = 'Residuals')

plot(subset(train, select = -c(Country))$Status[!is.na(subset(train, select = -c(Country))$Status)], model5_noc$residuals[!is.na(subset(train, select = -c(Country))$Status)], xlab = 'Status', ylab = 'residuals')
plot(subset(train, select = -c(Country))$Adult.Mortality[!is.na(subset(train, select = -c(Country))$Adult.Mortality)], model5_noc$residuals[!is.na(subset(train, select = -c(Country))$Adult.Mortality)], xlab = 'Adult Mortality', ylab = 'residuals')
plot(subset(train, select = -c(Country))$HepB[!is.na(subset(train, select = -c(Country))$HepB)], model5_noc$residuals[!is.na(subset(train, select = -c(Country))$HepB)], , xlab = 'HepB', ylab = 'residuals')
plot(subset(train, select = -c(Country))$Diphtheria[!is.na(subset(train, select = -c(Country))$Diphtheria)], model5_noc$residuals[!is.na(subset(train, select = -c(Country))$Diphtheria)], xlab = 'Diphtheria', ylab = 'residuals')
plot(subset(train, select = -c(Country))$HIV.AIDS[!is.na(subset(train, select = -c(Country))$HIV.AIDS)], model5_noc$residuals[!is.na(subset(train, select = -c(Country))$HIV.AIDS)], xlab = 'HIV/AIDS', ylab = 'residuals')
plot(subset(train, select = -c(Country))$thinness.5.9.years[!is.na(subset(train, select = -c(Country))$thinness.5.9.years)], model5_noc$residuals[!is.na(subset(train, select = -c(Country))$thinness.5.9.years)], xlab = 'Thinness 5-9 years', ylab = 'residuals')
plot(subset(train, select = -c(Country))$Schooling[!is.na(subset(train, select = -c(Country))$Schooling)], model5_noc$residuals[!is.na(subset(train, select = -c(Country))$Schooling)], xlab = 'Schooling', ylab = 'residuals')

#checking cooks distance
train[cooks.distance(model5_noc)>4/nrow(train),]

train2 = subset(train, select = -c(Infant.Deaths,percentage.expenditure,
            Income.composition.of.resources,Measles))

#Backwards Stepwise Regression
AIC(lm(Life.expectancy ~ . - Country, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - BMI, data = train2))

AIC(lm(Life.expectancy ~ . - Country - under.five.deaths, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Total.expenditure, data = train2))

AIC(lm(Life.expectancy ~ . - Country - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Schooling, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Population, data = train2))

AIC(lm(Life.expectancy ~ . - Country - GDP, data = train2))
#dropped polio

#next round
AIC(lm(Life.expectancy ~ . - Country - Polio, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - BMI, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - under.five.deaths, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - Total.expenditure, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - Schooling, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - Population, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP, data = train2))
#Dropped GDP

#next round
AIC(lm(Life.expectancy ~ . - Country - Polio - GDP, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - BMI, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - under.five.deaths, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Schooling, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Population, data = train2))
#Dropped total expenditure

#next round
AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - under.five.deaths, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - Schooling, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - Population, data = train2))
#Dropped BMI

#next round
AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - Schooling, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - Population, data = train2))
#Dropped under 5 deaths

#next round
AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Schooling, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population, data = train2))
#Dropped Population

#next round
AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Schooling, data = train2))
#Dropped Alcohol

#next round
AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol - Status, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol - Adult.Mortality, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol - HepB, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol - HIV.AIDS, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol - thinness.5.9.years, data = train2))

AIC(lm(Life.expectancy ~ . - Country - Polio - GDP - Total.expenditure - BMI - under.five.deaths - Population - Alcohol - Schooling, data = train2))
#stopped dropping - no model has lower AIC 
