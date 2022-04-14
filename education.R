library(readxl)

projectdata <- read_excel("C:/c/projectdata.xls")

library(tibble)
library(dplyr)
library(ggplot2)

qplot(data = projectdata, x = log(Ill_people), y = log(VRP))
qplot(data = projectdata, x = log(Capital_investments), y = log(VRP))
qplot(data = projectdata, x = log(Employment), y = log(VRP))
qplot(data = projectdata, x = log(High_Education), y = log(VRP))
qplot(data = projectdata, x = log(Meat_consumption), y = log(VRP))


model_nonlin = lm(formula = log(VRP) ~ log(Employment) + log(Capital_investments), data = projectdata)
summary(model_nonlin)


model_nonlin1 = lm(formula = log(VRP) ~
                     log(Employment) + log(Capital_investments)+
                     log(High_Education),
                   data = projectdata)
model_nonlin2 = lm(formula = log(VRP) ~
                     log(Employment) + log(Capital_investments)+
                     log(Ill_people),
                   data = projectdata)
model_nonlin3 = lm(formula = log(VRP) ~
                     log(Employment) + log(Capital_investments)+
                     log(Meat_consumption),
                   data = projectdata)
summary(model_nonlin1)
summary(model_nonlin2)
summary(model_nonlin3)
library("lmtest")
bptest(model_nonlin2, ~ log(VRP) - log(Capital_investments)-
         log(Employment)-log(High_Education) ,
       data = projectdata, studentize = TRUE)

install.packages('stargazer')
library(stargazer)

install.packages(c('sandwich', 'lmtest', 'regclass'))
library(sandwich)
library(lmtest)
library(regclass)

HC_model_res = coeftest(model_nonlin, vcov = vcovHC(model_nonlin))
HC_model_res
HC_model_res1 = coeftest(model_nonlin1, vcov = vcovHC(model_nonlin1))
HC_model_res1
HC_model_res2 = coeftest(model_nonlin2, vcov = vcovHC(model_nonlin2))
HC_model_res2
HC_model_res3 = coeftest(model_nonlin3, vcov = vcovHC(model_nonlin3))
HC_model_res3


ui=log(projectdata[,2])-log(projectdata[,4])-log(projectdata[,3])-log(projectdata[,5])
mean(ui$VRP)

summary(projectdata$VRP)
summary(projectdata$Employment)
summary(projectdata$Capital_investments)
summary(projectdata$High_Education)


help(boxplot)
boxplot(projectdata$VRP)
boxplot(projectdata$Employment)
boxplot(projectdata$Capital_investments)
boxplot(projectdata$Ill_people)
boxplot(projectdata$High_Education)
boxplot(projectdata$Meat_consumption)
indVRP <- which(projectdata$VRP %in%
                  boxplot.stats(projectdata$VRP)$out)
indCapital <- which(projectdata$Capital_investments %in%
                      boxplot.stats(projectdata$Capital_investments)$out)
indEmployment <- which(projectdata$Employment %in%
                         boxplot.stats(projectdata$Employment)$out)

indHE <- which(projectdata$High_Education %in%
                 boxplot.stats(projectdata$High_Education)$out)
VIF(model_nonlin)
VIF(model_nonlin1)
help(VIF)
confint(model_nonlin)
confint(model_nonlin1)
median(projectdata$Capital_investments)
max(projectdata$VRP)
max(projectdata$Employment)
max(projectdata$Capital_investments)
max(projectdata$High_Education)
help(var)
var(projectdata$VRP)
var(projectdata$Employment)
var(projectdata$Capital_investments)
var(projectdata$High_Education)