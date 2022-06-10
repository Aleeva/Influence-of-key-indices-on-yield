library(fpp3)
library(tidyverse)
library(urca)
library(quantmod)
library(forecast)
library(tseries)
library(FinTS)
library(rugarch)
library(lubridate)
library(forecast)
library(corrplot)

library(sandwich)
library(lmtest)
library(car)
library(zoo)
library(xts)
library(dplyr)
library(broom)
library(ggplot2)
library(forcats)

rts_0 <- read.table(file = "C:/Users/lenovo/Downloads/MOEX.ME_rts.csv",
                  header = TRUE,
                  stringsAsFactors = FALSE, sep = ";")
gazp_0 <- read.table(file = "C:/Users/lenovo/Downloads/GAZP.ME.csv", 
                  header =TRUE, 
                  stringsAsFactors = FALSE, sep = ";")
luk_0 <- read.table(file = "C:/Users/lenovo/Downloads/LKOH.ME.csv",
                    header = TRUE,
                    stringsAsFactors = FALSE, sep = ";")
chmf_0 <- read.table(file = "C:/Users/lenovo/Downloads/CHMF.ME.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE, sep = ";")
irao_0 <- read.table(file = "C:/Users/lenovo/Downloads/IRAO.ME.csv",
                     header = TRUE,
                     stringsAsFactors = FALSE, sep = ";")

rts <- tibble(date=dmy(rts_0[,1]),adjusted=rts_0[,6])
gazp <- tibble(date=dmy(gazp_0[,1]),adjusted=gazp_0[,6])
luk <- tibble(date = dmy(luk_0[,1]), adjusted = luk_0[,6])
chmf <- tibble(date = dmy(chmf_0[,1]), adjusted = chmf_0[,6])
irao <- tibble(date = dmy(irao_0[,1]), adjusted = irao_0[,6])

plot(rts, type = 'l', main = 'RTS')
plot(gazp, type = 'l', main = 'GAZP' )
plot(luk, type = 'l', main = 'LUKOIL')
plot(chmf, type = 'l', main = 'SEVERSTAL')
plot(irao, type = 'l', main = 'IRAO')


"rts.train <- timeseries.rts[1:100]
rts.test <-	timeseries.rts[101:108]
ga.train <- timeseries.rts[1:100]
rts.test <-	timeseries.rts[101:108]
class(rts.train)
rts_train_ts <- ts(rts.train, start=start(ts), 
                   end=end(ts), freq=4)
class(rts_train_ts)"
timeseries.rts <- ts(rts[,2], start=2013-05-01,
                     end=2027-04-01, freq=4)
timeseries.gazp <- ts(gazp[,2], start=2013-05-01,
                      end=2027-04-01, freq=4)
timeseries.luk <- ts(luk[,2], start=2013-05-01,
                     end=2027-04-01, freq=4)
timeseries.chmf <- ts(chmf[,2], start=2013-05-01,
                     end=2027-04-01, freq=4)
timeseries.irao <- ts(irao[,2], start=2013-05-01,
                      end=2027-04-01, freq=4)

tsdisplay(timeseries.rts)
tsdisplay(timeseries.gazp)
tsdisplay(timeseries.luk)
tsdisplay(timeseries.chmf)
tsdisplay(timeseries.irao)

dl.rts <- diff(log(timeseries.rts))
dl.luk <- diff(log(timeseries.luk))
dl.chmf <- diff(log(timeseries.chmf))
tsdisplay(dl.rts)
tsdisplay(dl.luk)
tsdisplay(dl.chmf)


adf.test(timeseries.rts)
adf.test(timeseries.gazp)
adf.test(timeseries.luk)
adf.test(timeseries.chmf)

dr <- diff(timeseries.rts)
tsdisplay(dr)
dg <- diff(timeseries.gazp)
tsdisplay(dg)
dl <- diff(timeseries.luk)
tsdisplay(dl)
dc <- diff(timeseries.chmf)
tsdisplay(dc)
di <- diff(timeseries.irao)
tsdisplay(di)

mod_a_rts <- auto.arima(timeseries.rts)
summary(mod_a_rts)

mod_a_gazp <- auto.arima(timeseries.gazp)
mod_a_gazp
summary(mod_a_gazp)

mod_a_luk <- auto.arima(timeseries.luk)
summary(mod_a_luk)

mod_a_chmf <- auto.arima(timeseries.chmf)
summary(mod_a_chmf)

mod_a_irao <- auto.arima(timeseries.irao)
summary(mod_a_irao)
mod_a_luk_1 <- auto.arima(dl)
summary(mod_a_luk_1)

prognoz_rts <- forecast(mod_a_rts)
prognoz_rts
plot(prognoz_rts)

prognoz_gazp <- forecast(mod_a_gazp)
prognoz_gazp
plot(prognoz_gazp)

prognoz_luk <- forecast(mod_a_luk)
prognoz_luk
plot(prognoz_luk)

prognoz_chmf <- forecast(mod_a_chmf)
prognoz_chmf
plot(prognoz_chmf)

prognoz_irao <- forecast(mod_a_irao)
prognoz_irao
plot(prognoz_irao)

arima_gazp <- Arima(timeseries.gazp, order = c(0,1,0),include.drift = T)  
                    summary(arima_gazp)
arima_gazp_0 <- Arima(timeseries.gazp, order = c(1,1,0))
                    summary(arima_gazp_0)
arima_rts <- Arima(timeseries.rts, order = c(1,1,0),include.drift = T)  
                    summary(arima_rts)
arima_rts_0 <- Arima(timeseries.rts, order = c(2,1,0))
                    summary(arima_rts_0)

                    tsdisplay(dl.rts)

ArchTest(dl.rts, lags = 5)
ArchTest(timeseries.rts, lags  = 5)
ArchTest(timeseries.gazp, lags = 5)
ArchTest(timeseries.luk, lags = 5)
ArchTest(timeseries.chmf, lags = 5)
ArchTest(timeseries.irao, lags = 5)

jarque.bera.test(resid_arima_rts)
jarque.bera.test(resid_arima_gazp)
jarque.bera.test(resid_arima_luk)
jarque.bera.test(resid_arima_chmf)
jarque.bera.test(resid_arima_irao)

adf.test(dl.rts)

resid_arima_rts <- resid(mod_a_rts)
resid_arima_rts
resid_arima_gazp <- resid(mod_a_gazp)
resid_arima_gazp
resid_arima_luk <- resid(mod_a_luk)
resid_arima_luk
resid_arima_chmf <- resid(mod_a_chmf)
resid_arima_chmf
resid_arima_irao <- resid(mod_a_irao)
resid_arima_irao

ArchTest(resid_arima_rts, lags  = 5)
ArchTest(resid_arima_gazp, lags = 5)
ArchTest(resid_arima_luk, lags = 5)
ArchTest(resid_arima_chmf, lags = 5)
ArchTest(resid_arima_irao, lags = 5)

Box.test(resid_arima_rts, lag = 10, type = "Ljung-Box", fitdf = 2)
"=> hypothesis doesn't reject"
resid_arima_gazp <- resid(mod_a_gazp)
Box.test(resid_arima_gazp, lag = 10, type = "Ljung-Box", fitdf = 2)
"hypothesis doesn't reject"
"model correctly describe the structure of autocorrelation"

Box.test(resid_arima_luk, lag = 10, type = "Ljung-Box", fitdf = 2)

Box.test(resid_arima_chmf, lag = 10, type = "Ljung-Box", fitdf = 2)

Box.test(resid_arima_irao, lag = 10, type = "Ljung-Box", fitdf = 2)

forecast(resid_arima_gazp, h = 20)

forecast(arima_rts_0, h = 20)
forecast(arima_gazp,h = 20)
future <- forecast(arima_rts, h = 20)

future_rts <- forecast(arima_rts, h = 20)
autoplot(future_rts)

future_rts_1 <- forecast(arima_rts_1, h = 20)
autoplot(future_rts_1)

future_gazp <- forecast(arima_gazp, h = 20)
autoplot(future_gazp)
AIC(arima_gazp)
BIC(arima_rts)

library(tidyverse)
library(broom)

model_lin_gazp = lm(formula = gazp_0$Adj.Close ~ rts_0$Adj.Close)
summary(model_lin_rts)

model_lin_luk = lm(formula = luk_0$Adj.Close ~ rts_0$Adj.Close)
summary(model_lin_luk)

model_lin_chmf = lm(formula = chmf_0$Adj.Close ~ rts_0$Adj.Close)
summary(model_lin_chmf)

model_lin_irao = lm(formula = irao_0$Adj.Close ~ rts_0$Adj.Close)
summary(model_lin_irao)


boxplot(gazp_0$Adj.Close)
boxplot(rts_0$Adj.Close)

library(ggplot2)
hist.adverts <- ggplot(rts_0$Adj.Close, aes(adverts))+
  geom_histogram(aes (y = ..density..))+theme_bw()+
  labs(x = 'date', y = 'adj.price')
hist.adverts
barplot(rts_0$Adj.Close)
barplot(gazp_0$Adj.Close)

median(gazp_0$Adj.Close)
median(rts_0$Adj.Close)
median(luk_0$Adj.Close)
median(chmf_0$Adj.Close)
median(irao_0$Adj.Close)

max(gazp_0$Adj.Close)
max(rts_0$Adj.Close)

var(rts_0$Adj.Close)
var(gazp_0$Adj.Close)
var(luk_0$Adj.Close)
var(chmf_0$Adj.Close)
var(irao_0$Adj.Close)

mean(gazp_0$Adj.Close)
mean(rts_0$Adj.Close)

summary(gazp_0$Adj.Close)
summary(rts_0$Adj.Close)
summary(luk_0$Adj.Close)
summary(chmf_0$Adj.Close)
summary(irao_0$Adj.Close)




library(ggplot2)


ggplot(rts_0, aes(x = rts_0$Adj.Close, y = gazp_0$Adj.Close)) +
geom_point()+
geom_smooth(se=FALSE, color='red')

ggplot(rts_0, aes(x = rts_0$Adj.Close, y = luk_0$Adj.Close)) +
  geom_point()+
  geom_smooth(se=FALSE, color='red')

ggplot(rts_0, aes(x = rts_0$Adj.Close, y = chmf_0$Adj.Close)) +
  geom_point()+
  geom_smooth(se=FALSE, color='red')

ggplot(rts_0, aes(x = rts_0$Adj.Close, y = irao_0$Adj.Close)) +
  geom_point()+
  geom_smooth(se=FALSE, color='red')



plot.ts(timeseries.rts)
plot.ts(timeseries.gazp)
plot.ts(timeseries.luk)
plot.ts(timeseries.chmf)
plot.ts(timeseries.irao)

Box.test(timeseries.rts, lag = 10, type = 'Ljung-Box')
Box.test(timeseries.gazp, lag = 10, type = 'Ljung-Box')
Box.test(timeseries.luk, lag = 10, type = 'Ljung-Box')



devtools::install_github('angusmoore/seasthedata')
library(fpp3)
library(tidyverse)


library(urca)
library(seasthedata)


res_adf_rts = ur.df(timeseries.rts, type = 'trend', selectlags = 'AIC')
  summary(res_adf_rts)

res_adf_gazp = ur.df(timeseries.gazp, type = 'drift', selectlags = 'AIC')
  summary(res_adf_gazp)

res_adf_luk = ur.df(timeseries.luk, type = 'trend', selectlags = 'AIC')
  summary(res_adf_luk) 

res_adf_chmf = ur.df(timeseries.chmf, type = 'trend', selectlags = 'AIC')
  summary(res_adf_chmf) 

res_adf_irao = ur.df(timeseries.irao, type = 'trend', selectlags = 'AIC')
  summary(res_adf_irao) 

rts$date <- as.Date(rts$date,"%Y,%m,%d",frequency=12)
glimpse(rts$date)
data = mutate(rts, Date = yearmonth(Date))

data_tr_rts <- rts[1:100,]
data_test_rts <- rts[101:108,]

data_tr_gazp <- gazp[1:100,]
data_test_gazp <- gazp[101:108,]

rts.rs <- rts_0$Adj.Close
gazp.rs <- gazp_0$Adj.Close

rts.train <- rts.rs[1:1245]
rts.test <- rts.rs[1246:1250]

del.rts<- diff(log(rts.rs))
del.rts.train <- del.rts[1:95]
del.rts.test <- dl.rts[96:100]

tsdisplay(del.rts.train)
tsdisplay(del.rts.test)

mod_tab = model(data_tr_rts,
                mod = ARIMA(adjusted ~ 1 + t + pdq(1,0,0) + PDQ(1,0,0)))

gg_tsresiduals(select(mod_tab, mod))

x<-(rts[,1])
plot(res_adf_rts)
plot(res_adf_gazp)
plot(res_adf_luk)
plot (res_adf_chmf)
plot (res_adf_irao)


gg_tsdisplay(x, rts$adjusted, plot_type = 'partial')


glimpse(data)
x<-as.Date(rts_0[,1])
class(x)
a <- as.Date(rts_0[,1], "%d.%m.%Y", frequency = 12) 
glimpse(a)
class(a)
b<-rts[,1]
data = as_tsibble(b, index = a)
head(data)
gg_tsdisplay(timeseries.rts, plot_type = 'partial')
class(timeseries.rts)


data_rts = as_tsibble(rts, index = date)
head(data_rts)
data_gazp = as_tsibble(gazp, index = date)
head(data_gazp)
data_luk = as_tsibble(luk, index = date)
head(data_luk)

autoplot(data_rts, data_rts$adjusted)
autoplot(data_gazp, data_gazp$adjusted)
autoplot(data_luk, data_luk$adjusted)

res_adf_rts_1 = ur.df(data_rts$adjusted, type = 'trend', selectlags = 'AIC')
summary(res_adf_rts_1)

res_adf_gazp_1 = ur.df(data_gazp$adjusted, type = 'trend', selectlags = 'AIC')
summary(res_adf_gazp_1)

res_adf_luk_1 = ur.df(data_luk$adjusted, type = 'trend', selectlags = 'AIC')
summary(res_adf_luk_1)

plot(res_adf_rts_1)
plot(res_adf_gazp_1)
plot(res_adf_luk_1)

tsdisplay(data_rts)
tsdisplay(data_gazp)


res_adf_rts_2 = ur.df(data_rts$adjusted, type = 'trend', lags = 10, selectlags = 'AIC')
summary(res_adf_rts_2)

library("dplyr")
library("memisc")
library("ggplot2")
ds <- diff(timeseries.chmf)

#“есты на структурные сдвиги

library(strucchange)

model1 <- Fstats(dr~1, from = 10)
sctest(model1)
strucchange::breakpoints(dr~1)

model2 <- Fstats(dg~1, from = 10)
sctest(model2)
strucchange::breakpoints(dg~1)

model3 <- Fstats(dl~1, from = 10)
sctest(model3)
strucchange::breakpoints(dl~1)

model4 <- Fstats(di~1, from = 10)
sctest(model4)
strucchange::breakpoints(di~1)

model5 <- Fstats(ds~1, from = 10)
sctest(model5)
strucchange::breakpoints(ds~1)



res_adf_dl.rts = ur.df(dl.rts, type = 'trend', selectlags = 'BIC')
summary(res_adf_dl.rts)



future <- forecast(mod_a_rts, h = 5)
autoplot(future)
future_rts <- forecast(arima_rts, h = 20)
autoplot(future_rts)

future_rts_1 <- forecast(arima_rts_1, h = 20)
autoplot(future_rts_1)

future_gazp <- forecast(arima_gazp, h = 20)
autoplot(future_gazp)

prognoz_luk <- forecast(mod_a_luk, h = 20)
autoplot(prognoz_luk)


library(fpp3)
library(tidyverse)
library(aTSA)
library(urca)
library(lmtest)
library(readxl)
library(ARDL)
library(vars)
library(gridExtra)


diff_rts <- diff(timeseries.rts)
diff_gazp <- diff(timeseries.gazp)
diff_luk <- diff(timeseries.luk)
diff_irao <- diff(timeseries.irao)
diff_chmf <- diff(timeseries.chmf)

jotest=ca.jo(dset, type="trace")
summary(jotest)
lagselect <- VARselect(data.frame(diff_rts, diff_gazp, diff_luk, diff_irao), 
                       lag.max = 7, type = 'const')
dset <- data.frame(diff_rts, diff_gazp, diff_chmf)
lagselect <- VARselect(dset, lag.max = 8, type = 'const')
lagselect$selection
#1 lag is optimal#
#1-1=1 lag#
ctest1t <- ca.jo(dset, type = 'trace', K=7)

#data.frame(rts$adjusted, gazp$adjusted)#
summary(ctest1t)
coint.test(dset, timeseries.rts)
# тест Ёнгла-√рейнджера
# остатки линейной модели провер€ютс€ на стационарность ADF тестом
coint.test(timeseries.rts, as.matrix(timeseries.gazp,timeseries.luk))
# найдем коинтеграцию
mod_1 = lm(timeseries.rts ~ timeseries.gazp + timeseries.luk + timeseries.irao)
summary(mod_1)
library(vars)
library(nfilter)
mod1<- VAR(data.frame(timeseries.rts,timeseries.gazp, timeseries.irao),p=1,type = 'trend')
summary(mod1)
