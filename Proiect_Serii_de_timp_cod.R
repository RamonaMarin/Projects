# Loading and installing the relevant libraries for Time Series courses
if(!require("dplyr")) {install.packages("dplyr"); library("dplyr")}
if(!require("ggplot2")) {install.packages("ggplot2"); library("ggplot2")}
if(!require("forecast")) {install.packages("forecast"); library("forecast")}
if(!require("GGally")) {install.packages("GGally"); library("GGally")}
if(!require("readxl")) {install.packages("readxl"); library("readxl")}
if(!require("TSstudio")) {install.packages("TSstudio"); library("TSstudio")}
if(!require("lmtest")) {install.packages("lmtest"); library("lmtest")}
if(!require("Metrics")) {install.packages("Metrics"); library("Metrics")}
if(!require("uroot")) {install.packages("uroot"); library("uroot")}
if(!require("urca")) {install.packages("urca"); library("urca")}
if(!require("tseries")) {install.packages("tseries"); library("tseries")}

y <- read.csv("C:/Users/Bianca/Desktop/Serii de timp/Proiect/baza_energy.csv")

energy <- ts(y$Energy, start = 2008,frequency=4)
summary(energy)


# Time series plots

my_ggplot <- autoplot(energy) +
  ggtitle("Evolutia consumului de energie electrica intre anii 2008-2022 in Romania") +
  xlab("Trimestru") +
  ylab("GWh")

my_ggplot +
  theme(plot.title = element_text(hjust = 0.5)) 

# Seasonal plot
ggseasonplot(energy, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("GWh") +
  ggtitle("Grafic sezonier: Consum energie electrica")

# Seasonal subseries plots
ggsubseriesplot(energy) +
  ylab("GWh") +
  ggtitle("Sezonalitatea pe subserii trimestriale")


#inspecting the corelogram of the series

# ACF and PACF
ggAcf(energy, lag=50)
ggPacf(energy,lag=50)
energy  %>% ggtsdisplay()

# 
# # decomposition using stl() function - Seasonal and Trend decomposition with Loess
# energy.decomp <- stl(energy, s.window="periodic", robust=TRUE) 
# 
# autoplot(m.decomp)

## tESTING THE EXISTENCE OF A UNIT ROOT -ADF TESTS
adf_energy <- energy %>%
  ur.df(., type='trend', selectlags=c("AIC"))

summary(adf_energy) # accept null -existence of unit root tests

adf_energy <- energy %>%
  ur.df(., type='drift', selectlags=c("AIC"))

summary(adf_energy) # accept null -existence of unit root tests

adf_energy <- energy %>%
  ur.df(., type="none", selectlags=c("AIC"))

summary(adf_energy) # accept null -existence of unit root tests


## ZA TESTS
za_energy <- energy %>%
  ur.za(., model="both", lag = 1)

summary(za_energy)
plot(za_energy) # breaking the significance levels in the mid-sample


## KPSS TEST
kpss_energy <- energy %>%
  ur.kpss(., type="mu", use.lag=NULL) # mu for level stationary

summary(kpss_energy)       # able to reject null of stationarity

#seasonal differencing
energy_s <- energy %>% diff(lag = 4)
ggtsdisplay(energy_s) #the seasonal difference of the series seems non-stationary

# Spliting the data intro training and test sets
training <- window(energy_s, start=2009, end=c(2019,4))
test <- tail(energy_s, 4*3)

#APPLYING ARIMA MODELS ON Seasonally adjusted electrical equipment orders
auto.arima(training)

##########################################
fit1 <- Arima(training, order=c(1,0,1))
coeftest(fit1)
summary(fit1)
fit2 <- Arima(training, order=c(1,0,2))
coeftest(fit2)
summary(fit2)
fit3 <- Arima(training, order=c(1,0,0))
coeftest(fit3)
summary(fit3)
fit4 <- Arima(training, order=c(0,0,1))
coeftest(fit4)
summary(fit4)
fit5 <- Arima(training, order=c(0,0,2))
coeftest(fit5)
summary(fit5)
fit6 <- Arima(training, order=c(1,0,1), seasonal = c(1,0,0))
coeftest(fit6)
summary(fit6)
fit7 <- Arima(training, order=c(0,0,1), seasonal = c(1,0,0))
coeftest(fit7)
summary(fit7)
fit8 <- Arima(training, order=c(1,0,0), seasonal = c(1,0,0))
coeftest(fit8)
summary(fit8)

# Find the model with lowest AICc
arma_res_aicc <- rep(0,8)
arma_res_aicc[1] <- fit1$aicc 
arma_res_aicc[2] <- fit2$aicc 
arma_res_aicc[3] <- fit3$aicc 
arma_res_aicc[4] <- fit4$aicc 
arma_res_aicc[5] <- fit5$aicc 
arma_res_aicc[6] <- fit6$aicc 
arma_res_aicc[7] <- fit7$aicc 
arma_res_aicc[8] <- fit8$aicc 
which(arma_res_aicc == min(arma_res_aicc))

#The optimal model based on AICc is SARIMA(1,0,0)X(1,0,0)[4] model

# Find the model with lowest BIC
arma_res_bic <- rep(0,8)
arma_res_bic[1] <- fit1$bic 
arma_res_bic[2] <- fit2$bic 
arma_res_bic[3] <- fit3$bic 
arma_res_bic[4] <- fit4$bic 
arma_res_bic[5] <- fit5$bic
arma_res_bic[6] <- fit6$bic 
arma_res_bic[7] <- fit7$bic 
arma_res_bic[8] <- fit8$bic
which(arma_res_bic == min(arma_res_bic))
#The optimal model based on BIC is SARIMA(1,0,0)X(1,0,0)[4] model

#Having the optimal model the next step is to verify the quality of residuals
checkresiduals(fit8)
shapiro.test(fit8$residuals)
#########FORECAST#######
#predictions with forecast()
fit8 %>% 
  forecast(h=12) %>% 
  autoplot() +
  ylab("Consumul trimestrial de energie electrica")
#Forecasting
fore <- forecast(fit8, h = 12)
#suprapunere forecast pe test
autoplot(test)
autoplot(energy_s, series="Data") +
  autolayer(fore, series="SARIMA", PI=F) +
  ggtitle("Predictie SARIMA pe datele din test, pe 3 ani") +
  xlab("Trimestre") + ylab("Consumul de energie electrica(GWh)") +
  scale_colour_manual(values=c("Data"="gray","SARIMA"="red"),
                      breaks=c("Data","SARIMA"))


##testarea acuratei prognozei pentru SARIMA
round(forecast::accuracy(fore),2)

####################NETEZIRE##################################


# Exponential smoothing

# Simple exponential smoothing
# Estimate parameters
fc <- ses(training, h=12)
checkresiduals(fc)
# Accuracy of one-step-ahead training errors
round(forecast::accuracy(fc),2)
##              ME    RMSE    MAE    MPE   MAPE    MASE ACF1
##Training set 18.49  600   440.67 102.77 226.43   0.53    0

###### Forecast simple exponential smoothing
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Consum energie electrica (GWh)") + xlab("Trimestre")

####### ETS models 
fit_ets <- ets(training)
checkresiduals(fit_ets)

summary(fit_ets)
fit_ets %>% forecast(h=12) %>%
  autoplot() +
  ylab("Consum energie electrica (GWh)")

autoplot(fit_ets)

############MODELE ARCH-GARCH###########

list.packages<-c("fGarch", "PerformanceAnalytics","rugarch","tseries","xts","FinTS")
new.packages <- list.packages[!(list.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#Loading Packages
invisible(lapply(list.packages, require, character.only = TRUE))

w <- read.csv("C:/Users/Bianca/Desktop/Serii de timp/Proiect/baza_energy_m.csv")

energy_m <- ts(w$Energy, start = 2008,frequency=12)
summary(energy_m)
plot(energy_m)

#log_returns <- diff(log(trainARCH), lag=1)

return_train <- diff(energy_m ) / energy_m [-length(energy_m)]
return_train
ggtsdisplay(return_train)


#apply ADF test with drift
ADF_Returns = ur.df(return_train, type = "drift",selectlags = "AIC" )
#summary of he test
summary(ADF_Returns)
##seria randamentelor este stationara

# plot returns with squared and absolute returns
dataToPlot = cbind(return_train, return_train^2, abs(return_train))
colnames(dataToPlot) = c("Returns", "Returns^2", "abs(Returns)")
plot.zoo(dataToPlot, main="Randamentele consumului lunar de energie electrica", col="blue")

#histograme with normal density curve
options(repr.plot.width=21, repr.plot.height=11)#to set the figure size
hist(return_train,prob=T,breaks=50,xlab="Randamente lunare",main = "Histograma randamentelor ale consumului lunar de energie electrica",
     ylab="Probabillty Distribution",col="cornflowerblue", cex.lab=1, cex.axis=1,cex.main=1) 
mu<-mean(return_train)  
sigma<-sd(return_train)
x<-seq(min(return_train),max(return_train),length=80) 
y<-dnorm(x,mu,sigma) 
lines(x,y,lwd=2,col="red")  

#conduct Jarque-Bera test for normality
jarque.bera.test(return_train)
## Convert to xts for time series features
Return.energy<-as.xts(return_train)
# plot autocorrelations of returns, returns^2 and abs(returns)
options(repr.plot.width=15, repr.plot.height=5)
par(mfrow=c(1,3))
acf(return_train, main="Randamente",cex.main=10)
acf(return_train^2, main="Randamente patrate",cex.main=10)
acf(abs(return_train), main="Randamente absolute",cex.main=10)
par(mfrow=c(1,1))
# use Ljung Box.test from stats package to check auto correlation in square returns
Box.test(coredata(return_train), type="Ljung-Box", lag = 6)
Box.test(coredata(return_train^2), type="Ljung-Box", lag = 6)
Box.test(coredata(abs(return_train)), type="Ljung-Box", lag = 6)
#ARCH LM Test
ArchTest(return_train)
ArchTest(return_train^2)
ArchTest(abs(return_train))

#Specify the model
spec1 = ugarchspec(variance.model=list(garchOrder=c(1,0)),
                   mean.model=list(armaOrder=c(0,0)),distribution.model="norm")
spec2 = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(0,0)),distribution.model="norm")

# Fit ARCH Model
arch1.fit=ugarchfit(data=return_train,spec=spec1)
garch11.fit=ugarchfit(data=return_train,spec=spec2)
#fitted model outcome
arch1.fit
garch11.fit

## E-GARCH
spec3 <- ugarchspec(
  variance.model = list(
    model = "eGARCH",
    garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model="norm")
egarch1.fit=ugarchfit(data=return_train,spec=spec3)
egarch1.fit

## AP-garch
spec4 = ugarchspec(variance.model = list(model="apARCH",garchOrder=c(1,1)),
                   mean.model = list(armaOrder = c(0, 0)),
                   distribution.model="norm")
aparch11.fit=ugarchfit(data=return_train,spec=spec4)
aparch11.fit


##ACURATETEA MODELELOR ARCH-GARCH##
model.list = list("arch(1,0)" = arch1.fit,
                  "garch(1,1)" = garch11.fit,
                  "egarch(1,1)" = egarch1.fit,
                  "aparch(1,1)" = aparch11.fit)
info.mat = sapply(model.list, infocriteria)
rownames(info.mat) = rownames(infocriteria(garch11.fit))
info.mat

##FORECAST####
def.fit2= ugarchfit(spec = spec4, data =energy_m )
bootp=ugarchboot(def.fit2,method=c("Partial","Full")[1],n.ahead = 24,n.bootpred=1000,n.bootfit=1000)
bootp
plot(bootp, which=2)
plot(bootp, which=3)

s_f=bootp@forc@forecast$seriesFor #this is for series forecasts
s_f
s_f1=as.vector(s_f) #for comparison, we make the forecasts as vector.
s_f1
v_f=bootp@forc@forecast$sigmaFor#this is for variance forecasts
v_f

#######################ANALIZA SPECTRALA#################

a<-w$Energy
b <-energy_s
#periodogram
sp<-spectrum(a)
sp_deseas<-spectrum(b)

#Fast Discrete Fourier Transform
I <- abs(fft(a))^2/240
P <- (4/240)*I[1:120]
f <- 0:119/240
plot(f[-1],P[-1], type="l", xlab="Frequency", ylab="Power")

plot(sp$freq, sp$spec, type="l")
fmax=sp$freq[which.max(sp$spec)]
fmax

T=1/sp$freq[which.max(sp$spec)]
T
result=cbind(1/sp$freq,sp$spec)
c=diff(a,4)

plot(c, type="l")

acf(c)

sp1<-spectrum(c)
fmax=sp1$freq[which.max(sp1$spec)]
fmax
T=1/sp1$freq[which.max(sp1$spec)]
T

d=diff(c,8)
plot(d, type="l")
acf(d)

sp2<-spectrum(d)

fmax=sp2$freq[which.max(sp2$spec)]
fmax
T=1/sp2$freq[which.max(sp2$spec)]
T
d=diff(c,2)
acf(d)


####################MODELE MULTIVARIATE############################

x <- read.csv("C:/Users/ramon/Desktop/proiect serii/baza_serii.csv")

# Declaram variabilele de tip ts
energy <- ts(x$Energy, start = 2008,frequency=4)
GDP <-ts(x$GDP, start = 2008,frequency=4)

# Crearea unui df cu toate cele 3 variabile
dset <- cbind(energy,GDP)

# Graficul seriei
autoplot(cbind(energy,GDP)) + 
  ylab('') + 
  ggtitle('Consumul de energie electrică VS PIB') + 
  theme_bw()

# Testarea stationaritatii
adf.energy <- ur.df(energy, type = "trend", selectlags = "AIC")
summary(adf.energy) 

adf.GDP <- ur.df(GDP, type = "trend", selectlags = "AIC")
summary(adf.GDP) 

# ACF and PACF
ggAcf(GDP, lag=50)
ggPacf(GDP,lag=50)
GDP  %>% ggtsdisplay()

# Cointegrare

# Selectarea lagului 
lagselect <- VARselect(dset, lag.max =10, type = 'const',season = 4)
lagselect$selection 


# Testul Johansen - metoda Trace si Eigenvalue
ctest1 <- ca.jo(dset, type = 'trace', ecdet = 'const',K=2)
summary(ctest1) 
ctest2 <- ca.jo(dset, type = 'eigen', ecdet = 'const',K=2)
summary(ctest2) 

# Modelul VECM - metoda de estimare 2OLS 
Model1 <- VECM(dset,
               lag = 1, 
               r=1,
               estim = ('2OLS'))
summary(Model1)

# Diagnosticul pe reziduuri

# Trebuie sa transformam obiectul VECM in obiect VAR
Model1VAR <- vec2var(ctest2, r = 1)

# Autocorelare
Serial1 <- serial.test(Model1VAR, lags.pt = 5, type = 'PT.asymptotic')
Serial1 # avem autocorelare in reziduuri

# Heteroschedascitate
Arch1 <- vars::arch.test(Model1VAR, lags.multi = 10, multivariate.only = TRUE)
Arch1 # reziduuri homosechedastice

# Normalitate
Norm1 <- normality.test(Model1VAR, multivariate.only = TRUE)
Norm1 # reziduurile nu sunt normal distribuite

# Granger pe termen scurt
modelVar <- VAR(dset, p = 1, type = 'const', season = NULL, exog = NULL)
summary(modelVar)
Granger1 <- causality(modelVar, cause = 'GDP')
Granger1
Granger2 <- causality(modelVar, cause = 'energy')
Granger2

# Functia de raspuns la impuls
GDPirf <- irf(Model1VAR, impulse = 'GDP', response = 'energy', n.ahead= 8, boot = TRUE)
plot(GDPirf, ylab = 'GDP', main = 'GDP shock to energy')
energyirf <- irf(Model1VAR, impulse = 'energy', response = 'GDP', n.ahead= 8, boot = TRUE)
plot(energyirf, ylab = 'energy', main = 'Energy shock to GDP')

# Descompunerea variantei
FEVD1 <- fevd(Model1VAR,n.ahead=8)
FEVD1
plot(FEVD1)

# Prognoza 
forecast <- predict(Model1VAR, n.ahead = 8, ci = 0.95) # prognoza pe 8 trimestre (2 ani)

plot(forecast, name = 'energy')
plot(forecast, name = 'GDP')


#######################XGBOOST########################################

library(xgboost)
library(caret)
library(readxl)
library(dplyr)

data <- read.csv("C:/Users/Bianca/Desktop/Serii de timp/Proiect/baza_serii.csv")

# Declaram variabilele de tip ts
energy <- ts(data$Energy, start = 2008,frequency=4)
GDP <-ts(data$GDP, start = 2008,frequency=4)

# Crearea unui df cu toate cele 3 variabile
dset <- cbind(energy,GDP)

parts = createDataPartition(data$Energy, p = .8, list = F)

train = data[parts, ]
test = data[-parts, ]

#define predictor and response variables in training set
train_x = data.matrix(train[, 3])
train_y = train[,2]

#define predictor and response variables in testing set
test_x = data.matrix(test[, 3])
test_y = test[, 2]

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)


#defining a watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#fit XGBoost model and display training and testing data at each iteartion
model = xgb.train(data = xgb_train, eta=0.05 , max.depth = 2, watchlist=watchlist, nrounds = 100)

#define final model
model_xgboost = xgboost(data = xgb_train, eta=0.05, max.depth = 2, nrounds = 86, verbose = 0)

summary(model_xgboost)

#use model to make predictions on test data
pred_y = predict(model_xgboost, xgb_test)

# performance metrics on the test data

mse = mean((test_y - pred_y)^2)
mae = caret::MAE(test_y, pred_y)
rmse = caret::RMSE(test_y, pred_y)

cat("MSE: ", mse, "MAE: ", mae, " RMSE: ", rmse)

x = 1:length(test_y)                   # visualize the model, actual and predicted data
plot(x, test_y, col = "red", type = "l")
lines(x, pred_y, col = "blue", type = "l")
legend(x = 8, y = 12600,  legend = c("original test_y", "predicted test_y"), 
       col = c("red", "blue"), box.lty = 1, cex = 0.8, lty = c(1, 1))




