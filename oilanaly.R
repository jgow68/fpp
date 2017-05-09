# load library fpp
library(fpp)
library(xts)

# test oil price, rigs, liquid demand & supply

# Setup data and time series ----------------------------------------------

### yet to be resolved
eiadata = read.csv("eia extract (Oct16).csv", header=T)
str(eiadata)
eiadatats = ts(eiadata, start=c(1990,1,1), frequency=12)
eiadatats

eiadatats[eiadatats["date"]>=as.Date('2003-03-01') & eiadatats["date"]<=as.Date('2016-07-01')]


eiadata[,1] = as.Date(eiadata[,1], format = "%d/%m/%Y") # convert dates to date format
eiadata_xts = xts(eiadata[,-1], order.by=as.Date(eiadata$date)) # form an extensible time series data
summary(eiadata_xts)


# extract data from Mar 2003 till Oct 2016 for the rig count to oil price analysis
testdata = eiadata_xts["2003-03-01::2016-10-01"]
anyNA(testdata) # check for any NAs, sig fast than any(is.na(x))
datats = as.ts(testdata)
datats = ts(datats, start=c(2003,3), frequency=12)



# ARIMA onshore / offshore rig with oil price regressors ------------------

# (optional) try to see if seasonality is required
plot.ts(grig)
abline(v=c(2004:2016),lty=2) # seasonality seems to exist in normal years excl. crisis years 2008-09, 2014-16
cor(datats[,1], datats[,2]) # shows negative correlation between oil price and rig count => bad
 
# consider approach to analyze impact of lagged oil prices 
oilpr = datats[,1] # oil price 
lrig = datats[,28] # land rigs
osrig = datats[,29] # offshore rigs
grig = datats[,30] # global rigs

# (optional) correlation between oil price and rigs are positive => good sign
cor(oilpr, lrig); cor(oilpr, osrig); cor(oilpr, grig)

# create dataset with lagged oil price

createlagdata = function(x, y) {
  
  c = as.matrix(y)
  b = x + 1
  a = matrix(NA, nrow=nrow(c), ncol=b)
  
  for (i in 1:b){
    a[,i] = c(rep(NA, i-1), c)[1:length(c)]
  }
  colnames(a) = paste("y", 0:x, sep="")
  print(a)
}

oilprlag6 = createlagdata(6, oilpr) # oil price lag up to 6 months
oilprlag12 = createlagdata(12, oilpr) # oil price lag up to 12 months
oilprlag24 = createlagdata(24, oilpr) # oil price lag up to 24 months

# identify number of oil price lags from model with the least aicc
sapply(1:12, function(x) (auto.arima(grig, xreg=oilprlag12[,1:x])$aicc)) # Results show that more historical oil price gives better model for global rigs
sapply(1:12, function(x) (auto.arima(osrig, xreg=oilprlag12[,1:x])$aicc)) # Results suggest to use max lag of oil price for offshore rig model


# refit model with suitablenumber of oil price lag
grigpr = auto.arima(grig, xreg=oilprlag6) # ARIMA(1,1,0)(1,0,0)[12]
grigpr = auto.arima(grig, xreg=oilprlag12) # ARIMA(1,1,0)(2,0,0)[12)
osrigpr = auto.arima(osrig, xreg=oilprlag12) # ARIMA(1,1,0)(2,0,0)[12]
grigpr = auto.arima(grig, xreg=oilprlag12, stepwise=FALSE, approximation=FALSE) # same result: ARIMA(1,1,0)(2,0,0)[12] 


# (optional) try to set model with non-seasonality
grigpr_NS = auto.arima(grig, xreg=oilprlag12, seasonal=F) # ARIMA(2,1,0)
plot(grigpr_NS$residuals)

# (optional) residuals looks like white noise although the grigpr model's forecast is suspicious
plot(grigpr$residuals)
plot(osrigpr$residuals)
Acf(grigpr$residuals)
Acf(osrigpr$residuals)


plot(forecast(grigpr, xreg=cbind(rep(50,6), # assume 50/bbl oil price for the next 6 months
                                 c(oilprlag6[161], rep(50,5)),
                                 c(oilprlag6[160:161], rep(50,4)),
                                 c(oilprlag6[159:161], rep(50,3)),
                                 c(oilprlag6[158:161], rep(50,2)),
                                 c(oilprlag6[157:161], rep(50,1)),
                                 c(oilprlag6[156:161])
                                 )
             )
)
                                 
plotlag = function(x, y, z, f){
  # x = oil price lag data, need to be same length as ARIMA model f
  # y = oil price to be inputed
  # z = number of months for repetition
  # f = ARIMA model
  
  a = ncol(x) # ncol of lag data, here ncol(oilprlag6) = 7
  b = nrow(x)
  historic = b # set range for historical data for predictor (oil price)
  predictor = matrix(nrow=z, ncol=a)
  
  predictor[,1] = rep(y, z) # repeat oil price y for z months, say 50/bbl for 6 months
  for (i in 2:a){
    repetition = z + 1 # repetition = 7
    if( repetition - i > 0){ 
      predictor[,i] = c(x[historic:b], rep(y, repetition-i))
      historic = historic - 1
    } else {
      predictor[,i] = c(x[historic:b])
      historic = historic - 1
    }
      
  }
  # list("data" = predictor) can be used to return multiple information in lists
  plot(forecast(f, xreg=predictor))
}

# we assume 50/bbl price for 24 months
forecast1 = plotlag(oilprlag12, 50, 24, grigpr) # forecast for global rigs using oil price with lag 12 months, looks suspicious
plotlag(oilprlag12, 50, 24, osrigpr) # forecast for offshore rigs using oil price with lag 12 months
plotlag(oilprlag12, 50, 24, grigpr_NS) # forecast shows a simple model

predictor_1 = plotlag(oilprlag12, 50, 24, osrigpr) # for testing only, extract predictors from the plot function

plot(forecast(osrigpr, xreg=predictor_1), xaxp=c(2003, 2018, 3), ylab="Rig count") # xaxp set start and end date with intervals set by xaxp
osrig_fcst = forecast(osrigpr, xreg=predictor_1)
abline(h=c(200,250,300), lty=3)
abline(v=c(2003:2018), lty=3)

# to identify col and data names
which(colnames(datats)=="Global_L")
grep("Global", colnames(datats))
# match("Global_OS", names(datats)) not applicable in time series? but ok in df?



# Testing new function in fpp - accuracy ----------------------------------

accuracy(grigpr)
accuracy(grigpr_NS)


# Neural Network Autoregression -------------------------------------------

plot(forecast(nnetar(osrig))) # Model: NNAR(1,1,2)[12] = yt-1, yt-12 inputs with 2 neurals in hidden layer


# VAR model ---------------------------------------------------------------

library(vars)
VAR_variable = cbind(Brent=datats[,1],Wells=datats[,2],liquid_surplus=datats[,3]-datats[,4]-datats[,5]) # variables are Brent price, active wells, liquid demand - supply
VAR_p1 = VAR(cbind(VAR_variable), p=1, type="both")
summary(VAR_p1)

ts.plot(VAR_variable)

# check if data contains non-stationary components 
summary(ur.df(VAR_variable[, 1], type = "drift", lags = 10, selectlags = "AIC")) # do not reject H0
summary(ur.df(VAR_variable[, 2], type = "drift", lags = 10, selectlags = "AIC")) # do not reject H0
summary(ur.df(VAR_variable[, 3], type = "drift", lags = 10, selectlags = "AIC")) # do not reject H0
# * need to check for different types?
# https://stats.stackexchange.com/questions/24072/interpreting-rs-ur-df-dickey-fuller-unit-root-test-results

# select order of the model
VARselect(VAR_variable, lag.max = 10, type="both")

# check co-integration
summary(ca.jo(VAR_variable, type = "eigen", ecdet = "const", K = 7)) # K= lag length, retrieved from the optimal lag length
summary(ca.jo(x, type = "trace", ecdet = "const", K = 7))


# subsequent steps
# https://stats.stackexchange.com/questions/191851/var-forecasting-methodology