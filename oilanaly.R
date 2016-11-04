# test oil price, rigs, liquid demand & supply

eiadata = read.csv("eia extract (Oct16).csv", header=T)
summary(eiadata)

# partition oil price data to start from 1/1/1994
oilpr = eiadata$Brent_price[-c(1:48)]
liqdem = eiadata$Liquid_demand[-c(1:48)]
OPECsup = eiadata$OPEC_supply[-c(1:48)]
nOPECsup = eiadata$nonOPEC_supply[-c(1:48)]
liqsup = OPECsup + nOPECsup
funda = liqdem - liqsup

diffoil = c(NA, diff(oilpr))
diffunda = c(NA, diff(funda))

# scale data by their mean and s.d. respecvitvely
oilpriceadj = scale(oilpr)
fundaadj = scale(funda)

# problem: liqdem and liqsup very similar
pricefunda = lm(oilpr ~ liqdem + liqsup)
summary(pricefunda)

summary(lm(diffoil ~ funda)) # there is relationship, but different lengths in the first place, reasonable?


# dynamic regression model ------------------------------------------------

### create time series object of oil price and (demand-supply) change
eiadata_changets = ts(cbind(diffoil,diffunda), start=1994, frequency=12)
ts.plot(eiadata_changets)

auto.arima(eiadata_changets[-1,1], xreg=eiadata_changets[-1,2], ic="aic") # shows ARIMA(1,0,0) to be the best fit

prfuts = Arima(eiadata_changets[-1,1], # fit AR(1) model to oil price  
               xreg=eiadata_changets[-1,2], # external reg = (demand - supply) change
               order=c(1,0,0)) 
plot(forecast(prfuts, xreg=rep(0,12))) # forecast for the next 12 months assuming there is no supply/demand surplus

### test the impact of lagged (demand-supply) change to oil prices

eiadatats = ts(cbind(oilpr, funda), start=1994, frequency=12)
summary(eiadatats)

# create lagged predictors of funda (demand-supply)
fundalag = cbind(funda,
                 c(NA, funda[1:270]),
                 c(NA,NA, funda[1:269]),
                 c(NA,NA,NA, funda[1:268]),
                 c(NA,NA,NA,NA, funda[1:267])
                 )

fundalag = function(x) {
    
  b = x + 1
  a = matrix(NA, nrow=271, ncol=b)
  
  for (i in 1:b){
    a[,i] = c(rep(NA, i-1), funda)[1:length(funda)]
      }
  colnames(a) = paste("fundaLag", 0:x, sep="")
  print(a)
}
  
fundalag4 = fundalag(4) # create dat of funda with lag 0 to 4
fundalag11 = fundalag(11) # create dat of funda with lag 0 to 11

fundalagfit = function(x){ # pass the fundalag outputs to here
  a = ncol(x)
  # fit = list()
  
  for (i in 1:a){ # fit columns start from lag 0 to lag a
    fit[i] = (auto.arima(oilpr[a:271], xreg=x[a:271,1:i], d=0))$aicc
      }
  
  b = which.min(fit) # choose model with lowest aicc
  c= round(fit[b], 4) # indicate aicc values for best fit
  paste("best fit is with lag", b-1, "with AICC", c)
  
  # print(which.min(ICval))
}

fundalagfit(fundalag11) 
# lowest AICC for ARIMA fit of oil price and lagged funda is lag 2 & AICC 1557.0995


prfundalag <- auto.arima(oilpr, xreg=fundalag11[,1:3], d=0) # refit model with funda lag 2, columns 1 to 3
prfundalag

## Test model for residuals
# A Ljung-Box test for residuals correlation
Box.test(residuals(fit2),fitdf=5,lag=10,type="Ljung")
# Normality test for residuals
prfundalagRES = prfundalag$residuals[-c(1:2)] # remove NAs

qqnorm(prfundalagRES)
qqline(prfundalagRES)
jarque.bera.test(prfundalagRES) # reject H0, res dun folo Normal
shapiro.test(prfundalagRES) # reject H0, res dun folo Normal
acf(prfundalagRES)
pacf(prfundalagRES)
# dunno how to solve this HAHA
# for dynamic regression models, we allow errors from regression to contain autocorrelation
# but the condition is variables must be stationary?


## forecast model
prfundalagFCST <- forecast(prfundalag, 
                           xreg=cbind(rep(0,20), # forecast 20 months ahead assuming funda stays at 0
                                c(fundalag11[271,1],rep(0,19)), # use actual values for first lag
                                c(fundalag11[271,2], fundalag11[271,1], rep(0,18))), # use the actual values for first 2 lag
                           h=20)

plot(prfundalagFCST, main="Forecast oil price with fundamanetal set to 0", ylab="Oil Price")


