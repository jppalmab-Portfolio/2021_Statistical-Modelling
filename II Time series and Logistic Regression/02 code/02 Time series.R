rm(list = ls())

library(pacman)
p_load(stargazer, xtable, texreg, dplyr, ggplot2,hrbrthemes,fpp2, tseries)

#Describing
df=read.csv("01 data/eComm_US.csv")

str(df)
tsales <-ts(df$ECOMNSA, start=c(1999,4), end=c(2021,2), frequency=4)
tsales

start(tsales)
end(tsales)
?plot
plot(tsales, main="Time serie description raw data")

# seeing the graph a a trend pattern exist

#seasonalplot
ggseasonplot(tsales, year.labels = TRUE, year.labels.left = TRUE)+
  ylab("billions US$") +
  ggtitle("Seasonal plot: e-commerce retail sales (in $billions)")

ggsubseriesplot(tsales) +
  ylab("billions US$") +
  ggtitle("Seasonal subseries plot: e-commerce retail sales (in $billions)")


#an increasing trend is present. 
# decline at the begining of the year, small increase in the first three period 
# and a significcat incresae in q4, except year 2020 and 2021.


#additive

#multiplicative

plot(tsales) #multiplicative is appropiate in here
seasonplot(tsales)

fit_dec_mul = decompose(tsales, type="multiplicative")
fit_dec_mul
plot(fit_dec_mul)
      ###check the assumption

#####
# Decomposition by Loess Smoothing
log_tsale = log(tsales)

plot(log_tsale, ylab="log(tsales")

fit_log_tsales = stl(log_tsale, s.window="period")
plot(fit_log_tsales)

exp(fit_log_tsales$time.series)
########################################################
########################################################
############### EXPONENTIAL SMOOTHING ##################
########################################################
########################################################

#############################
#Automated
fit_sales_auto = ets(tsales, model='ZZZ')
fit_sales_auto #ETS(M,A,M)


################################################################################
## Simple
# A simple exponential smoothing model (also called a single exponential model) 
# fits a time series that has a constant level and an irregular component at time 
# i but has neither a trend nor a seasonal component. Its only smoothing parameter 
# is level.
################################################################################

fit_sales_simple = ets(tsales, model = "ANN")
fit_sales_simple

fit_sales_simple_f= forecast(fit_sales_simple,3)

round(accuracy(fit_sales_simple),2)

          # Remember that these forecasts will only be suitable if the 
          # time series has no trend or seasonal component.


# can we dicard this?

autoplot(fit_sales_simple_f)

################################################################################
## Doble
# Holt expoonential smoothing
# A double exponential model (also called a Holt exponential smoothing) fits a 
# time series with both a level and a trend.
################################################################################

    # This model is appropriate for series in which there is a linear trend and 
    # no seasonality. Its smoothing parameters are level and trend, which are not 
    # constrained by each other's values.

fit_sales_doble = ets(tsales, model="AAN")
fit_sales_doble_f = forecast(fit_sales_doble,3)

round(accuracy(fit_sales_doble),2)

autoplot(fit_sales_doble_f)

################################################################################
## Tripple
# Holt-Winters exponential smoothing
# A triple exponential model (also called a Holt-Winters exponential smoothing) 
# fits a time series with level, trend, and seasonal components.
################################################################################

# The Holt-Winters seasonal method comprises the forecast equation and three 
# smoothing equations — one for the level, one for the trend , and one for the 
# seasonal component , with corresponding smoothing parameters α, β and γ.
#     • There are two variations to this method that differ in the nature of the 
#       seasonal component.
#           – The additive method is preferred when the seasonal variations are 
#             roughly constant through the series, while
#           – the multiplicative method is preferred when the seasonal variations 
#             are changing proportional to the level of the series.

?hw
fit_sales_triple_add = hw(tsales, seasonal = "additive")
fit_sales_triple_mul = hw(tsales, seasonal = "multiplicative")

autoplot(tsales)+
  autolayer(fit_sales_triple_add, series = "HW additive forecast", PI=FALSE)+
  autolayer(fit_sales_triple_mul, series = "HW multiplicative forecast", PI=FALSE)+
  xlab("Year")+
  ylab("billions US$")+
  ggtitle("Ecommerce Sales in US (in $billions)")+
  guides(colour=guide_legend(title = "Forecast"))


# Other method
#fit_sales_triple_1 = ets(tsales, model="AAA")
fit_sales_triple_2 = ets(tsales, model="MAM")

fit_sales_triple

fit_sales_triple_f1=forecast(fit_sales_triple_1)
fit_sales_triple_f2=forecast(fit_sales_triple_2)


round(accuracy(fit_sales_triple),2)

autoplot(fit_sales_triple_f2)



autoplot(tsales)+
  autolayer(fit_sales_triple_add, series = "HW additive forecast", PI=FALSE)+
  autolayer(fit_sales_triple_mul, series = "HW multiplicative forecast", PI=FALSE)+
  #autolayer(fit_sales_triple_f1, series = "Forecast model (A,A,A)", PI=FALSE)+
  autolayer(fit_sales_triple_f2, series = "Forecast model (M,A,M) \n R Recommended", PI=FALSE)+
  xlab("Year")+
  ylab("billions US$")+
  ggtitle("Ecommerce Sales in US (in $billions)")+
  guides(colour=guide_legend(title = "Forecast"))


#######################
### ASSIGNMENT
fit_sales_simple
fit_sales_doble
#fit_sales_triple_add
#fit_sales_triple_mul
fit_sales_triple_1
#fit_sales_triple_2
fit_sales_auto
auto.arima(tsales)

########################################################
########################################################
##################### ARIMA/SARIMA #####################
########################################################
########################################################

#Plot the tsales Time Series
plot(tsales)
autoplot(tsales)

#Check the order of differencing required
nsdiffs(tsales) #give you number of diff that have to apply

#Plot the differenced tsales Time Series
dtsales <- diff(tsales)
plot(dtsales)

#Assess stationarity of the differenced series
adf.test(dtsales) #p value reject null hyphotesis
adf.test(dtsales)

#ACF/PACF plots. Choosing p and q
Acf(dtsales)
Pacf(dtsales)

#############################
fit_sales_arima <- arima(tsales, order=c(1,1,0))
fit_sales_arima

#Evaluating Model fit_sales_arima
qqnorm(fit_sales_arima$residuals)
qqline(fit_sales_arima$residuals)
Box.test(fit_sales_arima$residuals, type="Ljung-Box")
#p value 0.2416 // i dont want to be signifcant

checkresiduals(fit_sales_arima)
accuracy(fit_sales_arima)

#Forecasting with the fit_sales_arimated model
forecast(fit_sales_arima, 2) # forecast for thre period ahead
plot(forecast(fit_sales_arima, 2), xlab="Year", ylab="Sales")

autoplot(forecast(fit_sales_arima, 4))

#########################################
#auto ARIMA function
plot(tsales)  #deferent dataset
fit_sales_arima_auto <- auto.arima(tsales)
fit_sales_arima_auto

################################################################################
#Seasonal ARIMA

library(fpp2)
tsales
plot(tsales)
plot(decompose(tsales))

#Use auto.arima function
auto.arima(tsales)
# ARIMA(1,1,0)(1,1,0)

fit_sales_sarima<-Arima(tsales,order = c(1,1,0),seasonal = c(1,1,0))
summary(fit_sales_sarima)

#checkresiduals
checkresiduals(fit_sales_sarima)

fit_sales_sarima %>%forecast(h=12) %>% autoplot()

forecast(fit_sales_sarima,h=3)

accuracy(fit_sales_sarima)

#fit_sales_sarima

#Evaluating Model fit_sales_sarima
qqnorm(fit_sales_sarima$residuals)
qqline(fit_sales_sarima$residuals)
Box.test(fit_sales_sarima$residuals, type="Ljung-Box")
#p value 0.2416 // i dont want to be signifcant
checkresiduals(fit_sales_sarima)
accuracy(fit_sales_sarima)

###############3
#fit sales auto
qqnorm(fit_sales_auto$residuals)
qqline(fit_sales_auto$residuals)
Box.test(fit_sales_auto$residuals, type="Ljung-Box")
#p value 0.2416 // i dont want to be signifcant

checkresiduals(fit_sales_auto)

#fit_sales_auto
#fit_sales_sarima




#########33

fit <- stl(tsales, t.window=13, s.window="periodic",
           robust=TRUE)

fit %>% seasadj() %>% naive() %>%
  autoplot() + ylab("E-commerce Sales US (in $billions)") +
  ggtitle("Naive forecasts of seasonally adjusted data")

fit %>% forecast(method="naive") %>%
  autoplot() + ylab("E-commerce Sales US (in $billions)")

summary(fit_sales_auto)

  