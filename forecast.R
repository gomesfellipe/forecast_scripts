###########################################################################
# Em construção
###########################################################################

# - -----------------------------------------------------------------------
# Forecast 
# - -----------------------------------------------------------------------

library(forecast)
library(ggplot2)
library(bsts)
library(prophet)
library(timetk)

# tema
theme_set(theme_bw()+theme(legend.position = "bottom"))

serie <- tk_ts(base$value,frequency = 12, start = c(1996,01))
# Treino, Teste e Validacao -----------------------------------------------

# Separando dados em test and train 
train <- window(serie, start = c(1996,01), end = c((end(serie)[1]-1), 12))
test  <- window(serie, start = c(end(serie)[1], 1), end = c(end(serie)[1], end(serie)[2]))

# Base Line ---------------------------------------------------------------

fc_naive     <- naive(train, 6) 
fc_snaive     <- snaive(train, 6) 

# STL-ARIMA ---------------------------------------------------------------

# Decomposição sazonal e tendência usando LOESS (STL)
trainSTL <- stl(train, s.window="periodic", robust=TRUE)
plot(trainSTL)

# Train the STL model, using ARIMA to forecast the remainder 
trainSTL_ARIMA <- stlf(train, method="arima",h = 6)

# TSLM ----------------------------------------------------------------------

# Decomposição sazonal e tendência usando LOESS (STL)
trainTSLM <- tslm(train ~ trend * season, data = train)
fc <- forecast(trainTSLM, h = 6)

# Série Temporal Estrutural Bayesiana -------------------------------------

#Train the BSTS model 
Trend_Seasonal_states <- AddSemilocalLinearTrend(list(),train)
Trend_Seasonal_states <- AddSeasonal(Trend_Seasonal_states,train, nseasons = 12)
trainBSTS <- bsts(train,state.specification = Trend_Seasonal_states, niter = 1000)

fc_BSTS <- predict.bsts(trainBSTS, horizon = 6)
fc_BSTS <- ts(fc_BSTS$mean,frequency=12, start=c(end(serie)[1],1))

# NNETAR ----------------------------------------------------------------------

trainNNETAR <- nnetar(train)
fc_NNETAR <- forecast(trainNNETAR, h = 6)


# Prophet Facebook --------------------------------------------------------
#Create a data frame in the right format for Prophet
FBTrain <- data.frame(ds = as.Date(as.yearmon(time(train))), y=as.matrix(train))

#Generate a model and then generate forecasts 
m <- prophet(FBTrain,yearly.seasonality=TRUE)

future <- make_future_dataframe(m, periods = 6, freq = 'month')
forecast <- predict(m, future)


ProphetForecast <- ts(forecast$yhat,frequency=12, start=start(serie))
ProphetForecast <- window(ProphetForecast, start = c(end(serie)[1], 1), end = c(end(serie)[1],end(serie)[2]))

# Grafico dos resultados --------------------------------------------------

#Plot dos resultados
options(repr.plot.width=8, repr.plot.height=4)

g <- 
  autoplot(serie , ylab = 'Passengers') +
  scale_x_yearmon() +
  autolayer(trainSTL_ARIMA$mean, series="STL + ARIMA")+
  autolayer(fc$mean, series="LMTS") +
  autolayer(fc_naive$mean, series="Naive") +
  autolayer(fc_snaive$mean, series="Sazonal Naive") +
  autolayer(fc_NNETAR$mean, series="NNETAR")+
  autolayer(fc_BSTS, series="BSTS") +
  autolayer(ProphetForecast, series="Prophet Facebook")
  

plotly::ggplotly(g) %>% 
  htmlwidgets::saveWidget("forecasts.html")



# Referencias: ------------------------------------------------------------

# https://www.datascience.com/blog/decomposition-based-approaches-to-time-series-forecasting?hs_amp=true


