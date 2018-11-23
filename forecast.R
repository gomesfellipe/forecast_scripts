# https://www.datascience.com/blog/decomposition-based-approaches-to-time-series-forecasting?hs_amp=true

# - -----------------------------------------------------------------------
# Forecast 
# - -----------------------------------------------------------------------

library(forecast)
library(ggplot)

# tema
theme_set(theme_bw())


# Treino, Teste e Validacao -----------------------------------------------

# Separando dados em test and train 
train <- window(AirPassengers, end = c(1958, 12))
test  <- window(AirPassengers, start = c(1959, 1), end = c(1960,12))


# STL ARIMA ---------------------------------------------------------------

# Decomposição sazonal e tendência usando LOESS (STL)
AirPassengersSTL <- stl(AirPassengers, s.window="periodic", robust=TRUE)
plot(AirPassengersSTL)

# Train the STL model, using ARIMA to forecast the remainder 
AirPassengersSTL_ARIMA <- stlf(train, method="arima")


# LM ----------------------------------------------------------------------

# Decomposição sazonal e tendência usando LOESS (STL)
AirPassengersTSLM <- tslm(train ~ trend + season, data = train)
fc <- forecast(AirPassengersTSLM, h = 12*2)


# Grafico dos resultados --------------------------------------------------

#Plot dos resultados
options(repr.plot.width=8, repr.plot.height=4)
autoplot(train , ylab = 'Passengers') +
  scale_x_yearmon() +
  autolayer(test, series="Test Data") +
  autolayer(AirPassengersSTL_ARIMA$mean, series="STL + ARIMA")+
  autolayer(fc$mean, series="LMTS")
