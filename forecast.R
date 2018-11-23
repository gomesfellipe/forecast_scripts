###########################################################################
# Em construção
###########################################################################

# - -----------------------------------------------------------------------
# Forecast 
# - -----------------------------------------------------------------------

library(forecast)
library(ggplot)

# tema
theme_set(theme_bw()+theme(legend.position = "bottom"))


# Treino, Teste e Validacao -----------------------------------------------

# Separando dados em test and train 
train <- window(AirPassengers, end = c(1959, 12))
test  <- window(AirPassengers, start = c(1960, 1), end = c(1960,12))


# Base Line ---------------------------------------------------------------

fc_naive     <- naive(train, 12) 
fc_snaive     <- snaive(train, 12) 

# STL-ARIMA ---------------------------------------------------------------

# Decomposição sazonal e tendência usando LOESS (STL)
trainSTL <- stl(train, s.window="periodic", robust=TRUE)
plot(trainSTL)

# Train the STL model, using ARIMA to forecast the remainder 
trainSTL_ARIMA <- stlf(train, method="arima",h = 12)

# TSLM ----------------------------------------------------------------------

# Decomposição sazonal e tendência usando LOESS (STL)
trainTSLM <- tslm(train ~ trend + season, data = train)
fc <- forecast(trainTSLM, h = 12)

# NNETAR ----------------------------------------------------------------------

trainNNETAR <- nnetar(train)
fc_NNETAR <- forecast(trainNNETAR, h = 12)


# Grafico dos resultados --------------------------------------------------

#Plot dos resultados
options(repr.plot.width=8, repr.plot.height=4)

g <- 
  autoplot(AirPassengers , ylab = 'Passengers') +
  scale_x_yearmon() +
  autolayer(trainSTL_ARIMA$mean, series="STL + ARIMA")+
  autolayer(fc$mean, series="LMTS") +
  autolayer(fc_naive$mean, series="Naive") +
  autolayer(fc_snaive$mean, series="Sazonal Naive") +
  autolayer(fc_NNETAR$mean, series="NNETAR")

plotly::ggplotly(g)
