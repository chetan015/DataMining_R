library('ggplot2')
library('forecast')
library('tseries')
x = AirPassengers
plot(x)
adf.test(diff(log(x)))

acf(x)
pacf(x)

auto.arima(x)
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

pred <- predict(fit, n.ahead = 10*12)

ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))
vec1=x[(n-ph+1):n] - pred211$pred
mse1=mean(vec1*vec1)

model=c("arima211")
err2=c(mse1)
err= data.frame(model,err2)
print(err)


aicval<-c(AIC(ar211))
aicdata= data.frame(model,aicval)
print(aicdata)


