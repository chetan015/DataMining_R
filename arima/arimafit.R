library('ggplot2')
library('forecast')
library('tseries')
daily_data = read.csv("Bike-Sharing-Dataset/day.csv",header=TRUE, stringsAsFactors=FALSE)
daily_data$Date = as.Date(daily_data$dteday)
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")
count_ts = ts(daily_data[, c('cnt')])
daily_data$clean_cnt = tsclean(count_ts)


x = daily_data$clean_cnt
diff_x = diff(x)
plot(diff_x, type="l")
adf.test(diff_x)
ph<-30
n=731
data1=diff_x[1:(n-ph)]
acf(data1)
pacf(data1)

data = x[1:(n-ph)]
ar111=arima(data,order = c(1,1,1))
pred111=predict(ar111, n.ahead=ph)
plot(x,type="l", xlim=c(680,731))
lines(pred111$pred,type="p", col="red")
vec1=x[(n-ph+1):n] - pred111$pred
mse1=mean(vec1*vec1)

model=c("arima100")
err2=c(mse1)
err= data.frame(model,err2)
print(err)


aicval<-c(AIC(ar111))
aicdata= data.frame(model,aicval)
print(aicdata)


