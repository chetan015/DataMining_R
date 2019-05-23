library(MASS)
summary(Cars93$Type)

plot(Cars93$MPG.city,Cars93$Weight,col="BLUE",xlab="MPG", ylab="Weight", main="MPG Vs Weight")

GPM<-1/Cars93$MPG.city
plot(GPM,Cars93$Weight,col="BLUE",main="GPM Vs Weight",xlab="GPM", ylab="weight")

plot(Cars93$Weight,GPM,abline(lm(GPM~Cars93$Weight)),col="BLUE",main="Linear Regression - GPM vs Weight", xlab="Weight", ylab="GPM")

plot(Cars93$Type,Cars93$MPG.city, xlab="Type", ylab="MPG", main="poor and good fuel economy cars", col="RED")

