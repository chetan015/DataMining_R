library(MASS)
cor(LCD$Smoker==1,LCD$Lung.Capacity..cc.)
cor(LCD$Smoker==0,LCD$Lung.Capacity..cc.)

cor(LCD$Gender==1,LCD$Smoker==1)
cor(LCD$Gender==0,LCD$Smoker==1)
cor(LCD$Gender==1,LCD$Smoker==0)
cor(LCD$Gender==0,LCD$Smoker==0)


plot(LCD$Lung.Capacity..cc.,LCD$Exercise)
LC<- LCD$Lung.Capacity..cc.
S<- LCD$Smoker
boxplot(LC~S==1, xlab="Smoker", ylab="LungCapacity", main="Boxplot of Lungcapacity", col="RED")
