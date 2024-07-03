x<- c(0,1,2,3,4,5,6)
px<- c(.1,.2,.2,.1,.2,.1,.1)
mean<-sum(px*x)
mean
variance<-sum((x^2)*px)-mean^2
variance
sd<-sqrt(sum((x^2)*px)-mean^2)
t<-sum((x-mean)^4*px)
kurtosis = t/(sd)^4
print(kurtosis)

list(mean=mean,variance=variance,kurtosis=kurtosis)
