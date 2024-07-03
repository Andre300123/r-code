
Voltage<-read_xls("C:/Users/Andreas/Downloads/VOLTAGE.XLS")
his<- hist(Voltage$VOLTAGE[Voltage$LOCATION == "OLD"], xlab= "Voltage", main= "voltage histogram")
old<- c(9.98,10.26,10.05,10.29,10.03,8.05,10.55,10.26,9.97,9.87,10.12,10.05
        ,9.8,10.15,10,9.87,9.55,9.95,9.7,8.72,9.84,10.15,10.02,9.8,9.73,10.01,9.98,8.72,8.8,9.84)
stem(old)        
his2 <- hist(Voltage$VOLTAGE[Voltage$LOCATION == "NEW"], main = "new")
new<- c(9.19,9.63,10.1,9.7,10.09,9.6,10.05,10.12,9.49,9.37,10.01,8.82,9.43,10.03,9.85
  ,9.27,8.83,9.39,9.48,9.64,8.82,8.65,8.51,9.14,9.75,8.78,9.35,9.54,9.36,8.68)
a<-mean(old)
median(old)
b<-sd(old)
x<-mean(new)
median(new)
y<-sd(new)
boxplot(new)
boxplot(old)
scale(new)
z<- (new-x)/y
print(z)

hist3 <- hist(LUMPYORE$PCTIRON)
iron <- c(62.66,62.87,63.22,63.01,62.1,63.43,63.22,63.57,61.75,63.15,63.08
,63.22,63.22,63.08,62.87,61.68,62.45,62.1,62.87,62.87,62.94,62.38,61.82
,63.01,63.01,62.8,62.8,63.01,62.1,63.29,63.37,61.75,63.29,62.38,62.59
,63.92
          ,63.29
          ,63.57
          ,62.8
          ,62.31
          ,63.01
          ,62.94
          ,63.08
          ,63.43
          ,62.24
          ,63.43
          ,62.87
          ,63.64
          ,63.92
          ,63.71
          ,63.64
          ,64.06
          ,62.73
          ,62.52
          ,62.1
          ,63.29
          ,63.01
          ,63.36
          ,63.08
          ,62.03
          ,64.34
          ,64.06
          ,62.87
          ,63.5
          ,63.78,62.1
)
m<-mean(iron)
s<-sd(iron)
m-2*s
m+2*2
quantile(iron)
quantile(iron, probs = c(.9))

ants <- c(3,3,52,7,5,49,5,4,4,5,4)
mean(ants)
median(ants)
dry <- c(40,52,40,43,27)
mean(dry)
median(dry)

dessert<-c(30,16,30,56,22,14)
mean(dessert)
median(dessert)
?piechart
oil<- EVOS$Oil
o <-table(oil)
pie(o)
st<- EVOS$Seabirds
st2 <- EVOS$Transect
?plot
boxplot(st, st2 )
v<-c(0,0,7.98816568,0,3.899721448,0.9100101112,7.77385159,36.22589532,0,16.16666667,0,5.921052632,0.9501187648,3.797468354,0,0,0,5.118110236,3.181818182,1.140065147,0,10,0,0,1.530612245,5.855855856,0,0,0.8695652174,2.110817942,1.410437236,3.022670025,0,0,0,0)
quantile(v)
v2<-c(0,0.6896551724
      ,3.020134228
      ,0
      ,0
      ,0
      ,2.691511387
      ,0
      ,3.5982009
      ,0
      ,8.73015873
      ,0
      ,10.32258065
      ,0
      ,2.25
      ,5.107526882
      ,6.333333333
      ,0
      ,0
      ,4.301075269
      ,0
      ,0
      ,5.501618123
      ,5.274725275
      ,0
      ,12.65243902
      ,0
      ,3.551912568
      ,0
      ,11.16005874
      ,16.69865643
      ,0
      ,5.330490405
      ,32.8358209
      ,22.94372294
      ,7.614213198
      ,7.168458781
      ,0
      ,6.198347107
      ,0.4398826979
      ,2.631578947
      ,0.7102272727
      ,2.707930368
      ,0.5703422053
      ,0.6309148265
      ,1.589242054
      ,0
      ,0
      ,0.75
      ,0
      ,0
      ,0.8995502249
      ,0
      ,9.141791045
      ,0
      ,2.058823529
      ,0.5997001499,0,0,3.020134228
)
quantile(v2)
fatal <- read_xls("C:/Users/Andreas/Downloads/FATAL.XLS")
fatal2<- fatal$Fatalities
print(fatal2)
x<-hist(fatal2)
y<-boxplot(fatal$Fatalities)
IQR<- IQR(fatal2)
IQR
a<-mean(fatal2)
b<-sd(fatal2)
z<- (fatal2-a)/b
print(z)
z2<-c(6.65)
c<-boxplot(z)
z3<-c(6.65,2.29,1.13,1.07)
p<- fatal2/12917
q<- 1-q
print(q)
pk<-sqrt((p*q)/12917)
print(pk)
k<- c(1:3)
k1<- a - k*b
k2 <- a+k*b
print(k1)
print(k2)
pChebk<- 1-1/(k^2)
l<- list(hist(fatal$Fatalities),boxplot(fatal$Fatalities),a=mean(fatal2),b=sd(fatal2),z2=z2,6.65,pk=pk,IQR=IQR,pChebk=pChebk)
print(l)

x<- FATAL$Fatalities
mydescr <- function(x, k){
  n<- length(x)
  
  if(n<= 25) {nbins<-6}
  if(n>25 & n<=50) {nbins <-8}
  if( n>50) {nbins <- 13}
  
  layout(matrix(1:2, nrow=1, ncol=2, byrow= TRUE))
  hist(x, nclass = nbins)
  
  b<- boxplot(x, range = 3, notch = TRUE)
  
  outb <- x[x< b$stats[1,1]| x> b$stats[5,1]]
  
  z<- scale(x)[,1]
  
  posoutz <- x[abs(z)>=2&abs(z)<=3]
  
  outz <- x[abs(z)>3]
  
  intv <- mean(x) + c(-1,1)*k*sd(x)
  
  insidek <- x[x>intv[1] & x<intv[2]]
  
  pk<- length(insidek)/n
  
  pChebk<- 1-1/(k^2)
  
  stdcheb <- (pk - pChebk)/sqrt(pk*(1-pk)/n)
  l<- list(iqr=IQR(x), xbar= mean(x), sdx= sd(x), posoutz= posoutz, outz= outz, pk=pk, stdcheb=stdcheb, outb=outb, z=z)
  return(l)
}
mydescr(x= fatal$Fatalities, k=2)
