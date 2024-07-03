G<- read_xls("C:/Users/Andreas/Downloads/GALAXY2.xls")
g2<- G$VELOCITY
hist(g2, main = "A1775", xlab = "velocity")

a<-mean(g2)
b<-sd(g2)
z<- (g2-a)/b
print(z<1 & z>-1)
print(z)
