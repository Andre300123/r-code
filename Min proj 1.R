ddt<- read.csv("DDT.csv")
myddt <- function(df, xsp,xri,d,l){
  dfxsp<-ddt[ddt$SPECIES == "CCATFISH",]
  dfxri <- ddt[ddt$RIVER == "TRM",]
  dfspri <- ddt[ddt$RIVER == "TRM" & ddt$SPECIES == "CCATFISH",]
  write.csv(x = dfxsp, 
          file = "dfsp.csv",
          row.names = FALSE)
  write.csv(x = dfspri, 
          file = "dfspri.csv",
          row.names = FALSE)
  write.csv(x = dfxri, 
          file = "dfri.csv",
          row.names = FALSE)
  read.csv("dfri.csv")
  d<-dir(pattern = ".csv")
  dfsp <- df[df$SPECIES == xsp,]
 box<- boxplot(ddt$LENGTH, xlab = "Species", ylab = "Length", bg= "Red", col = c("blue"),
        pch = 21)
  l<- list(dfspri=dfspri, dfxsp=dfxsp, dfxri=dfxri,d=d,l=l,box=box)
}
l2<-myddt(df=ddt, xsp= "CCATFISH", xri= "TRM",d=d, l=l )
print(l2)
