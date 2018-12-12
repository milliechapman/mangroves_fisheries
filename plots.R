library(reshape)

#format mangrove data
mangrove_loss<- read.csv("Data/Mangrove_loss.csv")
mdata <- melt(mangrove_loss, id=c("Country", "total_mangrove_area", "total_mangrove_loss"))
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
mdata$variable<-as.character(mdata$variable)
mdata$variable<-as.numeric(substrRight(mdata$variable,4))

mdata<-na.omit(mdata)
mdata$
plot(mdata$variable, mdata$value)



