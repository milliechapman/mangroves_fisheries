library(plyr)
library(ggplot2)
library(reshape)
rm(list = ls())
global_m<- read.csv("Data/global.csv")
global_m<-merge(data,global_m, by.x = "Scientific_Name", by.y = "Scientific_Name", all.x = TRUE)

global_man <- ddply(global_m, c("Name_En.x", "YEAR", "Mangrove"), summarise,
               quantity    = sum(QUANTITY))

#format mangrove data
mangrove_loss<- read.csv("Data/Mangrove_loss.csv")
mdata <- melt(mangrove_loss, id=c("Country", "total_mangrove_area", "total_mangrove_loss"))
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
mdata$variable<-as.character(mdata$variable)
mdata$variable<-as.numeric(substrRight(mdata$variable,4))
mangrove_loss<-mangrove_loss[,c(1:3)]

global_man2<-merge(global_m,mangrove_loss, by.x = "Name_En.x", by.y= "Country")
global_man2$total_mangrove_area<- ifelse(global_man2$total_mangrove_area>0,"Country with Mangroves","Country without Mangroves")

## Global
global_NA<-ddply(global_man2, c( "Scientific_Name", "Mangrove"), summarise,
                   quantity    = sum(QUANTITY)
)

## Global
global_plot<-ddply(global_man2, c( "YEAR", "Mangrove", "total_mangrove_area"), summarise,
                   quantity    = sum(QUANTITY)
)
global_plot$Mangrove<-ifelse(global_plot$Mangrove==2,1,global_plot$Mangrove)

global_plot<-global_plot[complete.cases(global_plot), ]

global_plot$Mangrove<-ifelse(global_plot$Mangrove<1, "Not Mangrove Associated Species", "Mangrove Associated Species")

tiff("global.tiff", width = 7, height = 5.5, units = 'in', res = 300)
ggplot(data = global_plot, aes(x = YEAR, y = quantity, group = Mangrove, colour=Mangrove)) + 
  geom_line() + labs(title= "Global", y= "Fisheries Landings (Tonnes)", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("darkgrey", "#56B4E9"))+
  theme(legend.position = c(0.2, 0.85))+facet_grid(.~total_mangrove_area)+
  geom_point() 
dev.off()

## Fiji
fi<-read.csv("Data/fiji_species.csv")
fiji<- global_m[which(global_m$Name_En.x=="Fiji"),]
fiji<-merge(fiji, fi, by.x = "Scientific_Name", by.y= "Species")
fiji<-ddply(fiji, c("YEAR", "Mangrove.y"), summarise,
            quantity    = sum(QUANTITY)
)
fiji$mangrove<-as.factor(fiji$Mangrove.y)
fiji$mangrove<-ifelse(fiji$mangrove=="0", "Not Mangrove Associated Species", "Mangrove Associated Species")

tiff("fiji.tiff", width = 6.5, height = 5.5, units = 'in', res = 300)
ggplot(data = fiji, aes(x = YEAR, y = quantity, group = mangrove, colour=mangrove)) + 
  geom_line() + labs(title= "Fiji", y= "Fisheries Landings (Tonnes)", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9"))+
  theme(legend.position = c(0.28, 0.85))
dev.off()

## Ecuador
ec<-read.csv("Data/ecuador_species.csv")
ecuador<- global_m[which(global_m$Name_En.x=="Ecuador"),]
ecuador<-merge(ecuador, ec, by.x = "Scientific_Name", by.y= "scientific_name")
ecuador<-ddply(ecuador, c("YEAR", "mangrove"), summarise,
            quantity    = sum(QUANTITY)
)
ecuador$mangrove<-as.factor(ecuador$mangrove)
ecuador$mangrove<-ifelse(ecuador$mangrove=="0", "Not Mangrove Associated Species", "Mangrove Associated Species")

tiff("ecuador.tiff", width = 6.5, height = 5.5, units = 'in', res = 300)
ggplot(data = ecuador, aes(x = YEAR, y = quantity, group = mangrove, colour=mangrove)) + 
  geom_line() + labs(title= "Ecuador", y= "Fisheries Landings (Tonnes)", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9"))+
  theme(legend.position = c(0.28, 0.85))
dev.off()

## Costa Rica
cr<-read.csv("Data/cr_species.csv")
costarica<- global_m[which(global_m$Name_En.x=="Costa Rica"),]
cr<-merge(costarica, cr, by.x = "Scientific_Name", by.y= "Species")
cr<-ddply(cr, c("YEAR", "Mangroves"), summarise,
               quantity    = sum(QUANTITY)
)
cr$mangrove<-as.factor(cr$Mangroves)
cr$mangrove<-ifelse(cr$mangrove=="0", "Not Mangrove Associated Species", "Mangrove Associated Species")

tiff("cr.tiff", width = 6.5, height = 5.5, units = 'in', res = 300)
ggplot(data = cr, aes(x = YEAR, y = quantity, group = mangrove, colour=mangrove)) + 
  geom_line() + labs(title= "Costa Rica", y= "Fisheries Landings (Tonnes)", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9"))+
  theme(legend.position = c(0.3, 0.85))
dev.off()

## Liberia
li<-read.csv("Data/liberia_species.csv")
liberia<- global_m[which(global_m$Name_En.x=="Liberia"),]
liberia<-merge(liberia, li, by.x = "Scientific_Name", by.y= "Species")
liberia<-ddply(liberia, c("YEAR", "Mangroves"), summarise,
               quantity    = sum(QUANTITY)
)
liberia$mangrove<-as.factor(liberia$Mangroves)
liberia$mangrove<-ifelse(liberia$mangrove=="0", "Not Mangrove Associated Species", "Mangrove Associated Species")

tiff("liberia.tiff", width = 6.5, height = 5.5, units = 'in', res = 300)
ggplot(data = liberia, aes(x = YEAR, y = quantity, group = mangrove, colour=mangrove)) + 
  geom_line() + labs(title= "Liberia", y= "Fisheries Landings (Tonnes)", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9"))+
  theme(legend.position = c(0.27, 0.85))
dev.off()

## Panama
pa<-read.csv("Data/panama_species.csv")
panama<- global_m[which(global_m$Name_En.x=="Panama"),]
panama<-merge(panama, pa, by.x = "Scientific_Name", by.y= "Species")
panama<-ddply(panama, c("YEAR", "Mangroves"), summarise,
               quantity    = sum(QUANTITY)
)
panama$mangrove<-as.factor(panama$Mangroves)
panama$mangrove<-ifelse(panama$mangrove=="0", "Not Mangrove Associated Species", "Mangrove Associated Species")

tiff("panama.tiff", width = 6.5, height = 5.5, units = 'in', res = 300)
ggplot(data = panama, aes(x = YEAR, y = quantity, group = mangrove, colour=mangrove)) + 
  geom_line() + labs(title= "Panama", y= "Fisheries Landings (Tonnes)", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9"))+
  theme(legend.position = c(0.3, 0.85))
dev.off()


##mangrove loss
mp<- mdata[which(mdata$Country=="Panama"),]
mp$YEAR<-as.integer(mp$variable)
mp$value<-mp$value*.078
tiff("panama_m.tiff", width = 6.5, height = 4, units = 'in', res = 300)
ggplot(data = mp, aes(x = YEAR, y = value)) + 
  geom_line() + labs(title= "Panama", y= "Mangrove Loss", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9", "blue"))+
  theme(legend.position = c(0.2, 0.85))+
  geom_line(aes(y = value, colour = "Total Mangrove Loss (Ha)"))
dev.off()

mp<- mdata[which(mdata$Country=="Costa Rica"),]
mp$YEAR<-as.integer(mp$variable)
mp$value<-mp$value*.078
tiff("cr_m.tiff", width = 6.5, height = 4, units = 'in', res = 300)
ggplot(data = mp, aes(x = YEAR, y = value)) + 
  geom_line() + labs(title= "Costa Rica", y= "Mangrove Loss", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9", "blue"))+
  theme(legend.position = c(0.2, 0.85))+
  geom_line(aes(y = value, colour = "Total Mangrove Loss (Ha)"))
dev.off()

mp<- mdata[which(mdata$Country=="Liberia"),]
mp$YEAR<-as.integer(mp$variable)
mp$value<-mp$value*.078
tiff("liberia_m.tiff", width = 6.5, height = 4, units = 'in', res = 300)
ggplot(data = mp, aes(x = YEAR, y = value)) + 
  geom_line() + labs(title= "Liberia", y= "Mangrove Loss", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9", "blue"))+
  theme(legend.position = c(0.2, 0.85))+
  geom_line(aes(y = value, colour = "Total Mangrove Loss (Ha)"))
dev.off()

mp<- mdata[which(mdata$Country=="Fiji"),]
mp$YEAR<-as.integer(mp$variable)
mp$value<-mp$value*.078
tiff("fiji_m.tiff", width = 6.5, height = 4, units = 'in', res = 300)
ggplot(data = mp, aes(x = YEAR, y = value)) + 
  geom_line() + labs(title= "Fiji", y= "Mangrove Loss", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9", "blue"))+
  theme(legend.position = c(0.2, 0.85))+
  geom_line(aes(y = value, colour = "Total Mangrove Loss (Ha)"))
dev.off()


## mangrove loss
## Global
global3<-ddply(global_man2, c( "Name_En.x","Mangrove", "total_mangrove_area"), summarise,
                 quantity    = sum(QUANTITY)
)
global3<-global3[which(global3$total_mangrove_area=="Country with Mangroves"),]

globaln<-global3[which(global3$Mangrove==1),]

mangrove_re<-merge(mangrove_loss, global3, by.x = "Country", by.y  = "Name_En.x")
plot(log(mangrove_re$total_mangrove_loss), log(mangrove_re$quantity))

mangrove_re$logq<-log(mangrove_re$quantity)
mangrove_re$loga<-log(mangrove_re$total_mangrove_area.x)
mangrove_re$logl<-log(mangrove_re$total_mangrove_loss/mangrove_re$total_mangrove_area.x)

plot(log(mangrove_re$total_mangrove_area.x), log(mangrove_re$quantity))

lm(mangrove_re$loga~mangrove_re$logq)

tiff("regression2.tiff", width = 6, height = 6, units = 'in', res = 300)
ggplot(mangrove_re,aes(logq,loga)) + geom_point(shape=1) +
  geom_smooth(method='lm')+ labs(x= "Fisheries Landings of Mangrove Associated Species (log(Tonnes))", y = "Mangrove Area  (log(Ha))")
dev.off()

tiff("regression1.tiff", width = 6, height = 6, units = 'in', res = 300)
ggplot(mangrove_re,aes(logq,logl)) + geom_point(shape=1) +
  geom_smooth(method='lm') +
  labs(x= "Fisheries Landings of Mangrove Associated Species (log(Tonnes))", y = "Mangrove Loss/Mangrove Area (log(Ha))")
dev.off()

##nonmangrove
global3<-ddply(global_man2, c( "Name_En.x","Mangrove", "total_mangrove_area"), summarise,
               quantity    = sum(QUANTITY)
)
global3<-global3[which(global3$total_mangrove_area=="Country with Mangroves"),]

globaln<-global3[which(global3$Mangrove==0),]

mangrove_re<-merge(mangrove_loss, global3, by.x = "Country", by.y  = "Name_En.x")
plot(log(mangrove_re$total_mangrove_loss), log(mangrove_re$quantity))

mangrove_re$logq<-log(mangrove_re$quantity)
mangrove_re$loga<-log(mangrove_re$total_mangrove_area.x)
mangrove_re$logl<-log(mangrove_re$total_mangrove_loss/mangrove_re$total_mangrove_area.x)

plot(log(mangrove_re$total_mangrove_area.x), log(mangrove_re$quantity))

lm(mangrove_re$loga~mangrove_re$logq)

tiff("regression3.tiff", width = 6, height = 6, units = 'in', res = 300)
ggplot(mangrove_re,aes(logq,loga)) + geom_point(shape=1) +
  geom_smooth(method='lm')+ labs(x= "Fisheries Landings of Non Mangrove Associated Species (log(Tonnes))", y = "Mangrove Area  (log(Ha))")
dev.off()

tiff("regression4.tiff", width = 6, height = 6, units = 'in', res = 300)
ggplot(mangrove_re,aes(logq,logl)) + geom_point(shape=1) +
  geom_smooth(method='lm') +
  labs(x= "Fisheries Landings of Non Mangrove Associated Species (log(Tonnes))", y = "Mangrove Loss/Mangrove Area (log(Ha))")
dev.off()
##all countries
ecuador$country<-"Ecuador"
fiji$country<-"Fiji"

countries<-rbind(ecuador, fiji)
g <- ggplot(data = countries, aes(x = YEAR, y = quantity, group = mangrove, colour=mangrove)) + 
  geom_line() + labs( y= "Fisheries Landings (Tonnes)", x = "Year")+ 
  theme(legend.title=element_blank())+ 
  scale_color_manual(values=c("#999999", "#56B4E9"))+
  theme(legend.position = c(0.2, 0.85))+
  facet_grid(.~country)
g

