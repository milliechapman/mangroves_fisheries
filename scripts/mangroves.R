##  Creates data table for classification of species from 
##  FAO data
##  Millie  Chapman, Nov 2018
library(dplyr)
rm(list=ls())

## Database
data<-read.csv("Data/GlobalProduction_2018/TS_FI_PRODUCTION.csv")
## Country  
country<- read.csv("Data/GlobalProduction_2018/CL_FI_COUNTRY_GROUPS.csv")
country<-country[,c(1,5)]
country$COUNTRY<-country$UN_Code
country<-country[,c(2,3)]
## Species
species<-read.csv("Data/GlobalProduction_2018/CL_FI_SPECIES_GROUPS.csv")
species<-species[,c(1,10)]
species$SPECIES<- species$X3Alpha_Code
species<-species[,c(2,3)]
## Production source
production<-read.csv("Data/GlobalProduction_2018/CL_FI_PRODUCTION_SOURCE.csv")
production$SOURCE<-production$Identifier
production<-production[,c(2,6)]
## Unit
unit<-read.csv("Data/GlobalProduction_2018/CL_FI_UNIT.csv")
unit$UNIT<-unit$Code
unit<-unit[,c(2,5)]

## Merge  together and creeate  DF for analysis
data<- merge(data, country, by = "COUNTRY")
data<- merge(data, species, by = "SPECIES")
data<-merge(data, production, by = "SOURCE")
data<-merge(data, unit, by = "UNIT")

data2<- data %>%
  group_by(Scientific_Name) %>%
  summarize(mode(Comments))
  summarize(sum(QUANTITY))
library(rfishbase)
fish<-unique(data$Scientific_Name)
fish<-as.vector(fish)
fish<-species(fish)
fish$Scientific_Name<-fish$sciname
fish<-merge(fish, data2, by = "Scientific_Name")
fishN<-fish[,c(1,102)]
write.csv(fishN, "fish.csv")
  
rm()
## create df for case  study countries
## (ecuador, CR, liberia, fiji)
ecuador<- data[which(data$Name_En.x=="Ecuador"),]
ecuador2<- ecuador %>%
  group_by(Scientific_Name, YEAR) %>%
  summarize(sum(QUANTITY))
ecuadorfish<-unique(ecuador$Scientific_Name)
ecuadorfish<-as.vector(ecuadorfish)
  
mozambique<- data[which(data$Name_En.x=="Mozambique"),]
mfish<-as.vector(unique(mozambique$Scientific_Name))
write.csv(mfish, "mfish")

fiji<- data[which(data$Name_En.x=="Fiji"),]
ffish<-as.vector(unique(fiji$Scientific_Name))
write.csv(ffish, "ffish")

panama<- data[which(data$Name_En.x=="Panama"),]
pfish<-as.vector(unique(fiji$Scientific_Name))
write.csv(pfish, "pfish")

brazil<- data[which(data$Name_En.x=="Brazil"),]
bfish<-as.vector(unique(brazil$Scientific_Name))
write.csv(bfish, "bfish")

CR<- data[which(data$Name_En.x=="Costa Rica"),]
CRfish<-as.vector(unique(CR$Scientific_Name))
write.csv(CRfish, "CRfish")

liberia<- data[which(data$Name_En.x=="Liberia"),]
Lfish<-as.vector(unique(liberia$Scientific_Name))
write.csv(Lfish, "Lfish")






