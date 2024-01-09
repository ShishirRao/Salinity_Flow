library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)


setwd("E:/Shishir/FieldData")

###### Salinity ######

Sal1 = read.csv("Analysis/Salinity/Sharavathi_Salinity_Seawater_April26.csv",header=T)
Sal2 = read.csv("Analysis/Salinity/Sharavathi_seawater_June22.csv",header = T)
Sal3 = read.csv("Analysis/Salinity/Shar_sewater_Sep15.csv",header = T)

nrow(Sal1)
nrow(Sal2)
nrow(Sal3)

names(Sal1) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
names(Sal2) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
names(Sal3) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")

Sal = rbind(Sal1,Sal2)
Sal = rbind(Sal,Sal3)
Sal$SlNo = seq(1:nrow(Sal))


head(Sal)
tail(Sal)

Sal$Date_time = mdy_hm(Sal$Date_time)

#Calculate Salinity from Conductivity
Sal$Salinity = 0.4665*(Sal$Conductivity_uScm*0.001)^1.0878

ggplot(Sal[Sal$SlNo>5 & Sal$SlNo<20676,], aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+ ggtitle("Sharavathi Salinity")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "10 day")+theme_bw()

#ggsave("E:/Shishir/FieldData/Results/Shar_Salinity.jpg", width = 8, height = 3,scale = 2)
