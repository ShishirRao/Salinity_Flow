library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)


setwd("E:/Shishir/FieldData")

###### Salinity ######

Sal1 = read.csv("Analysis/Salinity/Aghanashini_Salinity_Seawater_May1_clean.csv",header=T)
Sal2 = read.csv("Analysis/Salinity/Aghanashini_Salinity_Seawater_June21.csv",header = T)
Sal3 = read.csv("Analysis/Salinity/Agha_Salinity_Seawater_Sep8.csv",header=T)


nrow(Sal1)
nrow(Sal2)

names(Sal1) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
names(Sal2) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
names(Sal3) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
head(Sal3)


Sal = rbind(Sal1,Sal2)
nrow(Sal)
Sal = rbind(Sal,Sal3)
nrow(Sal)
Sal$SlNo = seq(1:nrow(Sal))


head(Sal)
tail(Sal)

Sal$Date_time = mdy_hm(Sal$Date_time)

#Calculate Salinity from Conductivity
Sal$Salinity = 0.4665*(Sal$Conductivity_uScm*0.001)^1.0878

ggplot(Sal[Sal$SlNo>5 & Sal$SlNo<19716,], aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+ ggtitle("Aghanashini Salinity")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "2 day")+theme_bw()
