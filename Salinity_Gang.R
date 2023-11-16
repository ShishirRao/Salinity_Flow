library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)


setwd("E:/Shishir/FieldData")

###### Salinity ######

Sal = read.csv("Analysis/Salinity/Gangavali_SeawaterSalinity_Oct30.csv",header=T)


nrow(Sal)

names(Sal) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")

Sal$SlNo = seq(1:nrow(Sal))


head(Sal)
tail(Sal)

Sal$Date_time = mdy_hm(Sal$Date_time)

#Calculate Salinity from Conductivity
Sal$Salinity = 0.4665*(Sal$Conductivity_uScm*0.001)^1.0878

ggplot(Sal, aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+ ggtitle("Gangavali Salinity")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "3 day")+theme_bw()

#ggsave("E:/Shishir/FieldData/Results/Shar_Salinity.jpg", width = 8, height = 3,scale = 2)
