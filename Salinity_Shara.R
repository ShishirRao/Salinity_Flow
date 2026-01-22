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
Sal4 = read.csv("Analysis/Salinity/21658009_recovered.csv",header = T)


nrow(Sal1)
nrow(Sal2)
nrow(Sal3)

names(Sal1) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
names(Sal2) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
names(Sal3) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
names(Sal4) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")

Sal = rbind(Sal1,Sal2)
Sal = rbind(Sal,Sal3)
Sal = rbind(Sal,Sal4)
Sal$SlNo = seq(1:nrow(Sal))


head(Sal)
tail(Sal)

Sal$Date_time = mdy_hm(Sal$Date_time)

#Calculate Salinity from Conductivity
Sal$Salinity = 0.4665*(Sal$Conductivity_uScm*0.001)^1.0878

nrow(Sal)

Shar_sal = ggplot(Sal, aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+ 
  ylab("")+
  scale_x_datetime(date_labels = "%j",date_breaks = "70 day")+theme_bw()+
    theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"))

ggplot(Sal[Sal$SlNo,], aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+ 
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "40 day")+theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))


Sal_Shar = Sal
Sal_Shar$month = month(Sal_Shar$Date_time)

Sal_Shar$Salinity[Sal_Shar$Date_time == ymd_hms("2023-04-26 09:10:00")] = NA
Sal_Shar$Salinity[Sal_Shar$Date_time >= ymd_hms("2024-03-20 14:15:00")] = NA



ggplot(Sal_Shar,aes(y = Salinity, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Salinity (ppm)")+
  scale_x_datetime(date_labels = "%b%n%d",date_breaks = "30 days")+theme_bw() + ggtitle ("Sharavathi")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))


ggplot(Sal_Shar,aes(y = Temp_degC_Cond, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Salinity (ppm)")+
  scale_x_datetime(date_labels = "%b%n%d",date_breaks = "30 days")+theme_bw() + ggtitle ("Sharavathi")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))



#ggsave("E:Sal_Shar#ggsave("E:/Shishir/FieldData/Results/Shar_Salinity.jpg", width = 8, height = 3,scale = 2)
