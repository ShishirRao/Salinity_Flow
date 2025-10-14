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


Sal_Gang = Sal

Gang_sal = ggplot(Sal, aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+
  ylab("")+
  scale_x_datetime(date_labels = "%j",date_breaks = "60 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"))

ggplot(Sal[Sal$SlNo,], aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+ 
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "20 day")+theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

#ggsave("E:/Shishir/FieldData/Results/Shar_Salinity.jpg", width = 8, height = 3,scale = 2)

Sal_Gang = Sal
Sal_Gang$month = month(Sal_Gang$Date_time)

Sal_Gang = Sal_Gang %>% filter(Date_time > ymd_hms("2023-06-04 11:00:00"))

# Likely that the logger got exposed, and read 0 values
Sal_Gang$Salinity[Sal_Gang$Date_time >= ymd_hms("2023-06-05 06:30:00") & Sal_Gang$Date_time <= ymd_hms("2023-06-05 07:15:00")] = NA
Sal_Gang$Salinity[Sal_Gang$Date_time == ymd_hms("2023-06-06 07:45:00")] = NA

Sal_Gang$Salinity[Sal_Gang$Date_time >= ymd_hms("2023-10-30 06:15:00")] = NA



ggplot(Sal_Gang[Sal_Gang$month == 10,],aes(y = Salinity, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Salinity (ppm)")+
  scale_x_datetime(date_labels = "%b%n%d",date_breaks = "1 days")+theme_bw() + ggtitle ("Gangavali")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))
