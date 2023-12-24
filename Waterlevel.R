library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)


setwd("E:/Shishir/FieldData")

###### water level ######
#Sal = read.csv("Analysis/  ",header=T)
WLR1 = read.csv("Analysis/WaterLevel/Agha/Agha_WLR_Feb-Mar.csv",header=T)
names(WLR1) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR1)

WLR2 = read.csv("Analysis/WaterLevel/Agha/Agha_30ftWLR_Apr15.csv",header=T)
names(WLR2) = c("SlNo","Date_time","kPa","Temp_degC_WLR")

WLR3 = read.csv("Analysis/WaterLevel/Agha/Agha_30ftWLR_May1.csv",header=T)
names(WLR3) = c("SlNo","Date_time","kPa","Temp_degC_WLR")

WLR4 = read.csv("Analysis/WaterLevel/Agha/Agha_30ftWLR_June27.csv",header=T) 
head(WLR4)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")

WLR5 = read.csv("Analysis/WaterLevel/Agha/Agha_30ft_nov14.csv",header=T) 
head(WLR5)
names(WLR5) = c("SlNo","Date_time","kPa","Temp_degC_WLR")


WLR = rbind(WLR1,WLR2)
WLR = rbind(WLR,WLR3)
WLR = rbind(WLR,WLR4)
WLR = rbind(WLR,WLR5)

names(WLR) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR)

WLR$Date_time = mdy_hm(WLR$Date_time)
tail(WLR)

ggplot(WLR[WLR$SlNo>10,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "10 day")+theme_bw()

head(WLR)

ggplot(WLR[WLR$Date_time>"2023-9-06",], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "8 day")+theme_bw()

 
##### Sharavathi ######

WLR4 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_Apr15.csv",header=T)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR4)

WLR5 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_May1.csv",header=T)
names(WLR5) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR5)

WLR6 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_June2.csv",header=T)
names(WLR6) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR6)

WLR7 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_June26.csv",header=T)
names(WLR7) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR7)

WLR8 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_Aug3.csv",header=T)
names(WLR8) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR8)


WLR9 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ftSep20.csv", header=T)
names(WLR9) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR9)

WLR_Shar = rbind(WLR4,WLR5)
WLR_Shar = rbind(WLR_Shar,WLR6)
WLR_Shar = rbind(WLR_Shar,WLR7)
WLR_Shar = rbind(WLR_Shar,WLR8)
WLR_Shar = rbind(WLR_Shar,WLR9)

names(WLR_Shar) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR_Shar)
tail(WLR_Shar)

WLR_Shar$Date_time = mdy_hm(WLR_Shar$Date_time)

ggplot(WLR_Shar, aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "3 day")+theme_bw()

#ggsave("E:/Shishir/FieldData/Results/Shar_WLR.jpg", width = 8, height = 3,scale = 2)



#### Gangavali ########

WLR1 = read.csv("Analysis/WaterLevel/Gang/Gang_WLR_13ft_May8.csv",header=T)
names(WLR1) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR1)

WLR2 = read.csv("Analysis/WaterLevel/Gang/Gang_13_ft_June16.csv",header=T)
names(WLR2) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR2)

WLR3 = read.csv("Analysis/WaterLevel/Gang/Gang_1_Aug_13.csv",header=T)
names(WLR3) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR3)

WLR4 = read.csv("Analysis/WaterLevel/Gang/Gangavali_30ftWLR_Dec5.csv",header=T)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR4)


WLR_Gang = rbind(WLR1,WLR2)
WLR_Gang = rbind(WLR_Gang,WLR3)
WLR_Gang = rbind(WLR_Gang,WLR4)

names(WLR_Gang) = c("SlNo","Date_time","kPa","Temp_degC_WLR")




WLR_Gang$Date_time = mdy_hm(WLR_Gang$Date_time)

ggplot(WLR_Gang[WLR_Gang$SlNo>10,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "10 day")+theme_bw()


### kali
WLR1 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR13ft_April30.csv",header=T)
names(WLR1) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR1)

WLR2 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR_13ft_June10.csv",header=T)
names(WLR2) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR2)

WLR3 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR_13ft_July10.csv",header=T)
names(WLR3) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR3)

WLR4 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR_30ft_Sep7.csv",header=T)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR4)


WLR = rbind(WLR1,WLR2)
WLR = rbind(WLR,WLR3)
WLR = rbind(WLR,WLR4)

names(WLR) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR)

WLR$Date_time = mdy_hm(WLR$Date_time)


ggplot(WLR[WLR$SlNo>10,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "3 day")+theme_bw()

