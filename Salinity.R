library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)


setwd("E:/Shishir/FieldData")

###### Salinity ######
Sal = read.csv("Analysis/Salinity/Agha_30ftWLR_June21.csv",header = T)
#Sal = read.csv("Analysis/Salinity/Sharavathi_Salinity_14Feb_13_March.csv",header=T)
Sal = read.csv("Analysis/Salinity/Aghanashini_Salinity_Seawater_May1_clean.csv",header=T)
#Sal = read.csv("Analysis/Salinity/Sharavathi_Salinity_Seawater_April26.csv",header=T)

names(Sal) = c("SlNo","Date_time","Conductivity_uScm","Temp_degC_Cond")
head(Sal)
tail(Sal)

Sal$Date_time = mdy_hm(Sal$Date_time)

#Calculate Salinity from Conductivity
Sal$Salinity = 0.4665*(Sal$Conductivity_uScm*0.001)^1.0878

ggplot(Sal[Sal$SlNo>5,], aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "2 day")+theme_bw()

ggplot(Sal[Sal$SlNo<7300,], aes(x=Date_time , y=Salinity)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "2 day")+theme_bw()


Sal1 = Sal %>% select("SlNo","Date_time","Temp_degC_Cond","Salinity") %>%
  gather( "Temp_degC","Salinity",key=variable,value=value)

ggplot(Sal1, aes(x=Date_time , y=value, col=variable)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "1 day")


ylim.prim <- c(range(Sal$Salinity,na.rm = TRUE))   # in this example, salinity
ylim.sec <- c(range(Sal$Temp_degC_Cond,na.rm = TRUE))    # in this example, temperature

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] 

#plot salinity with temperature
ggplot(Sal, aes(Date_time, Salinity)) +
  geom_line(size=0.3) +
  geom_line(aes(y = a+Temp_degC_Cond*b), color = "blue",size = 0.2)+
  scale_y_continuous("Salinity", sec.axis = sec_axis(~ (. - a)/b, name = "Temperature (deg c)"))+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "1.5 day")+theme_bw()


##### water level ####

# Get hourly Barometer data for Sharavathi
SurfPres = read.csv("Analysis/WaterLevel/Sharavathi_SurfPressure_Feb18th_Mar17th.csv", header=T)
head(SurfPres)
names(SurfPres) = c("SlNo","Date_time","AirPressure_KPa","geo")
SurfPres$Date_time<- gsub("T"," ", SurfPres$Date_time)
# temporarily change the hour from 2022 to 2023. This is just for testing. Use 2023 data when it is available
SurfPres$Date_time<- gsub("2022","2023", SurfPres$Date_time)
SurfPres = SurfPres %>% select("SlNo","Date_time","AirPressure_KPa")
SurfPres$Date_time = ymd_hms(SurfPres$Date_time)
SurfPres$AirPressure_KPa = SurfPres$AirPressure_KPa/1000
SurfPres$Date_hour = ymd_h(paste(date(SurfPres$Date_time), hour(SurfPres$Date_time)))
SurfPres = SurfPres %>% select("Date_hour","AirPressure_KPa")

# Get WLR reading
Press = read.csv("Analysis/WaterLevel/Sharavathi_WLR1_Feb18th_Mar17th.csv", header=T)
head(Press)
names(Press) = c("SlNo","Date_time","AbsPressure_KPa","Temp_degC_WLR")
Press$Date_time = mdy_hm(Press$Date_time)
Press$Date_hour = ymd_h(paste(date(Press$Date_time), hour(Press$Date_time)))

# join the abs pressure and air pressure data frames by date hour stamp.
Press = left_join(Press,SurfPres,by = 'Date_hour')


# Calculate water depth as per HOBO manual

Press$FluidDensity = (999.83952 + 16.945176 * Press$Temp_degC_WLR - 7.9870401e-03 * Press$Temp_degC_WLR^2 - 
                        46.170461e-06 * Press$Temp_degC_WLR^3 + 105.56302e-09 * Press$Temp_degC_WLR^4 - 
                        280.54253e-12 * Press$Temp_degC_WLR^5) / (1 + 16.879850e-03 * Press$Temp_degC_WLR)

#Density is converted to lb/ft3 via:
Press$FluidDensity = 0.0624279606 * Press$FluidDensity

#compute the hydraulic pressure at each time as the difference between
# measured downwell pressure values and measured barometric pressure values.
Press$Phyd = Press$AbsPressure_KPa - Press$AirPressure_KPa 

#The array of downwell pressure values, P, 
#are then converted to a density dependent fluid depth array, D[]
FEET_TO_METERS = 0.3048
KPA_TO_PSI = 0.1450377
PSI_TO_PSF = 144.0

Press$D = FEET_TO_METERS * (KPA_TO_PSI * PSI_TO_PSF * Press$Phyd) / Press$FluidDensity 

ggplot(Press, aes(x=Date_time , y=D)) + geom_line() +
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "1 day")

ylim.prim <- c(range(Press$D,na.rm = TRUE))   # in this example, depth
ylim.sec <- c(range(Press$Temp_degC_WLR,na.rm = TRUE))    # in this example, temperature


b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] 

#plot water levels with temperature
ggplot(Press, aes(Date_time, D)) +
  geom_line(size=0.3) +
  geom_line(aes(y = a+Temp_degC_WLR*b), color = "blue",size = 0.5)+
  scale_y_continuous("Depth(meters)", sec.axis = sec_axis(~ (. - a)/b, name = "Temperature (deg c)"))+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "1.5 day")+theme_bw() + ggtitle ("Water level Variation and Temperature")

#combine Salinity and WLR data

Sal = left_join(Sal, Press, by="Date_time")

ylim.prim <- c(range(Sal$Salinity,na.rm = TRUE))   # in this example, Salinty
ylim.sec <- c(range(Sal$D,na.rm = TRUE))    # in this example, temperature
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] 

#plot Salinity with water levels
ggplot(Sal[Sal$Date_time <= "2023-03-01 00:00:00",], aes(Date_time, Salinity)) +
  geom_line(size=0.5) +
  geom_line(aes(y = a+D*b), color = "blue",size = 0.5)+
  scale_y_continuous("Salinity", sec.axis = sec_axis(~ (. - a)/b, name = "Depth (meters)"))+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "1 day")+theme_bw() + ggtitle ("Salinity and Water depth")
