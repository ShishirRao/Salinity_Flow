library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(gridExtra)

### Get the Agha field measured air pressure ###

setwd("E:/Shishir/FieldData/Analysis/Barometer/Agha")

temp = list.files(pattern="\\.csv$")
Agha_bar_list = lapply(temp, read.delim)
Agha_bar = do.call(rbind,Agha_bar_list)
names(Agha_bar)

Agha_bar = separate(Agha_bar, Position.Date.Time.Ch1_Value.Ch1_Unit.Ch2_Value.Ch2_unit.Ch3_Value.Ch3_unit, 
         into = c("Position", "date","time", "Ch1_Value","Ch1_Unit","Ch2_Value","Ch2_unit","Ch3_Value","Ch3_unit"), sep = ",")

Agha_bar$date = mdy(Agha_bar$date)
Agha_bar$year = year(Agha_bar$date)
Agha_bar$day = day(Agha_bar$date)

Agha_bar = Agha_bar[Agha_bar$year >= 2023, ]

Agha_bar$datetime = ymd_hms(paste(Agha_bar$date," ",Agha_bar$time))

# ggplot(Agha_bar,aes(y = Ch3_Value, x = datetime))+geom_point()+
#   geom_smooth(span = .5) +
#   scale_x_datetime(date_labels = "%y",date_breaks = "30 day")+theme_bw()+
#   theme(axis.text=element_text(size=13),
#         axis.title=element_text(size=14,face="bold"))

Agha_bar$Ch3_Value = as.numeric(Agha_bar$Ch3_Value)

Agha_bar = Agha_bar %>% group_by(day) %>% mutate(daily_atm = median(Ch3_Value))

Agha_bar = Agha_bar %>% group_by(date,Ch3_Value,day,year,daily_atm) %>% distinct()


# ggplot(Agha_bar,aes(y = daily_atm, x = date))+geom_point()+
#   geom_smooth(span = .5)+
#   scale_x_date(date_labels = "%b-%d",date_breaks = "5 day")+theme_bw()

names(Agha_bar)
Agha_bar$hour = hour(Agha_bar$datetime)
Agha_bar$min = minute(Agha_bar$datetime)
Agha_bar$reminder = Agha_bar$min %% 5 
class(Agha_bar$reminder)

Agha_bar = Agha_bar[Agha_bar$reminder == as.numeric(0),]

Agha_bar$datetime_round = ymd_hms(paste0(Agha_bar$date," ",Agha_bar$hour,":",Agha_bar$min,":","00"))

class(Agha_bar$datetime)




### Read Agha's remote-sensed air pressure data ####
setwd("E:/Shishir/FieldData/Analysis/Barometer/Sateliite_SurfacePressure/")
Agha_bar_ERA5 = read.csv("Agha_SurfPressure_2023_ERA5.csv",header =T)
head(Agha_bar_ERA5)
Agha_bar_ERA5$datetime = ymd_hms(Agha_bar_ERA5$date)
Agha_bar_ERA5$date = date(Agha_bar_ERA5$datetime)

ggplot(Agha_bar_ERA5,aes(y = values, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

Agha_bar_ERA5$AirPressure_ERA5_hpa = Agha_bar_ERA5$values/100

ggplot(Agha_bar_ERA5,aes(y = AirPressure_ERA5_hpa, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

Agha_bar_ERA5$hour = hour(Agha_bar_ERA5$datetime)


### Now, get the Agha WLR data and merge field measured air pressure and remote sensed air pressure ###
Agha_depth = WLR_Agha
head(Agha_depth)
class(Agha_depth$Date_time)
Agha_depth$Date_time = mdy_hm(Agha_depth$Date_time)
Agha_depth$date = date(Agha_depth$Date_time)
Agha_depth$Time = time(Agha_depth$Date_time)
Agha_depth$hour = hour(Agha_depth$Date_time)
#Shar_depth$min = minute(Shar_depth$Date_time)
Agha_depth$TotalPressure_hPa = Agha_depth$kPa * 1000 / 100

by <- join_by(date,Date_time == datetime_round)
Agha_depth = left_join(Agha_depth,Agha_bar %>% select(date,datetime,datetime_round,Ch3_Value),by)
Agha_depth$month = month(Agha_depth$date)
names(Agha_depth)

names(Agha_bar)
names(Agha_bar_ERA5)

Agha_depth = left_join(Agha_depth,Agha_bar_ERA5 %>% select(date, hour, AirPressure_ERA5_hpa))

Agha_depth = Agha_depth %>% select(date,Date_time,month,Temp_degC_WLR,TotalPressure_hPa,Ch3_Value,AirPressure_ERA5_hpa)

Agha_depth$month == 5 | Agha_depth$month == 6

## the remote sensed air pressure matches closely with field measured air pressure but with an offset
ggplot(Agha_depth[Agha_depth$month == 5 ,],aes(y = Ch3_Value, x =Date_time ))+geom_point()+
  geom_smooth(span = .5)+geom_line(aes(x=Date_time,y=AirPressure_ERA5_hpa),span = 0.5)+
  #geom_line(aes(x=Date_time,y=TotalPressure_hPa))+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

mean(Agha_depth$Ch3_Value[Agha_depth$month == 5],na.rm = TRUE) -
  mean(Agha_depth$AirPressure_ERA5_hpa[Agha_depth$month == 5],na.rm = TRUE)

Agha_depth$WaterPressure_hPa = Agha_depth$TotalPressure_hPa - Agha_depth$AirPressure_ERA5_hpa

## h=ρgP  h = depth (m), 
# P = water pressure in pascals (Pa) — not total pressure
# ρ = water density (typically 1000 kg/m³ for freshwater, adjust for temperature/salinity if needed)
# g = acceleration due to gravity (9.81 m/s²)

Agha_depth$depth = Agha_depth$WaterPressure_hPa * 100 / (1000 * 9.81)

temp = Agha_depth
Agha_depth = temp

#corrections for issues while logger removal, redeploying and repositioning
# NA value at the beginning
Agha_depth = Agha_depth %>% filter(Agha_depth$Date_time >  ymd_hms("2023-03-08 13:50:00"))
# zero value read when logger was taken out to read
Agha_depth$depth[Agha_depth$Date_time == ymd_hms("2023-04-15 14:50:00")] = NA
Agha_depth$depth[Agha_depth$Date_time >= ymd_hms("2023-05-01 9:00:00") & Agha_depth$Date_time <= ymd_hms("2023-05-01 18:20:00")] = NA

# zero value read when logger was taken out to read
Agha_depth$depth[Agha_depth$Date_time == ymd_hms("2023-06-27 16:35:00")] = NA

#logger put back at a lower depth because the river was drying up. 
offset = Agha_depth$depth[Agha_depth$Date_time == ymd_hms("2023-06-27 16:30:00")] - Agha_depth$depth[Agha_depth$Date_time == ymd_hms("2023-06-27 17:15:00")]
# find the difference in depth (offset), and add the offset to all future values. 
Agha_depth$depth[Agha_depth$Date_time >= ymd_hms("2023-06-27 17:15:00")] = Agha_depth$depth[Agha_depth$Date_time >= ymd_hms("2023-06-27 17:15:00")] + offset

Agha_depth$depth[Agha_depth$Date_time >= ymd_hms("2023-11-14 17:00:00")] = NA

Agha_depth_plot = ggplot(Agha_depth,aes(y = depth, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Water depth (meters)")+
  scale_x_datetime(date_labels = "%b-%y",date_breaks = "30 days")+theme_bw() + ggtitle ("Aghanashini")
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))



### Let us look at Sharavathi barometric data ###

setwd("E:/Shishir/FieldData/Analysis/Barometer/Shar")

temp = list.files(pattern="\\.csv$")
Shar_bar_list = lapply(temp, read.delim)
Shar_bar = do.call(rbind,Shar_bar_list)
names(Shar_bar)

Shar_bar = separate(Shar_bar, Position.Date.Time.Ch1_Value.Ch1_Unit.Ch2_Value.Ch2_unit.Ch3_Value.Ch3_unit, 
                    into = c("Position", "date","time", "Ch1_Value","Ch1_Unit","Ch2_Value","Ch2_unit","Ch3_Value","Ch3_unit"), sep = ",")

Shar_bar$date = mdy(Shar_bar$date)
Shar_bar$year = year(Shar_bar$date)
Shar_bar$day = day(Shar_bar$date)

Shar_bar = Shar_bar[Shar_bar$year >= 2023, ]

Shar_bar$datetime = ymd_hms(paste(Shar_bar$date," ",Shar_bar$time))

# ggplot(Shar_bar,aes(y = Ch3_Value, x = datetime))+geom_point()+
#   geom_smooth(span = .5) +
#   scale_x_datetime(date_labels = "%b-%d",date_breaks = "5 day")+theme_bw()+
#   theme(axis.text=element_text(size=13),
#         axis.title=element_text(size=14,face="bold"))

Shar_bar$Ch3_Value = as.numeric(Shar_bar$Ch3_Value)


Shar_bar = Shar_bar %>% group_by(date,day) %>% mutate(daily_atm = median(Ch3_Value))

Shar_bar = Shar_bar %>% group_by(date,Ch3_Value,day,year,daily_atm) %>% distinct()

names(Shar_bar)
Shar_bar$hour = hour(Shar_bar$datetime)
Shar_bar$min = minute(Shar_bar$datetime)

Shar_bar$min_round = round(Shar_bar$min/5) * 5

Shar_bar$datetime_round = ymd_hms(paste0(Shar_bar$date," ",Shar_bar$hour,":",Shar_bar$min_round,":","00"))

class(Shar_bar$datetime)

 # ggplot(Shar_bar,aes(y = daily_atm, x = date))+geom_point()+
 #  geom_smooth(span = .5)+
 #  scale_x_date(date_labels = "%b-%d",date_breaks = "5 day")+theme_bw()


### Read Sharavathi's remote-sensed air pressure data ####
setwd("E:/Shishir/FieldData/Analysis/Barometer/Sateliite_SurfacePressure/")
Shar_bar_ERA5 = read.csv("Shar_SurfPressure_2023_2024_ERA5.csv",header =T)
head(Shar_bar_ERA5)
Shar_bar_ERA5$datetime = ymd_hms(Shar_bar_ERA5$date)
Shar_bar_ERA5$date = date(Shar_bar_ERA5$datetime)

ggplot(Shar_bar_ERA5,aes(y = values, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

Shar_bar_ERA5$AirPressure_ERA5_hpa = Shar_bar_ERA5$values/100

ggplot(Shar_bar_ERA5,aes(y = AirPressure_ERA5_hpa, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

Shar_bar_ERA5$hour = hour(Shar_bar_ERA5$datetime)

### let us take the Shar water pressure, and add 
# field measured and remote-sensed air pressure columns by joining hourly data

Shar_depth = WLR_Shar
head(Shar_depth)
class(Shar_depth$Date_time)
#Shar_depth$Date_times = ymd_hms(Shar_depth$Date_time)
Shar_depth$date = date(Shar_depth$Date_time)
Shar_depth$Time = time(Shar_depth$Date_time)
Shar_depth$hour = hour(Shar_depth$Date_time)
#Shar_depth$min = minute(Shar_depth$Date_time)
Shar_depth$TotalPressure_hPa = Shar_depth$kPa * 1000 / 100

by <- join_by(date,Date_time == datetime_round)
Shar_depth = left_join(Shar_depth,Shar_bar %>% select(date,datetime,datetime_round,Ch3_Value),by)
Shar_depth$month = month(Shar_depth$date)
names(Shar_depth)

names(Shar_bar)
names(Shar_bar_ERA5)


Shar_depth = left_join(Shar_depth,Shar_bar_ERA5 %>% select(date, hour, AirPressure_ERA5_hpa))

Shar_depth = Shar_depth %>% select(Date_time,month,Temp_degC_WLR,TotalPressure_hPa,Ch3_Value,AirPressure_ERA5_hpa)

Shar_depth$month == 5 | Shar_depth$month == 6

## the remote sensed air pressure matches closely with field measured air pressure but with an offset
ggplot(Shar_depth[Shar_depth$month == 6,],aes(y = Ch3_Value, x =Date_time ))+geom_point()+
  geom_smooth(span = .5)+geom_smooth(aes(x=Date_time,y=AirPressure_ERA5_hpa),span = 0.5)+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

mean(Shar_depth$Ch3_Value[Shar_depth$month == 5],na.rm = TRUE) -
mean(Shar_depth$AirPressure_ERA5_hpa[Shar_depth$month == 5],na.rm = TRUE)

Shar_depth$WaterPressure_hPa = Shar_depth$TotalPressure_hPa - Shar_depth$AirPressure_ERA5_hpa

## h=ρgP  h = depth (m), 
# P = water pressure in pascals (Pa) — not total pressure
# ρ = water density (typically 1000 kg/m³ for freshwater, adjust for temperature/salinity if needed)
# g = acceleration due to gravity (9.81 m/s²)

Shar_depth$depth = Shar_depth$WaterPressure_hPa * 100 / (1000 * 9.81)

Shar_depth$depth[Shar_depth$Date_time == ymd_hms("2023-04-15 11:20:00")] = NA
Shar_depth$depth[Shar_depth$Date_time == ymd_hms("2023-06-02 11:30:00")] = NA
Shar_depth$depth[Shar_depth$Date_time == ymd_hms("2023-06-26 11:40:00")] = NA
Shar_depth$depth[Shar_depth$Date_time == ymd_hms("2023-09-20 11:15:00")] = NA
Shar_depth$depth[Shar_depth$Date_time >= ymd_hms("2024-03-20 12:15:00")] = NA
Shar_depth$depth[Shar_depth$Date_time == ymd_hms("2023-05-01 11:10:00")] = NA
Shar_depth$depth[Shar_depth$Date_time == ymd_hms("2023-08-03 13:00:00")] = NA



Shar_depth_plot = ggplot(Shar_depth,aes(y = depth, x =Date_time ))+geom_line()+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 days")+theme_bw()



### Read Gangavali's remote-sensed air pressure data ####
setwd("E:/Shishir/FieldData/Analysis/Barometer/Sateliite_SurfacePressure/")
Gang_bar_ERA5 = read.csv("Gang_SurfPressure_2023_2024_ERA5.csv",header =T)
head(Gang_bar_ERA5)
Gang_bar_ERA5$datetime = ymd_hms(Gang_bar_ERA5$date)
Gang_bar_ERA5$date = date(Gang_bar_ERA5$datetime)

ggplot(Gang_bar_ERA5,aes(y = values, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

Gang_bar_ERA5$AirPressure_ERA5_hpa = Gang_bar_ERA5$values/100

ggplot(Gang_bar_ERA5,aes(y = AirPressure_ERA5_hpa, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%y",date_breaks = "30 day")+theme_bw()

Gang_bar_ERA5$hour = hour(Gang_bar_ERA5$datetime)

### let us take the Gang water pressure, and add remote-sensed air pressure columns by joining hourly data

Gang_depth = WLR_Gang
head(Gang_depth)
class(Gang_depth$Date_time)
#Gang_depth$Date_times = ymd_hms(Gang_depth$Date_time)
Gang_depth$date = date(Gang_depth$Date_time)
Gang_depth$Time = time(Gang_depth$Date_time)
Gang_depth$hour = hour(Gang_depth$Date_time)
#Shar_depth$min = minute(Shar_depth$Date_time)
Gang_depth$TotalPressure_hPa = Gang_depth$kPa * 1000 / 100

Gang_depth = left_join(Gang_depth,Gang_bar_ERA5 %>% select(date, hour, AirPressure_ERA5_hpa))
Gang_depth$month = month(Gang_depth$date)
Gang_depth = Gang_depth %>% select(Date_time,month,Temp_degC_WLR,TotalPressure_hPa,AirPressure_ERA5_hpa)


Gang_depth$WaterPressure_hPa = Gang_depth$TotalPressure_hPa - Gang_depth$AirPressure_ERA5_hpa

## h=ρgP  h = depth (m), 
# P = water pressure in pascals (Pa) — not total pressure
# ρ = water density (typically 1000 kg/m³ for freshwater, adjust for temperature/salinity if needed)
# g = acceleration due to gravity (9.81 m/s²)

Gang_depth$depth = Gang_depth$WaterPressure_hPa * 100 / (1000 * 9.81)

Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-05-08 17:00:00")] = NA
Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-06-16 14:40:00")] = NA
Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-08-13 14:30:00")] = NA
Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-12-05 13:45:00")] = NA


offset = Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-08-13 14:15:00")] - Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-08-13 15:30:00")]
Gang_depth$depth[Gang_depth$Date_time >= ymd_hms("2023-08-13 15:30:00")] = 
  Gang_depth$depth[Gang_depth$Date_time >= ymd_hms("2023-08-13 15:30:00")] + offset


offset = Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-12-05 14:00:00")] - Gang_depth$depth[Gang_depth$Date_time == ymd_hms("2023-12-05 13:15:00")]

Gang_depth$depth[Gang_depth$Date_time >= ymd_hms("2023-12-05 14:00:00")] = 
  Gang_depth$depth[Gang_depth$Date_time >= ymd_hms("2023-12-05 14:00:00")] - offset

Gang_depth = Gang_depth %>% filter(Date_time < ymd_hms("2024-03-14 13:15:00"))

Gang_depth_plot = ggplot(Gang_depth,aes(y = depth, x =Date_time ))+geom_line()+
  scale_x_datetime(date_labels = "%b:%d",date_breaks = "30 days")+theme_bw()

ggplot(Gang_depth,aes(y = depth, x =Date_time ))+geom_line()+
  scale_x_datetime(date_labels = "%b:%d",date_breaks = "10 days")+theme_bw()


### Read Kali's remote-sensed air pressure data ####
setwd("E:/Shishir/FieldData/Analysis/Barometer/Sateliite_SurfacePressure/")
Kali_bar_ERA5 = read.csv("Kali_SurfPressure_2023_2024_ERA5.csv",header =T)
head(Kali_bar_ERA5)
Kali_bar_ERA5$datetime = ymd_hms(Kali_bar_ERA5$date)
Kali_bar_ERA5$date = date(Kali_bar_ERA5$datetime)

ggplot(Kali_bar_ERA5,aes(y = values, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

Kali_bar_ERA5$AirPressure_ERA5_hpa = Kali_bar_ERA5$values/100

ggplot(Kali_bar_ERA5,aes(y = AirPressure_ERA5_hpa, x = datetime))+geom_point()+
  geom_smooth(span = .5)+
  scale_x_datetime(date_labels = "%b-%y",date_breaks = "30 day")+theme_bw()

Kali_bar_ERA5$hour = hour(Kali_bar_ERA5$datetime)

### let us take the Gang water pressure, and add remote-sensed air pressure columns by joining hourly data

Kali_depth = WLR_Kali
head(Kali_depth)
class(Kali_depth$Date_time)
#Gang_depth$Date_times = ymd_hms(Gang_depth$Date_time)
Kali_depth$date = date(Kali_depth$Date_time)
Kali_depth$Time = time(Kali_depth$Date_time)
Kali_depth$hour = hour(Kali_depth$Date_time)
#Shar_depth$min = minute(Shar_depth$Date_time)
Kali_depth$TotalPressure_hPa = Kali_depth$kPa * 1000 / 100

Kali_depth = left_join(Kali_depth,Kali_bar_ERA5 %>% select(date, hour, AirPressure_ERA5_hpa))
Kali_depth$month = month(Kali_depth$date)
Kali_depth = Kali_depth %>% select(Date_time,month,Temp_degC_WLR,TotalPressure_hPa,AirPressure_ERA5_hpa)


Kali_depth$WaterPressure_hPa = Kali_depth$TotalPressure_hPa - Kali_depth$AirPressure_ERA5_hpa

## h=ρgP  h = depth (m), 
# P = water pressure in pascals (Pa) — not total pressure
# ρ = water density (typically 1000 kg/m³ for freshwater, adjust for temperature/salinity if needed)
# g = acceleration due to gravity (9.81 m/s²)

Kali_depth$depth = Kali_depth$WaterPressure_hPa * 100 / (1000 * 9.81)

Kali_depth$year = year(Kali_depth$Date_time)


Kali_depth$depth[Kali_depth$Date_time > ymd_hms("2023-05-30 7:20:00") & 
             Kali_depth$Date_time < ymd_hms("2023-06-18 19:15:00")] = NA

Kali_depth$depth[Kali_depth$Date_time > ymd_hms("2023-07-08 00:00:00") & 
                   Kali_depth$Date_time < ymd_hms("2023-07-13 00:00:00")] = NA

Kali_depth$depth[Kali_depth$Date_time > ymd_hms("2023-09-06 12:00:00") & 
                   Kali_depth$Date_time < ymd_hms("2023-09-11 00:00:00")] = NA

Kali_depth$depth[Kali_depth$Date_time > ymd_hms("2023-12-18 12:00:00") & 
                   Kali_depth$Date_time < ymd_hms("2024-01-04 00:00:00")] = NA

Kali_depth$depth[Kali_depth$Date_time > ymd_hms("2024-03-24 07:15:00") & 
                   Kali_depth$Date_time < ymd_hms("2024-04-13 12:00:00")] = NA

Kali_depth$depth[Kali_depth$Date_time > ymd_hms("2024-05-10 09:00:00") & 
                   Kali_depth$Date_time < ymd_hms("2024-05-11 12:50:00")] = NA





  ggplot(Kali_depth[Kali_depth$year == 2023 & Kali_depth$month == 7,],aes(y = depth, x =Date_time ))+geom_line()+
  scale_x_datetime(date_labels = "%b%n%d",date_breaks = "30 days")+theme_bw()



##### plotting ######

Agha_depth_plot = ggplot(Agha_depth,aes(y = depth, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Water depth (meters)")+
  scale_x_datetime(date_labels = "%b%n%Y",date_breaks = "30 days")+theme_bw() + ggtitle ("Aghanashini")+
theme(axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      plot.title = element_text(size = 20, face = "bold"))

Shar_depth_plot = ggplot(Shar_depth,aes(y = depth, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Water depth (meters)")+
  scale_x_datetime(date_labels = "%b%n%Y",date_breaks = "30 days")+theme_bw() + ggtitle ("Sharavathi")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))

Gang_depth_plot = ggplot(Gang_depth,aes(y = depth, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Water depth (meters)")+
  scale_x_datetime(date_labels = "%b%n%Y",date_breaks = "30 days")+theme_bw() + ggtitle ("Gangavali")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))

Kali_depth_plot = ggplot(Kali_depth,aes(y = depth, x =Date_time ))+geom_line()+ xlab("Date")+ ylab("Water depth (meters)")+
  scale_x_datetime(date_labels = "%b%n%Y",date_breaks = "30 days")+theme_bw() + ggtitle ("Kali")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))

mylegend<-g_legend(Agha_depth_plot)

?sapply

WLRs <- grid.arrange(arrangeGrob(Agha_depth_plot + theme(legend.position="none") + ggtitle("Aghanashini (free)"),
                                 Shar_depth_plot + theme(legend.position="none") + ggtitle("Sharavathi (dammed)"),
                                 Gang_depth_plot + theme(legend.position="none") + ggtitle("Gangavali (free)"),
                                 Kali_depth_plot + theme(legend.position="none") + ggtitle("Kali (dammed)"),
                                 nrow=1,ncol=4),heights=c(6, 1))

WLRs <- grid.arrange(arrangeGrob(Agha_depth_plot + theme(legend.position="none") + ggtitle("Aghanashini (free)"),
                                 Shar_depth_plot + theme(legend.position="none") + ggtitle("Sharavathi (dammed)"),
                                 Gang_depth_plot + theme(legend.position="none") + ggtitle("Gangavali (free)"),
                                 Kali_depth_plot + theme(legend.position="none") + ggtitle("Kali (dammed)"),
                                 nrow=2,ncol=2),heights=c(6, 1))


plot(WLRs)


library(ggplot2)
library(cowplot)

# Create main plot
p_main <- ggplot(mtcars, aes(mpg, hp)) +
  geom_point() +
  ggtitle("Main Plot")

# Create inset plot
p_inset <- ggplot(mtcars, aes(cyl)) +
  geom_bar() +
  ggtitle("Inset Plot") +
  theme(plot.title = element_text(size = 8)) # Smaller title for inset

# Combine plots using cowplot
ggdraw() +
  draw_plot(p_main, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(p_inset, x = 0.6, y = 0.6, width = 0.35, height = 0.35) # Position and size of inset

