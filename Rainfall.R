library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(stringi)
library(stringdist)
library(data.table)
library("sp")
library("sf")


setwd("E:/Shishir/FieldData/Analysis/Rainfall/")

##### read daily rainfall and convert to monthly sum  ####
precip = read.csv("UK_Basin_1981_2023_meanPrecip.csv",header=T)
names(precip)
head(precip)
unique(precip$Basin_name)


precip$date = stri_sub(precip$date,from = 1,to = 10)
precip$date = ymd(precip$date)
precip$year = year(precip$date)
precip$month = lubridate::month(precip$date)

#Summarize the taluk wise rainfall by month, year
MonthPrecip = as.data.frame(precip %>%
                              dplyr::group_by(Basin_name, year, month) %>%
                              dplyr::summarize(Rain_mm = sum(mean, na.rm=TRUE)))

head(MonthPrecip)
unique(MonthPrecip$Taluk)

yearPrecip = as.data.frame(precip %>%
                             dplyr::group_by(Basin_name, year) %>%
                             dplyr::summarize(Rain_mm = sum(mean, na.rm=TRUE)))


UK_whsed = st_read("E:/Shishir/Thesis_GIS/UK/UK_Wshed_bounds.shp")
UK_whsed$Area = as.numeric(st_area(UK_whsed) / (1000*1000))
class(UK_whsed$Area)
names(UK_whsed)

yearPrecip = left_join(yearPrecip,UK_whsed %>% select(Basin_name,Area))

names(yearPrecip)
AnnualAvg = yearPrecip %>% group_by(Basin_name) %>% dplyr::summarise((AnnualAvg = round(mean(Rain_mm),2)))

yearPrecip$RainfallDepth = yearPrecip$Rain_mm/yearPrecip$Area
?st_read

ggplot(yearPrecip,aes(x=year,y=Rain_mm)) + geom_point(aes(color = Basin_name))+
  geom_smooth(aes(group = Basin_name,col = Basin_name,method = "auto"),span = 0.3)

ggplot(yearPrecip,aes(x=year,y=RainfallDepth)) + geom_point(aes(color = Basin_name))+
  xlab("year") + ylab("Rainfall_mm/sqkm")+
  geom_smooth(aes(group = Basin_name,col = Basin_name,method = "auto"),span = 0.3)



?geom_smooth
