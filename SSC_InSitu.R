library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(data.table)


setwd("E:/Shishir/FieldData/SSC Lab/")

#ssc = read.csv("SSC data_V1.csv",header=T)
#ssc = read.csv("SSC data_V2.csv",header=T)
ssc = read.csv("SSC data_V3.csv",header=T)

#convert samples which took two filters in to a single row
ssc[is.na(ssc$SSC..mg.l.),]

# row no. 24 and 25
ssc$SSC..mg.l.[ssc$Filter.ID == 24] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 24] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 25]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 24] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 25])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 24]
# delete row 25
ssc = ssc[ssc$Filter.ID != 25,]

ssc$SSC..mg.l.[ssc$Filter.ID == 145] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 145] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 158]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 145] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 158])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 145]
# delete row 158
ssc = ssc[ssc$Filter.ID != 158,]

ssc$SSC..mg.l.[ssc$Filter.ID == 171] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 171] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 172]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 171] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 172])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 171]
# delete row 158
ssc = ssc[ssc$Filter.ID != 172,]

ssc$SSC..mg.l.[ssc$Filter.ID == 173] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 173] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 174]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 173] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 174])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 173]
# delete row 158
ssc = ssc[ssc$Filter.ID != 174,]

ssc$SSC..mg.l.[ssc$Filter.ID == 176] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 176] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 177]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 176] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 177])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 176]
# delete row 158
ssc = ssc[ssc$Filter.ID != 177,]

ssc$SSC..mg.l.[ssc$Filter.ID == 185] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 185] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 186]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 185] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 186])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 185]
# delete row 158
ssc = ssc[ssc$Filter.ID != 186,]

ssc$SSC..mg.l.[ssc$Filter.ID == 192] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 192] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 193]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 192] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 193])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 192]
# delete row 158
ssc = ssc[ssc$Filter.ID != 193,]

ssc$SSC..mg.l.[ssc$Filter.ID == 200] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 200] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 201]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 200] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 201])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 200]
# delete row 158
ssc = ssc[ssc$Filter.ID != 201,]

ssc$SSC..mg.l.[ssc$Filter.ID == 206] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 206] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 207]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 206] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 207])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 206]
# delete row 158
ssc = ssc[ssc$Filter.ID != 207,]

ssc$SSC..mg.l.[ssc$Filter.ID == 213] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 213] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 214]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 213] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 214])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 213]
# delete row 158
ssc = ssc[ssc$Filter.ID != 214,]

ssc$SSC..mg.l.[ssc$Filter.ID == 217] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 217] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 218]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 217] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 218])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 217]
# delete row 158
ssc = ssc[ssc$Filter.ID != 218,]

ssc$SSC..mg.l.[ssc$Filter.ID == 224] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 224] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 225]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 224] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 225])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 224]
# delete row 158
ssc = ssc[ssc$Filter.ID != 225,]




#In Kali, date is confusing for two sets of samples. The date is just called July. 
# Put an approx date based on sampling schedule and the SSC value
# the high value SSC must be from late July, and the low value SSC must be early July
ssc[ssc$Sampling.Date == "July",]
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 134] = "20-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 147] = "12-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 150] = "12-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 187] = "12-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 192] = "20-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 205] = "20-July-23"


# convert to date format
ssc$Sampling.Date = parse_date_time(ssc$Sampling.Date,"d-b-y")
ssc$Sampling.Date = as.Date(ssc$Sampling.Date)
class(ssc$Sampling.Date)

?parse_date_time

# get month
ssc$SamplingMonth = lubridate::month(ssc$Sampling.Date,label = TRUE,abbr = TRUE)


#create timeseries of sampling dates
ScheduledDates = seq(as.Date('2023-03-30'),as.Date('2023-12-30'),by='8 days')
Schedule = data.frame(ScheduledDates = ScheduledDates, ID = seq(1:length(ScheduledDates)))

setDT(ssc)[, join_date := Sampling.Date]
setDT(Schedule)[, join_date := ScheduledDates]
ssc = Schedule[ssc, on = .(join_date), roll = "nearest"]

ssc = ssc %>% select("Sl.No","Date.of.testing", "Sampling.Date","ScheduledDates","Sampling.time","River","Filter.ID",
                     "SSC..mg.l.","Turbidity.1","Turbidity.2","Turbidity.3","Turbidity.4","Note","SamplingMonth")

#summary
todo = ssc %>% dplyr::group_by(SamplingMonth,River,ScheduledDates) %>% summarise(n = n())




is.na(ssc$Sampling.Date)
is.na(ssc$River)
class(ssc$Sampling.Date)
class(ssc$River)

ssc$River = as.factor(ssc$River)
ggplot(ssc,aes(y = SSC..mg.l., x = Sampling.Date))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River,method = "loess")) +
  scale_x_date(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


SSC = ggplot(ssc,aes(y = log(SSC..mg.l.), x = Sampling.Date))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River,method = "loess")) +
  xlab(" ")+ylab(" ")+
  scale_x_date(date_labels = "%j",date_breaks = "60 day")+theme_bw()+
  theme(axis.text=element_text(size=8),
        axis.title=element_text(size=9,face="bold"))+
  theme(legend.position="bottom")

#ggsave("SSC.jpg", SSC, device = "jpg",path = "E:/Shishir/FieldData/Results/",
#       scale = 1, width = 5, height = 3, 
#       dpi = 300, limitsize = TRUE)
unique(ssc$River)

names(ssc)
