library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)

setwd("E:/Shishir/FieldData/SSC Lab/")

#ssc = read.csv("SSC data_V1.csv",header=T)
ssc = read.csv("SSC data_V2.csv",header=T)

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
head(ssc)

# get month
ssc$SamplingMonth = month(ssc$Sampling.Date,label = TRUE,abbr = TRUE)

todo = ssc %>% dplyr::group_by(River,Sampling.Date) %>% summarise(n = n())



is.na(ssc$Sampling.Date)
is.na(ssc$River)
class(ssc$Sampling.Date)
class(ssc$River)

ssc$River = as.factor(ssc$River)
ggplot(ssc[ssc$Filter.ID,],aes(y = SSC..mg.l., x = Sampling.Date))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River,method = "loess")) +
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()

?geom_smooth
