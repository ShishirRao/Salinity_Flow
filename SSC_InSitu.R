library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)

setwd("E:/Shishir/FieldData/SSC Lab/")

ssc = read.csv("SSC data.csv",header=T)

#In Kali, date is confusing for two sets of samples. The date is just called July. 
# Put an approx date based on sampling schedule and the SSC value
# the high value SSC must be from late July, and the low value SSC must be early July
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 134] = "20-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 147] = "12-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 150] = "12-July-23"

#convert samples which took two filters in to a single row
# row no. 24 and 25
ssc$SSC..mg.l.[ssc$Filter.ID == 24] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 24] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 25]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 24] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 25])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 24]

# delete row 25
ssc = ssc[ssc$Filter.ID != 25,]

# convert to date format
ssc$Sampling.Date = parse_date_time(ssc$Sampling.Date,"d-b-y")

# get month
ssc$SamplingMonth = month(ssc$Sampling.Date,label = TRUE,abbr = TRUE)

todo = ssc %>% dplyr::group_by(River,SamplingMonth) %>% summarise(n = n())



is.na(ssc$Sampling.Date)
class(ssc$Sampling.Date)

ggplot(ssc,aes(y = SSC..mg.l., x = Sampling.Date))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River)) +theme_bw() +
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()


