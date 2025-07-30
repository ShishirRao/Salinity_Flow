library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(ggrepel)

setwd("E:/Shishir/FieldData/Analysis/Reflectance/")

######################### Agha ###############

# reflectance for dry season
Agh_refl_dry = read.csv("L8_Agha_Dry_properties.csv",header=T)
head(Agh_refl_dry)
Agh_refl_dry$Date_interval_start = ymd(stri_sub(Agh_refl_dry$Date_interval_start,from = 1,to = 10))

ggplot(Agh_refl_dry,aes(y = avgRED, x = Date_interval_start,label = Date_interval_start))+geom_text_repel()+
  geom_point()+
  geom_smooth() + ggtitle("all pixels_dry season")+  
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"))

# only pixels classifed as water 1 and/or water 2 by DWSE, and avg bright < 0.5
#Agh_refl_dry_class12 = read.csv("L8_Agha_Dry_properties_Class1_2Water.csv",header=T)
Agh_refl_dry_class12 = read.csv("L8_Agha_Dry_properties_Class1_2Water_v2.csv",header=T)
head(Agh_refl_dry_class12)
Agh_refl_dry_class12$Date_interval_start = ymd(stri_sub(Agh_refl_dry_class12$Date_interval_start,from = 1,to = 10))

ggplot(Agh_refl_dry_class12,aes(y = avgRED, x = Date_interval_start))+geom_point()+
  geom_smooth() + ggtitle("class1+2_dry season")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

# only pixels classifed as water 1 and/or water 2 by DWSE
Agh_refl_dry_class1 = read.csv("L8_Agha_Dry_properties_Class1Water.csv",header=T)
Agh_refl_dry_class1$Date_interval_start = ymd(stri_sub(Agh_refl_dry_class1$Date_interval_start,from = 1,to = 10))

ggplot(Agh_refl_dry_class1,aes(y = avgRED, x = Date_interval_start))+geom_point()+
  geom_smooth() + ggtitle("class1_dry season")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# reflectance for wet season
Agh_refl_wet = read.csv("L8_Agha_Wet_properties.csv",header=T)
head(Agh_refl_wet)
Agh_refl_wet$Date_interval_start = ymd(stri_sub(Agh_refl_wet$Date_interval_start,from = 1,to = 10))

ggplot(Agh_refl_wet,aes(y = avgRED, x = Date_interval_start))+geom_point()+
  geom_smooth() + ggtitle("all pixels_wet season")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


# in dry season, avgRED values above 0.5 are faulty. Check why the cloud check is failing. 
# Cloud check failing can be prevented by checking avg brightness < 0.5
# use class1_2 water instead. 
# Agh_refl_dry = Agh_refl_dry %>% filter(avgRED<=0.5)


# merge the wet and dry season data
# Agh_refl = rbind(Agh_refl_dry,Agh_refl_wet %>% filter(Date_interval_start == ymd("2023-08-05")))
Agh_refl = rbind(Agh_refl_dry_class12,Agh_refl_wet %>% filter(Date_interval_start == ymd("2023-08-05")))


ggplot(Agh_refl,aes(y = avgRED, x = Date_interval_start))+geom_point()+
  geom_smooth() + ggtitle("all pixels_wet+dry season")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


## Now get ssc values for Agh
ssc_agh = ssc %>% filter(River == "Agha") %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth"))

#compute the median SSC for each sampling date
ssc_agh =   ssc_agh %>%
    dplyr::group_by(Sampling.Date) %>%
    dplyr::summarize(ssc_mean = mean(SSC..mg.l., na.rm=TRUE))

#discard high SSC values from Oct and NOv
ssc_agh = ssc_agh %>% filter(ssc_agh$Sampling.Date < ymd("2023-10-16"))

names(Agh_refl)[1] = names(ssc_agh)[1]


#join ssc and reflectance data sets
ssc_agh = left_join(ssc_agh,Agh_refl)


plot((ssc_agh$ssc_mean)~ssc_agh$avgRED)


plot(log(ssc_agh$ssc_mean)~ssc_agh$avgRED)
Agh_lm = lm(log(ssc_agh$ssc_mean)~ssc_agh$avgRED)

abline(Agh_lm)


ggplot(ssc_agh, aes(x = avgRED, y = log(ssc_mean))) +
  geom_point() +
  geom_smooth(method = "lm", formula = log(y) ~ x, color = "red")
  
######################### Gangavali ###############

# reflectance for dry season
Gang_refl_dry = read.csv("L8_Gang_Dry_properties_Class1_2.csv",header=T)
head(Gang_refl_dry)
Gang_refl_dry$Date_interval_start = ymd(stri_sub(Gang_refl_dry$Date_interval_start,from = 1,to = 10))

ggplot(Gang_refl_dry,aes(y = avgRED, x = Date_interval_start,label = Date_interval_start))+geom_text_repel()+
  geom_point()+
  geom_smooth() + ggtitle("all pixels_dry season")+  
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10,face="bold"))



ssc_gang = ssc %>% filter(River == "Gangavali") %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth"))

#compute the median SSC for each sampling date
ssc_gang =   ssc_gang %>%
  dplyr::group_by(Sampling.Date) %>%
  dplyr::summarize(ssc_mean = mean(SSC..mg.l., na.rm=TRUE))

names(Gang_refl_dry)[1] = names(ssc_gang)[1]

ssc_gang$Sampling.Date[ssc_gang$Sampling.Date == "2023-04-08"]


#join ssc and reflectance data sets
ssc_gang = left_join(ssc_gang,Gang_refl_dry)


plot((ssc_agh$ssc_mean)~ssc_agh$avgRED)


plot(log(ssc_agh$ssc_mean)~ssc_agh$avgRED)
Agh_lm = lm(log(ssc_agh$ssc_mean)~ssc_agh$avgRED)

abline(Agh_lm)

