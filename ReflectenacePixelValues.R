library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(ggrepel)
library(stringi)

setwd("E:/Shishir/FieldData/Analysis/Reflectance/")

#Create a date series for 2023 spaced 8 days apart

LandsatDates = data.frame("ImgDates" = seq(ymd("2023-01-09"),ymd("2023-12-27"), by = "8 days"))

#pix = read.csv("pixelValues_v2.csv",header=T)
pix = read.csv("pixelValues_v3.csv",header=T)

names(pix)
head(pix)
nrow(pix)

pix = pix %>% select("name","B2","B3","B4","B5","pixel_qa","date")
names(pix) = c("site","Blue","Green","Red","NIR","pixel_qa","ImgDates")

pix$ImgDates = ymd(stri_sub(pix$ImgDates,from = 1,to = 10))
pix$River = stri_sub(pix$site,from = 1,to = 4)

# Any values greater than 0.5 are cloud pixels that went undetected by qa bits
# this is the avg brightness check that was earlier done in GEE, but GEE calculates
# avg brightness across all pixel values, and removes images whose avg,  avg brightness is >0.5
# However, it should be a pixel level operation which is being done here. 
pix %>% filter(Red > 0.5)
pix = pix %>% filter(Red <= 0.5)

pix = pix %>% group_by(site,ImgDates) %>% mutate(AvgRed = median(Red)) %>% select(site,ImgDates,River,AvgRed) 
pix = pix %>% distinct()

##### Aghanashini #####
pix = pix %>% filter(River == "Agha")


#These are the dates on which Agha_water_wet1 site has data compared to sampling site
wet1dates = pix$ImgDates[pix$site == "Agha_water_wet1"][which(!(unique(pix$ImgDates[pix$site == "Agha_water_wet1"]) %in% unique(pix$ImgDates[pix$site == "Agha_water_dry"])) == TRUE)]
wet2dates = pix$ImgDates[pix$site == "Agha_water_wet2"][which(!(unique(pix$ImgDates[pix$site == "Agha_water_wet2"]) %in% unique(pix$ImgDates[pix$site == "Agha_water_dry"])) == TRUE)]

pix_wide = pix %>% spread(site,AvgRed)

# combine the values for each site based on cloud free data availability
# Use mostly Agha_water_dry for dry season
pix_wide$AghaSSC = pix_wide$Agha_water_dry
pix_wide$AghaSSC[pix_wide$ImgDates == ymd("2023-08-05")] = pix_wide$Agha_water_wet1[pix_wide$ImgDates == ymd("2023-08-05")]
pix_wide$AghaSSC[pix_wide$ImgDates == ymd("2023-07-28")] = pix_wide$Agha_water_wet2[pix_wide$ImgDates == ymd("2023-07-28")]
pix_wide$AghaSSC[pix_wide$ImgDates == ymd("2023-08-21")] = pix_wide$Agha_water_wet2[pix_wide$ImgDates == ymd("2023-08-21")]
pix_wide = pix_wide %>% select(-c(Agha_water_dry,Agha_water_wet1,Agha_water_wet2))


names(ssc)
## Now get ssc values and compute the median SSC for each sampling date
ssc_mean =   ssc %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth","River")) 
ssc_mean = ssc_mean %>% group_by(ScheduledDates,River) %>% mutate(ssc_mean = mean(SSC..mg.l.))
ssc_mean = ssc_mean %>% select(-SSC..mg.l.) %>% distinct()
names(ssc_mean)
ssc_mean$logssc = log(ssc_mean$ssc_mean)
#discard high SSC values from Oct and NOv
ssc_mean = ssc_mean %>% filter(River == "Agha" & ScheduledDates < ymd("2023-10-16"))

?distinct

## 
#refl = left_join(LandsatDates,ssc_mean)
refl = left_join(LandsatDates,pix_wide)


names(refl)
names(ssc_mean)

by <- join_by(River,closest(ImgDates >= Sampling.Date))
refl = left_join(refl,ssc_mean,by)

refl$dateDiff = as.numeric(refl$ImgDates- refl$Sampling.Date)

refl = refl %>% filter(dateDiff <= 4)

#### plotting ####
plot((refl$ssc_mean)~refl$AghaSSC)


plot(log(refl$ssc_mean)~refl$AghaSSC)
Agh_lm = lm(log(refl$ssc_mean)~refl$AghaSSC)

abline(Agh_lm)


ggplot(ssc_agh, aes(x = avgRED, y = log(ssc_mean))) +
  geom_point() +
  geom_smooth(method = "lm", formula = log(y) ~ x, color = "red")


#### Gangavali #####
pix = pix %>% filter(River == "Gang")


#These are the dates on which Agha_water_wet1 site has data compared to sampling site
wet1dates = pix$ImgDates[pix$site == "Gang_water_wet1"][which(!(unique(pix$ImgDates[pix$site == "Gang_water_wet1"]) %in% unique(pix$ImgDates[pix$site == "Gang_water_dry"])) == TRUE)]
wet2dates = pix$ImgDates[pix$site == "Gang_water_wet2"][which(!(unique(pix$ImgDates[pix$site == "Gang_water_wet2"]) %in% unique(pix$ImgDates[pix$site == "Gang_water_dry"])) == TRUE)]

pix_wide = pix %>% spread(site,AvgRed)


names(ssc)
## Now get ssc values and compute the median SSC for each sampling date
ssc_mean =   ssc %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth","River")) 
ssc_mean = ssc_mean %>% group_by(ScheduledDates,River) %>% mutate(ssc_mean = mean(SSC..mg.l.))
ssc_mean = ssc_mean %>% select(-SSC..mg.l.) %>% distinct()
ssc_mean$Sampling.Date[ssc_mean$River == "Gangavali" & ssc_mean$Sampling.Date == ymd("2023-07-11")] = ymd("2023-07-28")

names(ssc_mean)
ssc_mean$logssc = log(ssc_mean$ssc_mean)
ssc_mean = ssc_mean %>% filter(River == "Gangavali")


# Use mostly Agha_water_dry for dry season
pix_wide$GangSSC = pix_wide$Gang_water_dry
pix_wide$GangSSC[pix_wide$ImgDates == ymd("2023-05-25")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-05-25")]
pix_wide$GangSSC[pix_wide$ImgDates == ymd("2023-07-20")] = pix_wide$Gang_water_wet2[pix_wide$ImgDates == ymd("2023-07-20")]
pix_wide$GangSSC[pix_wide$ImgDates == ymd("2023-07-28")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-07-28")]
pix_wide$GangSSC[pix_wide$ImgDates == ymd("2023-08-13")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-13")]
pix_wide$GangSSC[pix_wide$ImgDates == ymd("2023-08-21")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-21")]
pix_wide$GangSSC[pix_wide$ImgDates == ymd("2023-09-06")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-09-06")]
pix_wide = pix_wide %>% select(-c(Gang_water_dry,Gang_water_wet1,Gang_water_wet2))


refl = left_join(LandsatDates,pix_wide)
by <- join_by(River,closest(ImgDates = Sampling.Date))
refl = left_join(refl,ssc_mean)

refl$dateDiff = as.numeric(refl$ImgDates- refl$Sampling.Date)

refl = refl %>% filter(dateDiff <= 4)

