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
#pix = read.csv("pixelValues_v3.csv",header=T)
#pix = read.csv("pixelValues_v4.csv",header=T)
#pix = read.csv("pixelValues_v5.csv",header=T)
#pix = read.csv("pixelValues_v6.csv",header=T)
pix = read.csv("pixelValues_v7.csv",header=T)

names(pix)
head(pix)
nrow(pix)

pix = pix %>% select("name","B2","B3","B4","B5","pixel_qa","date")
names(pix) = c("site","Blue","Green","Red","NIR","pixel_qa","ImgDates")

pix$ImgDates = ymd(stri_sub(pix$ImgDates,from = 1,to = 10))
pix$River = stri_sub(pix$site,from = 1,to = 4)


# Any RED values greater than 0.5 are cloud pixels that went undetected by qa bits
# Any RED values lower than 0.002 also do not seem to be water pixels
# this is the avg brightness check that was earlier done in GEE, but GEE calculates
# avg brightness across all pixel values, and removes images whose avg,  avg brightness is >0.5
# However, it should be a pixel level operation which is being done here. 

pix %>% filter(Red > 0.5)
pix %>% filter(Red < 0.002)

pix = pix %>% filter(Red <= 0.5 & Red >= 0.002) 

pix_red = pix %>% group_by(site,ImgDates) %>% mutate(AvgRed = median(Red)) %>%
  select(site,ImgDates,River,AvgRed)  %>% distinct() %>% spread(site,AvgRed)

pix_green = pix %>% group_by(site,ImgDates) %>% mutate(AvgGreen = median(Green)) %>%
  select(site,ImgDates,River,AvgGreen)  %>% distinct() %>% spread(site,AvgGreen)

pix_blue = pix %>% group_by(site,ImgDates) %>% mutate(AvgBlue = median(Blue)) %>%
  select(site,ImgDates,River,AvgBlue)  %>% distinct() %>% spread(site,AvgBlue)

pix_NIR = pix %>% group_by(site,ImgDates) %>% mutate(AvgNIR = median(NIR)) %>%
  select(site,ImgDates,River,AvgNIR)  %>% distinct() %>% spread(site,AvgNIR)

# combine the values for each site based on cloud free data availability
AvgRed = SiteCombine(pix_red) %>% rename(AvgRed = Reflect)
AvgGreen = SiteCombine(pix_green) %>% rename(AvgGreen = Reflect)
AvgBlue = SiteCombine(pix_blue) %>% rename(AvgBlue = Reflect)
AvgNIR = SiteCombine(pix_NIR) %>% rename(AvgNIR = Reflect)


refl = left_join(AvgRed,AvgGreen)
refl = left_join(refl, AvgBlue)
refl = left_join(refl, AvgNIR)

refl_long = gather(refl,key = "Band",value = "Reflect",AvgRed, AvgGreen, AvgBlue,AvgNIR)
names(refl_long)

#plot band values against time. 
ggplot(refl_long,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River)) + facet_wrap(~Band) + ggtitle("Reflectance")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=14,face="bold"))


# function to combine the values for each site based on cloud free data availability
SiteCombine <- function(pix_wide) {
  
  pix_wide$Reflect = as.numeric(0)
  pix_wide$Reflect[pix_wide$River == "Agha"] = pix_wide$Agha_water_dry[pix_wide$River == "Agha"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet1[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Agha"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet1[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Agha"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet1[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Agha"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet2[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Agha"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet2[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Agha"]
  
  pix_wide$Reflect[pix_wide$River == "Gang"] = pix_wide$Gang_water_dry[pix_wide$River == "Gang"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Gang"]
  #wet 2 from Gangavali is down at the estuary!
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-20") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet2[pix_wide$ImgDates == ymd("2023-07-20") & pix_wide$River == "Gang"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Gang"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-13") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-13") & pix_wide$River == "Gang"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Gang"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Gang"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-11-01") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-11-01") & pix_wide$River == "Gang"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-03") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-12-03") & pix_wide$River == "Gang"]
  
  pix_wide$Reflect[pix_wide$River == "Kali"] = pix_wide$Kali_water_dry[pix_wide$River == "Kali"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Kali"] = pix_wide$Kali_water_wet1[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Kali"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-19") & pix_wide$River == "Kali"] = pix_wide$Kali_water_wet1[pix_wide$ImgDates == ymd("2023-12-19") & pix_wide$River == "Kali"]
  
  pix_wide$Reflect[pix_wide$River == "Shar"] = pix_wide$Shar_water_dry[pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-04-15") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-04-15") & pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-09") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry2[pix_wide$ImgDates == ymd("2023-05-09") & pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry3[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Shar"] = pix_wide$Shar_water_wet1[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry2[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-11-25") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-11-25") & pix_wide$River == "Shar"]
  pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-11") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-12-11") & pix_wide$River == "Shar"]

  pix_wide = pix_wide %>% select(-c(Agha_water_dry,Agha_water_wet1,Agha_water_wet2,
                                    Gang_water_dry,Gang_water_wet1,Gang_water_wet2,
                                    Kali_water_dry,Kali_water_wet1,
                                    Shar_water_dry,Shar_water_dry1,Shar_water_dry2,Shar_water_dry3,Shar_water_wet1))
  
  return (pix_wide)
}




# Now get the ssc data ready
names(ssc)
## Now get ssc values and compute the median SSC for each sampling date
ssc_mean =   ssc %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth","River")) 
# calculate mean SSC
ssc_mean = ssc_mean %>% group_by(Sampling.Date,ScheduledDates,River) %>% mutate(ssc_mean = mean(SSC..mg.l.))
ssc_mean = ssc_mean %>% select(-SSC..mg.l.) %>% distinct()

ssc_mean$logssc = log(ssc_mean$ssc_mean)

# rename the levels so that it matches with refletance data set
levels(ssc_mean$River)[levels(ssc_mean$River) == "Gangavali"] <- "Gang"
levels(ssc_mean$River)[levels(ssc_mean$River) == "Sharavathi"] <- "Shar"

#combine ssc and reflectance datasets
by <- join_by(River,ImgDates == Sampling.Date)
refl_long = left_join(refl_long,ssc_mean,by)
refl_long = refl_long[complete.cases(refl_long),]


temp = refl_long %>% filter(Band == "AvgGreen")
# plot log(ssc)~Reflect for each band combination
ggplot(refl_long,aes(y = logssc, x = Reflect))+geom_point()+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm') +facet_grid(Band~River, scales = "free")+
  ggtitle("Log(ssc) vs reflectance")+theme_bw()+ xlab("Red reflectance") +ylab("Log SSC")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))



plot(log(refl$ssc_mean)~refl$Reflect)
Agh_lm = lm(log(refl$ssc_mean)~refl$Reflect)

abline(Agh_lm)



