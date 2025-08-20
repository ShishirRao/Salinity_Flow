library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(ggrepel)
library(stringi)
library(broom)
library(broom.mixed)
library(tidyr)

setwd("E:/Shishir/FieldData/Analysis/Reflectance/")


# function to combine the values for each site based on cloud free data availability
SiteCombine <- function(pix_wide) {
  
  pix_wide$Reflect = as.numeric(0)
  # 
  # ## Agha
  pix_wide$Agha_water_dry[which(is.na(pix_wide$Agha_water_dry) & pix_wide$River == "Agha")] =
    pix_wide$Agha_water_wet1[which(is.na(pix_wide$Agha_water_dry) & pix_wide$River == "Agha")]

  pix_wide$Agha_water_dry[which(is.na(pix_wide$Agha_water_dry) & pix_wide$River == "Agha")] =
    pix_wide$Agha_water_wet2[which(is.na(pix_wide$Agha_water_dry) & pix_wide$River == "Agha")]

  pix_wide$Reflect[pix_wide$River == "Agha"] = pix_wide$Agha_water_dry[pix_wide$River == "Agha"]

  #Gang
  pix_wide$Gang_water_dry[which(is.na(pix_wide$Gang_water_dry) & pix_wide$River == "Gang")] =
    pix_wide$Gang_water_wet1[which(is.na(pix_wide$Gang_water_dry) & pix_wide$River == "Gang")]

  pix_wide$Gang_water_dry[which(is.na(pix_wide$Gang_water_dry) & pix_wide$River == "Gang")] =
    pix_wide$Gang_water_wet2[which(is.na(pix_wide$Gang_water_dry) & pix_wide$River == "Gang")]

  pix_wide$Reflect[pix_wide$River == "Gang"] = pix_wide$Gang_water_dry[pix_wide$River == "Gang"]

  #Kali
  pix_wide$Kali_water_dry[which(is.na(pix_wide$Kali_water_dry) & pix_wide$River == "Kali")] =
    pix_wide$Kali_water_wet1[which(is.na(pix_wide$Kali_water_dry) & pix_wide$River == "Kali")]

  pix_wide$Reflect[pix_wide$River == "Kali"] = pix_wide$Kali_water_dry[pix_wide$River == "Kali"]

  #Shar
  pix_wide$Shar_water_dry[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")] =
    pix_wide$Shar_water_dry1[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")]

  pix_wide$Shar_water_dry[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")] =
    pix_wide$Shar_water_dry2[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")]

  pix_wide$Shar_water_dry[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")] =
    pix_wide$Shar_water_dry3[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")]

  pix_wide$Shar_water_dry[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")] =
    pix_wide$Shar_water_wet1[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")]

  pix_wide$Reflect[pix_wide$River == "Shar"] = pix_wide$Shar_water_dry[pix_wide$River == "Shar"]

#   pix_wide$Reflect = as.numeric(0)
#   pix_wide$Reflect[pix_wide$River == "Agha"] = pix_wide$Agha_water_dry[pix_wide$River == "Agha"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet1[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Agha"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet1[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Agha"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet1[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Agha"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet2[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Agha"]
# #  cloud shadow affects wet1 on 2023-08-21 but not detected by algorithm. So use wet 2 point.
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Agha"] = pix_wide$Agha_water_wet2[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Agha"]
# 
# 
# 
#   pix_wide$Reflect[pix_wide$River == "Gang"] = pix_wide$Gang_water_dry[pix_wide$River == "Gang"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-05-25") & pix_wide$River == "Gang"]
#   #wet 2 from Gangavali is down at the estuary!
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-20") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet2[pix_wide$ImgDates == ymd("2023-07-20") & pix_wide$River == "Gang"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-07-28") & pix_wide$River == "Gang"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-13") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-13") & pix_wide$River == "Gang"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Gang"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Gang"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-11-01") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-11-01") & pix_wide$River == "Gang"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-03") & pix_wide$River == "Gang"] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-12-03") & pix_wide$River == "Gang"]
# 
# 
# 
# 
#   pix_wide$Reflect[pix_wide$River == "Kali"] = pix_wide$Kali_water_dry[pix_wide$River == "Kali"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Kali"] = pix_wide$Kali_water_wet1[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Kali"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-19") & pix_wide$River == "Kali"] = pix_wide$Kali_water_wet1[pix_wide$ImgDates == ymd("2023-12-19") & pix_wide$River == "Kali"]
# 
# 
# 
#   pix_wide$Reflect[pix_wide$River == "Shar"] = pix_wide$Shar_water_dry[pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-04-15") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-04-15") & pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-09") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry2[pix_wide$ImgDates == ymd("2023-05-09") & pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry3[pix_wide$ImgDates == ymd("2023-06-10") & pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Shar"] = pix_wide$Shar_water_wet1[pix_wide$ImgDates == ymd("2023-08-05") & pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry2[pix_wide$ImgDates == ymd("2023-08-21") & pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-09-06") & pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-11-25") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-11-25") & pix_wide$River == "Shar"]
#   pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-11") & pix_wide$River == "Shar"] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-12-11") & pix_wide$River == "Shar"]


  
  pix_wide = pix_wide %>% select(-c(Agha_water_dry,Agha_water_wet1,Agha_water_wet2,
                                    Gang_water_dry,Gang_water_wet1,Gang_water_wet2,
                                    Kali_water_dry,Kali_water_wet1,
                                    Shar_water_dry,Shar_water_dry1,Shar_water_dry2,Shar_water_dry3,Shar_water_wet1))
  
  return (pix_wide)
}


#Create a date series for 2023 spaced 8 days apart
LandsatDates = data.frame("ImgDates" = seq(ymd("2023-01-09"),ymd("2023-12-27"), by = "8 days"))

#pix = read.csv("pixelValues_v2.csv",header=T)
#pix = read.csv("pixelValues_v3.csv",header=T)
#pix = read.csv("pixelValues_v4.csv",header=T)
#pix = read.csv("pixelValues_v5.csv",header=T)
#pix = read.csv("pixelValues_v6.csv",header=T)
#pix = read.csv("pixelValues_v7.csv",header=T)
#pix = read.csv("PolygonValues_v9.csv",header=T)
pix = read.csv("PolygonValues_v10.csv",header=T)
#pix = read.csv("PolygonValues_v11.csv",header=T)

names(pix)
head(pix)
nrow(pix)

pix = pix %>% select("name","B2","B3","B4","B5","pixel_qa","date")
names(pix) = c("site","Blue","Green","Red","NIR","pixel_qa","ImgDates")

pix$ImgDates = ymd(stri_sub(pix$ImgDates,from = 1,to = 10))
pix$River = stri_sub(pix$site,from = 1,to = 4)

unique(pix$ImgDates)

# Any RED values greater than 0.3 are cloud pixels that went undetected by qa bits
# Any RED values lower than 0.009 also do not seem to be water pixels or cloud shadow bits
# this is the avg brightness check that was earlier done in GEE, but GEE calculates
# avg brightness across all pixel values, and removes images whose avg,  avg brightness is >0.5
# However, it should be a pixel level operation which is being done here. 

pix %>% filter(Red > 0.3) 
pix %>% filter(Red < 0.009)

#pix %>% filter(River == "Agha" & ImgDates == ymd("2023-07-28"))

pix = pix %>% filter(Red <= 0.3 & Red >= 0.009) 

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
refl$AvgRedbyNIR = refl$AvgRed / refl$AvgNIR

#write.csv(refl,"E:/Shishir/FieldData/Results/Refl_v4_polygonGang.csv")

refl_long = gather(refl,key = "Band",value = "Reflect",AvgRed, AvgGreen, AvgBlue,AvgNIR,AvgRedbyNIR)
names(refl_long)

# #plot band values against time. 
# ggplot(refl_long,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
#   geom_smooth(aes(group = River,col = River)) + facet_wrap(River~Band) + ggtitle("Reflectance")+
#   scale_x_date(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()+
#   theme(axis.text=element_text(size=6),
#         axis.title=element_text(size=14,face="bold"))

ggplot(refl_long %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = 0.4) + ggtitle("Reflectance")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%b",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))






#combine ssc and reflectance datasets
by <- join_by(River,ImgDates == Sampling.Date)
refl_long = left_join(refl_long,ssc_mean,by)
refl_long = refl_long[complete.cases(refl_long),]
names(refl_long)
refl_long = refl_long %>% select(ImgDates,ScheduledDates,SamplingMonth,River,Band,Reflect,ssc_mean,logssc)


# plot log(ssc)~Reflect for each band combination
SSCvsRedv2 = ggplot(refl_long %>% filter(Band == "AvgRed"),aes(y = logssc, x = Reflect))+geom_point()+
  stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm') +facet_wrap(~River, scales = "free")+
  ggtitle("Log(ssc) vs reflectance")+theme_bw()+ xlab("Reflectance") +ylab("Log SSC")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))

# ggsave("SSCvsRedv2.jpg", SSCvsRedv2, device = "jpg",path = "E:/Shishir/FieldData/Results/",
#       scale = 3, width = 5, height = 3,
#       dpi = 300, limitsize = TRUE)

# plot log(ssc)~Reflect for each band combination
SSCvsRed_allRiversv2 = ggplot(refl_long %>% filter(Band == "AvgRed"),aes(y = logssc, x = Reflect))+
  geom_point(aes(fill = River,color = River))+
  #stat_summary(fun.data= mean_cl_normal) + 
  geom_smooth(method='lm') +
  ggtitle("Log(ssc) vs reflectance")+theme_bw()+ xlab("Reflectance") +ylab("Log SSC")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))

# ggsave("SSCvsRed_allRiversv2.jpg", SSCvsRed_allRiversv2, device = "jpg",path = "E:/Shishir/FieldData/Results/",
#        scale = 3, width = 5, height = 3,
#        dpi = 300, limitsize = TRUE)




ggplotRegression <- function (fit) {
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

data = refl_long %>% filter(Band == "AvgRed") %>% filter(River == "Agha")
fit1 = lm(logssc ~ Reflect, data = data)
summary(fit1)
ggplotRegression(fit1)

head(refl_long)

plot(ssc_mean ~ Reflect, data = refl_long %>% filter(Band == "AvgRed"))
plot(logssc ~ Reflect, data = refl_long %>% filter(Band == "AvgRed"))



plot(log(refl$ssc_mean)~refl$Reflect)
Agh_lm = lm(log(refl$ssc_mean)~refl$Reflect)

abline(Agh_lm)

ggplot(refl_long)



data = refl_long %>% filter(River == "Agha" & Band == "AvgRed") 
model = lm(data$logssc~data$Reflect)
summary(model)

data = refl_long %>% filter(River == "Shar" & Band == "AvgRed") 
Agha_lm = lm(data$logssc~data$Reflect)
summary = summary(Agha_lm)
summary$adj.r.squared

