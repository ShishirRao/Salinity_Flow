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
library(purrr)
library(lme4)

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
#pix = read.csv("PolygonValues_v10.csv",header=T)
#pix = read.csv("PolygonValues_v11.csv",header=T)
#pix = read.csv("PolygonValues_v12.csv",header=T)
pix = read.csv("PolygonValues_v13.csv",header=T)


unique(pix$name)
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

#pix %>% filter(River == "Gang" & ImgDates == ymd("2023-10-24"))

#temp = pix %>% filter(River == "Gang" & ImgDates == ymd("2023-10-24")) %>% select(ImgDates,site,Red)
temp = pix %>% filter(Red >= 0.3)
temp = pix %>% filter(Red >= 0.3 | Red <= 0.009) 
unique(temp$ImgDates)

#pix = pix %>% filter(Red <= 0.3 & Red >= 0.009) 
pix = pix %>% filter(Red <= 0.15 & Red >= 0.009) 

temp = pix %>% filter(Red >= 0.12)
unique(temp$ImgDates)

unique(temp$site[temp$ImgDates == ymd("2023-07-20")])
(temp[temp$ImgDates == ymd("2023-07-20"),])

unique(pix$site[pix$ImgDates == ymd("1990-08-02")])
test = (pix[pix$ImgDates == ymd("1990-08-02"),])

pix_red = pix %>% group_by(site,ImgDates) %>% mutate(AvgRed = median(Red)) %>%
  select(site,ImgDates,River,AvgRed)  %>% distinct() %>% spread(site,AvgRed)

pix_green = pix %>% group_by(site,ImgDates) %>% mutate(AvgGreen = median(Green)) %>%
  select(site,ImgDates,River,AvgGreen)  %>% distinct() %>% spread(site,AvgGreen)

pix_blue = pix %>% group_by(site,ImgDates) %>% mutate(AvgBlue = median(Blue)) %>%
  select(site,ImgDates,River,AvgBlue)  %>% distinct() %>% spread(site,AvgBlue)

pix_NIR = pix %>% group_by(site,ImgDates) %>% mutate(AvgNIR = median(NIR)) %>%
  select(site,ImgDates,River,AvgNIR)  %>% distinct() %>% spread(site,AvgNIR)

pix_Bright = pix %>% group_by(site,ImgDates) %>% mutate(AvgNIR = mean(NIR)) %>%
  select(site,ImgDates,River,AvgNIR)  %>% distinct() %>% spread(site,AvgNIR)

temp2 = pix_Bright[pix_Bright$ImgDates == ymd("1994-06-26"),]


# combine the values for each site based on cloud free data availability
AvgRed = SiteCombine(pix_red) %>% rename(AvgRed = Reflect)
AvgGreen = SiteCombine(pix_green) %>% rename(AvgGreen = Reflect)
AvgBlue = SiteCombine(pix_blue) %>% rename(AvgBlue = Reflect)
AvgNIR = SiteCombine(pix_NIR) %>% rename(AvgNIR = Reflect)
AvgBright = SiteCombine(pix_Bright) %>% rename(AvgBright = Reflect)

refl = left_join(AvgRed,AvgGreen)
refl = left_join(refl, AvgBlue)
refl = left_join(refl, AvgNIR)
refl = left_join(refl, AvgBright)
refl$AvgRedbyNIR = refl$AvgRed / refl$AvgNIR


#write.csv(refl,"E:/Shishir/FieldData/Results/Refl_v6_polygonShar.csv")

refl_long = gather(refl,key = "Band",value = "Reflect",AvgRed, AvgGreen, AvgBlue,AvgNIR,AvgRedbyNIR,AvgBright)
names(refl_long)
unique(refl_long$ImgDates)

max(refl_long$Reflect[refl_long$Band == "AvgRed"])

BrightImages = refl_long %>% filter(Band == "AvgBright" & Reflect >= 0.5)

# #plot band values against time. 
# ggplot(refl_long,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
#   geom_smooth(aes(group = River,col = River)) + facet_wrap(River~Band) + ggtitle("Reflectance")+
#   scale_x_date(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()+
#   theme(axis.text=element_text(size=6),
#         axis.title=element_text(size=14,face="bold"))

ggplot(refl_long %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River,method = "auto"),span = .4) + ggtitle("Red Reflectance")+ xlab("Imagery date") + ylab("Red Reflectance from L8 and L9")+
  scale_x_date(date_labels = "%b",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="bottom")


refl_long$year = year(refl_long$ImgDates)
ggplot(refl_long %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+
  geom_boxplot(aes(group = year)) + facet_wrap(.~River,ncol=1,nrow=4)+
  ggtitle("Reflectance")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))






#combine ssc and reflectance datasets
by <- join_by(River,ImgDates == Sampling.Date)
refl_long = left_join(refl_long,ssc_mean,by)
refl_long = refl_long[complete.cases(refl_long),]
names(refl_long)
refl_long = refl_long %>% select(ImgDates,ScheduledDates,SamplingMonth,River,Band,Reflect,ssc_mean,logssc) %>%
            filter(Band == "AvgRed")


# plot log(ssc)~Reflect for each band combination
# SSCvsRedv2 =
  # ggplot(refl_long, aes(y = logssc, x = Reflect))+geom_point()+
  # stat_summary(fun.data= mean_cl_normal) + 
  # geom_smooth(method='lm') +facet_wrap(~River, scales = "free")+
  # ggtitle("Log(ssc) vs reflectance")+theme_bw()+ xlab("Reflectance") +ylab("Log SSC")+
  # theme(axis.text=element_text(size=14),
  #       axis.title=element_text(size=14,face="bold"))

# ggsave("SSCvsRedv2.jpg", SSCvsRedv2, device = "jpg",path = "E:/Shishir/FieldData/Results/",
#       scale = 3, width = 5, height = 3,
#       dpi = 300, limitsize = TRUE)


# plot log(ssc)~Reflect for each band combination
#SSCvsRed_allRiversv2 =
ggplot(refl_long ,aes(y = logssc, x = Reflect))+
geom_point(aes(fill = River,color = River))+
#stat_summary(fun.data= mean_cl_normal) +
geom_smooth(method='lm') +
ggtitle("Log(ssc) vs reflectance")+theme_bw()+ xlab("Reflectance") +ylab("Log SSC")+
theme(axis.text=element_text(size=14),
      axis.title=element_text(size=14,face="bold"))

# ggsave("SSCvsRed_allRiversv2.jpg", SSCvsRed_allRiversv2, device = "jpg",path = "E:/Shishir/FieldData/Results/",
#        scale = 3, width = 5, height = 3,
#        dpi = 300, limitsize = TRUE)

  
#multiple linear regressions
  summary = refl_long  %>% group_by(River) %>%
    do(mod = lm(logssc ~ Reflect, data = .)) %>% ungroup() %>% 
    mutate(tidy = map(mod, broom::tidy),
           glance = map(mod, broom::glance),
           rsq = glance %>% map_dbl('r.squared'),
           adj.rsq = glance %>% map_dbl('adj.r.squared'),
           p_val = glance %>% map_dbl('p.value'),
           intercept = tidy %>% map_dbl(function(x) x$estimate[1]),
           slope = tidy %>% map_dbl(function(x) x$estimate[2]))  %>%
    select(River,rsq,adj.rsq,p_val,intercept,slope)

refl_long = left_join(refl_long,summary) 

River_rsq = c(as.character(round(unique(refl_long$rsq),3)))
River_Adjrsq = c(as.character(round(unique(refl_long$adj.rsq),3)))
River_pval = c(as.character(round(unique(refl_long$p_val),3)))
River_slope = c(as.character(round(unique(refl_long$adj.rsq),3)))
River_intercept = c(as.character(round(unique(refl_long$intercept),3)))

hum_names <- as_labeller(
  c("Agha" = paste("Agha: ","AdjR2 =",River_Adjrsq[1],",Intercept = ",River_intercept[1],"Slope = ",River_slope[1],"p.val = ",River_pval[1]), 
    "Gang" = paste("Gang: ","AdjR2 =",River_Adjrsq[2],",Intercept = ",River_intercept[2],"Slope = ",River_slope[2],"p.val = ",River_pval[2]), 
    "Kali" = paste("Kali: ","AdjR2 =",River_Adjrsq[3],",Intercept = ",River_intercept[3],"Slope = ",River_slope[3],"p.val = ",River_pval[3]),  
    "Shar" = paste("Shar: ","AdjR2 =",River_Adjrsq[4],",Intercept = ",River_intercept[4],"Slope = ",River_slope[4],"p.val = ",River_pval[4]))) 

unique(refl_long$River)
  
  
ggplot(refl_long,aes(y = logssc, x = Reflect))+
    geom_point(aes(fill = River,color = River))+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm') + facet_wrap(.~River, scales = "free",labeller = hum_names)+
    theme(strip.text = element_text(size = 1))+
    ggtitle("Log(ssc) vs Red Reflectance for 2023")+theme_bw()+ xlab("Red Reflectance") +ylab("Log SSC")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=13,face="bold"))+
  theme(legend.position="bottom",text = element_text(size = 13))

# ggsave("SSCvsRed_withPvals.jpg", device = "jpg",path = "E:/Shishir/FieldData/Results/",
#        scale = 3, width = 5, height = 3,
#         dpi = 300, limitsize = TRUE)


refl_long = refl_long %>% select(ImgDates,ScheduledDates,SamplingMonth,River,Band,Reflect,ssc_mean,logssc) %>%
  filter(Band == "AvgRed")
data = refl_long %>% filter(Band == "AvgRed")

# linear regression with all the points pooled together
fit = lm(logssc ~ Reflect, data = data)
summary(fit)

data$adj.rsq = signif(summary(fit)$adj.r.squared, 5)
data$intercept = signif(fit$coef[[1]],5 )
data$Slope = signif(fit$coef[[2]], 5)
data$P = signif(summary(fit)$coef[2,4], 5)

ggplot(data ,aes(y = logssc, x = Reflect))+
  geom_point(aes(fill = River,color = River))+
  #stat_summary(fun.data= mean_cl_normal) +
  geom_smooth(method='lm') +
  ggtitle("Log(ssc) vs reflectance")+theme_bw()+ xlab("Reflectance") +ylab("Log SSC")+
  labs(title = paste("Adj R2 = ",data$adj.rsq,
                     "Intercept =",data$intercept,
                     " Slope =",data$Slope,
                     " P =",data$P))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=14,face="bold"))


##### trying random intercept and random slope model ######
names(refl_long)

fit_ran_incpt = lmer(logssc ~ Reflect + (1 | River), data = refl_long)
summary(fit_ran_incpt)
confint(fit_ran_incpt)

fit_ran_incpt_slp = lmer(logssc ~ Reflect + (1+Reflect | River), data = refl_long)
summary(fit_ran_incpt_slp)
confint(fit_ran_incpt_slp)


ggplot(refl_long,aes(x=Reflect,y=logssc,col=River)) + geom_jitter() + geom_boxplot(alpha=0.2) + facet_grid(~River)


isSingular(fit_ran_incpt_slp,1e-4)


##### extending the ssc~reflectance relationship to 1990 - 2023 ####
pix_back = read.csv("PolygonValues_v15_1989_2023.csv",header=T)

pix_back = pix_back %>% select("name","B2","B3","B4","B5","pixel_qa","date")
names(pix_back) = c("site","Blue","Green","Red","NIR","pixel_qa","ImgDates")

pix_back$ImgDates = ymd(stri_sub(pix_back$ImgDates,from = 1,to = 10))
pix_back$River = stri_sub(pix_back$site,from = 1,to = 4)

pix_back = pix_back %>% filter(Red <= 0.15 & Red >= 0.009) 

#### get some stats on water and clouds to remove cloud pixels ###
areas = read.csv("polygon_areas.csv",header=T)
head(areas)
areas = areas %>% select(area,name) %>% rename(area = area,site = name)

cloudStat = read.csv("CloudStat_1989_2023.csv",header=T)
cloudStat$date = ymd(stri_sub(cloudStat$date,from = 1,to = 10))
cloudStat = cloudStat %>% rename(site=name,ImgDates = date,Cloud = cloud_mask)
unique(cloudStat$site)

waterStat = read.csv("WaterStats_1989_2023.csv",header=T)
waterStat$date = ymd(stri_sub(waterStat$date,from = 1,to = 10))
waterStat = waterStat %>% rename(site=name,ImgDates = date) 
unique(waterStat$site)

pix_back = left_join(pix_back,areas)
pix_back = left_join(pix_back,cloudStat)
pix_back = left_join(pix_back,waterStat)

pix_back$cloudArea = pix_back$Cloud/pix_back$area

range(pix_back$cloudArea)

unique(pix_back$ImgDates)
unique(temp$ImgDates)

temp = pix_back %>% filter(Red >= 0.12)
unique(temp$ImgDates)

#write.csv(temp %>% select(site,ImgDates,River) %>% distinct(),"pix_back_red_above_0.12.csv")
#yyyy-mm-dd

## read the csv where I manually verified the images with reflectance > 0.12 and < 0.15 for cloud cover

Corrections = read.csv("pix_back_red_above_0.12.csv",header=TRUE)
names(Corrections)
Corrections = Corrections %>% select(site,ImgDates,River,Remove,Row.Site.Image,Comment)
Corrections$ImgDates = mdy(Corrections$ImgDates)

pix_back = left_join(pix_back, Corrections)
class(pix_back)
unique(pix_back$Remove)

pix_back$Remove = replace_na(pix_back$Remove,"No")
pix_back = pix_back %>% filter(Remove != "Yes")



pix_back_red = pix_back %>% group_by(site,ImgDates) %>% mutate(AvgRed = median(Red)) %>%
  select(site,ImgDates,River,AvgRed)  %>% distinct() %>% spread(site,AvgRed)

# combine the values for each site based on cloud free data availability
AvgRed = SiteCombine(pix_back_red) %>% rename(AvgRed = Reflect)
names(AvgRed)
refl_long_back = gather(AvgRed,key = "Band",value = "Reflect",AvgRed)
names(refl_long_back)
unique(refl_long$ImgDates)

ggplot(refl_long_back %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Reflectance")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))


refl_long_back$year = year(refl_long_back$ImgDates)
refl_long_back$month = month(refl_long_back$ImgDates)

ggplot(refl_long_back %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+
  geom_boxplot(aes(group = year)) + facet_wrap(.~River,ncol=1,nrow=4)+
  ggtitle("Reflectance")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))






##### Checking for overlapping images between Landsat 5Theoph##### Checking for overlapping images between Landsat 5, 7, 8 and 9 #####
#Create a date series for 2023 spaced 8 days apart
LandsatDates = data.frame("Date_interval_start" = seq(ymd("1989-01-01"),ymd("2023-12-27"), by = "8 days"))

overlap = read.csv("PolygonProperties_v15_1989_2023.csv",header=T)
overlap$Date_interval_start = ymd(stri_sub(overlap$Date_interval_start,from = 1,to = 10))

overlap = left_join(LandsatDates,overlap)
overlap$year = year(overlap$Date_interval_start)

ImageAvailability = overlap %>% group_by(year) %>% summarize(sum(Number_of_images,na.rm = TRUE))



