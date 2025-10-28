library(plyr)
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
library(lattice)

setwd("E:/Shishir/FieldData/Analysis/Reflectance/")


# function to combine the values for each site based on cloud free data availability
# If combine is yes, then the code checks for a valid reflectance from sampling site towards the downstream most site
# If combine is No, only sampling site is used. 
SiteCombine <- function(pix_wide,Combine) {
  
  pix_wide$Reflect = as.numeric(0)
  
  if(Combine == "Yes"){
      
  
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
  
  }else{
    pix_wide$Reflect[pix_wide$River == "Agha"] = pix_wide$Agha_water_dry[pix_wide$River == "Agha"]
    pix_wide$Reflect[pix_wide$River == "Gang"] = pix_wide$Gang_water_dry[pix_wide$River == "Gang"]
    pix_wide$Reflect[pix_wide$River == "Kali"] = pix_wide$Kali_water_dry[pix_wide$River == "Kali"]
    pix_wide$Reflect[pix_wide$River == "Shar"] = pix_wide$Shar_water_dry[pix_wide$River == "Shar"]
  }
  
  
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

# Reduced the threshold from 0.3 to 0.15. Any value higher than 0.15 is likely a cloud pixel
#pix = pix %>% filter(Red <= 0.3 & Red >= 0.009) 
pix = pix %>% filter(Red <= 0.15 & Red >= 0.009) 

temp = pix %>% filter(Red >= 0.12)
unique(temp$ImgDates)

unique(temp$site[temp$ImgDates == ymd("2023-07-20")])
(temp[temp$ImgDates == ymd("2023-07-20"),])

unique(pix$site[pix$ImgDates == ymd("1990-08-02")])
test = (pix[pix$ImgDates == ymd("1990-08-02"),])

pix_red = pix %>% dplyr::group_by(site,ImgDates) %>% dplyr::mutate(AvgRed = median(Red)) %>%
  dplyr::select(site,ImgDates,River,AvgRed)  %>% dplyr::distinct() %>% spread(site,AvgRed)

pix_green = pix %>% dplyr::group_by(site,ImgDates) %>% dplyr::mutate(AvgGreen = median(Green)) %>%
  dplyr::select(site,ImgDates,River,AvgGreen)  %>% dplyr::distinct() %>% spread(site,AvgGreen)

pix_blue = pix %>% dplyr::group_by(site,ImgDates) %>% dplyr::mutate(AvgBlue = median(Blue)) %>%
  dplyr::select(site,ImgDates,River,AvgBlue)  %>% dplyr::distinct() %>% spread(site,AvgBlue)

pix_NIR = pix %>% dplyr::group_by(site,ImgDates) %>% dplyr::mutate(AvgNIR = median(NIR)) %>%
  dplyr::select(site,ImgDates,River,AvgNIR)  %>% dplyr::distinct() %>% spread(site,AvgNIR)

pix_Bright = pix %>% dplyr::group_by(site,ImgDates) %>% dplyr::mutate(AvgNIR = mean(NIR)) %>%
  dplyr::select(site,ImgDates,River,AvgNIR)  %>% dplyr::distinct() %>% spread(site,AvgNIR)

temp2 = pix_Bright[pix_Bright$ImgDates == ymd("1994-06-26"),]


# combine the values for each site based on cloud free data availability
AvgRed = SiteCombine(pix_red, "Yes") %>% dplyr::rename(AvgRed = Reflect)
AvgGreen = SiteCombine(pix_green, "Yes") %>% dplyr::rename(AvgGreen = Reflect)
AvgBlue = SiteCombine(pix_blue, "Yes") %>% dplyr::rename(AvgBlue = Reflect)
AvgNIR = SiteCombine(pix_NIR, "Yes") %>% dplyr::rename(AvgNIR = Reflect)
AvgBright = SiteCombine(pix_Bright, "Yes") %>% dplyr::rename(AvgBright = Reflect)

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
refl_long = refl_long %>% select(ImgDates,ScheduledDates,SamplingMonth,River,Band,Reflect,ssc_mean,logssc,Season) %>%
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
River_slope = c(as.character(round(unique(refl_long$slope),3)))
River_intercept = c(as.character(round(unique(refl_long$intercept),3)))
River = c("Agha","Gang","Kali","Shar")

Reg_res = data.frame(slope = River_slope,intercept = River_intercept,River = River)

hum_names <- as_labeller(
  c("Agha" = paste("Agha: ","AdjR2 =",River_Adjrsq[1],",Intercept = ",River_intercept[1],"Slope = ",River_slope[1],"p.val = ",River_pval[1]), 
    "Gang" = paste("Gang: ","AdjR2 =",River_Adjrsq[2],",Intercept = ",River_intercept[2],"Slope = ",River_slope[2],"p.val = ",River_pval[2]), 
    "Kali" = paste("Kali: ","AdjR2 =",River_Adjrsq[3],",Intercept = ",River_intercept[3],"Slope = ",River_slope[3],"p.val = ",River_pval[3]),  
    "Shar" = paste("Shar: ","AdjR2 =",River_Adjrsq[4],",Intercept = ",River_intercept[4],"Slope = ",River_slope[4],"p.val = ",River_pval[4]))) 

hum_names <- as_labeller(
  c("Agha" = "Aghanashini",
    "Gang" = "Gangavali",
    "Kali" = "Kali", 
    "Shar" = "Sharavathi"))


data_text <- data.frame(label = c(paste("Log (SSC) = ",River_slope[1],"* RED +",River_intercept[1], "\n AdjR2 =",River_Adjrsq[1],",","p.val = ",River_pval[1]),
                                  paste("Log (SSC) = ",River_slope[2],"* RED +",River_intercept[2], "\n AdjR2 =",River_Adjrsq[2],",","p.val = ",River_pval[2]),
                                  paste("Log (SSC) = ",River_slope[3],"* RED +",River_intercept[3], "\n AdjR2 =",River_Adjrsq[3],",","p.val = ",River_pval[3]),
                                  paste("Log (SSC) = ",River_slope[4],"* RED +",River_intercept[4],"\n AdjR2 =",River_Adjrsq[4],",","p.val = ",River_pval[4])),
                        x = c(0.06, 0.067, 0.058, 0.05),
                        y = c(5.8, 8.2, 2.66, 1.88),
                        River)  


ggplot(refl_long,aes(y = logssc, x = Reflect))+
    geom_point(aes(color=Season),size = 3)+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm') + 
  #facet_wrap(.~River, scales = "free",labeller = hum_names)+
  facet_wrap(.~River, scales = "free", labeller = hum_names)+
    theme(strip.text = element_text(size = 1))+
    ggtitle(" ")+theme_bw()+ xlab("Red Reflectance") +ylab("log(SSC (mg/L))")+
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"),
        strip.text.x = element_text(size = 20))+
  theme(legend.position="bottom", legend.text = element_text(size = 20),  # Adjust size for legend entries
        legend.title = element_text(size = 22))+
  geom_text(data = data_text, size = 6,
            mapping = aes(x = x,
                          y = y,
                          label = label))

# ggsave("SSCvsRed_withPvals.jpg", device = "jpg",path = "E:/Shishir/FieldData/Results/",
#        scale = 3, width = 5, height = 3,
#         dpi = 300, limitsize = TRUE)

ggsave("E:/Shishir/FieldData/Results/SSCvsRed_withPvals_v1.jpg",  width = 6, height = 3.5,scale = 3)



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

# Random intercept
fit_ran_incpt_all = lmer(logssc ~ Reflect + (1 | River), data = refl_long)
summary(fit_ran_incpt_all)
confint(fit_ran_incpt_all)

# Random slope
fit_ran_incpt_slp = lmer(logssc ~ Reflect + (1+Reflect | River), data = refl_long)
summary(fit_ran_incpt_slp)
#confint(fit_ran_incpt_slp)

ggplot(refl_long,aes(x=Reflect,y=logssc,col=River)) + geom_jitter() + geom_boxplot(alpha=0.2) + facet_grid(~River)

# Random slope model didn't work because of singularity error
isSingular(fit_ran_incpt_slp,1e-4)

# Random intercept for all except Sharavathi
refl_long_without_Shar = refl_long %>% filter(River!= "Shar")
unique(refl_long_without_Shar$River)
class(refl_long_without_Shar$River)

fit_ran_incpt_without_Shar = lmer(logssc ~ Reflect + (1 | River), data = refl_long_without_Shar)
summary(fit_ran_incpt_without_Shar)
confint(fit_ran_incpt_without_Shar)

random_intercepts <- ranef(fit_ran_incpt_without_Shar)$River[, "(Intercept)"]
dotplot(ranef(fit_ran_incpt_without_Shar, condVar = TRUE))


random_effects_df <- data.frame(
  group = rownames(ranef(fit_ran_incpt_without_Shar)$River),
  intercept = random_intercepts
)

ggplot(random_effects_df, aes(x = group, y = intercept)) +
  geom_point() +
  labs(title = "Random Intercepts", x = "Grouping Variable", y = "Intercept Deviation") +
  theme_minimal()

refl_long_without_Shar$predicted_values <- predict(fit_ran_incpt_without_Shar, newdata = refl_long_without_Shar)

ggplot(refl_long_without_Shar, aes(x = Reflect, y = logssc, color = River)) +
  geom_point(alpha = 0.6) +
  geom_line(aes(y = predicted_values)) +
  labs(title = "Random Intercept Model ",
       x = "Red Reflectance", y = "log (SSC)") +
  theme_bw() +  theme(legend.position="bottom")


# Using coef() with summary()
fixed_effects <- coef(summary(fit_ran_incpt_without_Shar))[, "Estimate"]

# Extracting the intercept and slope by name or position
mean_intercept <- fixed_effects["(Intercept)"]
mean_slope <- fixed_effects["Reflect"]

Reg_res$slope = as.numeric(Reg_res$slope)
Reg_res$intercept = as.numeric(Reg_res$intercept)

Reg_res$slope[Reg_res$River == "Shar"] = mean_slope
Reg_res$intercept[Reg_res$River == "Shar"] = mean_intercept

##### extending the ssc~reflectance relationship to 1990 - 2023 ####
pix_back = read.csv("PolygonValues_v15_1989_2023.csv",header=T)
#pix_back = read.csv("PolygonValues_v15_1989_2023_waterClass4.csv",header=T)

pix_back = pix_back %>% select("name","B2","B3","B4","B5","pixel_qa","date")
names(pix_back) = c("site","Blue","Green","Red","NIR","pixel_qa","ImgDates")

pix_back$ImgDates = ymd(stri_sub(pix_back$ImgDates,from = 1,to = 10))
pix_back$River = stri_sub(pix_back$site,from = 1,to = 4)

pix_back = pix_back %>% filter(Red <= 0.15 & Red >= 0.009) 

#### get some stats on water and clouds to remove cloud pixels ###
areas = read.csv("polygon_areas.csv",header=T)
head(areas)
areas = areas %>% select(area,name,Type) %>% rename(area = area,site = name)


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

temp = pix_back %>% filter(Red >= 0.1) %>% filter(Red <= 0.12)
#unique(temp$ImgDates)
#names(temp)
#temp = left_join(temp, Corrections) 
#write.csv(temp %>% select(site,ImgDates,River,Remove,Row.Site.Image, Comment) %>% distinct(),"pix_back_red_above_0.1_lessthan0.12.csv")


## read the csv where I manually verified the images with reflectance > 0.12 and < 0.15 for cloud cover

Corrections1 = read.csv("pix_back_red_above_0.12.csv",header=TRUE)
Corrections2 = read.csv("pix_back_red_above_0.1_lessthan0.12.csv",header=TRUE)
Corrections = rbind(Corrections1,Corrections2)
Corrections = Corrections %>% select(site,ImgDates,River,Remove,Row.Site.Image) %>% distinct()
Corrections$ImgDates = mdy(Corrections$ImgDates)

Corrections[duplicated(Corrections),]


pix_back = left_join(pix_back, Corrections)
class(pix_back)
unique(pix_back$Remove)

# remove all the sites where pixels are affected by cloud cover
pix_back$Remove = replace_na(pix_back$Remove,"Keep")
pix_back = pix_back %>% filter(Remove != "Yes")

# Calcuate site-wise median values and select the red band
pix_back_red = pix_back %>% group_by(site,ImgDates) %>% mutate(AvgRed = median(Red)) %>%
  select(site,ImgDates,River,AvgRed)  %>% distinct() %>% spread(site,AvgRed)


# plotting all the individual sites. Only Gang_water_wet2 seems to deviate which is expected because it is in the estuary!
#allSites = gather(pix_back_red %>% select(-Gang_water_wet2)  ,key = "Site",value = "Reflect",-c(ImgDates,River))
allSites = gather(pix_back_red, key = "Site",value = "Reflect",-c(ImgDates,River))
ggplot(allSites,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = Site,col = Site))+
  geom_smooth(aes(group = Site,col = Site),span = .5) + ggtitle("Red Reflectance from 1988 - 2023")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "2000 day")+theme_bw()+ facet_grid(.~River) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))+
  theme(legend.position="bottom")


# First plot only the sampling site. Dont combine the sites
AvgRed_SamplingSite = SiteCombine(pix_back_red,"No") %>% rename(AvgRed = Reflect)
names(AvgRed_SamplingSite)
refl_long_back_SamplingSite = gather(AvgRed_SamplingSite,key = "Band",value = "Reflect",AvgRed)

# sampling sites only
ggplot(refl_long_back_SamplingSite  ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Red Reflectance from 1988 - 2023 from sampling site only")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))

#Remove the Gang estuary site because its reflectance is much consistenly higher than the sampling site / dry sites
pix_back_GangEstuary = pix_back_red
pix_back_GangEstuary$Gang_water_wet2 = NA

AvgRed_GangEstuary = SiteCombine(pix_back_GangEstuary,"Yes") %>% rename(AvgRed = Reflect)
refl_long_back_GangEstuary = gather(AvgRed_GangEstuary,key = "Band",value = "Reflect",AvgRed)
names(refl_long_back_GangEstuary)

ggplot(refl_long_back_GangEstuary %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Red Reflectance from 1988 - 2023 from all sites except Gangavali estuary")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))

## All sites combined including Gang estuary ##
pix_back_red$Gang_water_wet2 = NA
AvgRed = SiteCombine(pix_back_red,"Yes") %>% rename(AvgRed = Reflect)
refl_long_back = gather(AvgRed,key = "Band",value = "Reflect",AvgRed)
names(refl_long_back_GangEstuary)

ggplot(refl_long_back %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Red Reflectance from 1988 - 2023 from all sites")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))


## Seperate the seasons
refl_long_back$year = year(refl_long_back$ImgDates)
refl_long_back$month = month(refl_long_back$ImgDates)
refl_long_back$season = ifelse(refl_long_back$month >= 6 & refl_long_back$month <= 11,"wet","dry")

ggplot(refl_long_back %>% filter(season == "dry") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Red Reflectance from 1988 - 2023")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))

# now convert reflectance to ssc using the respective
# linear regression intercept and slope for Agha, Gang and Kali
# but random intercept mean and slope for Shar

refl_long_back = left_join(refl_long_back,Reg_res)
refl_long_back$SSC = (refl_long_back$Reflect * refl_long_back$slope) + refl_long_back$intercept
refl_long_back$logSSC = log(refl_long_back$SSC)

Reg_res

ggplot(refl_long_back ,aes(y = logSSC, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Log (SSC) from 1988 - 2023 ")+ xlab("Imagery date") + ylab("log SSC")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))


ggplot(refl_long_back %>% filter(season == "wet") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Wet Season (June - Nov) Red Reflectance from 1988 - 2023")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))


##### Checking for overlapping images between Landsat 5Theoph##### Checking for overlapping images between Landsat 5, 7, 8 and 9 #####
#Create a date series for 2023 spaced 8 days apart
LandsatDates = data.frame("Date_interval_start" = seq(ymd("1989-01-01"),ymd("2023-12-27"), by = "8 days"))

#overlap = read.csv("PolygonProperties_v15_1989_2023.csv",header=T)
overlap = read.csv("PolygonProperties_v15_1989_2023.csv")
overlap$Date_interval_start = ymd(stri_sub(overlap$Date_interval_start,from = 1,to = 10))

overlap = left_join(LandsatDates,overlap)
overlap = overlap[complete.cases(overlap),]
overlap$year = year(overlap$Date_interval_start)
overlap$month = month(overlap$Date_interval_start)
overlap$season = ifelse(overlap$month >= 6 & overlap$month <= 11,"wet","dry")

ImageAvailability = overlap %>% group_by(year) %>% summarize(sum(Number_of_images,na.rm = TRUE))
names(ImageAvailability) = c("Year","No_of_Images")

SeasonalImages = overlap %>% group_by(year,season) %>% summarize(sum(Number_of_images,na.rm = TRUE))
names(SeasonalImages) = c("Year","Season","No_of_Images_season")

#SeasonalImages = spread(SeasonalImages, key = Season, value = No_of_Images_season, fill = NA, convert = FALSE, drop = TRUE)
#SeasonalImages$Total = SeasonalImages$dry + SeasonalImages$wet

ggplot(ImageAvailability, aes(x=Year,y = No_of_Images )) + geom_bar(stat = "identity") +
  xlab("Year") + ylab("Number of Landsat Images") + theme_bw()

ggplot(data = SeasonalImages, aes(x = Year, y = No_of_Images_season, fill = Season)) +
  geom_bar(stat = "identity", position = "stack") + xlab("Year") + ylab("No of Landsat Images")+
  theme_bw() + theme(legend.position="bottom") +   theme(axis.text=element_text(size=12),
                                                  axis.title=element_text(size=12,face="bold"))



### PerRiverImage availability after corrections. ###
AvgRed$year = year(AvgRed$ImgDates)
AvgRed$month = month(AvgRed$ImgDates)
AvgRed$season = ifelse(AvgRed$month >= 6 & AvgRed$month <= 11,"wet","dry")
#Remove sites with NA
AvgRed = AvgRed[complete.cases(AvgRed),]

SeasonalImages = AvgRed %>% group_by(River,year,season) %>% mutate(Number_of_images = n()) %>%
                 select(River,year,season,Number_of_images) %>% distinct()

ggplot(data = SeasonalImages, aes(x = year, y = Number_of_images, fill = season)) +
  geom_bar(stat = "identity", position = "stack") + xlab("Year") + 
  ylab("Count")+ facet_grid(.~River)+
  theme_bw() + theme(legend.position="bottom") +   ggtitle("No of valid reflectance values from each river with all sites combined but not Gang estuary")+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))+
  scale_y_continuous(limits = c(0, 35))


## PerRiverImage availability after corrections at sampling site only ##
AvgRed_SamplingSite$year = year(AvgRed_SamplingSite$ImgDates)
AvgRed_SamplingSite$month = month(AvgRed_SamplingSite$ImgDates)
AvgRed_SamplingSite$season = ifelse(AvgRed_SamplingSite$month >= 6 & AvgRed_SamplingSite$month <= 11,"wet","dry")
#Remove sites with NA
AvgRed_SamplingSite = AvgRed_SamplingSite[complete.cases(AvgRed_SamplingSite),]

SeasonalImages_SamplingSite = AvgRed_SamplingSite %>% group_by(River,year,season) %>% mutate(Number_of_images = n()) %>%
  select(River,year,season,Number_of_images) %>% distinct()

ggplot(data = SeasonalImages_SamplingSite, aes(x = year, y = Number_of_images, fill = season)) +
  geom_bar(stat = "identity", position = "stack") + xlab("Year") + 
  ylab("Count")+ facet_grid(.~River)+
  ggtitle("No of valid reflectance values from each river for sampling sites only")+
  theme_bw() + theme(legend.position="bottom") +   
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold"))+
  scale_y_continuous(limits = c(0, 35))


#### Divide the years in to pre, construction vs. post dam periods ###
refl_long_back$DamStatus = NA
unique(refl_long_back$DamStatus)

#Kali

refl_long_back$DamStatus[refl_long_back$River == "Kali" & refl_long_back$year <1991] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Kali" & 
                           refl_long_back$year >= 1991 & refl_long_back$year <= 1999] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Kali" & refl_long_back$year > 1999 ] = "Post"

#Gang
refl_long_back$DamStatus[refl_long_back$River == "Gang" & refl_long_back$year < 1991] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Gang" & 
                           refl_long_back$year >= 1991 & refl_long_back$year <= 1999] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Gang" & refl_long_back$year > 1999 ] = "Post"

#Shar

refl_long_back$DamStatus[refl_long_back$River == "Shar" & refl_long_back$year <1999] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Shar" & refl_long_back$year >= 1999 
                         & refl_long_back$year <= 2003] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Shar" & refl_long_back$year > 2003 ] = "Post"

#Agha

refl_long_back$DamStatus[refl_long_back$River == "Agha" & refl_long_back$year <1999] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Agha" & refl_long_back$year >= 1998 
                         & refl_long_back$year <= 2003] = "Pre"
refl_long_back$DamStatus[refl_long_back$River == "Agha" & refl_long_back$year > 2003 ] = "Post"

unique(refl_long_back$DamStatus)

refl_long_back = refl_long_back[refl_long_back$year < 2010,]

refl_long_back$DamStatus <- factor(refl_long_back$DamStatus, levels = c("Pre", "Post"))
ggplot(refl_long_back ,aes(y = Reflect, x = DamStatus))+
  geom_boxplot(aes(group = DamStatus)) + facet_grid(.~River)+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))


Refl_summary = summarySE(refl_long_back,measurevar = "Reflect",groupvars = c("River","DamStatus"),na.rm = TRUE)
Refl_summary$River = as.factor(Refl_summary$River)
Refl_summary$DamStatus = as.factor(Refl_summary$DamStatus)
Refl_summary$Pair = "A"
Refl_summary$Pair[Refl_summary$River == "Kali" |Refl_summary$River == "Gang"  ] = "A"
Refl_summary$Pair[Refl_summary$River == "Agha" |Refl_summary$River == "Shar"  ] = "B"

pd = position_dodge(.5)  
Refl_summary$DamStatus <- factor(Refl_summary$DamStatus, levels = c("Pre", "Post"))
ggplot(Refl_summary, aes(x=DamStatus, y=Reflect,color=River,fill=River))+ 
  geom_point(size = 3,position = pd) +
  geom_errorbar(aes(ymin=Reflect-ci, ymax=Reflect+ci), position = pd) + facet_wrap(.~Pair)+
  ylab("Reflectance") + xlab("")+ 
  theme(legend.position="right")+
  scale_linetype(guide = FALSE) + scale_fill_discrete(guide=FALSE)
