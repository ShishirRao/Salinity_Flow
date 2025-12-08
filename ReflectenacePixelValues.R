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
library(lmerTest)
library(tidyverse)

setwd("E:/Shishir/FieldData/Analysis/Reflectance/")


# function to combine the values for each site based on cloud free data availability
# If combine is yes, then the code checks for a valid reflectance from sampling site towards the downstream most site
# If combine is No, only sampling site is used. 
SiteCombine <- function(pix_wide,Combine) {
  
  pix_wide$Reflect = as.numeric(0)
  pix_wide$Source = "Actual"
  
  if(Combine == "Yes"){
      
  
    pix_wide$Source[which(is.na(pix_wide$Agha_water_dry) & pix_wide$River == "Agha")] = "Downstream"
    pix_wide$Source[which(is.na(pix_wide$Gang_water_dry) & pix_wide$River == "Gang")] = "Downstream"
    pix_wide$Source[which(is.na(pix_wide$Kali_water_dry) & pix_wide$River == "Kali")] = "Downstream"
    pix_wide$Source[which(is.na(pix_wide$Shar_water_dry) & pix_wide$River == "Shar")] = "Downstream"
    
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
refl$AvgRedbyGreen = refl$AvgRed / refl$AvgGreen
refl$AvgRedbyBlue = refl$AvgRed / refl$AvgBlue
refl$AvgRedbyNIR = refl$AvgRed / refl$AvgNIR


#write.csv(refl,"E:/Shishir/FieldData/Results/Refl_v6_polygonShar.csv")

refl_long = gather(refl,key = "Band",value = "Reflect",AvgRed, AvgGreen, AvgBlue,AvgNIR,AvgRedbyGreen,AvgRedbyBlue,AvgRedbyNIR,AvgBright)
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

ggplot(refl_long %>% filter(Band == "AvgGreen") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River,method = "auto"),span = .4) + ggtitle("Green Reflectance")+ xlab("Imagery date") +
  ylab("Green Reflectance from L8 and L9")+
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
refl_long = refl_long %>% select(ImgDates,ScheduledDates,SamplingMonth,River,Band,Reflect,ssc_mean,logssc,Season,Source) %>%
            filter(Band == "AvgRed")

unique(refl_long$Band)

refl_long_summary = summarySE(refl_long,measurevar = "ssc_mean",groupvars = c("River","Season"),na.rm = TRUE)
names(refl_long_summary)[3] = "Image_matched_samples"

refl_long_stat = refl_long %>% dplyr::group_by(River,Source) %>% dplyr::mutate(Number_of_images = n()) %>% select(River,Source,Number_of_images) %>% distinct()


Sample_size_summary = left_join(ssc_mean_Season %>% select(River, Season,In_situ_sample_size ),
                                refl_long_summary %>% select(River, Season, Image_matched_samples))
Sample_size_summary$percent = Sample_size_summary$Image_matched_samples * 100 / Sample_size_summary$In_situ_sample_size 

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


factor(refl_long$River)

refl_long$River <- factor(refl_long$River, levels = c("Agha", "Shar","Gang","Kali"))

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
  
  summary[,2:6] = round(summary[,2:6],3)
  

#refl_long = left_join(refl_long,summary) 

#River_rsq = c(as.character(round(unique(refl_long$rsq),3)))
#River_Adjrsq = c(as.character(round(unique(refl_long$adj.rsq),3)))
#River_pval = c(as.character(round(unique(refl_long$p_val),3)))
#River_slope = c(as.character(round(unique(refl_long$slope),3)))
#River_intercept = c(as.character(round(unique(refl_long$intercept),3)))
#River = c("Agha","Shar","Gang","Kali")
#River <- factor(River, levels = c("Agha", "Shar","Gang","Kali"))


#Reg_res = data.frame(slope = River_slope,intercept = River_intercept,River = River)
Reg_res = data.frame(slope = summary$slope,intercept = summary$intercept,River = summary$River)


hum_names <- as_labeller(
  c("Agha" = "Aghanashini","Shar" = "Sharavathi","Gang" = "Gangavali", "Kali" = "Kali"))


data_text <- data.frame(label = c(paste("Log (SSC) = ",summary$slope[1],"* RED +",summary$intercept[1], "\n AdjR2 =",summary$adj.rsq[1],",","p.val = ",summary$p_val[1]),
                                  paste("Log (SSC) = ",summary$slope[2],"* RED +",summary$intercept[2], "\n AdjR2 =",summary$adj.rsq[2],",","p.val = ",summary$p_val[2]),
                                  paste("Log (SSC) = ",summary$slope[3],"* RED +",summary$intercept[3], "\n AdjR2 =",summary$adj.rsq[3],",","p.val = ",summary$p_val[3]),
                                  paste("Log (SSC) = ",summary$slope[4],"* RED +",summary$intercept[4],"\n AdjR2 =",summary$adj.rsq[4],",","p.val = ",summary$p_val[4])),
                        x = c(0.06, 0.05, 0.067, 0.058 ),
                        y = c(5.8, 1.88, 8.2, 2.66),
                        River)  

class(refl_long$River)

unique(refl_long$River)



ggplot(refl_long,aes(y = logssc, x = Reflect))+
    geom_point(aes(color=Season),size = 3)+
    #stat_summary(fun.data= mean_cl_normal) + 
    geom_smooth(method='lm') + 
  #facet_wrap(.~River, scales = "free",labeller = hum_names)+
  facet_wrap(~ River, scales = "free", labeller = hum_names)+
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

# ggsave("SSCvsRed_withPvals_v2.jpg", device = "jpg",path = "E:/Shishir/FieldData/Results/",
#        scale = 3, width = 5, height = 3,
#         dpi = 300, limitsize = TRUE)

#ggsave("E:/Shishir/FieldData/Results/SSCvsRedbyNIR_withPvals_v2.jpg",  width = 6, height = 3.5,scale = 3)



refl_long = refl_long %>% select(ImgDates,ScheduledDates,SamplingMonth,River,Band,Reflect,ssc_mean,logssc,Season) %>%
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

fit_ran_incpt_without_lmertestShar = lmerTest::lmer(logssc ~ Reflect + (1 | River), data = refl_long_without_Shar)
summ = summary(fit_ran_incpt_without_lmertestShar)

signif(summ$coefficients[10],2)

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



data_text <- data.frame(label = c(paste0("log (SSC) =",round(summ$coefficients[2],2),"* RED + ",round(summ$coefficients[1],2))),
                        x = 0.06,
                        y = 8,River)  

ggplot(refl_long_without_Shar, aes(x = Reflect, y = logssc, color = River)) +
  geom_point(alpha = 1.8) +
  geom_line(aes(y = predicted_values),linewidth = 1.5) +
  labs(x = "Red Reflectance", y = "log(SSC (mg/L))") +
  theme_bw() +  theme(legend.position="bottom")+
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  theme(legend.position="bottom", legend.text = element_text(size = 20),  # Adjust size for legend entries
        legend.title = element_text(size = 22))+
  geom_text(data = data_text , size = 6, col = "black",
            mapping = aes(x = x,
                          y = y,
                         label = label))

?geom_text

#ggsave("E:/Shishir/FieldData/Results/RandomIntercept_v1.jpg",  width = 3, height = 2,scale = 3)

# Using coef() with summary()
fixed_effects <- coef(summary(fit_ran_incpt_without_Shar))[, "Estimate"]

# Extracting the intercept and slope by name or position
mean_intercept <- fixed_effects["(Intercept)"]
mean_slope <- fixed_effects["Reflect"]

Reg_res$slope = as.numeric(Reg_res$slope)
Reg_res$intercept = as.numeric(Reg_res$intercept)

Reg_res$slope[Reg_res$River == "Shar"] = mean_slope
Reg_res$intercept[Reg_res$River == "Shar"] = mean_intercept





