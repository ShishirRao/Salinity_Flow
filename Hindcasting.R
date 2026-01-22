##### Hindcasting: extending the ssc~reflectance relationship to 1990 - 2023 ####
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
areas = areas %>% select(area,name,Type) %>% dplyr::rename(area = area,site = name)


cloudStat = read.csv("CloudStat_1989_2023.csv",header=T)
cloudStat$date = ymd(stri_sub(cloudStat$date,from = 1,to = 10))
cloudStat = cloudStat %>% dplyr::rename(site=name,ImgDates = date,Cloud = cloud_mask)
unique(cloudStat$site)

waterStat = read.csv("WaterStats_1989_2023.csv",header=T)
waterStat$date = ymd(stri_sub(waterStat$date,from = 1,to = 10))
waterStat = waterStat %>% dplyr::rename(site=name,ImgDates = date) 
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

pix_back$year = year(pix_back$ImgDates)
pix_back$month = lubridate::month(pix_back$ImgDates,label = TRUE,abbr = TRUE)
unique(pix_back$month)

## July, Aug, Sep, Oct - wet
## Nov, Dec, Jan, Feb - Post monsoon
## Mar, Apr, May, June - dry

names(pix_back)
length(unique(pix_back$ImgDates))

# Calcuate site-wise median values and select the red band
pix_back_red = pix_back %>% dplyr::group_by(site,ImgDates) %>% dplyr::mutate(AvgRed = median(Red)) %>%
  dplyr::select(site,ImgDates,River,AvgRed)  %>% distinct() %>% spread(site,AvgRed)

hum_names <- as_labeller(
  c("Agha" = "Aghanashini",
    "Gang" = "Gangavali",
    "Kali" = "Kali", 
    "Shar" = "Sharavathi"))

# plotting all the individual sites. Only Gang_water_wet2 seems to deviate which is expected because it is in the estuary!
#allSites = gather(pix_back_red %>% select(-Gang_water_wet2)  ,key = "Site",value = "Reflect",-c(ImgDates,River))
allSites = gather(pix_back_red, key = "Site",value = "Reflect",-c(ImgDates,River))

unique(allSites$Site)

ggplot(allSites,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = Site,col = Site))+
  geom_smooth(aes(group = Site,col = Site),span = .9) + ggtitle("Red Reflectance from 1989 - 2023")+ xlab("Year") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%Y",date_breaks = "3000 day")+theme_bw()+ facet_grid(.~River, labeller = hum_names) +
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"),
        strip.text.x = element_text(size = 20))+
  theme(legend.position="bottom", legend.text = element_text(size = 16),  # Adjust size for legend entries
        legend.title = element_text(size = 22))+
  scale_color_discrete(labels = c("Agha_water_dry" = "Aghanashini - SS", 
                                  "Agha_water_wet1" = "Aghanashini - DS1", 
                                  "Agha_water_wet2" = "Aghanashini - DS2", 
                                  "Gang_water_dry" = "Gangavali - SS",
                                  "Gang_water_wet1" = "Gangavali - DS1",
                                  "Gang_water_wet2" = "Gangavali - DS2",
                                  "Kali_water_dry" = "Kali - SS",
                                  "Kali_water_wet1" = "Kali - DS1",
                                  "Shar_water_dry" = "Sharavathi - SS", 
                                  "Shar_water_dry1" = "Sharavathi - DS1", 
                                  "Shar_water_dry2" = "Sharavathi - DS2",
                                  "Shar_water_dry3" = "Sharavathi - DS3", 
                                  "Shar_water_wet1" = "Sharavathi - DS4")) 


#ggsave("E:/Shishir/FieldData/Results/Hindcast_sitewise_v1.jpg",  width = 5, height = 3, scale = 3)

# First plot only the sampling site. Dont combine the sites
#pix_back_red$Gang_water_wet2 = NA
AvgRed_SamplingSite = SiteCombine(pix_back_red,"No") %>% dplyr::rename(AvgRed = Reflect)
names(AvgRed_SamplingSite)
refl_long_back= gather(AvgRed_SamplingSite,key = "Band",value = "Reflect",AvgRed)

# sampling sites only
ggplot(refl_long_back ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + #ggtitle("Red Reflectance from 1988 - 2023 from sampling site only")+
  xlab("Year") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%Y",date_breaks = "1000 day")+theme_bw()+
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"),
        strip.text.x = element_text(size = 20))+
  theme(legend.position="bottom", legend.text = element_text(size = 16),  # Adjust size for legend entries
        legend.title = element_text(size = 22))

#ggsave("E:/Shishir/FieldData/Results/RedReflect_SamplingSite_1989_2023_v1.jpg",  width = 5, height = 3, scale = 3)


#Let us use data from the sampling site only. This was we can justify the use of alternative sites for calibration only. If we use all the sites for 
#hindcasting, then we will have to leave out Gangavali estuary where the reflectance is consistently higher because it is much further downstream. 



## Seperate the seasons and check if trends are different in dry season. 
refl_long_back$year = year(refl_long_back$ImgDates)
refl_long_back$month = lubridate::month(refl_long_back$ImgDates,label = TRUE,abbr = TRUE)
#refl_long_back$season = ifelse(refl_long_back$month >= 6 & refl_long_back$month <= 11,"wet","dry")

refl_long_back$season = "wet"
refl_long_back$season[refl_long_back$month == "Apr" | 
                        refl_long_back$month == "Mar" |
                        refl_long_back$month == "May" |
                        refl_long_back$month == "Jun"] = "Dry" 

refl_long_back$season[refl_long_back$month == "Feb" | 
                        refl_long_back$month == "Nov" |
                        refl_long_back$month == "Dec" |
                        refl_long_back$month == "Jan"] = "Post-monsoon" 

ggplot(refl_long_back %>% filter(season == "dry") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Red Reflectance from 1988 - 2023: dry season")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))

#### converting reflectance to SSC ###
# now convert reflectance to ssc using the respective
# linear regression intercept and slope for Agha, Gang and Kali
# but random intercept mean and slope for Shar

refl_long_back = left_join(refl_long_back,Reg_res)
refl_long_back$logSSC = (refl_long_back$Reflect * refl_long_back$slope) + refl_long_back$intercept
range(refl_long_back$logSSC,na.rm = TRUE)
refl_long_back$SSC = exp((refl_long_back$logSSC))
range(refl_long_back$SSC,na.rm = TRUE)


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




#### Divide the years in to pre, construction vs. post dam periods ###
refl_long_back$DamStatus = NA
unique(refl_long_back$DamStatus)

#Kali

refl_long_back$DamStatus[refl_long_back$River == "Kali" & refl_long_back$year <1991] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Kali" & 
                           refl_long_back$year >= 1991 & refl_long_back$year <= 1999] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Kali" & refl_long_back$year > 1999 ] = "Post-dam"

#Gang
refl_long_back$DamStatus[refl_long_back$River == "Gang" & refl_long_back$year < 1991] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Gang" & 
                           refl_long_back$year >= 1991 & refl_long_back$year <= 1999] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Gang" & refl_long_back$year > 1999 ] = "Post-dam"

#Shar

refl_long_back$DamStatus[refl_long_back$River == "Shar" & refl_long_back$year <1999] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Shar" & refl_long_back$year >= 1999 
                         & refl_long_back$year <= 2003] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Shar" & refl_long_back$year > 2003 ] = "Post-dam"

#Agha

refl_long_back$DamStatus[refl_long_back$River == "Agha" & refl_long_back$year <1999] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Agha" & refl_long_back$year >= 1999 
                         & refl_long_back$year <= 2003] = "Pre-dam"
refl_long_back$DamStatus[refl_long_back$River == "Agha" & refl_long_back$year > 2003 ] = "Post-dam"

unique(refl_long_back$DamStatus)

# Use data until 2012 to maintain temporal consistency in image availability

refl_long_back$DamStatus <- factor(refl_long_back$DamStatus, levels = c("Pre-dam", "Post-dam"))
refl_long_back$River <- factor(refl_long_back$River, levels = c("Agha", "Shar","Gang","Kali"))


hum_names <- as_labeller(
  c("Agha" = "Aghanashini",
    "Gang" = "Gangavali",
    "Kali" = "Kali", 
    "Shar" = "Sharavathi"))

summary1 = refl_long_back[refl_long_back$year <= 2012,]  %>% group_by(River) %>%
  do(mod = t.test(logSSC ~ DamStatus, data = .)) %>% ungroup() %>% 
  mutate(tidy = map(mod, broom::tidy),
         glance = map(mod, broom::glance),
         t = glance %>% map_dbl('statistic'),
         p_val = glance %>% map_dbl('p.value'),
         df = glance %>% map_dbl('parameter'))%>%
  select(River,t,p_val,df)


summary1$t = round(summary1$t,2)
summary1$df = round(summary1$df,2)
summary1$p_val[1:3] = round(summary1$p_val[1:3],3)
summary1$p_val[4] = signif(summary1$p_val[4],3)
summary1 = as.data.frame(summary1)

2012-2000

data_text <- data.frame(label = c(paste0("t (",summary1$df[1],") = ", summary1$t[1], ", p.val = ",summary1$p_val[1]),
                                  paste0("t (",summary1$df[2],") = ", summary1$t[2], ", p.val = ",summary1$p_val[2]),
                                  paste0("t (",summary1$df[3],") = ", summary1$t[3], ", p.val = ",summary1$p_val[3]),
                                  paste0("t (",summary1$df[4],") = ", summary1$t[4], ", p.val = ",summary1$p_val[4])),
                        x = c("Post-dam", "Pre-dam", "Pre-dam", "Pre-dam"),
                        y = c(6.3, 5.8, 7.3, 2.3),
                        River = summary1$River)  

unique(refl_long_back$year)

ggplot(refl_long_back[refl_long_back$year <= 2012,] ,aes(y = logSSC, x = DamStatus))+
  geom_boxplot(aes(group = DamStatus)) + facet_wrap(.~River,nrow = 2, ncol = 2,labeller = hum_names,scales = "free")+ theme_bw()+
  ylab("log (SSC)") + xlab("")+
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"),
        strip.text.x = element_text(size = 20))+
  theme(legend.position="bottom", legend.text = element_text(size = 16),  # Adjust size for legend entries
        legend.title = element_text(size = 22))+
  geom_text(data = data_text, size = 6,
            mapping = aes(x = x,
                          y = y,
                          label = label,hjust = c(0.9,-0.1,0,-0.04)))

#ggsave("E:/Shishir/FieldData/Results/Pre_vs_Post_v1.jpg",  width = 5, height = 3, scale = 3)
## seasonality in SSC post-dam i.e after 2010 ### 

refl_long_back$day = lubridate::yday(refl_long_back$ImgDates)
names(refl_long_back)

class(refl_long_back$day)
unique(refl_long_back$season)

refl_long_back$year = as.factor(refl_long_back$year)

quantiles_to_calculate <- c(0.1, 0.25, 0.50, 0.75, 0.9)

### quantiles based on season ###
grouped_quantiles_monthly <- refl_long_back %>% filter(ImgDates >= ymd("2012-01-01")) %>%
  group_by(River,month,season) %>% 
  dplyr::summarize( Q10_monthly = quantile(logSSC, probs = quantiles_to_calculate[1],na.rm=TRUE),
             Q25_monthly = quantile(logSSC, probs = quantiles_to_calculate[2],na.rm=TRUE),
             Q50_monthly = quantile(logSSC, probs = quantiles_to_calculate[3],na.rm=TRUE),
             Q75_monthly = quantile(logSSC, probs = quantiles_to_calculate[4],na.rm=TRUE),
             Q90_monthly = quantile(logSSC, probs = quantiles_to_calculate[5],na.rm=TRUE))



refl_long_back = left_join(refl_long_back, grouped_quantiles_monthly)

ggplot(refl_long_back %>% filter(ImgDates >= ymd("2012-01-01")) ,aes(y = logSSC, x = as.Date(day)))+
  geom_point(aes(col = year))+
  #geom_smooth(col = "black", span = 0.1)+
  facet_wrap(.~River,nrow = 2, ncol = 2,scales = "free",labeller =  hum_names)+
  theme_bw()+
  ylab("log (SSC)") + xlab("Month")+
  #geom_hline(data = refl_long_back %>% filter(ImgDates >= ymd("2012-01-01")) %>% select(Q50, River), aes(yintercept = Q50), colour="black")
  geom_line(aes(y = Q50_monthly),color = "black")+
 geom_smooth(aes(y = Q50_monthly),color = "black",span = 0.1)+
  theme(legend.position="right",
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"),
        strip.text.x = element_text(size = 20))+
  scale_x_date(date_labels = "%b",date_breaks = "35 day")

grouped_quantiles_season <- refl_long_back %>% filter(ImgDates >= ymd("2012-01-01")) %>%
  group_by(River,season) %>% 
  dplyr::summarize(mean = mean(logSSC,na.rm = TRUE))



Percent_change = grouped_quantiles_season %>% filter(season != "Post-monsoon") %>% mutate(dry = lag(mean)) %>%
                           filter(season == "wet") %>% mutate(Percent_change = 100*(mean - dry)/dry)  
    



df <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May"),
  Sales = c(100, 120, 110, 150, 180)
)

df <- df %>%
  mutate(
    Previous_Sales = lag(Sales),
    Percentage_Increase = ((Sales - Previous_Sales) / Previous_Sales) * 100
  )



?lag

?geom_smooth

#ggsave("E:/Shishir/FieldData/Results/SamplingSiteOnlyV2.jpg",  width = 5, height = 3, scale = 3)

