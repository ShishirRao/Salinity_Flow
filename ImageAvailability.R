
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


##### Checking for overlapping images between Landsat 5Theoph##### Checking for overlapping images between Landsat 5, 7, 8 and 9 #####
#Create a date series for 2023 spaced 8 days apart

pix_back$year = year(pix_back$ImgDates)
pix_back$month = lubridate::month(pix_back$ImgDates,label = TRUE,abbr = TRUE)
unique(pix_back$month)

## July, Aug, Sep, Oct - wet
## Nov, Dec, Jan, Feb - Post monsoon
## Mar, Apr, May, June - dry

pix_back$season = "wet"
pix_back$season[pix_back$month == "Apr" | 
                  pix_back$month == "Mar" |
                  pix_back$month == "May" |
                  pix_back$month == "Jun"] = "Dry" 

pix_back$season[pix_back$month == "Feb" | 
                  pix_back$month == "Nov" |
                  pix_back$month == "Dec" |
                  pix_back$month == "Jan"] = "Post-monsoon" 


ImageAvailability = pix_back %>% select(ImgDates,year) %>%  distinct() %>% group_by(year) %>% mutate(Number_of_images = n()) %>%
  select(year, Number_of_images) %>% distinct()

mean(ImageAvailability$Number_of_images[ImageAvailability$year <=2012])
mean(ImageAvailability$Number_of_images[ImageAvailability$year > 2012])

SeasonalImages = pix_back %>% select(ImgDates,year,season) %>%  distinct() %>% group_by(year,season) %>% mutate(Number_of_images = n()) %>%
  select(year,season,Number_of_images) %>% distinct()





ggplot(data = SeasonalImages, aes(x = year, y = Number_of_images, fill = season)) +
  geom_bar(stat = "identity", position = "stack") + xlab("Year") + ylab("No. of Landsat Images")+ theme_bw()+
  theme(legend.position="bottom",
        axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  theme(legend.position="bottom", legend.text = element_text(size = 20),  # Adjust size for legend entries
        legend.title = element_text(size = 22))

#ggsave("E:/Shishir/FieldData/Results/LandsatImageAvailability_v1.jpg",  width = 3, height = 2,scale = 3)

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
