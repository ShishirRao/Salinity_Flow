library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(stringi)
library(stringdist)
library(data.table)
library("sp")
library("sf")


setwd("E:/Shishir/FieldData/Analysis/Rainfall/")

##### read daily rainfall and convert to monthly sum  ####
precip = read.csv("UK_Basin_1981_2023_meanPrecip.csv",header=T)
names(precip)
head(precip)
unique(precip$Basin_name)


precip$date = stri_sub(precip$date,from = 1,to = 10)
precip$date = ymd(precip$date)
precip$year = year(precip$date)
precip$month = lubridate::month(precip$date)

class(precip$year)

#Summarize the taluk wise rainfall by month, year
MonthPrecip = as.data.frame(precip %>%
                              dplyr::group_by(Basin_name, year, month) %>%
                              dplyr::summarize(Rain_mm = sum(mean, na.rm=TRUE)))

head(MonthPrecip)
unique(MonthPrecip$Taluk)

yearPrecip = as.data.frame(precip %>%
                             dplyr::group_by(Basin_name, year) %>%
                             dplyr::summarize(Rain_mm = sum(mean, na.rm=TRUE)))


UK_whsed = st_read("E:/Shishir/Thesis_GIS/UK/UK_Wshed_bounds.shp")
UK_whsed$Area = as.numeric(st_area(UK_whsed) / (1000*1000))
class(UK_whsed$Area)
names(UK_whsed)

yearPrecip = left_join(yearPrecip,UK_whsed %>% select(Basin_name,Area))

names(yearPrecip)
AnnualAvg = yearPrecip %>% group_by(Basin_name) %>% dplyr::summarise((AnnualAvg = round(mean(Rain_mm),2)))

yearPrecip$RainfallDepth = yearPrecip$Rain_mm/yearPrecip$Area
?st_read




ggplot(yearPrecip[yearPrecip$year >= 1989 & yearPrecip$year <= 2012,],aes(x=year,y=Rain_mm)) + geom_point(aes(color = Basin_name))+
  geom_smooth(aes(group = Basin_name,col = Basin_name,method = "auto"),span = 0.3)

ggplot(yearPrecip[yearPrecip$year >= 1989 & yearPrecip$year <= 2012,],aes(x=year,y=RainfallDepth)) + geom_point(aes(color = Basin_name))+
  xlab("year") + ylab("Rainfall_mm/sqkm")+ 
  geom_smooth(aes(group = Basin_name,col = Basin_name,method = "auto"),span = 0.3)
  

unique(yearPrecip$year)

yearPrecip = yearPrecip %>% filter(year >= 1989 & year <= 2012)
  
    
  yearPrecip$DamStatus = NA
  
  
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Kali" & yearPrecip$year <1991] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Kali" & 
                             yearPrecip$year >= 1991 & yearPrecip$year <= 1999] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Kali" & yearPrecip$year > 1999 ] = "Post-dam"
  
  #Gang
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Gangavali" & yearPrecip$year < 1991] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Gangavali" & 
                             yearPrecip$year >= 1991 & yearPrecip$year <= 1999] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Gangavali" & yearPrecip$year > 1999 ] = "Post-dam"
  
  #Shar
  
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Sharavathi" & yearPrecip$year <1999] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Sharavathi" & yearPrecip$year >= 1999 
                           & yearPrecip$year <= 2003] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Sharavathi" & yearPrecip$year > 2003 ] = "Post-dam"
  
  #Agha
  
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Aghanashini" & yearPrecip$year <1999] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Aghanashini" & yearPrecip$year >= 1999 
                           & yearPrecip$year <= 2003] = "Pre-dam"
  yearPrecip$DamStatus[yearPrecip$Basin_name == "Aghanashini" & yearPrecip$year > 2003 ] = "Post-dam"  


  yearPrecip$DamStatus <- factor(yearPrecip$DamStatus, levels = c("Pre-dam", "Post-dam"))
  yearPrecip$Basin_name <- factor(yearPrecip$Basin_name, levels = c("Aghanashini", "Sharavathi","Gangavali","Kali"))
  

  summary1 = yearPrecip[yearPrecip$year <= 2012,]  %>% group_by(Basin_name) %>%
    do(mod = t.test(RainfallDepth ~ DamStatus, data = .)) %>% ungroup() %>% 
    mutate(tidy = map(mod, broom::tidy),
           glance = map(mod, broom::glance),
           t = glance %>% map_dbl('statistic'),
           p_val = glance %>% map_dbl('p.value'),
           df = glance %>% map_dbl('parameter'))%>%
    select(Basin_name,t,p_val,df)
  
  
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
                          River = summary1$Basin_name)  
  
  
  ggplot(yearPrecip[yearPrecip$year <= 2012,] ,aes(y = RainfallDepth, x = DamStatus))+
    geom_boxplot(aes(group = DamStatus)) + facet_wrap(.~Basin_name,nrow = 2, ncol = 2,scales = "free")+ theme_bw()+
    ylab("log (SSC)") + xlab("")+
    theme(legend.position="bottom",
          axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size = 25, face = "bold"),
          strip.text.x = element_text(size = 20))+
    theme(legend.position="bottom", legend.text = element_text(size = 16),  # Adjust size for legend entries
          legend.title = element_text(size = 22))
  

?geom_smooth
