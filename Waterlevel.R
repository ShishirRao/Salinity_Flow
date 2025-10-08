library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(gridExtra)

setwd("E:/Shishir/FieldData")

###### water level ######
#Sal = read.csv("Analysis/  ",header=T)
WLR1 = read.csv("Analysis/WaterLevel/Agha/Agha_WLR_Feb-Mar.csv",header=T)
names(WLR1) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR1)

WLR2 = read.csv("Analysis/WaterLevel/Agha/Agha_30ftWLR_Apr15.csv",header=T)
names(WLR2) = c("SlNo","Date_time","kPa","Temp_degC_WLR")

WLR3 = read.csv("Analysis/WaterLevel/Agha/Agha_30ftWLR_May1.csv",header=T)
names(WLR3) = c("SlNo","Date_time","kPa","Temp_degC_WLR")

WLR4 = read.csv("Analysis/WaterLevel/Agha/Agha_30ftWLR_June27.csv",header=T) 
head(WLR4)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")

WLR5 = read.csv("Analysis/WaterLevel/Agha/Agha_30ft_nov14.csv",header=T) 
head(WLR5)
names(WLR5) = c("SlNo","Date_time","kPa","Temp_degC_WLR")


WLR = rbind(WLR1,WLR2)
WLR = rbind(WLR,WLR3)
WLR = rbind(WLR,WLR4)
WLR = rbind(WLR,WLR5)
WLR_Agha = WLR
WLR_Agha$River = "Agha"

names(WLR) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR)

WLR$Date_time = mdy_hm(WLR$Date_time)
tail(WLR)

WLR$kPa  / (103) * (9.81)

Agha_WLR =  ggplot(WLR[WLR$SlNo>10,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  ylab(" ")+
  xlab(" ")+
  scale_x_datetime(date_labels = "%b",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"))

head(WLR)

ggplot(WLR[WLR$Date_time>"2023-9-06",], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  scale_x_datetime(date_labels = "%b\n%d",date_breaks = "8 day")+theme_bw()

ggplot(WLR, aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+ 
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "20 day")+theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))


 
##### Sharavathi ######

WLR4 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_Apr15.csv",header=T)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR4)

WLR5 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_May1.csv",header=T)
names(WLR5) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR5)

WLR6 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_June2.csv",header=T)
names(WLR6) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR6)

WLR7 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_June26.csv",header=T)
names(WLR7) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR7)

WLR8 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_Aug3.csv",header=T)
names(WLR8) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR8)

WLR9 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ftSep20.csv", header=T)
names(WLR9) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR9)

WLR10 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_Nov_14_0.csv", header=T)
names(WLR10) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR10)

WLR11 = read.csv("Analysis/WaterLevel/Shara/Sharavathi_WLR2_30ft_Mar20_0.csv", header=T)
names(WLR11) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR11)



WLR_Shar = rbind(WLR4,WLR5)
WLR_Shar = rbind(WLR_Shar,WLR6)
WLR_Shar = rbind(WLR_Shar,WLR7)
WLR_Shar = rbind(WLR_Shar,WLR8)
WLR_Shar = rbind(WLR_Shar,WLR9)
WLR_Shar = rbind(WLR_Shar,WLR10)
WLR_Shar = rbind(WLR_Shar,WLR11)

names(WLR_Shar) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR_Shar)
tail(WLR_Shar)

WLR_Shar$Date_time = mdy_hm(WLR_Shar$Date_time)
WLR_Shar$River = "Shar"

Shar_WLR = ggplot(WLR_Shar, aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  ylab(" ")+
  scale_x_datetime(date_labels = "%b",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

ggplot(WLR_Shar, aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+ 
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "40 day")+theme_bw()+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"))

head(WLR_Shar)
#ggsave("E:/Shishir/FieldData/Results/Shar_WLR.jpg", width = 8, height = 3,scale = 2)



#### Gangavali ########

WLR1 = read.csv("Analysis/WaterLevel/Gang/Gang_WLR_13ft_May8.csv",header=T)
names(WLR1) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR1)

WLR2 = read.csv("Analysis/WaterLevel/Gang/Gang_13_ft_June16.csv",header=T)
names(WLR2) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR2)

WLR3 = read.csv("Analysis/WaterLevel/Gang/Gang_1_Aug_13.csv",header=T)
names(WLR3) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR3)

WLR4 = read.csv("Analysis/WaterLevel/Gang/Gangavali_30ftWLR_Dec5.csv",header=T)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR4)


WLR_Gang = rbind(WLR1,WLR2)
WLR_Gang = rbind(WLR_Gang,WLR3)
WLR_Gang = rbind(WLR_Gang,WLR4)

names(WLR_Gang) = c("SlNo","Date_time","kPa","Temp_degC_WLR")




WLR_Gang$Date_time = mdy_hm(WLR_Gang$Date_time)

WLR_Gang$River = "Gang"

WLR_Gang$Month = month(WLR_Gang$Date_time)

Gang_WLR = ggplot(WLR_Gang[WLR_Gang$SlNo>10,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  ylab(" ")+
  scale_x_datetime(date_labels = "%b",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"))

ggplot(WLR_Gang[WLR_Gang$Month >= 9 & WLR_Gang$Month <= 12,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+ 
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "8 day")+theme_bw()+
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=18,face="bold"))


### kali
WLR1 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR13ft_April30.csv",header=T)
names(WLR1) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR1)

WLR2 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR_13ft_June10.csv",header=T)
names(WLR2) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR2)

WLR3 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR_13ft_July10.csv",header=T)
names(WLR3) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR3)

WLR4 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR_30ft_Sep7.csv",header=T)
names(WLR4) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR4)

WLR5 = read.csv("Analysis/WaterLevel/Kali/Kali_30ft_Titanium_Mar24.csv",header=T)
names(WLR5) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR5)
tail(WLR5)

WLR6 = read.csv("Analysis/WaterLevel/Kali/Kali_30ft_Titanium_Dec12.csv",header=T)
names(WLR6) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR6)
tail(WLR6)

WLR7 = read.csv("Analysis/WaterLevel/Kali/Kali_WLR_30ft_Dec19.csv",header=T)
names(WLR7) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR7)
tail(WLR7)




WLR = rbind(WLR1,WLR2)
WLR = rbind(WLR,WLR3)
WLR = rbind(WLR,WLR4)
WLR = rbind(WLR,WLR5)
WLR = rbind(WLR,WLR6)
WLR = rbind(WLR,WLR7)

#WLR = WLR5

names(WLR) = c("SlNo","Date_time","kPa","Temp_degC_WLR")
head(WLR)

WLR$Date_time = mdy_hm(WLR$Date_time)

WLR_Kali = WLR

Kali_WLR = ggplot(WLR[WLR$SlNo>10,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+
  ylab(" ")+
  scale_x_datetime(date_labels = "%b-%y",date_breaks = "30 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"))

Kali_WLR

ggplot(WLR[WLR$SlNo>2000 ,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+ 
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "8 day")+theme_bw()+
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=14,face="bold"))

ggplot(WLR[WLR$SlNo>9000 & WLR$SlNo<13500 ,], aes(x=Date_time , y=kPa)) +
  geom_line() + 
  xlab("")+ 
  scale_x_datetime(date_labels = "%b-%d",date_breaks = "1 day")+theme_bw()+
  theme(axis.text=element_text(size=5),
        axis.title=element_text(size=14,face="bold"))


##################
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(Agha_WLR)

?sapply

WLRs <- grid.arrange(arrangeGrob(Agha_WLR + theme(legend.position="none") + ggtitle("Aghanashini (free)"),
                                 Shar_WLR + theme(legend.position="none") + ggtitle("Sharavathi (dammed)"),
                                 Gang_WLR + theme(legend.position="none") + ggtitle("Gangavali (free)"),
                                 Kali_WLR + theme(legend.position="none") + ggtitle("Kali (dammed)"),
                                          nrow=1,ncol=4),heights=c(6, 1))

WLRs <- grid.arrange(arrangeGrob(Agha_WLR + theme(legend.position="none") + ggtitle("Aghanashini (free)"),
                                 Shar_WLR + theme(legend.position="none") + ggtitle("Sharavathi (dammed)"),
                                 Gang_WLR + theme(legend.position="none") + ggtitle("Gangavali (free)"),
                                 Kali_WLR + theme(legend.position="none") + ggtitle("Kali (dammed)"),
                                 nrow=2,ncol=2),heights=c(6, 1))


plot(WLRs)


#ggsave("WLRs.jpg", WLRs, device = "jpg",path = "E:/Shishir/FieldData/Results/",
#    scale = 1, width = 10, height = 3, 
#     dpi = 300, limitsize = TRUE)



Sals <- grid.arrange(arrangeGrob(Agha_sal + theme(legend.position="none"),
                                 Shar_sal + theme(legend.position="none"),
                                 Gang_sal + theme(legend.position="none"),
                                 Kali_sal + theme(legend.position="none"),
                                 nrow=1,ncol=4),heights=c(6, 1))

#ggsave("Sals.jpg", Sals, device = "jpg",path = "E:/Shishir/FieldData/Results/",
#    scale = 1, width = 10, height = 3, 
#     dpi = 300, limitsize = TRUE)

