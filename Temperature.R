library(padr)
library(cowplot)

#### Aghanashini #####
nrow(Agha_depth)
nrow(Sal_Agha)

Date_time = data.frame(Date_time = seq(from = ymd_hms("2023-03-08 13:50:00"), to = ymd_hms("2023-11-14 17:00:00"),by = "5 mins"))
Agha_depth$Temp_degC_WLR = as.numeric(Agha_depth$Temp_degC_WLR)
Agha_fig = left_join(Date_time,Agha_depth %>% select(Date_time,depth,Temp_degC_WLR))
Sal_Agha$Temp_degC_Cond = as.numeric(Sal_Agha$Temp_degC_Cond)
Agha_fig = left_join(Agha_fig,Sal_Agha %>% select(Date_time,Salinity,month,Temp_degC_Cond))
Agha_fig = Agha_fig %>% filter(!is.na(Salinity))


Agha_fig = gather(Agha_fig,key = "parameter",value = "value",depth,Temp_degC_WLR,Salinity,Temp_degC_Cond )




ggplot(Agha_fig, aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b%-%y",date_breaks = "30 days")+theme_bw()
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                         Water depth(m)")+
  theme(strip.background = element_blank(), strip.text = element_blank())

  
  
  Date_time = data.frame(Date_time = seq(from = min(Shar_depth$Date_time), to = max(Shar_depth$Date_time),by = "5 mins"))
  Shar_fig = left_join(Date_time,Shar_depth %>% select(Date_time,depth))
  Shar_fig = left_join(Shar_fig,Sal_Shar %>% select(Date_time,Salinity,month,Temp_degC_Cond))
  
  Shar_fig = Shar_fig %>% filter(!(is.na(Shar_fig$depth) & is.na(Shar_fig$Salinity)))
  
  Shar_fig = gather(Shar_fig,key = "parameter",value = "value",depth,Salinity,Temp_degC_Cond)
  Shar_fig = Shar_fig[complete.cases(Shar_fig),]
  
  ggplot(Shar_fig [Shar_fig$month == 9,], aes(x = Date_time, y = value)) + geom_line() + 
    facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")
  scale_x_datetime(date_labels = "%b%-%d",date_breaks = "30 days")+theme_bw() +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=16,face="bold"),
          plot.title = element_text(size = 20, face = "bold"))+
    xlab("Date") + ylab("Salinity (ppm)                         Water depth(m)")+
    theme(strip.background = element_blank(), strip.text = element_blank())