library(padr)

#### Aghanashini #####
nrow(Agha_depth)
nrow(Sal_Agha)

Date_time = data.frame(Date_time = seq(from = ymd_hms("2023-03-08 13:50:00"), to = ymd_hms("2023-11-14 17:00:00"),by = "5 mins"))
Agha_fig = left_join(Date_time,Agha_depth %>% select(Date_time,depth))
Agha_fig = left_join(Agha_fig,Sal_Agha %>% select(Date_time,Salinity,month))
Agha_fig = Agha_fig %>% filter(!is.na(Salinity))


Agha_fig = gather(Agha_fig,key = "parameter",value = "value",depth,Salinity)

#Agha_fig = Agha_fig[complete.cases(Agha_fig),]

ggplot(Agha_fig, aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b%-%y",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                         Water depth(m)")+
  theme(strip.background = element_blank(), strip.text = element_blank())

# Create main plot
p_main <- ggplot(Agha_fig, aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b\n%Y",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                                        Water depth(m)")+
  ggtitle("2.1 Aghanashini")+
  theme(strip.background = element_blank(), strip.text = element_blank())

# Create inset plot
p_inset1 <- ggplot(Agha_fig[Agha_fig$parameter == "depth" & Agha_fig$month == 4,], aes(x = Date_time, y = value)) + geom_line() + 
  scale_x_datetime(date_labels = "%d-%b",date_breaks = "5 days")+theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+ ggtitle("2.1a")+
  xlab("") + ylab("")+
  theme(strip.background = element_blank(), strip.text = element_blank())

# Create inset plot
p_inset2 <- ggplot(Agha_fig[Agha_fig$parameter == "Salinity" & Agha_fig$month == 4,], aes(x = Date_time, y = value)) + geom_line() + 
  scale_x_datetime(date_labels = "%d-%b",date_breaks = "5 days")+theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+ggtitle("2.1b")+
  xlab("") + ylab("")+
  theme(strip.background = element_blank(), strip.text = element_blank())

ggplot(Agha_fig[Agha_fig$month ==4,], aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b\n%Y",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                                        Water depth(m)")+
  ggtitle("2.1 Aghanashini")+
  theme(strip.background = element_blank(), strip.text = element_blank())

# Combine plots using cowplot
ggdraw() +
  draw_plot(p_main, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(p_inset1, x = 0.58, y = 0.70, width = 0.4, height = 0.24)+ # Position and size of inset
  draw_plot(p_inset2, x = 0.58, y = 0.26, width = 0.4, height = 0.24) # Position and size of inset

ggsave("E:/Shishir/FieldData/Results/Agha_Sal_Depth_v2.jpg", width = 6, height = 3.5,scale = 3)

#### Gangavali #####
head(Gang_depth)
head(Sal_Gang)

Date_time = data.frame(Date_time = seq(from = min(Gang_depth$Date_time), to = max(Gang_depth$Date_time),by = "5 mins"))
Gang_fig = left_join(Date_time,Gang_depth %>% select(Date_time,depth,month))
Gang_fig = left_join(Gang_fig,Sal_Gang %>% select(Date_time,Salinity))

Gang_fig = Gang_fig %>% filter(!(is.na(Gang_fig$depth) & is.na(Gang_fig$Salinity)))

#Gang_fig = Gang_fig %>% filter(!is.na(Salinity))

Gang_fig = gather(Gang_fig,key = "parameter",value = "value",depth,Salinity)

ggplot(Gang_fig[Gang_fig$Date_time <= ymd_hms("2023-11-14 17:00:00"),], aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b%-%y",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                         Water depth(m)")+
  theme(strip.background = element_blank(), strip.text = element_blank())


ggplot(Gang_fig[Gang_fig$Date_time <= ymd_hms("2023-11-14 17:00:00"),], aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b\n%Y",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                                        Water depth(m)")+
  ggtitle("2.2 Gangavali")+
  theme(strip.background = element_blank(), strip.text = element_blank())

ggplot(Gang_fig[Gang_fig$parameter == "depth" & Gang_fig$month == 4,], aes(x = Date_time, y = value)) + geom_line() + 
  scale_x_datetime(date_labels = "%d-%b",date_breaks = "5 days")+theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+ ggtitle("2.1a")+
  xlab("") + ylab("")+
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave("E:/Shishir/FieldData/Results/Gang_Sal_Depth_v2.jpg", width = 6, height = 3.5,scale = 3)


#### Sharavathi ####
head(Shar_depth)
head(Sal_Shar)

tail(Shar_depth)
tail(Sal_Shar)



Date_time = data.frame(Date_time = seq(from = min(Shar_depth$Date_time), to = max(Shar_depth$Date_time),by = "5 mins"))
Shar_fig = left_join(Date_time,Shar_depth %>% select(Date_time,depth))
Shar_fig = left_join(Shar_fig,Sal_Shar %>% select(Date_time,Salinity,month))

Shar_fig = Shar_fig %>% filter(!(is.na(Shar_fig$depth) & is.na(Shar_fig$Salinity)))

Shar_fig = gather(Shar_fig,key = "parameter",value = "value",depth,Salinity)
Shar_fig = Shar_fig[complete.cases(Shar_fig),]

ggplot(Shar_fig, aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b%-%d",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                         Water depth(m)")+
  theme(strip.background = element_blank(), strip.text = element_blank())

# Create main plot
ggplot(Shar_fig[Shar_fig$Date_time <= ymd_hms("2023-11-14 17:00:00"),], aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b\n%Y",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                                        Water depth(m)")+
  ggtitle("3.1 Sharavathi")+
  theme(strip.background = element_blank(), strip.text = element_blank())

ggplot(Shar_fig[Shar_fig$Date_time <= ymd_hms("2023-11-14 17:00:00") & Shar_fig$month == 7,], aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%d\n%H",date_breaks = "24 hour")+theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  xlab("Time (hours)") + ylab("Salinity (ppm)                                        Water depth(m)")+
  ggtitle("4.2 Sharavathi: July, 2023")+
  theme(strip.background = element_blank(), strip.text = element_blank())

ggsave("E:/Shishir/FieldData/Results/Shar_Sal_Depth_v3_July.jpg",  width = 6, height = 3.5,scale = 3)

### Kali ###

head(Kali_depth)
head(Kali_sal)

Kali_depth$depth[is.na(Kali_depth$depth)] = 0
Date_time = data.frame(Date_time = seq(from = min(Kali_depth$Date_time), to = max(Kali_depth$Date_time),by = "5 mins"))

Kali_fig = left_join(Date_time,Kali_depth %>% select(Date_time,depth,year,month))
Kali_fig = left_join(Kali_fig,Kali_sal %>% select(Date_time,Salinity))

Kali_fig = Kali_fig %>% filter(!(is.na(Kali_fig$depth) & is.na(Kali_fig$Salinity)))

Kali_fig = gather(Kali_fig,key = "parameter",value = "value",depth,Salinity)
Kali_fig$value[Kali_fig$parameter == "depth" & Kali_fig$value == 0] = NA

Kali_fig$value[Kali_fig$parameter == "depth" ]  = Kali_fig$value[Kali_fig$parameter == "depth" ]

ggplot(Kali_fig[Kali_fig$month ==6 & Kali_fig$year == 2024,], aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b%-%d",date_breaks = "30 days")+theme_bw() +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                         Water depth(m)")+
  theme(strip.background = element_blank(), strip.text = element_blank())

p_main <- ggplot(Kali_fig[Kali_fig$year == 2024,],aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%b\n%Y",date_breaks = "30 days",
                   limits = c(ymd_hms("2024-01-01 00:00:00"),ymd_hms("2024-11-20 00:00:00")))+
                     theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                                        Water depth(m)")+
  ggtitle("3.2 Kali")+
  theme(strip.background = element_blank(), strip.text = element_blank())

# Create inset plot
p_inset1 <- ggplot(Kali_fig[Kali_fig$parameter == "depth" &  Kali_fig$year == 2024 &
                              Kali_fig$Date_time >= ymd_hms("2024-05-20 00:00:00") &
                              Kali_fig$Date_time <= ymd_hms("2024-06-13 00:00:00"),],
                   aes(x = Date_time, y = value)) + geom_line() + 
  scale_x_datetime(date_labels = "%d-%b",date_breaks = "5 days")+theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+ ggtitle("3.2a")+
  xlab("") + ylab("")+
  theme(strip.background = element_blank(), strip.text = element_blank())

# Create inset plot
p_inset2 <- ggplot(Kali_fig[Kali_fig$parameter == "Salinity" &   Kali_fig$year == 2024 &
                              Kali_fig$Date_time >= ymd_hms("2024-05-20 00:00:00") &
                              Kali_fig$Date_time <= ymd_hms("2024-06-13 00:00:00"),],
                   aes(x = Date_time, y = value)) + geom_line() + 
  scale_x_datetime(date_labels = "%d-%b",date_breaks = "5 days")+theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+ggtitle("3.2b")+
  xlab("") + ylab("")+
  theme(strip.background = element_blank(), strip.text = element_blank())

p_inset3 <- ggplot(Kali_fig[Kali_fig$parameter == "depth" &   Kali_fig$year == 2024 &
                              Kali_fig$Date_time >= ymd_hms("2024-09-16 00:00:00") &
                              Kali_fig$Date_time <= ymd_hms("2024-10-01 00:00:00"),],
                   aes(x = Date_time, y = value)) + geom_line() + 
  scale_x_datetime(date_labels = "%d-%b",date_breaks = "5 days")+theme_bw() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 20, face = "bold"))+ggtitle("3.2c")+
  xlab("") + ylab("")+
  theme(strip.background = element_blank(), strip.text = element_blank())

ggplot(Kali_fig[Kali_fig$year == 2024 & Kali_fig$month == 10,],aes(x = Date_time, y = value)) + geom_line() + 
  facet_grid(parameter ~ ., scales = "free_y") + theme(legend.position = "none")+
  scale_x_datetime(date_labels = "%d",date_breaks = "1 day")+
  theme_bw() +
  theme(axis.text=element_text(size=18),
        axis.title=element_text(size=20,face="bold"),
        plot.title = element_text(size = 25, face = "bold"))+
  xlab("Date") + ylab("Salinity (ppm)                                        Water depth(m)")+
  ggtitle("3.2 Kali")+
  theme(strip.background = element_blank(), strip.text = element_blank())


# Combine plots using cowplot
ggdraw() +
  draw_plot(p_main, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(p_inset1, x = 0.05, y = 0.74, width = 0.33, height = 0.21)+ # Position and size of inset
  draw_plot(p_inset2, x = 0.05, y = 0.30, width = 0.33, height = 0.21)+ # Position and size of inset
  draw_plot(p_inset3, x = 0.74, y = 0.74, width = 0.24, height = 0.21) # Position and size of inset
  


ggsave("E:/Shishir/FieldData/Results/Kali_Sal_Depth_v2.jpg",  width = 6, height = 3.5,scale = 3)
