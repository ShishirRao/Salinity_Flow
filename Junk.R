# row no. 24 and 25
ssc$SSC..mg.l.[ssc$Filter.ID == 24] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 24] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 25]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 24] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 25])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 24]
# delete row 25
ssc = ssc[ssc$Filter.ID != 25,]

ssc$SSC..mg.l.[ssc$Filter.ID == 145] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 145] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 158]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 145] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 158])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 145]
# delete row 158
ssc = ssc[ssc$Filter.ID != 158,]

ssc$SSC..mg.l.[ssc$Filter.ID == 171] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 171] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 172]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 171] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 172])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 171]
# delete row 158
ssc = ssc[ssc$Filter.ID != 172,]

ssc$SSC..mg.l.[ssc$Filter.ID == 173] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 173] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 174]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 173] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 174])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 173]
# delete row 158
ssc = ssc[ssc$Filter.ID != 174,]

ssc$SSC..mg.l.[ssc$Filter.ID == 176] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 176] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 177]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 176] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 177])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 176]
# delete row 158
ssc = ssc[ssc$Filter.ID != 177,]

ssc$SSC..mg.l.[ssc$Filter.ID == 185] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 185] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 186]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 185] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 186])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 185]
# delete row 158
ssc = ssc[ssc$Filter.ID != 186,]

ssc$SSC..mg.l.[ssc$Filter.ID == 192] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 192] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 193]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 192] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 193])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 192]
# delete row 158
ssc = ssc[ssc$Filter.ID != 193,]

ssc$SSC..mg.l.[ssc$Filter.ID == 200] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 200] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 201]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 200] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 201])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 200]
# delete row 158
ssc = ssc[ssc$Filter.ID != 201,]

ssc$SSC..mg.l.[ssc$Filter.ID == 206] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 206] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 207]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 206] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 207])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 206]
# delete row 158
ssc = ssc[ssc$Filter.ID != 207,]

ssc$SSC..mg.l.[ssc$Filter.ID == 213] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 213] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 214]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 213] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 214])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 213]
# delete row 158
ssc = ssc[ssc$Filter.ID != 214,]

ssc$SSC..mg.l.[ssc$Filter.ID == 217] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 217] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 218]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 217] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 218])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 217]
# delete row 158
ssc = ssc[ssc$Filter.ID != 218,]

ssc$SSC..mg.l.[ssc$Filter.ID == 224] = 
  (ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 224] + ssc$Filter.weight.after.filtration..gm.[ssc$Filter.ID == 225]-
     ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 224] - ssc$Filter.weight.before.filtration..gm.[ssc$Filter.ID == 225])*1000/ssc$Volume.of.water..liters.[ssc$Filter.ID == 224]
# delete row 158
ssc = ssc[ssc$Filter.ID != 225,]



# convert to date format
ssc$Sampling.Date = parse_date_time(ssc$Sampling.Date,"d-b-y")
ssc$Sampling.Date = as.Date(ssc$Sampling.Date)
class(ssc$Sampling.Date)


##### Aghanashini #####

#These are the dates on which Agha_water_wet1 site has data compared to sampling site
wet1dates = pix$ImgDates[pix$site == "Agha_water_wet1"][which(!(unique(pix$ImgDates[pix$site == "Agha_water_wet1"]) %in% unique(pix$ImgDates[pix$site == "Agha_water_dry"])) == TRUE)]
wet2dates = pix$ImgDates[pix$site == "Agha_water_wet2"][which(!(unique(pix$ImgDates[pix$site == "Agha_water_wet2"]) %in% unique(pix$ImgDates[pix$site == "Agha_water_dry"])) == TRUE)]





#### Gangavali #####



#These are the dates on which Agha_water_wet1 site has data compared to sampling site
wet1dates = pix$ImgDates[pix$site == "Gang_water_wet1"][which(!(unique(pix$ImgDates[pix$site == "Gang_water_wet1"]) %in% unique(pix$ImgDates[pix$site == "Gang_water_dry"])) == TRUE)]
wet2dates = pix$ImgDates[pix$site == "Gang_water_wet2"][which(!(unique(pix$ImgDates[pix$site == "Gang_water_wet2"]) %in% unique(pix$ImgDates[pix$site == "Gang_water_dry"])) == TRUE)]

pix_wide = pix %>% filter(River == "Gang") %>% spread(site,AvgRed)


names(ssc)
## Now get ssc values and compute the median SSC for each sampling date
ssc_mean =   ssc %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth","River")) 
ssc_mean = ssc_mean %>% group_by(Sampling.Date,ScheduledDates,River) %>% mutate(ssc_mean = mean(SSC..mg.l.))
ssc_mean = ssc_mean %>% select(-SSC..mg.l.) %>% distinct()
# this is likely a mistake in the date
#ssc_mean$ScheduledDates[ssc_mean$River == "Gangavali" & ssc_mean$ScheduledDates == ymd("2023-07-12")] = ymd("2023-07-28")


names(ssc_mean)
unique(ssc_mean$River)
ssc_mean$logssc = log(ssc_mean$ssc_mean)
ssc_mean = ssc_mean %>% filter(River == "Gangavali")

levels(ssc_mean$River)[levels(ssc_mean$River) == "Gangavali"] <- "Gang"


# Use mostly Agha_water_dry for dry season
pix_wide$Reflect = pix_wide$Gang_water_dry
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-25")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-05-25")]
#pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-20")] = pix_wide$Gang_water_wet2[pix_wide$ImgDates == ymd("2023-07-20")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-07-28")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-07-28")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-13")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-13")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-08-21")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-09-06")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-09-06")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-11-01")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-11-01")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-03")] = pix_wide$Gang_water_wet1[pix_wide$ImgDates == ymd("2023-12-03")]


pix_wide = pix_wide %>% select(-c(Gang_water_dry,Gang_water_wet1,Gang_water_wet2))

ggplot(pix_wide,aes(y = Reflect, x = ImgDates))+geom_point()+
  geom_smooth() + ggtitle("all pixels_wet+dry season")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "8 day")+theme_bw()+
  theme(axis.text=element_text(size=4),
        axis.title=element_text(size=14,face="bold"))


refl = left_join(LandsatDates,pix_wide)

by <- join_by(River,ImgDates == ScheduledDates)
refl = left_join(refl,ssc_mean,by)
refl = refl[complete.cases(refl),]

plot((refl$ssc_mean)~refl$Reflect)


plot(log(refl$ssc_mean)~refl$Reflect)
Gang_lm = lm(log(refl$ssc_mean)~refl$Reflect)

abline(Gang_lm)




#### Kali ##### 


pix_wide = pix %>% filter(River == "Kali") %>% spread(site,AvgRed)


#compute the median SSC for each sampling date
## Now get ssc values and compute the median SSC for each sampling date
ssc_mean =   ssc %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth","River")) 
ssc_mean = ssc_mean %>% group_by(Sampling.Date,ScheduledDates,River) %>% mutate(ssc_mean = mean(SSC..mg.l.))
ssc_mean = ssc_mean %>% select(-SSC..mg.l.) %>% distinct()

ssc_mean$logssc = log(ssc_mean$ssc_mean)
ssc_mean = ssc_mean %>% filter(River == "Kali")

# Use mostly Agha_water_dry for dry season
pix_wide$Reflect = pix_wide$Kali_water_dry
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21")] = pix_wide$Kali_water_wet1[pix_wide$ImgDates == ymd("2023-08-21")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-19")] = pix_wide$Kali_water_wet1[pix_wide$ImgDates == ymd("2023-12-19")]
pix_wide = pix_wide %>% select(-c(Kali_water_dry,Kali_water_wet1))

refl = left_join(LandsatDates,pix_wide)
ggplot(refl,aes(y = Reflect, x = ImgDates))+geom_point()+
  geom_smooth() + ggtitle("all pixels_wet+dry season")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

by <- join_by(River,ImgDates == ScheduledDates)
refl = left_join(refl,ssc_mean,by)
refl = refl[complete.cases(refl),]

plot((refl$ssc_mean)~refl$Reflect)


plot(log(refl$ssc_mean)~refl$Reflect)
Kali_lm = lm(log(refl$ssc_mean)~refl$Reflect)

abline(Kali_lm)


##### Sharavathi #####

pix_wide = pix %>% filter(River == "Shar") %>% spread(site,AvgRed)


#compute the median SSC for each sampling date
## Now get ssc values and compute the median SSC for each sampling date
ssc_mean =   ssc %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth","River")) 
ssc_mean = ssc_mean %>% group_by(Sampling.Date,ScheduledDates,River) %>% mutate(ssc_mean = mean(SSC..mg.l.))
ssc_mean = ssc_mean %>% select(-SSC..mg.l.) %>% distinct()
ssc_mean$logssc = log(ssc_mean$ssc_mean)
ssc_mean = ssc_mean %>% filter(River == "Sharavathi")
levels(ssc_mean$River)[levels(ssc_mean$River) == "Sharavathi"] <- "Shar"


# Use mostly Agha_water_dry for dry season
pix_wide$Reflect = pix_wide$Shar_water_dry
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-04-15")] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-04-15")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-05-09")] = pix_wide$Shar_water_dry2[pix_wide$ImgDates == ymd("2023-05-09")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-06-10")] = pix_wide$Shar_water_dry3[pix_wide$ImgDates == ymd("2023-06-10")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-05")] = pix_wide$Shar_water_wet1[pix_wide$ImgDates == ymd("2023-08-05")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-08-21")] = pix_wide$Shar_water_dry2[pix_wide$ImgDates == ymd("2023-08-21")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-09-06")] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-09-06")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-11-25")] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-11-25")]
pix_wide$Reflect[pix_wide$ImgDates == ymd("2023-12-11")] = pix_wide$Shar_water_dry1[pix_wide$ImgDates == ymd("2023-12-11")]

pix_wide = pix_wide %>% select(-c(Shar_water_dry,Shar_water_dry1,Shar_water_dry2,Shar_water_dry3,Shar_water_wet1))

refl = left_join(LandsatDates,pix_wide)
ggplot(refl,aes(y = Reflect, x = ImgDates))+geom_point()+
  geom_smooth() + ggtitle("all pixels_wet+dry season")+
  scale_x_date(date_labels = "%b-%d",date_breaks = "16 day")+theme_bw()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

by <- join_by(River,ImgDates == ScheduledDates)
refl = left_join(refl,ssc_mean,by)
refl = refl[complete.cases(refl),]

refl %>% filter(Reflect > 0.08)


plot((refl$ssc_mean)~refl$Reflect)


plot(log(refl$ssc_mean)~refl$Reflect)
Shar_lm = lm(log(refl$ssc_mean)~refl$Reflect)

abline(Shar_lm)



# Get lm coefficients for each river. 
model = refl_long %>% nest_by(River)

model = refl_long %>% filter(Band == "AvgRed") %>% group_by(River) %>%
  mutate(mod = list(lm(logssc ~ Reflect)))

reframe(tidy(model$mod))

unique(model$mod)

list_of_dfs <- split(refl_long, refl_long$River)

?nest_by


##

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



df<-refl_long %>% filter(Band == "AvgRed") %>% group_by(River) %>% nest()

my_lm <- function(df) {
  fit = lm(logssc ~ Reflect, data = df) %>% broom::tidy()
  AdjR2 = signif(summary(fit)$adj.r.squared,5) %>% broom::tidy()
  return(AdjR2)
}

?paste

hmm = df %>% mutate(coefs = map(data, my_lm)) %>% 
  unnest(coefs) 



df %>% map(data,my_lm)




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
summary(fit1)$ad
ggplotRegression(fit1)



y <- c(25, 14, 68, 79, 64, 139, 49, 119, 111)
A <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3))
X <- c(1, 14, 22, 2, 9, 20, 2, 13, 22)
plot(X, y, col = c(rep("red", 3), rep("blue", 3), rep("green", 3)),xlim = c(1, 25), ylim = c(0, 140))
summary(fm <- lm(y ~ A-1 + X))
model.matrix(~A-1+X)


date = "2018-05-03"
unique(temp$site[temp$ImgDates == ymd(date)])
temp1 = (temp[temp$ImgDates == ymd(date),])

unique(pix_back$site[pix_back$ImgDates == ymd(date)])
temp2 = (pix_back[pix_back$ImgDates == ymd(date),])

#boxplot
ggplot(refl_long_back %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+
  geom_boxplot(aes(group = year)) + facet_wrap(.~River,ncol=1,nrow=4)+
  ggtitle("Reflectance")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))

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





library(BiodiversityR)
library(plyr)
library(ggplot2)

#detach("package:Rcmdr")

#BiodiversityRGUI()
library(rpart)
library(ggplot2)

library(tidyr)
library(reshape2)
library(dplyr)

library(tree)
library(rpart)
library(rattle)
library(rpart.plot)

######################################################################################



## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm),
                     max = max   (xx[[col]], na.rm=na.rm),
                     min = min   (xx[[col]], na.rm=na.rm),
                     range = max(xx[[col]], na.rm=na.rm) - min(xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


ssc_mean$season = "wet"
ssc_mean$season[ssc_mean$SamplingMonth == "Apr" | 
                  ssc_mean$SamplingMonth == "Mar" |
                  ssc_mean$SamplingMonth == "May" |
                  ssc_mean$SamplingMonth == "Feb"] = "Dry" 

ssc_mean$season[ssc_mean$SamplingMonth == "Jan" | 
                  ssc_mean$SamplingMonth == "Oct" |
                  ssc_mean$SamplingMonth == "Nov" |
                  ssc_mean$SamplingMonth == "Dec"] = "Post-monsoon" 

ssc_mean_season = summarySE(ssc_mean,measurevar = "ssc_mean",groupvars = c("River","season"),na.rm = TRUE)
ssc_mean_season$ssc_mean = round(ssc_mean_season$ssc_mean,2)
ssc_mean_season$se = round(ssc_mean_season$se,2)
names(ssc_mean_season)
ssc_mean_season = ssc_mean_season %>% select("River","Season","N","ssc_mean","se")





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

LandsatDates = data.frame("Date_interval_start" = seq(ymd("1989-01-01"),ymd("2023-12-27"), by = "1 days"))
names(LandsatDates)
head(LandsatDates)
names(pix_back)
class(pix_back$ImgDates)
class(LandsatDates$ImgDates)


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
#pix_back_red$Gang_water_wet2 = NA
AvgRed = SiteCombine(pix_back_red,"Yes") %>% rename(AvgRed = Reflect)
refl_long_back_allSites = gather(AvgRed,key = "Band",value = "Reflect",AvgRed)
names(refl_long_back_allSites)

ggplot(refl_long_back_allSites %>% filter(Band == "AvgRed") ,aes(y = Reflect, x = ImgDates))+geom_point(aes(group = River,col = River))+
  geom_smooth(aes(group = River,col = River),span = .5) + ggtitle("Red Reflectance from 1988 - 2023 from all sites")+ xlab("Imagery date") + ylab("Red Reflectance")+
  scale_x_date(date_labels = "%y",date_breaks = "365 day")+theme_bw()+
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=14,face="bold"))



data(mtcars)
# Compare mpg between automatic and manual cars
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))

p <- ggplot(mtcars, aes(x = am, y = mpg, fill = am)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(x = "Transmission Type", y = "Miles per Gallon (mpg)",
       title = "Comparison of MPG by Transmission Type")
p

library(ggpubr)

p + stat_compare_means(method = "t.test", label = "p.format") +
  theme(legend.position = "none")

ttest <- t.test(mpg ~ am, data = mtcars)
ttest


annot_text <- paste0("t = ", round(ttest$statistic, 2),
                     ", df = ", round(ttest$parameter, 0),
                     ", p = ", signif(ttest$p.value, 3))

p + annotate("text", x = 1.5, y = max(mtcars$mpg) + 2, label = annot_text) +
  stat_compare_means(method = "t.test", label = "p.format")



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


grouped_quantiles <- refl_long_back %>% filter(ImgDates >= ymd("2012-01-01")) %>%
  group_by(River) %>% 
  summarize( Q10 = quantile(logSSC, probs = quantiles_to_calculate[1],na.rm=TRUE),
             Q25 = quantile(logSSC, probs = quantiles_to_calculate[2],na.rm=TRUE),
             Q50 = quantile(logSSC, probs = quantiles_to_calculate[3],na.rm=TRUE),
             Q75 = quantile(logSSC, probs = quantiles_to_calculate[4],na.rm=TRUE),
             Q90 = quantile(logSSC, probs = quantiles_to_calculate[5],na.rm=TRUE))



max(refl_long_back$Reflect[refl_long_back$River == "Shar" & refl_long_back$ImgDates>= ymd("2012-01-01")],na.rm = TRUE)

grouped_quantiles$Q25[1]

hline_10 = data.frame(River=c("Agha", "Shar", "Gang", "Kali"),
                      threshold=c(grouped_quantiles$Q10[1],
                                  grouped_quantiles$Q10[2],
                                  grouped_quantiles$Q10[3],
                                  grouped_quantiles$Q10[4]))


hline_25 = data.frame(River=c("Agha", "Shar", "Gang", "Kali"),
                      threshold=c(grouped_quantiles$Q25[1],
                                  grouped_quantiles$Q25[2],
                                  grouped_quantiles$Q25[3],
                                  grouped_quantiles$Q25[4]))

hline_50 = data.frame(River=c("Agha", "Shar", "Gang", "Kali"),
                      threshold=c(grouped_quantiles$Q50[1],
                                  grouped_quantiles$Q50[2],
                                  grouped_quantiles$Q50[3],
                                  grouped_quantiles$Q50[4]))

hline_75 = data.frame(River=c("Agha", "Shar", "Gang", "Kali"),
                      threshold=c(grouped_quantiles$Q75[1],
                                  grouped_quantiles$Q75[2],
                                  grouped_quantiles$Q75[3],
                                  grouped_quantiles$Q75[4]))

hline_90 = data.frame(River=c("Agha", "Shar", "Gang", "Kali"),
                      threshold=c(grouped_quantiles$Q90[1],
                                  grouped_quantiles$Q90[2],
                                  grouped_quantiles$Q90[3],
                                  grouped_quantiles$Q90[4]))


ggplot(refl_long_back %>% filter(ImgDates >= ymd("2012-01-01")) ,aes(y = logSSC, x = day))+ geom_point(aes(col = year))+
  #geom_smooth(aes(col = year),span = 0.8)+
  facet_wrap(.~River,nrow = 2, ncol = 2,scales = "free")+ theme_bw()+
  ylab("log (SSC)") + xlab("Day of the year")+
  geom_hline(data=hline_25, aes(yintercept=threshold), colour="salmon") +
  geom_hline(data=hline_75, aes(yintercept=threshold), colour="green") +
  # geom_hline(data=hline_10, aes(yintercept=threshold), colour="blue") +
  # geom_hline(data=hline_90, aes(yintercept=threshold), colour="red")+
  geom_hline(data=hline_50, aes(yintercept=threshold), colour="black")



outliers <- refl_long_back %>% filter(ImgDates >= ymd("2012-01-01")) %>%
  group_by(River) %>%
  dplyr::summarise(
    n_75th_percentile = quantile(logSSC, probs = 0.75, na.rm = TRUE),
    n_25th_percentile = quantile(logSSC, probs = 0.25, na.rm = TRUE),
    n_50th_percentile = quantile(logSSC, probs = 0.50, na.rm = TRUE),
    #count_exceeding_90th = sum(logSSC > n_90th_percentile,na.rm = TRUE),
    CD = (n_75th_percentile - n_25th_percentile)/n_50th_percentile
  )



hum_names <- as_labeller(
  c("Agha" = paste("Agha: ","AdjR2 =",River_Adjrsq[1],",Intercept = ",River_intercept[1],"Slope = ",River_slope[1],"p.val = ",River_pval[1]), 
    "Gang" = paste("Gang: ","AdjR2 =",River_Adjrsq[2],",Intercept = ",River_intercept[2],"Slope = ",River_slope[2],"p.val = ",River_pval[2]), 
    "Kali" = paste("Kali: ","AdjR2 =",River_Adjrsq[3],",Intercept = ",River_intercept[3],"Slope = ",River_slope[3],"p.val = ",River_pval[3]),  
    "Shar" = paste("Shar: ","AdjR2 =",River_Adjrsq[4],",Intercept = ",River_intercept[4],"Slope = ",River_slope[4],"p.val = ",River_pval[4]))) 
