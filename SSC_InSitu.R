library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(stringr)
library(data.table)
library(stringi)


setwd("E:/Shishir/FieldData/SSC Lab/")

#ssc = read.csv("SSC data_V1.csv",header=T)
#ssc = read.csv("SSC data_V2.csv",header=T)
#ssc = read.csv("SSC data_V3.csv",header=T)
#ssc = read.csv("SSC data_V4.csv",header=T) # this is full data after completing data entry
#ssc = read.csv("SSC data_V5.csv",header=T) # Volume was wrong for filter ID 145. Corrected it here
ssc = read.csv("SSC data_V6.csv",header=T) # Volume was wrong for filter ID 145. Corrected it here

unique(ssc$River)


#####calcualte SSC ######

#Ignore that this is already done in excel. Just overwrite

ssc$SSC..mg.l. = as.numeric((ssc$Filter.weight.after.filtration..gm.-ssc$Filter.weight.before.filtration..gm.)*1000/ssc$Volume.of.water..liters.)
NAs = ssc[which(is.na(ssc$SSC..mg.l.) == TRUE),]

# Read comments where "filter change" is mentioned
ssc$Note[grep("change", ssc$Note)]
ssc$Filter.ID[grep("change", ssc$Note)]

#convert samples which took two filters in to a single row

#create a new row which contains information on filter ID of the row that took two filters
ssc$filterChangeID = NA
ssc$filterChangeID[ssc$Filter.ID[grep("change", ssc$Note)]] = 
  as.numeric((stri_sub(ssc$Note[grep("change", ssc$Note)],from = 18,to = 21)))

FilterIDs = ssc$Filter.ID[grep("change", ssc$Note)] 
FilterChangeIDs = ssc$filterChangeID[ssc$Filter.ID[grep("change", ssc$Note)]] 

#calculate SSC for samples that took two filters
for(i in 1:length(FilterIDs)){
  #ignore rows where after filtration weight is missing.
  if (!(is.na(ssc$Filter.weight.after.filtration..gm.[FilterIDs[i]]) | is.na(ssc$Filter.weight.after.filtration..gm.[FilterChangeIDs[i]]))){
    # ssc = ((after1 + after2) - (before1 + before2))*1000 / volume of the water sample
    ssc$SSC..mg.l.[FilterChangeIDs[i]] = (ssc$Filter.weight.after.filtration..gm.[FilterChangeIDs[i]] + 
                                         ssc$Filter.weight.after.filtration..gm.[FilterIDs[i]]-
       ssc$Filter.weight.before.filtration..gm.[FilterChangeIDs[i]] - 
       ssc$Filter.weight.before.filtration..gm.[FilterIDs[i]])*1000/ssc$Volume.of.water..liters.[FilterChangeIDs[i]]
  }
  # 
}

ssc$SSC..mg.l. = round(ssc$SSC..mg.l.,3)

#now remove rows where the second filter was used
ssc = ssc[which(is.na(ssc$SSC..mg.l.) == FALSE),]


#process turbidity by taking mean of 3 / 4 samples 
ssc$Turbidity =ifelse(is.na(ssc$Turbidity.4),
                      (ssc$Turbidity.1 + ssc$Turbidity.2 + ssc$Turbidity.3)/3,
                      (ssc$Turbidity.1 + ssc$Turbidity.2 + ssc$Turbidity.3 + ssc$Turbidity.4)/4)

## removes columns that are no longer useful
names(ssc)
ssc = ssc %>% select(-c("Turbidity.1","Turbidity.2","Turbidity.3","Turbidity.4",
                        "Volume.of.water..liters.", "Filter.weight.before.filtration..gm.",
                        "Filter.weight.after.filtration..gm.","filterChangeID"))


#######next look at rows where either the date was confusing -- remove them or correct them based on notes, 
####### or where there were issues with sampling methods or with filtration#####

#In Kali, sampling date is confusing for two sets of samples. The date is just called July. 
# Put an approx date based on sampling schedule and the SSC value
# In July, there are 4 sampling dates July 4, 12, 20 and 28. The bottles say that were
# sampled on Thursday. 20th falls on Thursday. not sure the day can be relied upon
# 134, 192 and 205 are higher values compared to 147, 150 and 187. 
# the low value SSC must be early July because the range of SSC is comparable to June values. So this must be 4th July
# Assigning the high value to 28th because there was a reservoir release around this period
ssc[ssc$Sampling.Date == "July" & ssc$River == "Kali",]

ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 134] = "04-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 192] = "04-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 205] = "04-July-23"

ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 147] = "28-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 150] = "28-July-23"
ssc$Sampling.Date[ssc$Sampling.Date == "July" & ssc$Filter.ID == 187] = "28-July-23"


# Notes say that samples from 2023-10-16 and 2023-11-01 from Aghanashini could have been interchanged
# because reflectance for 2023-11-01 is high. Moving higher SSC samples from 2023-10-16 to 2023-11-01 and
# lower SSC samples from 2023-11-01 to 2023-10-16

ssc$Sampling.Date = dmy(ssc$Sampling.Date) 
# get month
ssc$SamplingMonth = lubridate::month(ssc$Sampling.Date,label = TRUE,abbr = TRUE)

ssc[ssc$SamplingMonth == "Nov" & ssc$River == "Agha",]
ssc[ssc$Sampling.Date == ymd("2023-10-16") & ssc$River == "Agha",]
ssc[ssc$Sampling.Date == ymd("2023-11-01") & ssc$River == "Agha",]

#swapping 142 and 216 from 2023-10-16 with 153 and 210 from 2023-11-01 

temp = ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-10-16") & ssc$River == "Agha" & ssc$Filter.ID == 142]
ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-10-16") & ssc$River == "Agha" & ssc$Filter.ID == 142] = 
  ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-11-01") & ssc$River == "Agha" & ssc$Filter.ID == 153] 
ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-11-01") & ssc$River == "Agha" & ssc$Filter.ID == 153]  = temp

temp = ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-10-16") & ssc$River == "Agha" & ssc$Filter.ID == 216]
ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-10-16") & ssc$River == "Agha" & ssc$Filter.ID == 216] = 
  ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-11-01") & ssc$River == "Agha" & ssc$Filter.ID == 210] 
ssc$SSC..mg.l.[ssc$Sampling.Date == ymd("2023-11-01") & ssc$River == "Agha" & ssc$Filter.ID == 210]  = temp


# 07-24 was not a scehduled sampling date. It must be either 07-20 or 07-28. Assigning it to 07-28 because
# the SSC is high and reflectance is high as well
ssc$Sampling.Date[ssc$Sampling.Date == ymd("2023-07-24") & ssc$River == "Agha"] = rep(ymd("2023-07-28"),4)

# 07-20 has low SSC but WLR shows a maximum peak and reflectance is very high as well. 
# 07-11 was not a scheduled sampling date but has high SSC even WLR doesn't show a peak. 
# Likely the two samples got exchanged.
ssc$Sampling.Date[ssc$Sampling.Date == ymd("2023-07-20") & ssc$River == "Gangavali"] = rep(ymd("2023-07-12"),4)
ssc$Sampling.Date[ssc$Sampling.Date == ymd("2023-07-11") & ssc$River == "Gangavali"] = rep(ymd("2023-07-20"),4)



#####now let us address rows where sample size is more than 4 because at no sampling site, more than 4 replicates were collected###
#calculate a column which tells how many samples were collected. If there are replicates, then problematic rows can be discarded
ssc = ssc %>% dplyr::group_by(Sampling.Date,River) %>% dplyr::mutate(Samplesize = n())

high = ssc[ssc$Samplesize>4,]

#on 6-10, Kali has 5 samples. One of the comments says the date was 02 but was struck down and re written as 10. 
# but 6-2 Kali has only 3 samples. So, the missing one must be the struck down one. Filter ID 148
ssc$Sampling.Date[ssc$River == "Kali" & ssc$Filter.ID == 148] = ymd("2023-06-02")

#Next is Gangavali sample dated 2023-10-08. Clearly three values are extremely high SSC and there are low
#In October, sampling dates are 10-8, 10-16 and 10-24. 
#The higher values must be 10-16, because samples dated 10-24 are already present

ssc[ssc$River == "Gangavali" & ssc$Sampling.Date == ymd("2023-10-24"),]

ssc$Sampling.Date[ssc$River == "Gangavali" & ssc$Filter.ID == 273] = ymd("2023-10-16")
ssc$Sampling.Date[ssc$River == "Gangavali" & ssc$Filter.ID == 275] = ymd("2023-10-16")
ssc$Sampling.Date[ssc$River == "Gangavali" & ssc$Filter.ID == 322] = ymd("2023-10-16")

ssc = ssc %>% dplyr::group_by(Sampling.Date,River) %>% dplyr::mutate(Samplesize = n())
high = ssc[ssc$Samplesize>4,]




#no more high values where sample size > 4. 


# Look at all the confusing samples based on the notes
issue = ssc[which((ssc$Note) != ""),]


# ssc = ssc[-grep("confusing", ssc$Note,ignore.case = TRUE),]
# ssc = ssc[-grep("ants", ssc$Note,ignore.case = TRUE),]
# ssc = ssc[-grep("bottle", ssc$Note,ignore.case = TRUE),]
# ssc = ssc[-grep("estimate", ssc$Note,ignore.case = TRUE),]
# ssc = ssc[-grep("unknown", ssc$Note,ignore.case = TRUE),]
# ssc = ssc[-grep("confusion", ssc$Note,ignore.case = TRUE),]
# ssc = ssc[-grep("vessel", ssc$Note,ignore.case = TRUE),]
# ssc = ssc[-grep("forgot", ssc$Note,ignore.case = TRUE),]

issue = ssc[which((ssc$Note) != ""),] # these remaining ones are just comments. 



#create timeseries of sampling dates
ScheduledDates = seq(as.Date('2023-03-30'),as.Date('2023-12-30'),by='8 days')
Schedule = data.frame(ScheduledDates = ScheduledDates, ID = seq(1:length(ScheduledDates)))

#join based on closes sampling data and imagery date
setDT(ssc)[, join_date := Sampling.Date]
setDT(Schedule)[, join_date := ScheduledDates]
ssc = Schedule[ssc, on = .(join_date), roll = "nearest"]

ssc$River = as.factor(ssc$River)
# 
# ggplot(ssc,aes(y = SSC..mg.l., x = Sampling.Date))+geom_point(aes(group = River,col = River))+
#   geom_smooth(aes(group = River,col = River,method = "loess")) +
#   scale_x_date(date_labels = "%b-%d",date_breaks = "30 day")+theme_bw()+
#   theme(axis.text=element_text(size=12),
#         axis.title=element_text(size=14,face="bold"))


#ssc_onlyCleandata 
  
  ggplot(ssc,aes(y = log(SSC..mg.l.), x = Sampling.Date))+geom_point(aes(group = River,col = River),size = 1.9)+
  geom_smooth(aes(group = River,col = River,method = "auto"),span = 0.4,linewidth = 2) +
  xlab("Date")+ylab("log(SSC (mg/L)) ")+ ggtitle("In-situ SSC")+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "30 day")+theme_bw()+
    theme(axis.text=element_text(size=18),
          axis.title=element_text(size=20,face="bold"),
          plot.title = element_text(size = 25, face = "bold"))+
  theme(legend.position="bottom", legend.text = element_text(size = 20),  # Adjust size for legend entries
      legend.title = element_text(size = 22))
  


ggsave("E:/Shishir/FieldData/Results/ssc_onlyCleandata_v1.jpg",  width = 6, height = 3.5,scale = 3)
  
  
ggsave("ssc_onlyCleandata_v1.jpg", ssc_onlyCleandata, device = "jpg",path = "E:/Shishir/FieldData/Results/",
       scale = 3, width = 5, height = 3,
       dpi = 300, limitsize = TRUE)


###### check if turbidity and SSC have a relationship ##### 


ggplot(ssc[ssc$SSC..mg.l.<150 & ssc$SSC..mg.l.>0,],aes(x = Turbidity,y = (SSC..mg.l.)))+geom_point(aes(group = River,col = River))+
  theme_bw()+xlab("Turbidity") + ylab("SSC") +ggtitle("SSC vs turbidity")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))
  
ggplot(ssc,aes(x = Turbidity,y = (SSC..mg.l.)))+geom_point(aes(group = River,col = River))+
    theme_bw()+xlab("Turbidity") + ylab("SSC") +ggtitle("SSC vs turbidity")+
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))
  



######## Now get ssc values and compute the median SSC for each sampling date #######
ssc_mean =   ssc %>% select(c("Sampling.Date","ScheduledDates","SSC..mg.l.","SamplingMonth","River")) 
# calculate mean SSC
ssc_mean = ssc_mean %>% dplyr::group_by(Sampling.Date,ScheduledDates,River) %>% dplyr::mutate(ssc_mean = mean(SSC..mg.l.))
ssc_mean = ssc_mean %>% dplyr::select(-SSC..mg.l.) %>% distinct()

ssc_mean$logssc = log(ssc_mean$ssc_mean)

# rename the levels so that it matches with refletance data set
levels(ssc_mean$River)[levels(ssc_mean$River) == "Gangavali"] <- "Gang"
levels(ssc_mean$River)[levels(ssc_mean$River) == "Sharavathi"] <- "Shar"

ssc_mean$Season = "wet"
ssc_mean$Season[ssc_mean$SamplingMonth == "Apr" | 
                ssc_mean$SamplingMonth == "Mar" |
                ssc_mean$SamplingMonth == "May" |
                ssc_mean$SamplingMonth == "Jun"] = "Dry" 

ssc_mean$Season[ssc_mean$SamplingMonth == "Oct" | 
                  ssc_mean$SamplingMonth == "Nov" |
                  ssc_mean$SamplingMonth == "Dec" |
                  ssc_mean$SamplingMonth == "Jan"] = "Post-monsoon" 

names(ssc_mean)

ssc_mean_Season = summarySE(ssc_mean,measurevar = "ssc_mean",groupvars = c("River","Season"),na.rm = TRUE)
ssc_mean_Season$ssc_mean = round(ssc_mean_Season$ssc_mean,2)
ssc_mean_Season$se = round(ssc_mean_Season$se,2)
names(ssc_mean_Season)
ssc_mean_Season = ssc_mean_Season %>% select("River","Season","N","ssc_mean","se")
