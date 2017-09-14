####Swedish temperature and precipitation trends
#set up libraries
library(lubridate)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)

#### prep data ####
#set working directory
setwd("~/Box Sync/Post-Doc/Swedish plants")

#read in data
TempPrecip<-read.csv("TempPrecip.csv", stringsAsFactors = FALSE)

#convert blank precipitation to 0 
TempPrecip$Precipitation[TempPrecip$Precipitation==""] <- 0

#remove NA
TempPrecip <- TempPrecip[-which(TempPrecip$Precipitation == "n/a"), ]

#remove odd entries
TempPrecip <- TempPrecip[-which(TempPrecip$Precipitation == "."), ]
TempPrecip <- TempPrecip[-which(TempPrecip$Precipitation == "?"), ]

#coerce factors to numbers
TempPrecip$Precipitation <- as.numeric(as.character(TempPrecip$Precipitation))

#code in date
TempPrecip$Date <- TempPrecip$Time
TempPrecip$Time <- ymd(TempPrecip$Time)
TempPrecip$Year <- year(TempPrecip$Time)
TempPrecip$Month <- month(TempPrecip$Time)
TempPrecip$Day <- day(TempPrecip$Time)

#### plot time series figures ####

#make plot of temperature over time
Temp<-ddply(TempPrecip, .(Year, Month), .fun= summarise, mean.temp=mean(Temp_avg))

Temp.plot<-ggplot(Temp, aes(x = Year, y = mean.temp))+geom_point()+facet_wrap(~Month, scales = "free_y")+
  xlab("Year")+ylab("Mean temperature")+theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")

#make plot of precipitation over time
Precip<-ddply(TempPrecip, .(Year, Month), .fun= summarise, mean.precip=mean(Precipitation))

Precip<-na.omit(Precip)

Precip.plot<-ggplot(Precip, aes(x = Year, y = mean.precip))+geom_point()+facet_wrap(~Month, scales = "free_y")+
  xlab("Year")+ylab("Mean precipitation")+theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")

Figure.1<-arrangeGrob(Temp.plot,Precip.plot)

#ggsave("Figure.1.pdf", plot=Figure.1, width = 11, height=17)

#### coefficent of variantion ####
CV<-ddply(TempPrecip, .variables=.(Year, Month), .fun= summarise, CV.temp = sd(Temp_avg)/mean(Temp_avg),
          CV.precip = sd(Precipitation)/mean(Precipitation))

CV.year.mean<-ddply(CV, .(Year), .fun= summarise, 
                    mean.cv.temp=mean(CV.temp),
                    mean.cv.precip=mean(CV.precip),
                    cv.temp.SE=sd(CV.temp)/sqrt(length(CV.temp)),cv.precip.SE=sd(CV.precip)/sqrt(length(CV.precip)))
                                                
#plot CV
Temp.cv.plot<-ggplot(CV, aes(x = Year, y = CV.temp))+geom_point()+
  xlab("Year")+ylab("Mean monthly temperature CV")+
  facet_wrap(~Month, scales = "free_y")+
  theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")


Precip.cv.plot<-ggplot(CV, aes(x = Year, y = CV.precip))+geom_point()+
  xlab("Year")+ylab("Mean monthly precipitation CV")+
  facet_wrap(~Month, scales = "free_y")+
  theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")


Figure.2<-arrangeGrob(Temp.cv.plot,Precip.cv.plot)

#ggsave("Figure.2.pdf", plot=Figure.2, width = 11, height=17)

#### growing season #####
#subset only positive temps
Temp.pos<-subset(TempPrecip, Temp_avg > 0)

#get length of growing season by month and year
Growing.month.season<-ddply(Temp.pos, .(Year, Month), .fun= summarise, length=length(Temp_avg))

Growing.year.season<-ddply(Growing.month.season, .(Year), .fun= summarise, length.year=sum(length))

Growing.season.month.plot<-ggplot(Growing.month.season, aes(x = Year, y = length))+geom_point()+
  xlab("Year")+ylab("Length of growing season (days)")+
  facet_wrap(~Month, scales = "free_y")+
  theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")

Growing.year.month.plot<-ggplot(Growing.year.season, aes(x = Year, y = length.year))+geom_point()+
  xlab("Year")+ylab("Length of growing season (days)")+
  theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")

Figure.3<-arrangeGrob(Growing.season.month.plot,Growing.year.month.plot)

#ggsave("Figure.3.pdf", plot=Figure.3, width = 11, height=17)


#### snow and ice ####
#subset winter days (11, 12, 1, 2, 3, 4)
Nov.Temp.Precip<-subset(TempPrecip, Month==11)
Dec.Temp.Precip<-subset(TempPrecip, Month==12)
Jan.Temp.Precip<-subset(TempPrecip, Month==1)
Feb.Temp.Precip<-subset(TempPrecip, Month==2)
Mar.Temp.Precip<-subset(TempPrecip, Month==3)
Apr.Temp.Precip<-subset(TempPrecip, Month==4)

Winter.Temp.Precip<-rbind(Nov.Temp.Precip,Dec.Temp.Precip,Jan.Temp.Precip,Feb.Temp.Precip,Mar.Temp.Precip,Apr.Temp.Precip)

#code for days where temp is >0 and precip>0
Winter.Temp.Precip<-ddply(Winter.Temp.Precip, .(), .fun= transform, boiler = ifelse(Temp_avg> 0 & Precipitation >0 ,"Y","N"),
                          snow = ifelse(Temp_avg< 0 & Precipitation >0 ,"Y","N"))

Winter.Temp.Precip.Month<-ddply(Winter.Temp.Precip, .(Year,Month), summarise, 
      rain.days = length(boiler[boiler=="Y"]), snow.days = length(snow[snow=="Y"]))

#convert to long
Winter.Temp.Precip.Month.long<-gather(Winter.Temp.Precip.Month, Precip.type, Days, rain.days:snow.days)

#subset boiler days 
Winter.Temp.Precip.subset <- Winter.Temp.Precip[which(Winter.Temp.Precip$boiler == "Y"), ]

#count the number of boiler days per year
Winter.Temp.Precip.Year<-ddply(Winter.Temp.Precip.subset, .(Year), .fun= summarise, count=length(boiler))

#plot
Boiler.plot.month<-ggplot(Winter.Temp.Precip.Month.long, aes(x = Year, y = Days, colour=Precip.type, group=Precip.type))+
  geom_point()+
  xlab("Year")+ylab("Number of days")+
  facet_grid(Precip.type~Month, scales = "free_y")+
  theme_minimal()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")+
  scale_color_manual(values = c("grey", "dark blue"))+theme(legend.position="none")


Boiler.plot.year<-ggplot(Winter.Temp.Precip.Year, aes(x = Year, y = count))+geom_point()+
  xlab("Year")+ylab("Number of positive temperature and precipitation winter days")+
  theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")


ggsave("Figure.4.pdf", plot=Boiler.plot.month, width = 11, height=8.5)

ggsave("Figure.5.pdf", plot=Boiler.plot.year, width = 11, height=8.5)



#### decade comparison ####
TempPrecip$decade<-ifelse(TempPrecip$Year <=1922,"1913-1922",
                         ifelse(TempPrecip$Year <=1932,"1923-1932",
                                ifelse(TempPrecip$Year <=1942,"1933-1942",
                                       ifelse(TempPrecip$Year <=1952,"1943-1952",
                                              ifelse(TempPrecip$Year <=1962,"1953-1962",
                                                     ifelse(TempPrecip$Year <=1972,"1963-1972",
                                                     ifelse(TempPrecip$Year <=1982,"1973-1982",
                                                     ifelse(TempPrecip$Year <=1992,"1983-1992",
                                                     ifelse(TempPrecip$Year <=2002,"1993-2002", "2003-2016")))))))))


#average temp and precipitation by decade
Decade.Temp.Precip<-ddply(TempPrecip, .variables=.(decade, Month), .fun= summarise, mean.temp = mean(Temp_avg),
      mean.precip = mean(Precipitation))

#make plot
Decade.Temp.plot<-ggplot(Decade.Temp.Precip, aes(x = decade, y = mean.temp))+geom_point()+
  xlab("Year")+ylab("Average temperature (C)")+
  facet_wrap(~Month, scales = "free_y")+
  theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")+
  theme(axis.text.x = element_text(angle = 90))

Decade.Precip.plot<-ggplot(Decade.Temp.Precip, aes(x = decade, y = mean.precip))+geom_point()+
  xlab("Year")+ylab("Average precipitation")+
  facet_wrap(~Month, scales = "free_y")+
  theme_classic()+stat_smooth(colour="red",se=FALSE, size=0.5, method = "lm")+
  theme(axis.text.x = element_text(angle = 90))

Figure.6<-arrangeGrob(Decade.Temp.plot,Decade.Precip.plot)

ggsave("Figure.6.pdf", plot=Figure.6, width = 11, height=9.5)

