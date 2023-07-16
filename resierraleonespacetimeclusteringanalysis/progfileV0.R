################################################################################
## Dengue - Data analysis ##
################################################################################
rm(list=ls()) #Remove all previous R objects#
## Packages ##
library(maptools)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(ggplot2)
library(tidyverse)
setwd("E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\resierraleonespacetimeclusteringanalysis")

sldata <- read.csv("DataDhkpt.csv", header = T)

shp <- readOGR(dsn = "E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\Dhaka", "cc486qp3429")

head(shp@data)
xLon = sldata$HLon
xLat = sldata$HLat

SL.map <- fortify(shp, region = "fid")

map1 <- ggplot() + 
  geom_polygon(data = SL.map, aes(x = long, y = lat, group = group), colour = "cadetblue", fill = "azure2") +
  labs(title = "Location of Dengue patients (Red) and Hospitals (Green)") +
  xlab(label="Longitute") + ylab(label="Latitute")
map1
map2 <- map1 +  geom_point(data=sldata, aes(x=HLon, y=HLat), colour = "darkgreen", size = 7)+
  geom_point(data=sldata, aes(x=sldata$Longitude, y=sldata$Latitude), colour = "red", size = 2)+ 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 15))
  
  
map2

sldata <- read.csv("Data.csv", header = T)

#Week transformation (daily to weekly)
library(lubridate)
sldata$Date2 <- as.Date(as.character(sldata$Date),format="%Y-%m-%d")
sldata$Date2
sldata$Week <- week(as.Date(as.character(sldata$Date2),format="%Y-%m-%d"))
sldata$Week

ggplot(sldata, aes(x=Week))+
  geom_histogram(color="darkgreen", fill="red") + 
  xlab("Weeks") + ylab("Dengue Patient Counts")+ 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))


ggplot(sldata, aes(x=Age..year.))+
  geom_histogram(color="black", fill="darkgreen") + 
  xlab("Age of patients") + ylab("Dengue Patient Counts")+ 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

library(ggplot2)
# Barplot
slpie <- sldata %>%
  group_by(Sex) %>%
  summarise(count = n()) %>%
  group_by(Sex) %>% mutate(per=round(count/sum(count)*100,2))
slpie

ggplot(slpie, aes(x = "", y = per, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = per),cex=10,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ 
  xlab("") + ylab("") + theme(legend.title = element_text(size=20),
                              legend.text = element_text(size=20))


# Barplot
slpie <- sldata %>%
  group_by(AgeGroup) %>%
  summarise(count = n()) %>%
  group_by(AgeGroup) %>% mutate(per=round(count/sum(slpie$count)*100,2))
slpie

ggplot(slpie, aes(x = "", y = per, fill = AgeGroup)) +
  geom_col(color = "black") +
  geom_text(aes(label = per),cex=10,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ 
  xlab("") + ylab("") + theme(legend.title = element_text(size=20),
                              legend.text = element_text(size=20))



################################################################################
## Descriptive statistics ##
################################################################################
sldata <- read.csv("Data.csv", header = T)
names(sldata)
sldatanm <- sldata[!is.na(sldata$HospitalLocation), ]

#Hospital Location Vs Age
describe(sldatanm$Age..year.)
describe.by(sldatanm$Age..year., sldatanm$HospitalLocation)
t.test(sldatanm$Age..year. ~ sldatanm$HospitalLocation)

tab <- table(sldatanm$AgeGroup, sldatanm$HospitalLocation)
tab
prop.table(tab,1)*100
chisq.test(tab)
tab <- table(sldatanm$AgeGroup)
tab
prop.table(tab)*100

#Hospital Location Vs Sex

tab <- table(sldatanm$Sex, sldatanm$HospitalLocation)
tab
prop.table(tab,1)*100
chisq.test(tab)
tab <- table(sldatanm$AgeGroup)
tab
prop.table(tab)*100

################################################################################

