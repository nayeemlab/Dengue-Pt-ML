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

sldata <- read.csv("DataNew_17_8_23.csv", header = T)
NROW(sldata)

sldata_inside <- sldata[sldata$Latitude >= 23.7 & sldata$Latitude <= 23.9,]
sldata_inside$Latitude
max(sldata_inside$Latitude, na.rm=T)
sldata_inside <- sldata_inside[sldata_inside$Longitude >= 90.35 & sldata_inside$Longitude <= 90.45,]
sldata_inside$Longitude
max(sldata_inside$Longitude, na.rm=T)
NROW(sldata_inside)

x <- NROW(sldata_inside)/NROW(sldata)
x

#sldata_inside <- sldata[sldata$Location == "Inside",]
#sldata_inside <- sldata[sldata$Location == "Outside",]
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
map2 <- map1 +  geom_point(data=sldata_inside, aes(x=Longitude, y=Latitude), colour = "darkgreen", size = 1)+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 15))


map2


#bangladesh
setwd("E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\resierraleonespacetimeclusteringanalysis")

sldata <- read.csv("DataNew_17_8_23.csv", header = T)

shp <- readOGR(dsn = "E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\sle_admbnda_adm4_1m_gov_ocha", "BGD_adm2")

head(shp@data)
xLon = sldata$Longitude
xLat = sldata$Latitude

SL.map <- fortify(shp, region = "NAME_2")

map1 <- ggplot() + 
  geom_polygon(data = SL.map, aes(x = long, y = lat, group = group), colour = "cadetblue", fill = "azure2") +
  labs(title = "Location of Dengue patients") +
  xlab(label="Longitute") + ylab(label="Latitute")
map1
map2 <- map1 +  geom_point(data=sldata, aes(x=Longitude, y=Latitude), colour = "darkgreen", size = 2)+ 
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


#Pie chart
df <- data.frame(
  Sex = c("Female", "Male"),
  value = c(36.10, 63.90)
)
head(df)

library(ggplot2)

x <- ggplot(df, aes(x = "", y = value, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=30,
            position = position_stack(vjust = 0.5))  + ggtitle("Dengue Cases") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=50),
                                 legend.text = element_text(size=50),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=50))

x

df <- data.frame(
  Sex = c("Male", "Female"),
  value = c(44.22, 55.78)
)
head(df)

library(ggplot2)

y <- ggplot(df, aes(x = "", y = value, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=30,
            position = position_stack(vjust = 0.5))  + ggtitle("Dengue Deaths") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=50),
                                 legend.text = element_text(size=50),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=50))

y

library(gridExtra)
tiff("DCDD.tiff", units="in", width=35, height=20, res=300)
gridExtra::grid.arrange(x,y, ncol=2)
dev.off()



#bar chart
survey <- data.frame(AgeGroup=c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60",
                                "61-70","71-80","80+"),
                     DengueCases=c(6685, 10867, 15050, 8921, 4853, 3195,
                                   1554, 544, 163))

x <- ggplot(survey, aes(x=AgeGroup, y=DengueCases, fill=AgeGroup)) + 
  geom_bar(stat="identity",fill="#56B4E9") +
  scale_fill_brewer(palette="Set1")+ 
  xlab("Age Groups") + ylab("Dengue Cases")+ theme(legend.title = element_text(size=50),
                                                   legend.text = element_text(size=50),
                                                   plot.title = element_text(hjust = 0.5),
                                                   text=element_text(size=50),
                                                   legend.position="none")
x

survey <- data.frame(AgeGroup=c("0-10", "11-20", "21-30", "31-40", "41-50", "51-60",
                                "61-70","71-80","80+"),
                     DengueCases=c(23, 30, 49, 43, 29, 32,
                                   31, 10, 4))

y <- ggplot(survey, aes(x=AgeGroup, y=DengueCases, fill=AgeGroup)) + 
  geom_bar(stat="identity",fill="#56B4E9") +
  scale_fill_brewer(palette="Set1")+ 
  xlab("Age Groups") + ylab("Dengue Deaths")+ theme(legend.title = element_text(size=50),
                                                    legend.text = element_text(size=50),
                                                    plot.title = element_text(hjust = 0.5),
                                                    text=element_text(size=50),
                                                    legend.position="none")

y

library(gridExtra)
tiff("DCDDAge3.tiff", units="in", width=40, height=20, res=300)
gridExtra::grid.arrange(x,y, ncol=2)
dev.off()






#The 2023 Deadly Dengue Outbreaks in Bangladesh Highlights a paradigm shift of occurrences of dengue cases in diverse geographical regions of the country 

options(scipen=999)
setwd("E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\resierraleonespacetimeclusteringanalysis")

sldata <- read.csv("DistrictWise.csv", header = T)

t.test(sldata$Cases,sldata$PopSize)
t.test(sldata$Cases[2:64],sldata$PopSize[2:64])

cor.test(sldata$Cases,sldata$PopSize)
cor.test(sldata$Cases[2:64],sldata$PopSize[2:64])

library(ggplot2)
library(ggrepel)
a <- ggplot(sldata, aes(x = Cases, y = PopSize))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Total Dengue Cases (With Dhaka)") + ylab("Population Size")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
a

Cases63 <- sldata$Cases[2:64]
PopSize63 <- sldata$PopSize[2:64]

dat <- data.frame(Cases63,PopSize63)

b <- ggplot(dat, aes(x = Cases63, y = PopSize63))  + 
  theme(plot.title = element_text(hjust = 0.5,size=12,face = "bold"),text = element_text(size = 12))+
  geom_point(size = 3.5,
             show.legend = F,colour="black")+
  xlab("Total Dengue Cases (Without Dhaka)") + ylab("Population Size")  +
  geom_smooth(method = "lm", se = FALSE,colour="black")+ 
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10))
b

library(gridExtra)
tiff("CorrPlot.tiff", units="in", width=30, height=10, res=300)
gridExtra::grid.arrange(a,b, ncol=2)
dev.off()

shp <- readOGR(dsn = "bgd_adm_bbs_20201113_SHP", "bgd_admbnda_adm2_bbs_20201113")

head(shp@data)
shp@data$ADM2_EN

sldata <- sldata[order(sldata$ADM2_EN),]

joined_df <- merge(shp@data, sldata, by = "ADM2_EN")

q_1 <- fortify(shp, region = "ADM2_EN")


library(dplyr)

q_1 <- q_1 %>%
  mutate(prev = case_when(q_1$id=="Bagerhat" ~ sldata$Cases[1],q_1$id=="Bandarban" ~ sldata$Cases[2],q_1$id=="Barguna" ~ sldata$Cases[3],q_1$id=="Barisal" ~ sldata$Cases[4],
                          q_1$id=="Bhola" ~ sldata$Cases[5],q_1$id=="Bogra" ~ sldata$Cases[6],q_1$id=="Brahamanbaria" ~ sldata$Cases[7],q_1$id=="Chandpur" ~ sldata$Cases[8],
                          q_1$id=="Chittagong" ~ sldata$Cases[9],q_1$id=="Chuadanga" ~ sldata$Cases[10],q_1$id=="Comilla" ~ sldata$Cases[11],q_1$id=="Cox's Bazar" ~ sldata$Cases[12],
                          q_1$id=="Dhaka" ~ sldata$Cases[13],q_1$id=="Dinajpur" ~ sldata$Cases[14],q_1$id=="Faridpur" ~ sldata$Cases[15],q_1$id=="Feni" ~ sldata$Cases[16],
                          q_1$id=="Gaibandha" ~ sldata$Cases[17],q_1$id=="Gazipur" ~ sldata$Cases[18],q_1$id=="Gopalganj" ~ sldata$Cases[19],q_1$id=="Habiganj" ~ sldata$Cases[20],
                          q_1$id=="Jamalpur" ~ sldata$Cases[21],q_1$id=="Jessore" ~ sldata$Cases[22],q_1$id=="Jhalokati" ~ sldata$Cases[23],q_1$id=="Jhenaidah" ~ sldata$Cases[24],
                          q_1$id=="Joypurhat" ~ sldata$Cases[25],q_1$id=="Khagrachhari" ~ sldata$Cases[26],q_1$id=="Khulna" ~ sldata$Cases[27],q_1$id=="Kishoreganj" ~ sldata$Cases[28],
                          q_1$id=="Kurigram" ~ sldata$Cases[29],q_1$id=="Kushtia" ~ sldata$Cases[30],q_1$id=="Lakshmipur" ~ sldata$Cases[31],q_1$id=="Lalmonirhat" ~ sldata$Cases[32],
                          q_1$id=="Madaripur" ~ sldata$Cases[33],q_1$id=="Magura" ~ sldata$Cases[34],q_1$id=="Manikganj" ~ sldata$Cases[35],q_1$id=="Maulvibazar" ~ sldata$Cases[36],
                          q_1$id=="Meherpur" ~ sldata$Cases[37],q_1$id=="Munshiganj" ~ sldata$Cases[38],q_1$id=="Mymensingh" ~ sldata$Cases[39],q_1$id=="Naogaon" ~ sldata$Cases[40],
                          q_1$id=="Narail" ~ sldata$Cases[41],q_1$id=="Narayanganj" ~ sldata$Cases[42],q_1$id=="Narsingdi" ~ sldata$Cases[43],q_1$id=="Natore" ~ sldata$Cases[44],
                          q_1$id=="Nawabganj" ~ sldata$Cases[45],q_1$id=="Netrakona" ~ sldata$Cases[46],q_1$id=="Nilphamari" ~ sldata$Cases[47],q_1$id=="Noakhali" ~ sldata$Cases[48],
                          q_1$id=="Pabna" ~ sldata$Cases[49],q_1$id=="Panchagarh" ~ sldata$Cases[50],q_1$id=="Patuakhali" ~ sldata$Cases[51],q_1$id=="Pirojpur" ~ sldata$Cases[52],
                          q_1$id=="Rajbari" ~ sldata$Cases[53],q_1$id=="Rajshahi" ~ sldata$Cases[54],q_1$id=="Rangamati" ~ sldata$Cases[55],q_1$id=="Rangpur" ~ sldata$Cases[56],
                          q_1$id=="Satkhira" ~ sldata$Cases[57],q_1$id=="Shariatpur" ~ sldata$Cases[58],q_1$id=="Sherpur" ~ sldata$Cases[59],q_1$id=="Sirajganj" ~ sldata$Cases[60],
                          q_1$id=="Sunamganj" ~ sldata$Cases[61],q_1$id=="Sylhet" ~ sldata$Cases[62],q_1$id=="Tangail" ~ sldata$Cases[63],q_1$id=="Thakurgaon" ~ sldata$Cases[64],
  ))

Casesmap1 <- ggplot(data = q_1 , aes(x = long, y = lat)) + 
  geom_polygon(aes(group=group,fill=prev),colour= "lightgrey") +
  scale_fill_distiller(name='Cases',palette ="Greys", direction=1)+labs(title = "Number of Dengue Cases (Districtwise)") +
  xlab(label="Longitute") + ylab(label="Latitute")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 25),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))

Casesmap1




q_1 <- q_1 %>%
  mutate(prevD = case_when(q_1$id=="Bagerhat" ~ sldata$Deaths[1],q_1$id=="Bandarban" ~ sldata$Deaths[2],q_1$id=="Barguna" ~ sldata$Deaths[3],q_1$id=="Barisal" ~ sldata$Deaths[4],
                           q_1$id=="Bhola" ~ sldata$Deaths[5],q_1$id=="Bogra" ~ sldata$Deaths[6],q_1$id=="Brahamanbaria" ~ sldata$Deaths[7],q_1$id=="Chandpur" ~ sldata$Deaths[8],
                           q_1$id=="Chittagong" ~ sldata$Deaths[9],q_1$id=="Chuadanga" ~ sldata$Deaths[10],q_1$id=="Comilla" ~ sldata$Deaths[11],q_1$id=="Cox's Bazar" ~ sldata$Deaths[12],
                           q_1$id=="Dhaka" ~ sldata$Deaths[13],q_1$id=="Dinajpur" ~ sldata$Deaths[14],q_1$id=="Faridpur" ~ sldata$Deaths[15],q_1$id=="Feni" ~ sldata$Deaths[16],
                           q_1$id=="Gaibandha" ~ sldata$Deaths[17],q_1$id=="Gazipur" ~ sldata$Deaths[18],q_1$id=="Gopalganj" ~ sldata$Deaths[19],q_1$id=="Habiganj" ~ sldata$Deaths[20],
                           q_1$id=="Jamalpur" ~ sldata$Deaths[21],q_1$id=="Jessore" ~ sldata$Deaths[22],q_1$id=="Jhalokati" ~ sldata$Deaths[23],q_1$id=="Jhenaidah" ~ sldata$Deaths[24],
                           q_1$id=="Joypurhat" ~ sldata$Deaths[25],q_1$id=="Khagrachhari" ~ sldata$Deaths[26],q_1$id=="Khulna" ~ sldata$Deaths[27],q_1$id=="Kishoreganj" ~ sldata$Deaths[28],
                           q_1$id=="Kurigram" ~ sldata$Deaths[29],q_1$id=="Kushtia" ~ sldata$Deaths[30],q_1$id=="Lakshmipur" ~ sldata$Deaths[31],q_1$id=="Lalmonirhat" ~ sldata$Deaths[32],
                           q_1$id=="Madaripur" ~ sldata$Deaths[33],q_1$id=="Magura" ~ sldata$Deaths[34],q_1$id=="Manikganj" ~ sldata$Deaths[35],q_1$id=="Maulvibazar" ~ sldata$Deaths[36],
                           q_1$id=="Meherpur" ~ sldata$Deaths[37],q_1$id=="Munshiganj" ~ sldata$Deaths[38],q_1$id=="Mymensingh" ~ sldata$Deaths[39],q_1$id=="Naogaon" ~ sldata$Deaths[40],
                           q_1$id=="Narail" ~ sldata$Deaths[41],q_1$id=="Narayanganj" ~ sldata$Deaths[42],q_1$id=="Narsingdi" ~ sldata$Deaths[43],q_1$id=="Natore" ~ sldata$Deaths[44],
                           q_1$id=="Nawabganj" ~ sldata$Deaths[45],q_1$id=="Netrakona" ~ sldata$Deaths[46],q_1$id=="Nilphamari" ~ sldata$Deaths[47],q_1$id=="Noakhali" ~ sldata$Deaths[48],
                           q_1$id=="Pabna" ~ sldata$Deaths[49],q_1$id=="Panchagarh" ~ sldata$Deaths[50],q_1$id=="Patuakhali" ~ sldata$Deaths[51],q_1$id=="Pirojpur" ~ sldata$Deaths[52],
                           q_1$id=="Rajbari" ~ sldata$Deaths[53],q_1$id=="Rajshahi" ~ sldata$Deaths[54],q_1$id=="Rangamati" ~ sldata$Deaths[55],q_1$id=="Rangpur" ~ sldata$Deaths[56],
                           q_1$id=="Satkhira" ~ sldata$Deaths[57],q_1$id=="Shariatpur" ~ sldata$Deaths[58],q_1$id=="Sherpur" ~ sldata$Deaths[59],q_1$id=="Sirajganj" ~ sldata$Deaths[60],
                           q_1$id=="Sunamganj" ~ sldata$Deaths[61],q_1$id=="Sylhet" ~ sldata$Deaths[62],q_1$id=="Tangail" ~ sldata$Deaths[63],q_1$id=="Thakurgaon" ~ sldata$Deaths[64],
  ))

Deathsmap1 <- ggplot(data = q_1 , aes(x = long, y = lat)) + 
  geom_polygon(aes(group=group,fill=prevD),colour= "lightgrey") +
  scale_fill_distiller(name='Deaths',palette ="Greys", direction=1)+labs(title = "Number of Dengue Deaths (Districtwise)") +
  xlab(label="Longitute") + ylab(label="Latitute")+ 
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 25),
        legend.title = element_text(size=20),
        legend.text = element_text(size=20))

Deathsmap1

tiff("DistrictwiseDengue.tiff", units="in", width=30, height=15, res=300)
gridExtra::grid.arrange(Casesmap1,Deathsmap1, ncol=2)
dev.off()






#Pie chart
df <- data.frame(
  Location = c("Dhaka City", "Outside Dhaka City"),
  value = c(76.49, 23.51)
)
head(df)

library(ggplot2)

a <- ggplot(df, aes(x = "", y = value, fill = Location)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=20,
            position = position_stack(vjust = 0.5))  + ggtitle("   (28 June 2023) \n Dengue Cases") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=50),
                                 legend.text = element_text(size=50),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=50))+ scale_fill_brewer(palette="Blues", direction=-1)


a

df <- data.frame(
  Location = c("Dhaka City", "Outside Dhaka City"),
  value = c(47.77, 52.23)
)
head(df)

library(ggplot2)

b <- ggplot(df, aes(x = "", y = value, fill = Location)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=20,
            position = position_stack(vjust = 0.5))  + ggtitle("   (25 August 2023) \n Dengue Cases") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=50),
                                 legend.text = element_text(size=50),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=50))+ scale_fill_brewer(palette="Blues", direction=-1)

b

library(gridExtra)
tiff("DCVsODC.tiff", units="in", width=40, height=20, res=300)
gridExtra::grid.arrange(a,b, ncol=2)
dev.off()




#Pie chart
df <- data.frame(
  Location = c("Dhaka City", "Outside Dhaka City"),
  value = c(78.72, 21.28)
)
head(df)

library(ggplot2)

x <- ggplot(df, aes(x = "", y = value, fill = Location)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=20,
            position = position_stack(vjust = 0.5))  + ggtitle("Dengue Deaths") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=50),
                                 legend.text = element_text(size=50),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=50))+ scale_fill_brewer(palette="OrRd", direction=-1)


x

df <- data.frame(
  Location = c("Dhaka City", "Outside Dhaka City"),
  value = c(74.05, 25.95)
)
head(df)

library(ggplot2)

y <- ggplot(df, aes(x = "", y = value, fill = Location)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=20,
            position = position_stack(vjust = 0.5))  + ggtitle("Dengue Deaths") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=50),
                                 legend.text = element_text(size=50),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=50)) + scale_fill_brewer(palette="OrRd", direction=-1)

y

library(gridExtra)
tiff("DCVsODCD.tiff", units="in", width=40, height=20, res=300)
gridExtra::grid.arrange(x,y, ncol=2)
dev.off()


library(gridExtra)
tiff("DCVsODCDBoth.tiff", units="in", width=40, height=20, res=300)
gridExtra::grid.arrange(a,b,x,y, nrow=2,ncol=2)
dev.off()

MISdata <- read.csv("DataNew_17_8_23.csv", header = T)

MISdata$Age <- as.numeric(MISdata$Age..year.)

aggregate(MISdata$Age, list(MISdata$Sex), FUN=median, na.rm=T)


setwd("E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\resierraleonespacetimeclusteringanalysis")

Dailydata <- read.csv("DengueDGHSDataCasesDeathAge.csv", header = T)

#######Inside########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "January"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "January"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "January"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "January"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "January"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "January"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "January"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "January"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "January"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "January"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

#####Grand Total Case and Deaths######

GTC310123 <- Dailydata$Total.Cumulative.Daily.Case[Dailydata$Date == "1/31/2023"]
GTC310123

GTD310123 <- Dailydata$Total.Cumulative.Daily.Deaths[Dailydata$Date == "1/31/2023"]
GTD310123


#######Inside February########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "February"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "February"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "February"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "February"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "February"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "February"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "February"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "February"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "February"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "February"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

#####Grand Total Case and Deaths######

GTC310123 <- Dailydata$Total.Cumulative.Daily.Case[Dailydata$Date == "2/28/2023"]
GTC310123

GTD310123 <- Dailydata$Total.Cumulative.Daily.Deaths[Dailydata$Date == "2/28/2023"]
GTD310123


#######Inside March########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "March"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "March"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID

DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "March"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "March"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "March"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "March"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "March"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "March"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "March"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "March"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

#####Grand Total Case and Deaths######

GTC310123 <- Dailydata$Total.Cumulative.Daily.Case[Dailydata$Date == "3/31/2023"]
GTC310123

GTD310123 <- Dailydata$Total.Cumulative.Daily.Deaths[Dailydata$Date == "3/31/2023"]
GTD310123


#######Inside April########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "April"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "April"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "April"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "April"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "April"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "April"]
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "April"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "April"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "April"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "April"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

#####Grand Total Case and Deaths######

GTC310123 <- Dailydata$Total.Cumulative.Daily.Case[Dailydata$Date == "4/30/2023"]
GTC310123

GTD310123 <- Dailydata$Total.Cumulative.Daily.Deaths[Dailydata$Date == "4/30/2023"]
GTD310123


#######Inside May########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "May"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "May"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "May"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "May"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "May"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "May"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "May"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "May"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "May"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "May"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

#####Grand Total Case and Deaths######

GTC310123 <- Dailydata$Total.Cumulative.Daily.Case[Dailydata$Date == "5/31/2023"]
GTC310123

GTD310123 <- Dailydata$Total.Cumulative.Daily.Deaths[Dailydata$Date == "5/31/2023"]
GTD310123


#######Inside June########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "June"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "June"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "June"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "June"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "June"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "June"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "June"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "June"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "June"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "June"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

#####Grand Total Case and Deaths######

GTC310123 <- Dailydata$Total.Cumulative.Daily.Case[Dailydata$Date == "6/30/2023"]
GTC310123

GTD310123 <- Dailydata$Total.Cumulative.Daily.Deaths[Dailydata$Date == "6/30/2023"]
GTD310123


#######Inside July########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "July"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "July"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "July"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "July"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "July"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "July"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "July"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "July"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "July"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "July"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

#####Grand Total Case and Deaths######

GTC310123 <- Dailydata$Total.Cumulative.Daily.Case[Dailydata$Date == "7/31/2023"]
GTC310123

GTD310123 <- Dailydata$Total.Cumulative.Daily.Deaths[Dailydata$Date == "7/31/2023"]
GTD310123


#######Inside August########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "August"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "August"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "August"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "August"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "August"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "August"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "August"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "August"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "August"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "August"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res


#######Inside September########

DailyCID310123 <- sum(Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "September"])
DailyCID310123

TCID310123 <- sum(Dailydata$Total.Daily.Case[Dailydata$Month == "September"])
TCID310123

PCID <- DailyCID310123*100/TCID310123
PCID


DailyDID310123 <- sum(Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "September"])
DailyDID310123

TDID310123 <- sum(Dailydata$Total.Daily.Deaths[Dailydata$Month == "September"])
TDID310123

PCID <- DailyDID310123*100/TDID310123
PCID


######Outside##########

DailyCOD310123 <- sum(Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "September"])
DailyCOD310123

PCOD <- DailyCOD310123*100/TCID310123
PCOD

DailyDOD310123 <- sum(Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "September"])
DailyDOD310123

PCOD <- DailyDOD310123*100/TDID310123
PCOD

########T-test Cases########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City[Dailydata$Month == "September"]
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City[Dailydata$Month == "September"]
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res

########T-test Cases overall########## 
MonthlyCID <- Dailydata$Daily.Case.inside.Dhaka.City
MonthlyCID

MonthlyCOD <- Dailydata$Daily.Case.outside.Dhaka.City
MonthlyCOD

# Compute t-test
res <- t.test(MonthlyCID, MonthlyCOD, paired = TRUE)
res


########T-test Deaths########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City[Dailydata$Month == "September"]
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City[Dailydata$Month == "September"]
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

########T-test Deaths overall########## 
MonthlyDID <- Dailydata$Daily.Deaths.inside.Dhaka.City
MonthlyDID

MonthlyDOD <- Dailydata$Daily.Deaths.outside.Dhaka.City
MonthlyDOD

# Compute t-test
res <- t.test(MonthlyDID, MonthlyDOD, paired = TRUE)
res

setwd("E:\\ResearchProject\\Najmul Bhai\\Dengue\\Dengue Spatio\\resierraleonespacetimeclusteringanalysis")

Dailydata <- read.csv("DengueDGHSDataCasesDeathAge.csv", header = T)


dat <- data.frame(ds = seq(as.Date('2023-01-01'), as.Date('2023-09-08'), by = 'd'),
                  y =data.frame(Dailydata$Cumulative.Case[1:251],Dailydata$Cumulative.Case.1[1:251]))

colnames(dat) <- c("Date", "Daily Cases inside Dhaka City", "Daily Cases outside Dhaka City")

library("tidyverse")
df <- dat %>%
  select(Date, "Daily Cases inside Dhaka City", "Daily Cases outside Dhaka City") %>%
  gather(key = "Characteristics", value = "value", -Date)
head(df)

# Visualization
case <- ggplot(df, aes(x = Date, y = log10(value))) +
  geom_line(aes(color = Characteristics, linetype = Characteristics),cex=2) + 
  scale_color_manual(values = c("darkred", "blue"))+  ylab("Cases (log10)") + 
  theme(legend.title = element_text(size=30),
        legend.text = element_text(size=30),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5),
        text=element_text(size=30)) 

dates_vline <- as.Date(c("2023-06-28"))                 # Define positions of vline
dates_vline <- which(df$Date %in% dates_vline)

case <- case + geom_vline(xintercept = as.numeric(df$Date[dates_vline]),
                          col = "black", lwd = 2)

case

dat <- data.frame(ds = seq(as.Date('2023-01-01'), as.Date('2023-09-08'), by = 'd'),
                  y =data.frame(Dailydata$Cumulative.Death[1:251],Dailydata$Cumulative.Death.1[1:251]))

colnames(dat) <- c("Date", "Daily deaths inside Dhaka City", "Daily deaths outside Dhaka City")

library("tidyverse")
df <- dat %>%
  select(Date, "Daily deaths inside Dhaka City", "Daily deaths outside Dhaka City") %>%
  gather(key = "Characteristics", value = "value", -Date)
head(df)

# Visualization
death <- ggplot(df, aes(x = Date, y = log(value))) +
  geom_line(aes(color = Characteristics, linetype = Characteristics),cex=2) + 
  scale_color_manual(values = c("darkred", "blue"))+  ylab("Deaths (log10)") + theme(legend.title = element_text(size=30),
                                                                                     legend.text = element_text(size=30),
                                                                                     legend.position="bottom",
                                                                                     plot.title = element_text(hjust = 0.5),
                                                                                     
                                                                                     text=element_text(size=30))
dates_vline <- as.Date(c("2023-06-28"))                 # Define positions of vline
dates_vline <- which(df$Date %in% dates_vline)

death <- death + geom_vline(xintercept = as.numeric(df$Date[dates_vline]),
                            col = "black", lwd = 2)
death



library(gridExtra)
tiff("LineGraph.tiff", units="in", width=40, height=20, res=300)
gridExtra::grid.arrange(case,death, ncol=2)
dev.off()



colors = c("grey") 
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep","Oct") 
regions <- c("Cases") 
#6,3,0,2,2,34,204,342,396,266
# Create the matrix of the values. 
Values <- matrix(c(566,166,111,143,1036,5956,43854,71976,79598,51640),  
                 nrow = 2, ncol = 10, byrow = TRUE) 

# Create the bar chart 
barplot(Values, main = "Total Revenue", names.arg = months,  
        xlab = "Month", ylab = "Revenue",  
        col = colors, beside = TRUE) 

# Add the legend to the chart 
legend("topleft", regions, cex = 0.7, fill = colors) 

