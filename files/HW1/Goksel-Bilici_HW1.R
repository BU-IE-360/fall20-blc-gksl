library("xlsx")
library("ggplot2")
library("lubridate")

## FIRST
egitim_data <- read.xlsx("D:/AILEvs/EZ/EGITIM/AAAA_EE_4.YIL/IE_360/HW1/Illere_gore_egitim.xls", sheetIndex=1)

ggplot(egitim_data,aes(City, Total))+
  geom_bar(stat="identity", aes(fill = factor(City)),color= factor(egitim_data$City_code))+
  facet_wrap(egitim_data$Year)+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  labs(x="Cities",
       y="University Students",
       fill = "The Cities",
       title="University students in different cities from 2008 to 2019")

ggplot(egitim_data,aes(City, Total))+
  geom_bar(stat="identity",position = position_dodge(width = 0.5), aes(fill = factor(Year)), color= factor(egitim_data$City_code)) +
  labs(x="5 Big Cities",
       y="University Students",
       fill = "Years",
       title="Total University Student")

ggplot(egitim_data,aes(factor(Year), Total))+
  geom_boxplot(aes(fill = factor(Year)))+
  theme(legend.position = "none")+
  labs(x="Years",y="Students",
       title="Number of University Students in the 5 cities between 2008 to 2019")

universite_arama <- read.xlsx("D:/AILEvs/EZ/EGITIM/AAAA_EE_4.YIL/IE_360/HW1/universite.xlsx", sheetIndex=1)

ggplot(universite_arama,aes(factor(year(Zaman)), universite))+
  geom_boxplot(aes(fill=factor(year(Zaman))))+
  theme(legend.position = "none")+
  labs(x="Years",y="Search of 'universite'",
       title="Search of 'universite' by Years")


###SECOND
tufe_data <- read.xlsx("D:/AILEvs/EZ/EGITIM/AAAA_EE_4.YIL/IE_360/HW1/Tuketici_fiyatlari.xlsx", sheetIndex=1)

ggplot(tufe_data,aes(x=factor(month(Zaman)),y= TUFE_AYLIK))+
  geom_bar(stat="identity", aes(fill = factor(month(Zaman))),color="green")+
  facet_wrap(~year(Zaman))+
  theme(legend.position = "none")+
  labs(x="Months",
       y="Monthly TUFE",
       title="Histograms of Monthly TUFE rates of Years")

ggplot(tufe_data, aes(Zaman, TUFE_AYLIK)) +
  geom_bar(stat = "identity", color = "yellow") +
  geom_smooth(color = "cyan")

ggplot(tufe_data,aes(x=factor(month(Zaman)),y= TUFE_AYLIK))+
  geom_boxplot(aes(fill = factor(month(Zaman))))+
  theme(legend.position = "none")+
  labs(x="Months",y="TUFE Rates",
       title="TUFE rates of Months in 2005-2020")

tufe_arama <- read.xlsx("D:/AILEvs/EZ/EGITIM/AAAA_EE_4.YIL/IE_360/HW1/tufe.xlsx", sheetIndex=1)

ggplot(tufe_arama,aes(factor(month(Zaman)), tufe))+
  geom_boxplot(aes(fill=factor(month(Zaman))))+
  theme(legend.position = "none")+
  labs(x="Months",y="Search of 'TUFE'",
       title="Search of 'TUFE' by Months")

##THIRD
yoksulluk_data <- read.xlsx("D:/AILEvs/EZ/EGITIM/AAAA_EE_4.YIL/IE_360/HW1/yoksulluk.xls", sheetIndex=1)

ggplot(yoksulluk_data, aes(Region, Share_of_poor_by_sixty_percent_Median_income)) +
  geom_bar(stat = "identity", aes(fill = factor(Region)))+
  facet_wrap(yoksulluk_data$Year)+
  theme(legend.position = "none",
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())+
  labs(x="Regions",
       y="Powerty Rates",
       fill = "The Regions",
       title="Powerty rates from 2014 to 2019 in Turkey")

ggplot(yoksulluk_data, aes(Region, Share_of_poor_by_sixty_percent_Median_income)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), aes(fill = factor(Year)), color = factor(yoksulluk_data$Region))+
  theme(legend.position = "none",
   axis.ticks.x = element_blank(),
   axis.text.x = element_blank())+
  labs(x="Regions",
       y="Powerty Rates",
       fill = "The Regions",
       title="Powerty rates from 2014 to 2019 in Turkey")

ggplot(yoksulluk_data, aes(Year, Share_of_poor_by_sixty_percent_Median_income)) +
  geom_boxplot(aes(fill = factor(Year)))+
  theme(legend.position = "none")+
  labs(x="Years",
       y="Powerty Rates",
       title="Powerty rates from 2014 to 2019 in Turkey")

aclik_siniri_arama <- read.xlsx("D:/AILEvs/EZ/EGITIM/AAAA_EE_4.YIL/IE_360/HW1/aclik_siniri.xlsx", sheetIndex=1)

ggplot(aclik_siniri_arama,aes(factor(year(Zaman)), aclik_siniri))+
  geom_boxplot(aes(fill=factor(year(Zaman))))+
  theme(legend.position = "none")+
  labs(x="Years",y="Search of 'aclik siniri'",
       title="Search of 'aclik siniri' by Years")

yoksulluk_arama <- read.xlsx("D:/AILEvs/EZ/EGITIM/AAAA_EE_4.YIL/IE_360/HW1/yoksullukarama.xlsx", sheetIndex=1)

ggplot(yoksulluk_arama,aes(factor(year(Zaman)), Yoksulluk))+
  geom_boxplot(aes(fill=factor(year(Zaman))))+
  theme(legend.position = "none")+
  labs(x="Years",y="Search of 'yoksulluk'",
       title="Search of 'yoksulluk' by Years")
