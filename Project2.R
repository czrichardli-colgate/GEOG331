library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)

datD <- read.csv("Z:\\cli2\\carbon-monitor-CITIES-maingraphdatas.csv") 
datD

datD$dateMod = dmy(datD$date)
datD

#Residential

datDRes <- subset(datD, sector == "Residential")
datDRes

##### Figure 4 #####
ggplot(data=datDRes,aes(x=dateMod,y=ktCO2.per.day,color=city))+geom_line(data = subset(datDRes,
        city=="Chicago"))+geom_line(data = subset(datDRes,
        city=="Houston"))+geom_line(data = subset(datDRes,
        city=="Los Angeles"))+geom_line(data = subset(datDRes,
        city=="Miami"))+geom_line(data = subset(datDRes,
        city=="New York"))+geom_line(data = subset(datDRes,
        city=="San Francisco"))+geom_line(data = subset(datDRes,
        city=="Seattle"))+geom_line(data = subset(datDRes,
        city=="Washington"))+labs(
        x="Date",y="CO2 emission (kT)",title="Daily CO2 Emission")+theme(legend.position = "right")
##### Figure 4 #####

##### Figure 6 #####
ggplot(data=datDRes,aes(x=dateMod,y=ktCO2.per.day,color=city))+geom_line(data = subset(datDRes,
                                                                                       city=="Washington"))+geom_line(data = subset(datDRes,
                                                                                                                                    city=="Miami"))+geom_line(data = subset(datDRes,
                                                                                                                                                                            city=="Houston"))+geom_line(data = subset(datDRes,
                                                                                                                                                                                                                      city=="Chicago"))+labs(x="Date",y="CO2 emission (kT)",
                                                                                                                                                                                                                                             title="Daily CO2 Emission")+theme(legend.position = "right")

##### Figure 6 #####


datW <- read.csv("Z:\\cli2\\dataWeatherMetric.csv")
datW

datW$dateMod = ymd(datW$DATE)
datW

datW$city[datW$NAME == "MIAMI INTERNATIONAL AIRPORT, FL US"] <- "Miami"
datW$city[datW$NAME == "CHICAGO OHARE INTERNATIONAL AIRPORT, IL US"] <- "Chicago"
datW$city[datW$NAME == "LOS ANGELES INTERNATIONAL AIRPORT, CA US"] <- "Los Angeles"
datW$city[datW$NAME == "WASHINGTON REAGAN NATIONAL AIRPORT, VA US"] <- "Washington"
datW$city[datW$NAME == "SEATTLE TACOMA AIRPORT, WA US"] <- "Seattle"
datW$city[datW$NAME == "HOUSTON INTERCONTINENTAL AIRPORT, TX US"] <- "Houston"
datW$city[datW$NAME == "SAN FRANCISCO INTERNATIONAL AIRPORT, CA US"] <- "San Francisco"
datW$city[datW$NAME == "JFK INTERNATIONAL AIRPORT, NY US"] <- "New York"
datW

ggplot(data=datW,aes(x=dateMod,y=TAVG,color=city))+geom_line(data = subset(datW,
        city=="Chicago"))+geom_line(data = subset(datW,
        city=="Houston"))+geom_line(data = subset(datW,
        city=="Los Angeles"))+geom_line(data = subset(datW,
        city=="Miami"))+geom_line(data = subset(datW,
        city=="New York"))+geom_line(data = subset(datW,
        city=="San Francisco"))+geom_line(data = subset(datW,
        city=="Seattle"))+geom_line(data = subset(datW,
        city=="Washington"))+labs(
        x="Date",y="Average Temperature (C)",title="Daily Average Temperature")+theme(legend.position = "right")





datDRes$factor = paste0(datDRes$city, ",", datDRes$dateMod)
datW$factor = paste0(datW$city, ",", datW$dateMod)
datJ = left_join(datDRes,datW,by="factor")
datJ
colnames(datJ)[colnames(datJ) == "city.x"] = "city"
colnames(datJ)[colnames(datJ) == "dateMod.x"] = "dateMod"
datJ$city.y = NULL
datJ$dateMod.y = NULL
datJ

ggplot(data = datJ, aes(x=TAVG,y=ktCO2.per.day,color = city))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 (kT)",
                                                                        title="Correlation between daily temperature and CO2 emission among cities")



datJ = datJ %>%
  mutate(CO2Mod = case_when(
    city == "Chicago" ~ datJ$ktCO2.per.day/9408576*1000000,
    city == "Houston" ~ datJ$ktCO2.per.day/7796182*1000000,
    city == "Los Angeles" ~ datJ$ktCO2.per.day/12927614*1000000,
    city == "Miami" ~ datJ$ktCO2.per.day/6457998*1000000,
    city == "New York" ~ datJ$ktCO2.per.day/19940274*1000000,
    city == "San Francisco" ~ datJ$ktCO2.per.day/4648486*1000000,
    city == "Seattle" ~ datJ$ktCO2.per.day/4145494*1000000,
    city == "Washington" ~ datJ$ktCO2.per.day/6436489*1000000,
    TRUE ~ NA
    ))
datJ

ggplot(data = datJ, aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = city))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                  title="Correlation between daily temperature and CO2 emission among cities (adjusted with population)")
ggplot(data = datJ, aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = city))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                             title="Correlation between daily temperature and CO2 emission among cities (adjusted with population)")+geom_smooth()
ggplot(data = datJ, aes(x=TAVG,y=CO2Mod, color = city))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                             title="Correlation between daily temperature and CO2 emission among cities (adjusted with population)")+geom_smooth()
ggplot(data = datJ, aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = city))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                         title="Correlation between daily temperature and CO2 emission among cities (adjusted with population)")+geom_smooth()+geom_violin(size = 1.5,fill = NA,aes(color = city))


##### Figure 5 #####
ggplot(data = datJ, aes(x=TAVG,y=ktCO2.per.day, color = city))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 (kT)",
                                                                          title="Correlation between daily temperature and CO2 emission among cities")+geom_smooth()
cor.test(x=datJ$TAVG[datJ$city=="Chicago"], y=datJ$ktCO2.per.day[datJ$city=="Chicago"], method = "pearson")
cor.test(x=datJ$TAVG[datJ$city=="Houston"], y=datJ$ktCO2.per.day[datJ$city=="Houston"], method = "pearson")
cor.test(x=datJ$TAVG[datJ$city=="Los Angeles"], y=datJ$ktCO2.per.day[datJ$city=="Los Angeles"], method = "pearson")
cor.test(x=datJ$TAVG[datJ$city=="Miami"], y=datJ$ktCO2.per.day[datJ$city=="Miami"], method = "pearson")
cor.test(x=datJ$TAVG[datJ$city=="New York"], y=datJ$ktCO2.per.day[datJ$city=="New York"], method = "pearson")
cor.test(x=datJ$TAVG[datJ$city=="San Francisco"], y=datJ$ktCO2.per.day[datJ$city=="San Francisco"], method = "pearson")
cor.test(x=datJ$TAVG[datJ$city=="Seattle"], y=datJ$ktCO2.per.day[datJ$city=="Seattle"], method = "pearson")
cor.test(x=datJ$TAVG[datJ$city=="Washington"], y=datJ$ktCO2.per.day[datJ$city=="Washington"], method = "pearson")
##### Figure 5 #####

ggplot(data=datJ,aes(x=dateMod,y=CO2Mod,color=city))+geom_line(data = subset(datJ,
                      city=="Chicago"))+geom_line(data = subset(datJ,
                      city=="Houston"))+geom_line(data = subset(datJ,
                      city=="Los Angeles"))+geom_line(data = subset(datJ,
                      city=="Miami"))+geom_line(data = subset(datJ,
                      city=="New York"))+geom_line(data = subset(datJ,
                      city=="San Francisco"))+geom_line(data = subset(datJ,
                      city=="Seattle"))+geom_line(data = subset(datJ,
                      city=="Washington"))+labs(x="Date",y="Daily CO2 per capita (kg)",title="Daily CO2 Emission")+theme(legend.position = "right")







ggplot(data=datJ,aes(x=dateMod,y=CO2Mod,color=city))+geom_line(data = subset(datJ,
                                                                             city=="Washington"))+geom_line(data = subset(datJ,
                                                                                                                          city=="Miami"))+geom_line(data = subset(datJ,
                                                                                                                                                                  city=="Houston"))+geom_line(data = subset(datJ,
                                                                                                                                                                                                            city=="Chicago"))+labs(x="Date",y="CO2 emission (kT)",
                                                                                                                                                                                                                                   title="Daily CO2 Emission")+theme(legend.position = "right")




datJ$month <- paste0(year(datJ$dateMod),'-',quarter(datJ$dateMod))
datJ

ggplot(data = subset(datJ, city=="Chicago"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in Chicago")+geom_smooth()
ggplot(data = subset(datJ, city=="Houston"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in Houston")+geom_smooth()
ggplot(data = subset(datJ, city=="Los Angeles"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in Los Angeles")+geom_smooth()
ggplot(data = subset(datJ, city=="Miami"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in Miami")+geom_smooth()
ggplot(data = subset(datJ, city=="New York"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in New York")+geom_smooth()
ggplot(data = subset(datJ, city=="San Francisco"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in San Francisco")+geom_smooth()
ggplot(data = subset(datJ, city=="Seattle"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in Seattle")+geom_smooth()
ggplot(data = subset(datJ, city=="Washington"), aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = month))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                                      title="Correlation between daily temperature and CO2 emission among cities (adjusted with population) in Washington")+geom_smooth()



# Power

datDPow <- subset(datD, sector == "Power")
datDPow

ggplot(data=datDPow,aes(x=dateMod,y=ktCO2.per.day,color=city))+geom_line(data = subset(datDPow,
        city=="Chicago"))+geom_line(data = subset(datDPow,
        city=="Houston"))+geom_line(data = subset(datDPow,
        city=="Los Angeles"))+geom_line(data = subset(datDPow,
        city=="Miami"))+geom_line(data = subset(datDPow,
        city=="New York"))+geom_line(data = subset(datDPow,
        city=="San Francisco"))+geom_line(data = subset(datDPow,
        city=="Seattle"))+geom_line(data = subset(datDPow,
        city=="Washington"))+labs(
        x="Date",y="CO2 emission (kT)",title="Daily CO2 Emission")+theme(legend.position = "right")

datDPow$factor = paste0(datDPow$city, ",", datDPow$dateMod)
datW$factor = paste0(datW$city, ",", datW$dateMod)
datJP = left_join(datDPow,datW,by="factor")
datJP
colnames(datJP)[colnames(datJP) == "city.x"] = "city"
colnames(datJP)[colnames(datJP) == "dateMod.x"] = "dateMod"
datJP$city.y = NULL
datJP$dateMod.y = NULL
datJP

ggplot(data = datJP, aes(x=TAVG,y=ktCO2.per.day,color = city))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 (kT)",
                                                                                title="Correlation between daily temperature and CO2 emission among cities")


datJP = datJP %>%
  mutate(CO2Mod = case_when(
    city == "Chicago" ~ datJP$ktCO2.per.day/8671746*1000000,
    city == "Houston" ~ datJP$ktCO2.per.day/5853575*1000000,
    city == "Los Angeles" ~ datJP$ktCO2.per.day/12237376*1000000,
    city == "Miami" ~ datJP$ktCO2.per.day/6077522*1000000,
    city == "New York" ~ datJP$ktCO2.per.day/19426449*1000000,
    city == "San Francisco" ~ datJP$ktCO2.per.day/3515933*1000000,
    city == "Seattle" ~ datJP$ktCO2.per.day/3544011*1000000,
    city == "Washington" ~ datJP$ktCO2.per.day/5174759*1000000,
    TRUE ~ NA
  ))
datJP

ggplot(data = datJP, aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = city))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                             title="Correlation between daily temperature and CO2 emission among cities (adjusted with population)")
ggplot(data = datJP, aes(x=TAVG,y=CO2Mod))+geom_point(aes(color = city))+labs(x="Daily temperature (C)",y="Daily CO2 per capita (kg)",
                                                                             title="Correlation between daily temperature and CO2 emission among cities (adjusted with population)")+geom_smooth()







# ABANDONED CODE #




# Quarterly Aggregate

# datDRes$quarter = quarters(datDRes$dateMod)
# datDRes$year = year(datDRes$dateMod)
# datDRes$factor = paste0(datDRes$city, ",", datDRes$year,datDRes$quarter)
# datDRes
# datQD = aggregate(x = datDRes$ktCO2.per.day, data = datDRes, by = list(datDRes$factor), FUN="mean", na.rm = TRUE)
# colnames(datQD) <- c("factor","quarterlyCO2")
# datQD
# 
# datW$quarter = quarters(datW$dateMod)
# datW$year = year(datW$dateMod)
# datW$factor = paste0(datW$city, ",", datW$year,datW$quarter)
# datW
# datQW = aggregate(x = datW$TAVG, data = datW, by = list(datW$factor), FUN="mean", na.rm = TRUE)
# colnames(datQW) <- c("factor","quarterlyTemp")
# datQ = left_join(datQD,datQW,by = "factor")
# datQ
# 
# ggplot(data=datQ,aes(x=quarterlyTemp,y=quarterlyCO2))+geom_point()+labs(x="Quarterly temperature (C)",y="Quarterly CO2 (kT)",
#         title="Correlation between quarterly temperature and quarterly CO2")+geom_smooth(method = "lm")
# 
# 
# datQDiff = data.frame(factor = 
#                       c("Chicago,2023", "Chicago,2024","Chicago,2025",
#                         "Houston,2023", "Houston,2024","Houston,2025",
#                         "Los Angeles,2023", "Los Angeles,2024","Los Angeles,2025",
#                         "Miami,2023", "Miami,2024","Miami,2025",
#                         "New York,2023", "New York,2024","New York,2025",
#                         "San Francisco,2023", "San Francisco,2024","San Francisco,2025",
#                         "Seattle,2023", "Seattle,2024","Seattle,2025",
#                         "Washington,2023", "Washington,2024","Washington,2025"),
#                       diffCO2 = 
#                       c(datQ$quarterlyCO2[datQ$factor=="Chicago,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="Chicago,2023Q3"],datQ$quarterlyCO2[datQ$factor=="Chicago,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="Chicago,2024Q3"],datQ$quarterlyCO2[datQ$factor=="Chicago,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="Chicago,2025Q3"],
#                         datQ$quarterlyCO2[datQ$factor=="Houston,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="Houston,2023Q3"],datQ$quarterlyCO2[datQ$factor=="Houston,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="Houston,2024Q3"],datQ$quarterlyCO2[datQ$factor=="Houston,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="Houston,2025Q3"],
#                         datQ$quarterlyCO2[datQ$factor=="Los Angeles,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="Los Angeles,2023Q3"],datQ$quarterlyCO2[datQ$factor=="Los Angeles,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="Los Angeles,2024Q3"],datQ$quarterlyCO2[datQ$factor=="Los Angeles,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="Los Angeles,2025Q3"],
#                         datQ$quarterlyCO2[datQ$factor=="Miami,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="Miami,2023Q3"],datQ$quarterlyCO2[datQ$factor=="Miami,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="Miami,2024Q3"],datQ$quarterlyCO2[datQ$factor=="Miami,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="Miami,2025Q3"],
#                         datQ$quarterlyCO2[datQ$factor=="New York,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="New York,2023Q3"],datQ$quarterlyCO2[datQ$factor=="New York,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="New York,2024Q3"],datQ$quarterlyCO2[datQ$factor=="New York,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="New York,2025Q3"],
#                         datQ$quarterlyCO2[datQ$factor=="San Francisco,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="San Francisco,2023Q3"],datQ$quarterlyCO2[datQ$factor=="San Francisco,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="San Francisco,2024Q3"],datQ$quarterlyCO2[datQ$factor=="San Francisco,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="San Francisco,2025Q3"],
#                         datQ$quarterlyCO2[datQ$factor=="Seattle,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="Seattle,2023Q3"],datQ$quarterlyCO2[datQ$factor=="Seattle,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="Seattle,2024Q3"],datQ$quarterlyCO2[datQ$factor=="Seattle,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="Seattle,2025Q3"],
#                         datQ$quarterlyCO2[datQ$factor=="Washington,2023Q1"]-datQ$quarterlyCO2[datQ$factor=="Washington,2023Q3"],datQ$quarterlyCO2[datQ$factor=="Washington,2024Q1"]-datQ$quarterlyCO2[datQ$factor=="Washington,2024Q3"],datQ$quarterlyCO2[datQ$factor=="Washington,2025Q1"]-datQ$quarterlyCO2[datQ$factor=="Washington,2025Q3"]),
#                       diffTemp = 
#                       c(datQ$quarterlyTemp[datQ$factor=="Chicago,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="Chicago,2023Q1"],datQ$quarterlyTemp[datQ$factor=="Chicago,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="Chicago,2024Q1"],datQ$quarterlyTemp[datQ$factor=="Chicago,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="Chicago,2025Q1"],
#                         datQ$quarterlyTemp[datQ$factor=="Houston,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="Houston,2023Q1"],datQ$quarterlyTemp[datQ$factor=="Houston,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="Houston,2024Q1"],datQ$quarterlyTemp[datQ$factor=="Houston,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="Houston,2025Q1"],
#                         datQ$quarterlyTemp[datQ$factor=="Los Angeles,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="Los Angeles,2023Q1"],datQ$quarterlyTemp[datQ$factor=="Los Angeles,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="Los Angeles,2024Q1"],datQ$quarterlyTemp[datQ$factor=="Los Angeles,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="Los Angeles,2025Q1"],
#                         datQ$quarterlyTemp[datQ$factor=="Miami,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="Miami,2023Q1"],datQ$quarterlyTemp[datQ$factor=="Miami,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="Miami,2024Q1"],datQ$quarterlyTemp[datQ$factor=="Miami,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="Miami,2025Q1"],
#                         datQ$quarterlyTemp[datQ$factor=="New York,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="New York,2023Q1"],datQ$quarterlyTemp[datQ$factor=="New York,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="New York,2024Q1"],datQ$quarterlyTemp[datQ$factor=="New York,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="New York,2025Q1"],
#                         datQ$quarterlyTemp[datQ$factor=="San Francisco,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="San Francisco,2023Q1"],datQ$quarterlyTemp[datQ$factor=="San Francisco,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="San Francisco,2024Q1"],datQ$quarterlyTemp[datQ$factor=="San Francisco,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="San Francisco,2025Q1"],
#                         datQ$quarterlyTemp[datQ$factor=="Seattle,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="Seattle,2023Q1"],datQ$quarterlyTemp[datQ$factor=="Seattle,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="Seattle,2024Q1"],datQ$quarterlyTemp[datQ$factor=="Seattle,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="Seattle,2025Q1"],
#                         datQ$quarterlyTemp[datQ$factor=="Washington,2023Q3"]-datQ$quarterlyTemp[datQ$factor=="Washington,2023Q1"],datQ$quarterlyTemp[datQ$factor=="Washington,2024Q3"]-datQ$quarterlyTemp[datQ$factor=="Washington,2024Q1"],datQ$quarterlyTemp[datQ$factor=="Washington,2025Q3"]-datQ$quarterlyTemp[datQ$factor=="Washington,2025Q1"]))
# datQDiff
# ggplot(data=datQDiff,aes(x=diffTemp,y=diffCO2))+geom_point()+labs(x="Temperature difference (C)",y="CO2 emission difference (kT)",
#                                                                         title="Correlation between temperature and CO2 emission differences in different cities")+geom_smooth(method = "lm")
