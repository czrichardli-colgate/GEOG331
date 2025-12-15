# Outdoor Analysis

datJul <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Jun-30-2023-to-Jul-31-2023.csv") 
datAug <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Jul-31-2023-to-Aug-31-2023.csv")
datSep <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Aug-31-2023-to-Sep-30-2023.csv")
datOct <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Sep-30-2023-to-Oct-31-2023.csv")
datNov <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Oct-31-2023-to-Nov-30-2023.csv")
datDec <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Nov-30-2023-to-Dec-31-2023.csv")
datJan <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Dec-31-2023-to-Jan-31-2024.csv")
datFeb <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Jan-31-2024-to-Feb-29-2024.csv")
datMar <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Feb-29-2024-to-Mar-31-2024.csv")
datApr <- read.csv("Z:\\cli2\\Outdoor CoE\\Outdoor_CoE_Mar-31-2024-to-Apr-30-2024.csv")
datJoined = rbind(datJul,datAug,datSep,datOct,datNov,datDec,datJan,datFeb,datMar,datApr)

library(lubridate)
datJoined$time = mdy_hms(datJoined$Timestamp.America.New_York.)
datJoined$date = date(datJoined$time)
datJoined$month = month(datJoined$time)

#Daily
datJoinedDailyCO2 = aggregate(as.numeric(gsub(" ppm","",datJoined$CO2)), by=list(datJoined$date), FUN="mean",na.rm = TRUE)
colnames(datJoinedDailyCO2) <- c("date","dailyCO2")
datJoinedDailyCO2

datJoinedDailyPM2.5 = aggregate(as.numeric(gsub(" ppm","",datJoined$PM2.5)), by=list(datJoined$date), FUN="mean",na.rm = TRUE)
colnames(datJoinedDailyPM2.5) <- c("date","dailyPM2.5")
datJoinedDailyPM2.5

datJoinedDailyPM10 = aggregate(as.numeric(gsub(" ppm","",datJoined$PM10)), by=list(datJoined$date), FUN="mean",na.rm = TRUE)
colnames(datJoinedDailyPM10) <- c("date","dailyPM10")
datJoinedDailyPM10

datJoinedDailyTemp = aggregate(as.numeric(gsub("°C","",datJoined$Temperature)), by=list(datJoined$date), FUN="mean",na.rm = TRUE)
colnames(datJoinedDailyTemp) <- c("date","dailyTemp")
datJoinedDailyTemp

library(dplyr)
datJoinedDaily = left_join(datJoinedDailyCO2,datJoinedDailyTemp,by = "date")
datJoinedDaily = left_join(datJoinedDailyPM2.5,datJoinedDaily,by = "date")
datJoinedDaily = left_join(datJoinedDailyPM10,datJoinedDaily,by = "date")
datJoinedDaily

library(reshape2)
library(tidyr)

datJMod = melt(datJoinedDaily, id.vars = "date", variable.names = "variables")

library(ggplot2)
##### Figure 1 #####
ggplot(data=datJoinedDaily,aes(x=dailyTemp,y=dailyCO2))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 (ppm)",
        title="Correlation between daily temperature and daily CO2")
cor.test(x=datJoinedDaily$dailyTemp, y=datJoinedDaily$dailyCO2, method = "pearson")
##### Figure 1 #####

ggplot(data=datJoinedDaily[datJoinedDaily$year == "2023" & datJoinedDaily$month >= 9,],aes(x=dailyTemp,y=dailyCO2))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 (ppm)",
                                                                          title="Correlation between daily temperature and daily CO2")+geom_smooth()

summary(lm(datJoinedDaily$dailyCO2~datJoinedDaily$dailyTemp))

ggplot(data=datJoinedDaily,aes(x=date, y=dailyCO2))+geom_line()+labs(x="Date",y="Daily emission (ppm)",
                                                                          title="Correlation between date and outdoor CO2")+geom_smooth()
ggplot(data=datJoinedDaily[datJoinedDaily$year == "2023" & datJoinedDaily$month >= 9,],aes(x=date, y=dailyCO2))+geom_line()+labs(x="Date",y="Daily emission (ppm)",
                                                                     title="Correlation between date and outdoor CO2")+geom_smooth()

datJoinedDaily$year = year(datJoinedDaily$date)
datJoinedDaily$month = month(datJoinedDaily$date)

##### Figure 2 #####
ggplot(data=datJoinedDaily[datJoinedDaily$year == "2023" & datJoinedDaily$month >= 9,])+geom_line(aes(x=date, y=dailyPM2.5))+geom_line(aes(x=date, y=dailyPM10))+labs(x="Date",y="Daily emission (ppm)",title="Change in outdoor greenhouse gas emissions")+geom_smooth(aes(x=date, y=dailyPM2.5))+geom_smooth(aes(x=date, y=dailyPM10))
##### Figure 2 #####



ggplot(data=datJoinedDaily[datJoinedDaily$year == "2023" & datJoinedDaily$month >= 9,],aes(x=date, y=dailyPM2.5))+geom_line(na.rm=TRUE)+labs(x="Date",y="Daily emission (ppm)",title="Correlation between date and outdoor PM2.5")+geom_smooth()
ggplot(data=datJoinedDaily[datJoinedDaily$year == "2023" & datJoinedDaily$month >= 9,],aes(x=date, y=dailyCO2))+geom_line()+labs(x="Date",y="Daily emission (ppm)",
                                                                     title="Correlation between date and outdoor CO2")+geom_smooth()
ggplot(data=datJoinedDaily[datJoinedDaily$year == "2023" & datJoinedDaily$month >= 9,],aes(x=dailyTemp,y=dailyPM2.5))+geom_point()+labs(x="Daily temperature (C)",y="Daily emission (ppm)",
                                                                                                                                      title="Correlation between daily temperature and daily PM2.5")+geom_smooth()




ggplot(data=datJoinedDaily[datJoinedDaily$year == "2023" & datJoinedDaily$month == 10,],aes(x=dailyTemp,y=dailyPM2.5))+geom_point()+labs(x="Daily temperature (C)",y="Daily emission (ppm)",
                                                                                                                                        title="Correlation between daily temperature and daily PM2.5")+geom_smooth()


#Monthly
datJoinedMonthlyCO2 = aggregate(as.numeric(gsub(" ppm","",datJoined$CO2)), by=list(datJoined$month), FUN="mean",na.rm = TRUE)
colnames(datJoinedMonthlyCO2) <- c("month","monthlyCO2")
datJoinedMonthlyCO2

datJoinedMonthlyTemp = aggregate(as.numeric(gsub("°C","",datJoined$Temperature)), by=list(datJoined$month), FUN="mean",na.rm = TRUE)
colnames(datJoinedMonthlyTemp) <- c("month","monthlyTemp")
datJoinedMonthlyTemp

library(dplyr)
datJoinedMonthly = left_join(datJoinedMonthlyCO2,datJoinedMonthlyTemp,by = "month")
datJoinedMonthly

library(ggplot2)
ggplot(data=datJoinedMonthly,aes(x=monthlyTemp,y=monthlyCO2))+geom_point()+labs(x="Monthly temperature (C)",y="Monthly CO2 (ppm)",
        title="Correlation between monthly temperature and monthly CO2")+geom_smooth(method = "lm")

summary(lm(datJoinedMonthly$monthlyCO2~datJoinedMonthly$monthlyTemp))

ggplot(data=datJoinedMonthly,aes(x=month,y=monthlyCO2))+geom_line()+labs(x="Month",y="Monthly CO2 (ppm)",
                                                                    title="Correlation between month and monthly CO2")
# Indoor Analysis

datJulIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Jun-30-2023-to-Jul-31-2023.csv")
datOctIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Sep-30-2023-to-Oct-31-2023.csv")
datNovIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Oct-31-2023-to-Nov-30-2023.csv")
datDecIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Nov-30-2023-to-Dec-31-2023.csv")
datJanIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Dec-31-2023-to-Jan-31-2024.csv")
datFebIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Jan-31-2024-to-Feb-29-2024.csv")
datMarIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Feb-29-2024-to-Mar-31-2024.csv")
datAprIn <- read.csv("Z:\\cli2\\B2 IAQ\\B-2-IAQ-LR_Mar-31-2024-to-Apr-30-2024.csv")
datJIn = rbind(datJulIn,datOctIn,datNovIn,datDecIn,datJanIn,datFebIn,datMarIn,datAprIn)

library(lubridate)
datJIn$time = mdy_hms(datJIn$Timestamp.America.New_York.)
datJIn$date = date(datJIn$time)
datJIn$month = month(datJIn$time)

#Daily
datJInDailyCO2 = aggregate(as.numeric(gsub(" ppm","",datJIn$CO2)), by=list(datJIn$date), FUN="mean",na.rm = TRUE)
colnames(datJInDailyCO2) <- c("date","dailyCO2")
datJInDailyCO2

datJInDailyPM2.5 = aggregate(as.numeric(gsub(" ppm","",datJIn$PM2.5)), by=list(datJIn$date), FUN="mean",na.rm = TRUE)
colnames(datJInDailyPM2.5) <- c("date","dailyPM2.5")
datJInDailyPM2.5

datJInDailyPM10 = aggregate(as.numeric(gsub(" ppm","",datJIn$PM10)), by=list(datJIn$date), FUN="mean",na.rm = TRUE)
colnames(datJInDailyPM10) <- c("date","dailyPM10")
datJInDailyPM10

datJInDailyTemp = aggregate(as.numeric(gsub("°C","",datJIn$Temperature)), by=list(datJIn$date), FUN="mean",na.rm = TRUE)
colnames(datJInDailyTemp) <- c("date","dailyTemp")
datJInDailyTemp

library(dplyr)
datJInDaily = left_join(datJInDailyCO2,datJInDailyTemp,by = "date")
datJInDaily = left_join(datJInDailyPM2.5,datJInDaily,by = "date")
datJInDaily = left_join(datJInDailyPM10,datJInDaily,by = "date")
datJInDaily

library(reshape2)
library(tidyr)

datInMod = melt(datJInDaily, id.vars = "date", variable.names = "variables")

library(ggplot2)
ggplot(data=datJInDaily,aes(x=dailyTemp,y=dailyCO2))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 (ppm)",
                                                                          title="Correlation between daily indoor temperature and daily indoor CO2")+geom_smooth()

summary(lm(datJInDaily$dailyCO2~datJInDaily$dailyTemp))

ggplot(data=datJInDaily,aes(x=date, y=dailyCO2))+geom_line()+labs(x="Date",y="Daily emission (ppm)",
                                                                     title="Correlation between date and indoor CO2")+geom_smooth()

ggplot(data=datJInDaily)+geom_line(aes(x=date, y=dailyPM2.5))+geom_line(aes(x=date, y=dailyPM10))+labs(x="Date",y="Daily emission (ppm)",title="Correlation between date and indoor greenhouse gases")







# Heating Energy Consumption Analysis

datA7 = read.csv("Z:\\cli2\\A_7.csv")
datA8 = read.csv("Z:\\cli2\\A_8.csv")

datA7$timestamp <- mdy_hm(datA7$Date...Time)
datA7$date <- date(datA7$timestamp)
datA7$nonHeating <- rowSums(datA7[,2:12])+datA7[,16]
datA7$heating <- rowSums(datA7[,13:15])
datA7

datA7aggnh = aggregate(datA7$nonHeating, by=list(datA7$date), FUN="sum",na.rm = TRUE)
colnames(datA7aggnh) <- c("date","nonHeating")
datA7aggh = aggregate(datA7$heating, by=list(datA7$date), FUN="sum",na.rm = TRUE)
colnames(datA7aggh) <- c("date","heating")
datA7agg = left_join(datA7aggh, datA7aggnh, by = "date")
datA7agg

datA8$timestamp <- mdy_hm(datA8$Date...Time)
datA8$date <- date(datA8$timestamp)
datA8$nonHeating <- rowSums(datA8[,2:12])+datA8[,16]
datA8$heating <- rowSums(datA8[,13:15])
datA8

datA8aggnh = aggregate(datA8$nonHeating, by=list(datA8$date), FUN="sum",na.rm = TRUE)
colnames(datA8aggnh) <- c("date","nonHeating")
datA8aggh = aggregate(datA8$heating, by=list(datA8$date), FUN="sum",na.rm = TRUE)
colnames(datA8aggh) <- c("date","heating")
datA8agg = left_join(datA8aggh, datA8aggnh, by = "date")
datA8agg

datAJ.date = datA7agg$date
datAJ.nonHeating = datA7agg$nonHeating+datA8agg$nonHeating
datAJ.heating = datA7agg$heating+datA8agg$heating

datAJ <- data.frame(datAJ.date, datAJ.nonHeating, datAJ.heating)
colnames(datAJ) = c("date", "nonHeating", "heating")
datAJ

datAJ$total = datAJ$nonHeating+datAJ$heating
datAJ$temp = datJoinedDaily$dailyTemp[33:286]
datAJ

ggplot(data=datAJ, aes(x=date,y=heating))+geom_line()+labs(x="Date",y="Energy Consumption (kWh)",title="Heating energy Consumption")+geom_smooth()

datAJlong <- datAJ %>% pivot_longer(
  cols = c(heating, nonHeating),
  names_to = "type",
  values_to = "value"
)
datAJlong$type = factor(datAJlong$type, levels = c("heating", "nonHeating"))
datAJlong <- datAJlong %>% arrange(date, type)
datAJlong

a = -2000/9
b = 60000/9

##### Figure 3 #####
ggplot(data=datAJlong, aes(x=date))+geom_bar(stat = "identity", position = position_stack(reverse = TRUE), aes(y=value,fill = type))+
  geom_line(aes(y = temp*a+b), size = 2) +
  scale_y_continuous(
    name = "Energy Consumption (kWh)",
    sec.axis = sec_axis(~(.-b)/a, name = "Daily Average Temperature (C)")
  ) +
  labs(x="Date",title="Energy Consumption compared to Daily Temperature")
cor.test(x=datAJ$temp, y=datAJ$heating, method = "pearson")
##### Figure 3 #####


