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

datJoinedDailyTemp = aggregate(as.numeric(gsub("°C","",datJoined$Temperature)), by=list(datJoined$date), FUN="mean",na.rm = TRUE)
colnames(datJoinedDailyTemp) <- c("date","dailyTemp")
datJoinedDailyTemp

library(dplyr)
datJoinedDaily = left_join(datJoinedDailyCO2,datJoinedDailyTemp,by = "date")
datJoinedDaily

library(ggplot2)
ggplot(data=datJoinedDaily,aes(x=dailyTemp,y=dailyCO2))+geom_point()+labs(x="Daily temperature (C)",y="Daily CO2 (ppm)",
        title="Correlation between daily temperature and daily CO2")+geom_smooth(method = "lm")

summary(lm(datJoinedDaily$dailyCO2~datJoinedDaily$dailyTemp))



#Monthly
datJoinedMonthlyCO2 = aggregate(as.numeric(gsub(" ppm","",datJoined$CO2)), by=list(datJoined$month), FUN="mean",na.rm = TRUE)
colnames(datJoinedMonthlyCO2) <- c("date","monthlyCO2")
datJoinedMonthlyCO2

datJoinedMonthlyTemp = aggregate(as.numeric(gsub("°C","",datJoined$Temperature)), by=list(datJoined$month), FUN="mean",na.rm = TRUE)
colnames(datJoinedMonthlyTemp) <- c("date","monthlyTemp")
datJoinedMonthlyTemp

library(dplyr)
datJoinedMonthly = left_join(datJoinedMonthlyCO2,datJoinedMonthlyTemp,by = "date")
datJoinedMonthly

library(ggplot2)
ggplot(data=datJoinedMonthly,aes(x=monthlyTemp,y=monthlyCO2))+geom_point()+labs(x="Monthly temperature (C)",y="Monthly CO2 (ppm)",
        title="Correlation between monthly temperature and monthly CO2")+geom_smooth(method = "lm")

summary(lm(datJoinedMonthly$monthlyCO2~datJoinedMonthly$monthlyTemp))
