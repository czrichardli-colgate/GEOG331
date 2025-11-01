#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("Z:\\cli2\\hw5_data\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)

#read in precipitation data
#hourly precipitation is in mm
datP <- read.csv("Z:\\cli2\\hw5_data\\2049867.csv")                            
head(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

#### define time for streamflow #####
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

#### define time for precipitation #####    
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

#### get decimal formats #####
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#calculate times for datP                       
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year),datP$year + (datP$decDay/366),
                       datP$year + (datP$decDay/365))

#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#####Q5#####
dat2017 <- subset(datD,year==2017)
ave2017 <- aggregate(dat2017$discharge, by=list(dat2017$doy), FUN="mean")
colnames(ave2017) <- c("doy","discharge")

ymin<-0
ymax<-max(c(aveF$dailyAve+sdF$dailySD, ave2017$discharge))

monthStarts <- yday(ymd(paste(2017,1:12,1,sep = "-")))
monthLabs <- month.abb

par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     ylim = c(ymin,ymax),
     xlab="Month", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA)#no border

axis(1, at = monthStarts,
     lab=monthLabs)
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)


lines(ave2017$doy,ave2017$discharge, lwd = 2, col = "#880000")

legend("topright", c("mean","1 standard deviation","2017"), #legend items
       lwd=c(2,NA,2),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2),"#880000"),#colors
       pch=c(NA,15,NA),#symbols
       bty="n")#no legend border
#####Q5#####

#####Q7#####

library(dplyr)

datP$ymd <- as.Date(dateP)
datPDaily <- aggregate(datP$HPCP, by=list(datP$ymd), FUN="length")
colnames(datPDaily) <- c("ymd","prcpCount")

datD$ymd <- as.Date(datesD)
datPDaily$prcpFull <- ifelse(datPDaily$prcpCount==24,TRUE,FALSE)
datPDaily

datD <- datD %>% left_join(datPDaily,by="ymd")
datD

plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))


points(datD$decYear[datD$prcpFull],datD$discharge[datD$prcpFull],col = "blue",pch = 16)

#####Q7#####


#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
  polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
            hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
          c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#####Q8#####
#subsest discharge and precipitation within range of interest
hydroDQ8 <- datD[datD$doy >= 350 & datD$doy < 351 & datD$year == 2007,]
hydroPQ8 <- datP[datP$doy >= 350 & datP$doy < 351 & datP$year == 2007,]

min(hydroDQ8$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroDQ8$discharge))-1
#ceiling rounds up to the integer
yh <- ceiling(max(hydroDQ8$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroPQ8$HPCP))+.5
#scale precipitation to fit on the 
hydroPQ8$pscale <- (((yh-yl)/(pm-pl)) * hydroPQ8$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroDQ8$decDay,
     hydroDQ8$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroPQ8)){
  polygon(c(hydroPQ8$decDay[i]-0.017,hydroPQ8$decDay[i]-0.017,
            hydroPQ8$decDay[i]+0.017,hydroPQ8$decDay[i]+0.017),
          c(yl,hydroPQ8$pscale[i],hydroPQ8$pscale[i],yl),
          col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}
#####Q8#####

library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
  geom_violin()

#####Q9#####

datD1617 <- subset(datD,year==2016|year==2017)
datD1617$quarter <- datD1617$year+quarter(datD1617$ymd)/10
#specify season as a factor
datD1617$seasonPlot <- as.factor(datD1617$quarter)

#make a violin plot
ggplot(data= datD1617, aes(seasonPlot,discharge)) + 
  geom_violin()

#####Q9#####